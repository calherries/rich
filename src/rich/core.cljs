(ns rich.core
  (:require [clojure.walk :as walk]
            [clojure.zip :as zip]
            [hickory.core :as hick]
            [hickory.zip :as hzip]
            [clojure.string :as str]
            [applied-science.js-interop :as j]
            [com.wotbrew.relic :as rel]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(defn p [x] (def x x) (prn x) x)
(enable-console-print!)

(def state
  (r/atom {:content {:attrs   nil,
                     :content [{:attrs   {:style {:font-size "1em"
                                                  :color "brown"}},
                                :content ["Some text"],
                                :tag     :span,
                                :type    :element}
                               {:attrs   {:style {:font-size "1em"}},
                                :content ["More text"],
                                :tag     :span,
                                :type    :element}],
                     :tag     :div,
                     :type    :element}}))

;; (add-watch state :selection (fn [key ref old-state new-state] (js/console.log (select-keys new-state [:anchor :focus]))))


(comment
  (-> (hzip/hickory-zip (first (:content @state)))
      zip/next zip/next))

(defn dissoc-in
  "Dissociate a value in a nested associative structure, identified by a sequence
  of keys. Any collections left empty by the operation will be dissociated from
  their containing structures."
  ([m ks]
   (if-let [[k & ks] (seq ks)]
     (if (seq ks)
       (let [v (dissoc-in (get m k) ks)]
         (if (empty? v)
           (dissoc m k)
           (assoc m k v)))
       (dissoc m k))
     m))
  ([m ks & kss]
   (if-let [[ks' & kss] (seq kss)]
     (recur (dissoc-in m ks) ks' kss)
     (dissoc-in m ks))))

(defn at-path [path]
  (vec (interleave (repeat :content) path)))

(defn update-node [content path f]
  (if (seq path)
    (update-in content (at-path path) f)))

(defn update-text [content path f]
  (update-node content (conj path 0) f))

(comment
  (at-path [0 0])
  (vec (interpose :content [0 0 0]))
  [0 :content 0])

(defn insert-text [content {:keys [path text offset]}]
  (update-in content (at-path (conj path 0))
             (fn [old-text]
               (let [[before after] (split-at offset old-text)]
                 (str/join (concat before (seq text) after))))))

(comment
  (def content (:content @state))
  (insert-text content {:path [0 0] :text "...!" :offset 0}))

(defn as-hiccup [content]
  (walk/prewalk (fn [node]
                  (cond
                    (:tag node)
                    (-> [(:tag node)]
                        (cond-> (:attrs node) (conj (assoc (:attrs node) :data-rich-node true)))
                        (cond-> (:content node) (into (:content node))))
                    :else
                    node))
                content))

(defn find-rich-node [target]
  (.closest target "[data-rich-node]"))

(defn rich-node? [target]
  (.hasAttribute target "data-rich-node"))

(defn index-of-child [element]
  (.indexOf (array-seq (.-children (.-parentElement element))) element))

(defn path-to-node [element]
  (if (not (rich-node? element))
    []
    (into [(index-of-child element)] (path-to-node (.-parentElement element)))))

(comment
  (as-hiccup (:content @state)))

(defn rich-range-from-selection [{:keys [anchor focus]}]
  (let [start-point (min-key (fn [point] [(:path point) (:offset point)]) anchor focus)
        end-point   (if (= start-point anchor)
                      focus
                      anchor)]
    {:start-point start-point
     :end-point   end-point}))

(defn select-paths [m paths]
  (into {} (map (fn [p]
                  [(last p) (get-in m p)]))
        paths))

(defn get-selection []
  (let [selection (.getSelection js/window)]
    (when (.-anchorNode selection)
      {:anchor {:node   (.-anchorNode selection)
                :path   (path-to-node (.-parentElement (.-anchorNode selection)))
                :offset (.-anchorOffset selection)}
       :focus  {:node   (.-focusNode selection)
                :path   (path-to-node (.-parentElement (.-focusNode selection)))
                :offset (.-focusOffset selection)}})))

(defn zip-next-seq
  "Given a clojure.zip zipper location loc return a lazy sequence of all
  clojure.zip/next locations from loc."
  [loc]
  (if (zip/end? loc)
    ()
    (lazy-seq (cons loc (zip-next-seq (zip/next loc))))))

(defn nth-child-zip [zipper n]
  (nth (iterate zip/right (zip/down zipper)) n))

(defn get-in-zip [zipper path]
  (if (seq path)
    (get-in-zip (nth-child-zip zipper (first path)) (rest path))
    zipper))

(defn leaf-zips-after
  "Returns all leaf nodes after loc, inclusive."
  [loc]
  (filter (complement zip/branch?) ;filter only non-branch nodes
          (take-while (complement zip/end?) ;take until the :end
                      (iterate zip/next loc))))

(defn leaf-zips-before
  "Returns all leaf nodes before loc, inclusive."
  [loc]
  (filter (complement zip/branch?) ;filter only non-branch nodes
          (take-while (complement nil?) ;take until the root
                      (iterate zip/prev loc))))

(defn path-to-zipper [zipper]
  (if (nil? (zip/path zipper))
    []
    (conj (path-to-zipper (zip/up zipper)) (count (zip/lefts zipper)))))

(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) nil)
       (cons (first s) (take-until pred (rest s)))))))

(defn leaf-zips-between
  "Returns all leaf zippers between start-path and end-path, inclusive."
  [zipper start-path end-path]
  (take-until (fn [z] (= (path-to-zipper z) end-path)) (leaf-zips-after (get-in-zip zipper start-path))))

(defn paths-between
  "All paths between start-path and end-path"
  [zipper start-path end-path]
  (map path-to-zipper (leaf-zips-between zipper start-path end-path)))

(comment
  (zip/path (zip/vector-zip [1 [2 [0 1] 3] 4]))
  (nth-child-zip (zip/vector-zip [1 [2 3] 4]) 2)
  (list (zip-next-seq (zip/vector-zip [1 [2 [0 1] 3] 4])))
  (get-in [1 [2 [0 1] 3] 4] [1 1 0])
  (get-in-zip (zip/vector-zip [1 [2 [0 1] 3] 4]) [1 1 0])
  (leaf-zips-before (get-in-zip (zip/vector-zip [1 [2 [0 1] 3] 4]) [1 1 0]))
  (leaf-zips-after (get-in-zip (zip/vector-zip [1 [2 [0 1] 3] 4]) [1 1 0]))
  (path-to-zipper (get-in-zip (zip/vector-zip [1 [2 [0 1] 3] 4]) [1 1 0]))
  (leaf-zips-between (zip/vector-zip [1 [2 [0 1] 3] 4]) [1 1 0] [1 2])
  (paths-between (zip/vector-zip [1 [2 [0 1] 3] 4]) [1 1 0] [1 2])
  )

(comment
  (dissoc-in-content (:content @state) [0]))

(defn set-in-range [content range path value])

(defn move-range [content {:keys [path offset] :as range} distance]
  )

(defn backwards-selection? [{:keys [anchor focus]}]
  (pos? (compare (conj (:path anchor) (:offset anchor)) (conj (:path focus) (:offset focus)))))

(defn delete-backwards [content {:keys [path offset unit]}]
  (if (= offset 0)
    (delete-backwards content {:path nil})
    (update-in content (p (into (at-path path) [:content 0]))
               (fn [old-text]
                 (let [[before after] (split-at offset old-text)]
                   (str/join (concat (str/join (butlast (seq before))) after)))))))

(comment
  (get-in (:content @state) [0 ])
  (let [{:keys [content]} @state
        {:keys [start-point end-point] :as range} (rich-range-from-selection @state)
        #_elements-in-range #_(elements-in-range content range)]
    (get-in content (at-path (:path start-point))))
  )

(defn selection? [{:keys [anchor focus]}]
  (not= anchor focus))

(defn vec-remove
  [coll pos]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn dissoc-in-remove
  "Like dissoc-in, but works with vectors too"
  [data path]
  (cond
    (seq (rest path))
    (if (contains? data (first path))
      (assoc data (first path) (dissoc-in-remove (get data (first path)) (rest path)))
      data)
    (first path)
    (if (vector? data)
      (vec-remove data (first path))
      (dissoc data (first path)))

    :else
    data))

(defn dissoc-in-content
  [content path]
  (dissoc-in-remove content (at-path path)))

(defn delete-range [content selection]
  (let [{:keys [start-point
                end-point]} (rich-range-from-selection selection)
        paths           (->> (paths-between (hzip/hickory-zip content) (:path start-point) (:path end-point))
                             butlast
                             rest)
        content (if (= (:path start-point) (:path end-point))
                  (update-text content (:path start-point) (fn [s] (str (subs s 0 (:offset start-point))
                                                                        (subs s (:offset end-point)))))
                  (-> content
                      (update-text (:path start-point) (fn [s] (subs s 0 (:offset start-point))))
                      (update-text (:path end-point) (fn [s] (subs s (:offset end-point))))))]
    (reduce dissoc-in-content content paths)))

(comment
  (vec-remove [1 2 3] 2)
  (dissoc-in-remove {:a [1 2 3]} [:a 0])
  (delete-range (:content @state) @state)
  (update-node (:content @state) [0 0] (fn [s] "Here"))
  (rest '())
  (contains? [1 2 3] 0)
  )

(defn editable []
  (let [on-selection-change (fn []
                              (when-let [selection (get-selection)]
                                (let [selection-values (fn [s]
                                                         (select-paths s [[:anchor :path] [:anchor :offset]
                                                                          [:focus :path] [:focus :offset]]))]
                                  (when (not= (selection-values selection) (selection-values @state))
                                    (swap! state merge selection)))))]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (js/window.document.addEventListener "selectionchange" on-selection-change))
      :component-will-unmount
      (fn [this]
        (js/document.removeEventListener "selectionchange" on-selection-change))
      :component-did-update
      (fn [this]
        ;; Check whether DOM selection is out of sync
        (when-let [selection (get-selection)]
          (let [selection-values (fn [s]
                                   (select-paths s [[:anchor :path] [:anchor :offset]
                                                    [:focus :path] [:focus :offset]]))]
            ;; DOM selection is out of sync, so update it.
            (when (not= (selection-values selection) (selection-values @state))
              (let [{:keys [start-point end-point]} (rich-range-from-selection @state)
                    dom-range     (js/window.document.createRange)
                    dom-selection (js/window.getSelection)]
                (.setStart dom-range (:node start-point) (:offset start-point))
                (.setEnd dom-range (:node end-point) (:offset end-point))
                (.setBaseAndExtent dom-selection
                                   (.-startContainer dom-range)
                                   (.-startOffset dom-range)
                                   (.-endContainer dom-range)
                                   (.-endOffset dom-range)))))))
      :reagent-render
      (fn []
        (let [content (:content @state)]
          [:div
           {:content-editable                  true
            :suppress-content-editable-warning true
            ;; :on-key-down  (fn [e]
            ;;                 (when (and (= (.-key e) "b") (.-metaKey e))
            ;;                   (.preventDefault e)
            ;;                   (swap! state (fn [state]
            ;;                                  (let [selection-range (rich-range-from-selection state)]
            ;;                                    (update state :content set-in-range selection-range [:style :font-size] "bold"))))))
            :on-before-input
            (fn [e]
              (.preventDefault e)
              (js/console.log "on-before-input")
              (let [text (.-data e)]
                (swap! state (fn [state]
                               (-> state
                                   (update :content insert-text {:text   text
                                                                 :path   (get-in state [:focus :path])
                                                                 :offset (get-in state [:focus :offset])})
                                   (update-in [:anchor :offset] + (count text))
                                   (update-in [:focus :offset] + (count text)))))))
            ;; on-input fires when the delete key is pressed
            :on-input (fn [e]
                        (.preventDefault e)
                        (swap! state (fn [state]
                                       (if (selection? state)
                                         (let [state (update state :content delete-range state)]
                                           (if (backwards-selection? state)
                                             (assoc state :anchor (:focus state))
                                             (assoc state :focus (:anchor state))))
                                         (-> state
                                             (update :content delete-backwards {:unit   "char"
                                                                                :path   (get-in state [:focus :path])
                                                                                :offset (get-in state [:focus :offset])})
                                             (update-in [:anchor :offset] - 1)
                                             (update-in [:focus :offset] - 1))))))}
           (into (as-hiccup content))]))})))

(def parsed-doc (hick/parse-fragment (.-outerHTML (js/document.getElementById "app"))))

(defn app []
  [editable])

(defn ^:dev/after-load start []
  (js/console.log "Starting...")
  (rdom/render [app] (js/document.getElementById "app")))

(defn ^:export init []
  (start))

(comment
  (insert-text content {:path [0 0 0] :text "...."})
  (update-node content [0 0] (fn [content]
                               (update content :text #(str % ".....")))))
