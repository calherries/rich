(ns rich.core
  (:require [applied-science.js-interop :as j]
            [clojure.data :as data]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(defn p [x] (def x x) (prn x) x)
(enable-console-print!)

(def state
  (r/atom {:content {:attrs   nil,
                     :content [{:attrs   {:style {:font-size "1em"}},
                                :content ["Type something awesome"],
                                :tag     :span,
                                :type    :element}],
                     :tag     :div,
                     :type    :element}}))

(defn hickory-zip
  "Returns a zipper for html dom maps (as from as-hickory),
  given a root element."
  [root]
  (zip/zipper (fn [node]
                (and (not (string? node))
                     (not (string? (first (:content node))))))
              (comp seq :content)
              (fn [node children]
                (assoc node :content (and children (apply vector children))))
              root))

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

(defn insert-text [content {:keys [path text offset]}]
  (update-text content path
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
                        (cond-> (:content node) (into (if (= (:content node) [""])
                                                        ["\uFEFF"]
                                                        (:content node)))))
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
  (let [[start-point end-point] (if (= (compare [(:path anchor) (:offset anchor)]
                                                [(:path focus) (:offset focus)])
                                       -1)
                                  [anchor focus]
                                  [focus anchor])]
    {:start-point start-point
     :end-point   end-point}))

(defn select-paths [m paths]
  (into {} (map (fn [p]
                  [(last p) (get-in m p)]))
        paths))

(defn get-selection []
  (let [selection (.getSelection js/window)]
    (when (and (.-anchorNode selection)
               (.closest (.-parentElement (.-anchorNode selection)) "#rich-editable"))
      {:anchor {:path   (path-to-node (.-parentElement (.-anchorNode selection)))
                :offset (.-anchorOffset selection)}
       :focus  {:path   (path-to-node (.-parentElement (.-focusNode selection)))
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
  (filter (complement zip/branch?) ; filter only leaf nodes
          (take-while (complement zip/end?) ;take until the :end
                      (iterate zip/next loc))))

(defn leaf-zips-before
  "Returns all leaf nodes before loc, inclusive."
  [loc]
  (filter (complement zip/branch?) ; filter only leaf nodes
          (take-while (complement nil?) ;take until the root
                      (iterate zip/prev loc))))

(defn next-leaf [node]
  (second (leaf-zips-after node)))

(defn prev-leaf [node]
  (second (leaf-zips-before node)))

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

(defn backwards-selection? [{:keys [anchor focus]}]
  (pos? (compare (conj (:path anchor) (:offset anchor)) (conj (:path focus) (:offset focus)))))

(defn delete-backwards [state {:keys [path offset unit]}]
  (if (= offset 0)
    (let [prev-zip (second (leaf-zips-before (get-in-zip (hickory-zip (:content state)) path)))]
      (if (nil? prev-zip)
        state ; Do nothing, we are at the beginning of the first leaf.
        (let [prev-path    (path-to-zipper prev-zip)
              prev-node    (zip/node prev-zip)
              text-content (first (:content prev-node))]
          (delete-backwards (-> state
                                (assoc :anchor {:path   prev-path
                                                :offset (count text-content)})
                                (assoc :focus {:path   prev-path
                                               :offset (count text-content)}))
                            {:path   prev-path
                             :offset (count text-content)
                             :unit   unit}))))
    (-> state
        (assoc-in [:anchor :offset] (dec offset))
        (assoc-in [:focus :offset] (dec offset))
        (update :content update-text path
                (fn [old-text]
                  (let [[before after] (split-at offset old-text)]
                    (str/join (concat (str/join (butlast (seq before))) after))))))))

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
        paths           (->> (paths-between (hickory-zip content) (:path start-point) (:path end-point))
                             butlast
                             rest)
        content (if (= (:path start-point) (:path end-point))
                  (update-text content (:path start-point) (fn [s] (str (subs s 0 (:offset start-point))
                                                                        (subs s (:offset end-point)))))
                  (-> content
                      (update-text (:path start-point) (fn [s] (subs s 0 (:offset start-point))))
                      (update-text (:path end-point) (fn [s] (subs s (:offset end-point))))))]
    (reduce dissoc-in-content content paths)))

(defn delete-selection [state]
  (let [state (update state :content delete-range state)]
    (if (backwards-selection? state)
      (assoc state :anchor (:focus state))
      (assoc state :focus (:anchor state)))))

(defn get-editor []
  (js/document.getElementById "rich-editable"))

(defn find-dom-node [path]
  (let [editor     (get-editor)
        loop-fn    (fn loop-fn [node path]
                     (if (seq path)
                       (loop-fn (aget (.-childNodes node) (first path))
                                (rest path))
                       node))
        start-node (-> editor .-childNodes (aget 0))]
    (loop-fn start-node path)))

(defn update-range [state update-fn]
  (let [content                         (:content state)
        {:keys [start-point end-point]} (rich-range-from-selection state)]
    (if (= (:path start-point) (:path end-point))
      (let [content-zip       (hickory-zip content)
            original-node-zip (get-in-zip content-zip (:path start-point))
            original-node     (zip/node original-node-zip)
            original-text     (first (:content original-node))
            before-text       (subs original-text 0 (:offset start-point))
            split-text        (subs original-text (:offset start-point) (:offset end-point))
            split-node        (update-fn (assoc original-node :content [split-text]))
            after-text        (subs original-text (:offset end-point))
            split-node-zip    (if (not-empty before-text)
                                (-> original-node-zip
                                    (zip/replace (assoc original-node :content [before-text]))
                                    (zip/insert-right split-node)
                                    (zip/right))
                                (-> original-node-zip
                                    (zip/replace split-node)))
            content           (-> split-node-zip
                                  (cond-> (not-empty after-text)
                                    (zip/insert-right (assoc original-node :content [after-text])))
                                  (zip/root))
            start-point       {:path   (path-to-zipper split-node-zip)
                               :offset 0}
            end-point         {:path   (path-to-zipper split-node-zip)
                               :offset (count split-text)}
            [anchor focus]    (if (backwards-selection? state)
                                [end-point start-point]
                                [start-point end-point])]
        (-> state
            (merge {:content content
                    :anchor  anchor
                    :focus   focus})))
      ;; if start and end are on different nodes
      (let [content              (assoc-in content (conj (at-path (:path end-point)) :end-point-flag) true)
            content-zip          (hickory-zip content)
            start-node-zip       (get-in-zip content-zip (:path start-point))
            start-node           (zip/node start-node-zip)
            start-text           (first (:content start-node))
            start-before-text    (subs start-text 0 (:offset start-point))
            start-split-text     (subs start-text (:offset start-point))
            start-split-node     (update-fn (assoc start-node :content [start-split-text]))
            start-split-node-zip (if (not-empty start-before-text)
                                   (-> start-node-zip
                                       (zip/replace (assoc start-node :content [start-before-text]))
                                       (zip/insert-right start-split-node)
                                       (zip/right))
                                   (-> start-node-zip
                                       (zip/replace start-split-node)))
            end-split-node-zip   (loop [node start-split-node-zip]
                                   (if (:end-point-flag (zip/node node))
                                     (zip/edit node dissoc :end-point-flag)
                                     (let [next-node (next-leaf node)]
                                       (recur (cond-> next-node
                                                (nil? (:end-point-flag (zip/node next-node)))
                                                (zip/edit update-fn))))))
            end-split-node       (zip/node end-split-node-zip)
            end-text             (first (:content end-split-node))
            end-split-text       (subs end-text 0 (:offset end-point))
            end-after-text       (subs end-text (:offset end-point))
            content              (-> end-split-node-zip
                                     (zip/edit update-fn)
                                     (zip/edit assoc :content [end-split-text])
                                     (cond-> (not-empty end-after-text)
                                       (zip/insert-right (assoc end-split-node :content [end-after-text])))
                                     (zip/root))
            start-point          {:path   (path-to-zipper start-split-node-zip)
                                  :offset 0}
            end-point            {:path   (path-to-zipper end-split-node-zip)
                                  :offset (count end-split-text)}
            [anchor focus]    (if (backwards-selection? state)
                                [end-point start-point]
                                [start-point end-point])]
        (-> state
            (merge {:content content
                    :anchor  anchor
                    :focus   focus}))))))

(defn things-in-both
  "Recursively diffs a and b to find the common values. Maps are subdiffed where keys match and values differ."
  [a b]
  (nth (data/diff a b) 2))

(defn universal-leaf-attrs [content start-path stop-path]
  (let [leaf-nodes (->> (leaf-zips-between (hickory-zip content) start-path stop-path)
                        (map (comp :attr zip/node)))]
    (reduce things-in-both leaf-nodes)))

(defn universal-leaf-attrs-in-selection [state]
  (let [content (:content state)
        {:keys [start-point end-point]} (rich-range-from-selection state)]
    (universal-leaf-attrs content start-point end-point)))

;; (defn universal-block-props [node start-path stop-path]
;;   (let [block-nodes (->> (block-zips-between node start-path stop-path)
;;                          (map :attr))]
;;     (reduce things-in-both block-nodes)))

(defn editable []
  (let [on-selection-change (fn []
                              (when-let [selection (get-selection)]
                                (let [selection-values (fn [s]
                                                         (select-paths s [[:anchor :path] [:anchor :offset]
                                                                          [:focus :path] [:focus :offset]]))]
                                  (when (not= (selection-values selection) (selection-values @state))
                                    (swap! state merge selection)))))
        on-before-input     (fn [e]
                              (.preventDefault e)
                              (let [text (.-data e)
                                    type (.-inputType e)]
                                (case type
                                  "insertText"
                                  (swap! state (fn [state]
                                                 (-> state
                                                     (update :content insert-text {:text   text
                                                                                   :path   (get-in state [:focus :path])
                                                                                   :offset (get-in state [:focus :offset])})
                                                     (update-in [:anchor :offset] + (count text))
                                                     (update-in [:focus :offset] + (count text)))))
                                  "insertParagraph"
                                  (swap! state (fn [state]
                                                 (-> state
                                                     (update :content insert-text {:text   "\n"
                                                                                   :path   (get-in state [:focus :path])
                                                                                   :offset (get-in state [:focus :offset])})
                                                     (update-in [:anchor :offset] inc)
                                                     (update-in [:focus :offset] inc))))
                                  "deleteContentBackward"
                                  (swap! state (fn [state]
                                                 (if (selection? state)
                                                   (delete-selection state)
                                                   (delete-backwards state {:unit   "char"
                                                                            :path   (get-in state [:focus :path])
                                                                            :offset (get-in state [:focus :offset])}))))

                                  "deleteSoftLineBackward"
                                  (swap! state (fn [state]
                                                 (if (selection? state)
                                                   (delete-selection state)
                                                   (delete-backwards state {:unit   "char"
                                                                            :path   (get-in state [:focus :path])
                                                                            :offset (get-in state [:focus :offset])}))))

                                  "deleteWordBackward"
                                  (swap! state (fn [state]
                                                 (if (selection? state)
                                                   (delete-selection state)
                                                   (delete-backwards state {:unit   "char"
                                                                            :path   (get-in state [:focus :path])
                                                                            :offset (get-in state [:focus :offset])})))))))]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (js/window.document.addEventListener "selectionchange" on-selection-change)
        (.addEventListener (rdom/dom-node this) "beforeinput" on-before-input))
      :component-will-unmount
      (fn [this]
        (js/document.removeEventListener "selectionchange" on-selection-change)
        (.removeEventListener (rdom/dom-node this) "beforeinput" on-before-input))
      :component-did-update
      (fn [this]
        (when-let [selection (get-selection)]
          (let [selection-values (fn [s]
                                   (select-paths s [[:anchor :path] [:anchor :offset]
                                                    [:focus :path] [:focus :offset]]))]
            ;; DOM selection is out of sync, so update it.
            (when (not= (selection-values selection) (selection-values @state))
              (let [{:keys [start-point end-point]} (rich-range-from-selection @state)
                    dom-range     (js/window.document.createRange)
                    dom-selection (js/window.getSelection)]
                ;; WIP: need to be able to get node from point
                (.setStart dom-range (find-dom-node (conj (:path start-point) 0)) (:offset start-point))
                (.setEnd dom-range (find-dom-node (conj (:path end-point) 0)) (:offset end-point))
                (.setBaseAndExtent dom-selection
                                   (.-startContainer dom-range)
                                   (.-startOffset dom-range)
                                   (.-endContainer dom-range)
                                   (.-endOffset dom-range)))))))
      :reagent-render
      (fn []
        (let [content (:content @state)]
          [:div
           {:id                                "rich-editable"
            :content-editable                  true
            :suppress-content-editable-warning true
            :width                             "100%"
            :height                            "100%"
            :on-key-down  (fn [e]
                            (when (and (= (.-key e) "b") (.-metaKey e))
                              (.preventDefault e)
                              (swap! state (fn [state]
                                             (update-range state (fn [node]
                                                                   (if (= (get-in node [:attrs :style :font-weight]) "bold")
                                                                     (dissoc-in node [:attrs :style :font-weight])
                                                                     (assoc-in node [:attrs :style :font-weight] "bold"))))))))
            :on-paste                          (fn [e]
                                                 (.preventDefault e)
                                                 (let [text (-> e .-clipboardData (.getData "Text"))]
                                                   (swap! state
                                                          (fn [state]
                                                            (-> state
                                                                (cond-> (selection? state) delete-selection)
                                                                (update :content insert-text {:text   text
                                                                                              :path   (get-in state [:focus :path])
                                                                                              :offset (get-in state [:focus :offset])})
                                                                (update-in [:anchor :offset] + (count text))
                                                                (update-in [:focus :offset] + (count text)))))))}
           (into (as-hiccup content))]))})))

(comment
  (insert-text content {:path [0 0 0] :text "...."})
  (update-node content [0 0] (fn [content]
                               (update content :text #(str % ".....")))))
