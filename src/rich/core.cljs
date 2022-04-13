(ns rich.core
  (:require [applied-science.js-interop :as j]
            [clojure.data :as data]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [hyperfiddle.rcf :refer [tests]]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(defn p [x]
  (def x x)
  (prn x)
  x)

;;;;;;;;;;;;;;;;
;; SEQ

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

;;;;;;;;;;;;;;;;;;;;
;; DEEPLY NESTED DATA

(defn lexicographic-less-than
  "Like <, but also compares (deeply-nested) vectors lexicographically."
  [a b]
  (= (compare a b) -1))

(defn vec-remove
  "Removes the element in this vector at the given index."
  [v index]
  (into (subvec v 0 index) (subvec v (inc index))))

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

(defn dissoc-in-remove
  "Like dissoc-in, but works with vectors too."
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

(defn select-paths
  "Like select-keys, but with paths instead of keys"
  [m paths]
  (into {}
        (map (fn [p]
               [(last p) (get-in m p)]))
        paths))

(defn update-path-with-delete
  "Updates this path, assuming a node was deleted at delete-path."
  [path delete-path]
  (if (= (count delete-path) 1)
    (if (< (first delete-path)
           (first path))
      (update path 0 dec)
      path)
    (into [(first path)] (update-path-with-delete (vec (rest path))
                                                  (vec (rest delete-path))))))

(defn update-path-with-insert-right
  "Updates this path, assuming a node was inserted at insert-path."
  [path insert-path]
  (if (= (count insert-path) 1)
    (if (< (first insert-path)
           (first path))
      (update path 0 inc)
      path)
    (into [(first path)] (update-path-with-insert-right (vec (rest path))
                                                        (vec (rest insert-path))))))

(defn path-left
  "The path to the left of the given path. If there is no node to the left, return nil."
  [path]
  (if (zero? (last path))
    nil
    (conj (vec (butlast path)) (dec (last path)))))

(defn path-right
  "The path to the right of the given path."
  [path]
  (if (zero? (last path))
    nil
    (conj (vec (butlast path)) (inc (last path)))))

(tests
  (update-path-with-delete [0 2] [0 0]) := [0 1]
  (update-path-with-insert-right [0 2] [0 1]) := [0 3]
  )

;;;;;;;;;;;;;;;;
;; HICKORY

(defn hickory-zip
  "Returns a zipper for hickory maps given a root element."
  [root]
  (zip/zipper (fn [node]
                (and (not (string? node))
                     (not (string? (first (:content node))))))
              (comp seq :content)
              (fn [node children]
                (assoc node :content (and children (apply vector children))))
              root))

(defn hickory-path
  "Returns the path to a node in a hickory map, given a path that
   assumes nodes are replaced by their content."
  [path]
  (vec (interleave (repeat :content) path)))

(defn hickory-update-in
  "Updates a node at this path with a function f."
  [hickory path f]
  (when (seq path)
    (update-in hickory (hickory-path path) f)))

(defn hickory-dissoc-in
  [hickory path]
  (dissoc-in-remove hickory (hickory-path path)))

(defn hickory-update-text
  "Updates a node's text hickory with a function f."
  [hickory path f]
  (hickory-update-in hickory (conj path 0) f))

(defn hickory-insert-text
  "Inserts text at this path and an offset number of characters"
  [hickory {:keys [path text offset]}]
  (hickory-update-text hickory
                       path
                       (fn [old-text]
                         (let [[before after] (split-at offset old-text)]
                           (str/join (concat before (seq text) after))))))

(defn hickory->hiccup
  "Converts a hickory map to hiccup."
  [hickory]
  (walk/prewalk (fn [node]
                  (cond
                    ;; COMPAT: Browsers will collapse trailing new lines at the end of blocks,
                    ;; so we need to add an extra trailing new lines to prevent that.
                    (and (string? node)
                         (= (last node) "\n"))
                    (str node "\n")

                    ;; If the node is a span with no attributes, replace it with its text content.
                    (and (= (:tag node) :span)
                         (empty? (:attrs node)))
                    (first (:content node))

                    (:tag node)
                    (-> [(:tag node)]
                        (cond-> (not-empty (:attrs node))
                          (conj (:attrs node)))
                        (cond-> (:content node)
                          (into (:content node))))

                    :else
                    node))
                hickory))

;;;;;;;;;;;;;;;;;;;;
;; ZIP UTILS

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
  (filter (complement zip/branch?)
          (take-while (complement zip/end?)
                      (iterate zip/next loc))))

(defn leaf-zips-before
  "Returns all leaf nodes before loc, inclusive."
  [loc]
  (filter (complement zip/branch?)
          (take-while (complement nil?)
                      (iterate zip/prev loc))))

(defn next-leaf [node]
  (second (leaf-zips-after node)))

(defn prev-leaf [node]
  (second (leaf-zips-before node)))

(defn path-to-zip [zipper]
  (if (nil? (zip/path zipper))
    []
    (conj (path-to-zip (zip/up zipper)) (count (zip/lefts zipper)))))

(defn leaf-zips-between
  "Returns all leaf zippers between start-path and end-path, inclusive."
  [zipper start-path end-path]
  (take-until (fn [z] (= (path-to-zip z) end-path)) (leaf-zips-after (get-in-zip zipper start-path))))

(defn paths-between
  "All paths between start-path and end-path"
  [zipper start-path end-path]
  (map path-to-zip (leaf-zips-between zipper start-path end-path)))

(tests
 (def example-zip (zip/vector-zip [1 [2 [0 1] 3] 4]))
 (path-to-zip example-zip) := []
 (zip/node (nth-child-zip example-zip 2)) := 4
 (zip/node (get-in-zip example-zip [1 1 0])) := 0
 (mapv zip/node (leaf-zips-before (get-in-zip example-zip [1 1 0]))) := [0 2 1]
 (mapv zip/node (leaf-zips-after (get-in-zip example-zip [1 1 0]))) := [0 1 3 4]
 (path-to-zip (get-in-zip example-zip [1 1 0])) := [1 1 0]
 (mapv zip/node (leaf-zips-between example-zip [1 1 0] [1 2])) := [0 1 3]
 (paths-between example-zip [1 1 0] [1 2]) := '([1 1 0] [1 1 1] [1 2])
)

;;;;;;;;;;;;;;;;;;
;; EDITABLE STATE

(defn range-from-selection
  "Return a range (start and end) from this selection (anchor and focus),
  where the start and end points are ordered lexicographically."
  [{:keys [anchor focus]}]
  (let [[start-point end-point] (if (lexicographic-less-than [(:path anchor) (:offset anchor)]
                                                             [(:path focus) (:offset focus)])
                                  [anchor focus]
                                  [focus anchor])]
    {:start-point start-point
     :end-point   end-point}))

(defn backwards-selection?
  "Returns true if this selection is backwards, meaning that it's anchor is before it's focus."
  [{:keys [anchor focus]}]
  (lexicographic-less-than [(:path focus) (:offset focus)]
                           [(:path anchor) (:offset anchor)]))

(defn delete-backwards
  "Returns a new state after deleting a unit of characters starting from a given point."
  [state {:keys [path offset unit]}]
  (if (= offset 0)
    ;; We are deleting from the start of the leaf, so we need to delete into the previous leaf
    (let [prev-zip (second (leaf-zips-before (get-in-zip (hickory-zip (:content state)) path)))]
      (if (nil? prev-zip)
        ;; Do nothing. We are at the first leaf, and we can't delete further
        state
        ;; Delete backwards from the end of the previous leaf.
        (let [prev-path    (path-to-zip prev-zip)
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
        (update :content hickory-update-text path
                (fn [old-text]
                  (let [[before after] (split-at offset old-text)]
                    (str/join (concat (str/join (butlast (seq before))) after))))))))

(defn selection?
  "Returns true if there is a selection of text."
  [{:keys [anchor focus]}]
  (not= anchor focus))

(defn delete-range
  "Deletes the characters in the current selection."
  [content selection]
  (let [{:keys [start-point
                end-point]} (range-from-selection selection)
        paths           (->> (paths-between (hickory-zip content) (:path start-point) (:path end-point))
                             butlast
                             rest)
        content (if (= (:path start-point) (:path end-point))
                  (hickory-update-text content (:path start-point) (fn [s] (str (subs s 0 (:offset start-point))
                                                                                (subs s (:offset end-point)))))
                  (-> content
                      (hickory-update-text (:path start-point) (fn [s] (subs s 0 (:offset start-point))))
                      (hickory-update-text (:path end-point) (fn [s] (subs s (:offset end-point))))))]
    (reduce hickory-dissoc-in content paths)))

(defn delete-selection
  "Deletes the text in the current selection."
  [state]
  (let [state (update state :content delete-range state)]
    (if (backwards-selection? state)
      (assoc state :anchor (:focus state))
      (assoc state :focus (:anchor state)))))

(defn mergeable?
  "Returns true if two nodes are equivalent except for their content."
  [node-a node-b]
  (= (dissoc node-a :content)
     (dissoc node-b :content)))

(defn merge-right
  "Merges the current node with the node to the right."
  [zip]
  (let [next-zip  (zip/right zip)
        zip-text  (get-in (zip/node zip) [:content 0])
        next-text (get-in (zip/node next-zip) [:content 0])]
    (-> zip
        (zip/edit (fn [node]
                    (assoc-in node [:content 0] (str zip-text next-text))))
        (next-leaf)
        (zip/remove))))

(defn merge-between
  "Merges adjacent nodes between given start and end paths (inclusive), when adjacent nodes are mergeable."
  [{:keys [content anchor focus] :as state} start-path end-path]
  (let [content-zip                (hickory-zip content)
        start-node-zip             (get-in-zip content-zip start-path)
        {:keys [zip anchor focus]} (loop [{:keys [zip anchor focus end-path]} {:zip      start-node-zip
                                                                               :anchor   anchor
                                                                               :focus    focus
                                                                               :end-path end-path}]
                                     (if (lexicographic-less-than end-path (path-to-zip zip))
                                       {:zip    zip
                                        :anchor anchor
                                        :focus  focus}
                                       (if (nil? (zip/right zip))
                                         (if-let [next-zip (next-leaf zip)]
                                           (recur {:zip      next-zip
                                                   :anchor   anchor
                                                   :focus    focus
                                                   :end-path end-path})
                                           {:zip    zip
                                            :anchor anchor
                                            :focus  focus})
                                         (let [next-zip (zip/right zip)]
                                           (recur (if (mergeable? (zip/node zip)
                                                                  (zip/node next-zip))
                                                    (let [path-to-delete (path-to-zip zip)
                                                          text-length    (count (get-in (zip/node zip) [:content 0]))
                                                          zip            (merge-right zip)
                                                          zip            (if (zip/left zip)
                                                                           (zip/left zip)
                                                                           zip)
                                                          end-path       (if (= end-path path-to-delete)
                                                                           (path-right end-path)
                                                                           end-path)
                                                          end-path       (update-path-with-delete end-path path-to-delete)]
                                                      {:zip      zip
                                                       :anchor   (-> anchor
                                                                     (cond-> (= (:path anchor) (path-to-zip next-zip))
                                                                       (update :offset #(+ % text-length)))
                                                                     (update :path update-path-with-delete path-to-delete))
                                                       :focus    (-> focus
                                                                     (cond-> (= (:path focus) (path-to-zip next-zip))
                                                                       (update :offset #(+ % text-length)))
                                                                     (update :path update-path-with-delete path-to-delete))
                                                       :end-path end-path})
                                                    {:zip      next-zip
                                                     :anchor   anchor
                                                     :focus    focus
                                                     :end-path end-path}))))))]
    (assoc state
           :content (zip/root zip)
           :anchor anchor
           :focus focus)))

(defn update-selection
  "Updates all the nodes in the current selected range with a function."
  [state update-fn]
  (let [content                         (:content state)
        {:keys [start-point end-point]} (range-from-selection state)]
    (if (= (:path start-point) (:path end-point))
      (let [content-zip        (hickory-zip content)
            original-zip       (get-in-zip content-zip (:path start-point))
            original-node      (zip/node original-zip)
            original-text      (first (:content original-node))
            before-text        (subs original-text 0 (:offset start-point))
            split-text         (subs original-text (:offset start-point) (:offset end-point))
            split-node         (update-fn (assoc original-node :content [split-text]))
            after-text         (subs original-text (:offset end-point))
            split-zip          (if (not-empty before-text)
                                 (-> original-zip
                                     (zip/replace (assoc original-node :content [before-text]))
                                     (zip/insert-right split-node)
                                     (zip/right))
                                 (-> original-zip
                                     (zip/replace split-node)))
            content            (-> split-zip
                                   (cond-> (not-empty after-text)
                                     (zip/insert-right (assoc original-node :content [after-text])))
                                   (zip/root))
            start-point        {:path   (path-to-zip split-zip)
                                :offset 0}
            end-point          {:path   (path-to-zip split-zip)
                                :offset (count split-text)}
            [anchor focus]     (if (backwards-selection? state)
                                 [end-point start-point]
                                 [start-point end-point])
            state              (-> state
                                   (merge {:content content
                                           :anchor  anchor
                                           :focus   focus}))
            path-left-of-start (path-left (:path start-point))]
        (merge-between state (or path-left-of-start (:path start-point)) (:path end-point)))
      ;; If start and end are on different nodes
      (let [content-zip       (hickory-zip content)
            start-zip          (get-in-zip content-zip (:path start-point))
            start-node         (zip/node start-zip)
            start-text         (first (:content start-node))
            start-before-text  (subs start-text 0 (:offset start-point))
            start-split-text   (subs start-text (:offset start-point))
            start-split-node   (update-fn (assoc start-node :content [start-split-text]))
            [start-split-zip
             end-point]        (if (not-empty start-before-text)
                                 [(-> start-zip
                                      (zip/replace (assoc start-node :content [start-before-text]))
                                      (zip/insert-right start-split-node)
                                      (zip/right))
                                  (update end-point :path update-path-with-insert-right (path-to-zip start-zip))]
                                 [(-> start-zip
                                      (zip/replace start-split-node))
                                  end-point])
            end-split-zip      (loop [zip start-split-zip]
                                 (if (= (path-to-zip zip) (:path end-point))
                                   zip
                                   (let [next-zip (next-leaf zip)]
                                     (recur (cond-> next-zip
                                              (not (= (path-to-zip next-zip) (:path end-point)))
                                              (zip/edit update-fn))))))
            end-split-node     (zip/node end-split-zip)
            end-text           (first (:content end-split-node))
            end-split-text     (subs end-text 0 (:offset end-point))
            end-after-text     (subs end-text (:offset end-point))
            end-split-zip      (-> end-split-zip
                                   (zip/edit update-fn)
                                   (zip/edit assoc :content [end-split-text]))
            end-split-zip      (if (not-empty end-after-text)
                                 (zip/insert-right end-split-zip (assoc end-split-node :content [end-after-text]))
                                 end-split-zip)
            content            (zip/root end-split-zip)
            start-point        {:path   (path-to-zip start-split-zip)
                                :offset 0}
            end-point          {:path   (:path end-point)
                                :offset (count end-split-text)}
            [anchor focus]     (if (backwards-selection? state)
                                 [end-point start-point]
                                 [start-point end-point])
            path-left-of-start (path-left (:path start-point))
            start-path         (or path-left-of-start (:path start-point))]
        (-> state
            (merge {:content content
                    :anchor  anchor
                    :focus   focus})
            (merge-between start-path (:path end-point)))))))

(defn things-in-both
  "Recursively diffs a and b to find the common values. Maps are subdiffed where keys match and values differ."
  [a b]
  (nth (data/diff a b) 2))

(defn universal-leaf-attrs
  "Returns the common attributes of all nodes between two paths."
  [content start-path end-path]
  (let [leaf-nodes (->> (leaf-zips-between (hickory-zip content) start-path end-path)
                        (map (comp :attrs zip/node)))]
    (reduce things-in-both leaf-nodes)))

(defn universal-leaf-attrs-in-selection
  "Returns the common attributes of all nodes in the selection."
  [state]
  (let [content                         (:content state)
        {:keys [start-point end-point]} (range-from-selection state)]
    (universal-leaf-attrs content (:path start-point) (:path end-point))))

(comment
  (let [state {:anchor {:offset 12, :path [0]},
               :content {:attrs {},
                         :content [{:attrs {:style {:font-size "1em"}},
                                    :content ["Type something awesome"],
                                    :tag :span,
                                    :type :element}],
                         :tag :div,
                         :type :element},
               :focus {:offset 19, :path [0]}}]
    (universal-leaf-attrs-in-selection state))
  ;; => {:style {:font-size "1em"}}
  (let [state {:anchor {:offset 0, :path [1]},
               :content {:attrs {},
                         :content [{:attrs {:style {:font-size "1em"}},
                                    :content ["Type someth"],
                                    :tag :span,
                                    :type :element}
                                   {:attrs {:style
                                            {:font-size "1em", :font-weight "bold"}},
                                    :content ["ing awe"],
                                    :tag :span,
                                    :type :element}
                                   {:attrs {:style {:font-size "1em"}},
                                    :content ["some"],
                                    :tag :span,
                                    :type :element}],
                         :tag :div,
                         :type :element},
               :focus {:offset 7, :path [1]}}]
    (universal-leaf-attrs-in-selection state))
  ;; => {:style {:font-size "1em", :font-weight "bold"}}
  )

;; FIXME: :active-attrs and :remove-attrs should be replaced with a more precise abstraction
(defn selection-toggle-attr
  "Toggles an attribute in the selection."
  [state attr-path value]
  (if (selection? state)
    (update-selection state (fn [node]
                              (if (= (get-in (universal-leaf-attrs-in-selection state) attr-path) value)
                                (update node :attrs (fn [attrs]
                                                                                             ;; attrs must be an empty map, not nil
                                                      (or (dissoc-in attrs attr-path)
                                                          {})))
                                (assoc-in node (into [:attrs] attr-path) value))))
    (if (or (= (get-in (:active-attrs state) attr-path) value)
            (and (= (-> (get-in (:content state) (hickory-path (get-in state [:anchor :path])))
                        (get-in (into [:attrs] attr-path)))
                    value)
                 (not (:remove-attrs state))))
      (-> state
          (assoc :active-attrs (:attrs (get-in (:content state) (hickory-path (get-in state [:anchor :path])))))
          (dissoc-in (into [:active-attrs] attr-path))
          (as-> x (if (and (empty? (get-in x [:active-attrs]))
                           (not-empty (:attrs (get-in (:content state) (hickory-path (get-in state [:anchor :path]))))))
                    (assoc x :remove-attrs true)
                    x)))
      (-> state
          (dissoc :remove-attrs)
          (assoc :active-attrs (assoc-in (:attrs (get-in (:content state) (hickory-path (get-in state [:anchor :path]))))
                                         attr-path value))))))

;;;;;;;;;;;;;;;;;;;;;
;; EDITABLE COMPONENT

(def state
  (r/atom {:content {:attrs   {},
                     :content [{:attrs   {}
                                :content ["Type something awesome"],
                                :tag     :span,
                                :type    :element}],
                     :tag     :div,
                     :type    :element}}))

(defn editable-hiccup
  "Converts hickory map to hiccup for the editable component"
  [content]
  (walk/prewalk (fn [node]
                  (cond
                    ;; COMPAT: Browsers will collapse trailing new lines at the end of blocks,
                    ;; so we need to add an extra trailing new lines to prevent that.
                    (and (string? node)
                         (= (last node) "\n"))
                    (str node "\n")
                    (:tag node)
                    (-> [(:tag node)]
                        (conj (-> (:attrs node)
                                  (assoc :data-rich-node true)))
                        (cond-> (:content node)
                          (into (if (= (:content node) [""])
                                  ["\uFEFF"]
                                  (:content node)))))
                    :else
                    node))
                content))

(defn get-root-element []
  (js/document.getElementById "rich-editable"))

(defn find-element
  "Finds the element in the DOM at this path."
  [path]
  (let [editor     (get-root-element)
        start-node (-> editor .-childNodes (aget 0))]
    (loop [node start-node
           path path]
      (if (seq path)
        (recur (aget (.-childNodes node) (first path))
               (rest path))
        node))))

(defn editable-node?
  "Returns true if this element is a node element of the editable component."
  [element]
  (.hasAttribute element "data-rich-node"))

(defn root-element
  "Returns the root element of the editable component"
  [element]
  (.closest element "#rich-editable"))

(defn index-from-parent
  "Returns the index of the element in its parent's children."
  [element]
  (let [children (-> element .-parentElement .-children array-seq)]
    (.indexOf children element)))

(defn path-to-node
  "Returns the path to the element from the root node, assuming each node is represented by a vector of its children"
  [element]
  (if (editable-node? (.-parentElement element))
    (into [(index-from-parent element)] (path-to-node (.-parentElement element)))
    []))

(defn get-selection
  "Returns the current selection in the editor from the DOM."
  []
  (let [selection (.getSelection js/window)]
    (when (and (.-anchorNode selection)
               (root-element (.-parentElement (.-anchorNode selection))))
      {:anchor {:path   (path-to-node (.-parentElement (.-anchorNode selection)))
                :offset (.-anchorOffset selection)}
       :focus  {:path   (path-to-node (.-parentElement (.-focusNode selection)))
                :offset (.-focusOffset selection)}})))

(defn editable
  []
  (let [on-selection-change (fn []
                              (swap! state dissoc :active-attrs :remove-attrs)
                              (when-let [selection (get-selection)]
                                (when (not= selection (select-keys @state [:anchor :focus]))
                                  (swap! state merge selection))))
        on-before-input     (fn [e]
                              (.preventDefault e)
                              (let [text (.-data e)
                                    type (.-inputType e)]
                                (case type
                                  "insertText"
                                  (swap! state (fn [state]
                                                 (cond
                                                   (:remove-attrs state)
                                                   (let [state (-> state
                                                                   (update-selection (fn [node]
                                                                                       (assoc node :attrs {})))
                                                                   (assoc-in [:anchor :offset] 0)
                                                                   (assoc-in [:focus :offset] 0))]
                                                     (-> state
                                                         (update :content hickory-insert-text {:text   text
                                                                                               :path   (get-in state [:focus :path])
                                                                                               :offset (get-in state [:focus :offset])})
                                                         (update-in [:anchor :offset] + (count text))
                                                         (update-in [:focus :offset] + (count text))
                                                         (dissoc :remove-attrs)))
                                                   (:active-attrs state)
                                                   (let [state (-> state
                                                                   (update-selection (fn [node]
                                                                                       (assoc node :attrs (:active-attrs state))))
                                                                   (assoc-in [:anchor :offset] 0)
                                                                   (assoc-in [:focus :offset] 0))]

                                                     (-> state
                                                         (update :content hickory-insert-text {:text   text
                                                                                               :path   (get-in state [:focus :path])
                                                                                               :offset (get-in state [:focus :offset])})
                                                         (update-in [:anchor :offset] + (count text))
                                                         (update-in [:focus :offset] + (count text))
                                                         (dissoc :active-attrs)))
                                                   :else
                                                   (-> state
                                                       (update :content hickory-insert-text {:text   text
                                                                                             :path   (get-in state [:focus :path])
                                                                                             :offset (get-in state [:focus :offset])})
                                                       (update-in [:anchor :offset] + (count text))
                                                       (update-in [:focus :offset] + (count text))))))
                                  "insertParagraph"
                                  (swap! state (fn [state]
                                                 (-> state
                                                     (update :content hickory-insert-text {:text   "\n"
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
              (let [{:keys [start-point end-point]} (range-from-selection @state)
                    dom-range     (js/window.document.createRange)
                    dom-selection (js/window.getSelection)]
                (.setStart dom-range (find-element (conj (:path start-point) 0)) (:offset start-point))
                (.setEnd dom-range (find-element (conj (:path end-point) 0)) (:offset end-point))
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
            :style                             {:width         "100%"
                                                :height        "100%"
                                                :white-space   "pre-wrap"
                                                :overflow-wrap "break-word"}
            :on-key-down                       (fn [e]
                                                 (when (and (= (.-key e) "b") (.-metaKey e))
                                                   (.preventDefault e)
                                                   (swap! state selection-toggle-attr [:style :font-weight] "bold"))
                                                 (when (and (= (.-key e) "i") (.-metaKey e))
                                                   (.preventDefault e)
                                                   (swap! state selection-toggle-attr [:style :font-style] "italic")))
            :on-paste                          (fn [e]
                                                 (.preventDefault e)
                                                 (let [text (-> e .-clipboardData (.getData "Text"))]
                                                   (swap! state
                                                          (fn [state]
                                                            (-> state
                                                                (cond-> (selection? state) delete-selection)
                                                                (update :content hickory-insert-text {:text   text
                                                                                                      :path   (get-in state [:focus :path])
                                                                                                      :offset (get-in state [:focus :offset])})
                                                                (update-in [:anchor :offset] + (count text))
                                                                (update-in [:focus :offset] + (count text)))))))}
           (editable-hiccup content)]))})))
