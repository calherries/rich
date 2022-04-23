(ns rich.core
  (:require [clojure.data :as data]
            [lambdaisland.deep-diff2 :as deep-diff]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            ["lodash.debounce" :as debounce]
            ["lodash.throttle" :as throttle]
            [hyperfiddle.rcf :refer [tests]]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

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

;;;;;;;;;;;;
;; VECTORS

(defn lexicographic-less-than
  "Like <, but also compares (deeply-nested) vectors lexicographically."
  [a b]
  (= (compare a b) -1))

(defn vec-remove
  "Removes the element in this vector at this index."
  [v index]
  (into (subvec v 0 index) (subvec v (inc index))))

(defn vec-insert
  "Inserts this element in this vector at this index."
  [v index el]
  (vec (concat (subvec v 0 index) [el] (subvec v index))))

(defn update-subvec
  "Updates the subvector with this function.
   Flattens the results back into the original vector"
  [v f start end]
  (let [end (if (nil? end)
              (count v)
              end)]
    (-> (subvec v 0 start)
        (into (f (subvec v start end)))
        (into (subvec v end)))))

(defn replace-subvec
  "Replaces this subvector with another subvector."
  [v start end xs]
  (update-subvec v (fn [] xs) start end))

(defn remove-subvec
  "Removes this subvector."
  [v start end]
  (update-subvec v (fn [] []) start end))

(tests
 (update-subvec [0 1 2 3] (fn [xs] (map #(* 100 %) xs)) 1 3)
 := [0 100 200 3]
 (replace-subvec [0 1 2 3] 1 3 [100])
 := [0 100 3]
 (remove-subvec [0 1 2 3] 1 3)
 := [0 3])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEEPLY NESTED DATA STRUCTURES

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

(defn deep-merge
  "Recursively merges maps together. If all the maps supplied have nested maps
  under the same keys, these nested maps are merged. Otherwise the value is
  overwritten, as in `clojure.core/merge`."
  ([])
  ([a] a)
  ([a b]
   (when (or a b)
     (letfn [(merge-entry [m e]
               (let [k  (key e)
                     v' (val e)]
                 (if (contains? m k)
                   (assoc m k (let [v (get m k)]
                                (if (and (map? v) (map? v'))
                                  (deep-merge v v')
                                  v')))
                   (assoc m k v'))))]
       (reduce merge-entry (or a {}) (seq b)))))
  ([a b & more]
   (reduce deep-merge (or a {}) (cons b more))))

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
  (conj (vec (butlast path)) (inc (last path))))

(tests
 (update-path-with-delete [0 2] [0 0]) := [0 1]
 (update-path-with-insert-right [0 2] [0 1]) := [0 3])

;;;;;;;;;;;;;;;;
;; HICKORY

(defn text-node? [node]
  (and (= (count (:content node)) 1)
       (string? (first (:content node)))
       (= (:tag node) :span)))

(defn text-node->text [text-node]
  (first (:content text-node)))

(def empty-text-node
  {:attrs {}, :content [""], :tag :span, :type :element})

(defn split-text-node-at
  "Splits this text-node at this character offset.
   If any node's contents are empty, it is nil in the final result."
  [text-node offset]
  (let [split-text (->> text-node
                        text-node->text
                        (split-at offset)
                        (map #(apply str %)))]
    (->> split-text
         (map (fn [text]
                (assoc text-node :content [text])))
         (vec))))

(defn split-text-nodes-at
  "Splits this non-empty list of text-nodes into two arrays of text nodes at this point.
   If there is at least one text node in each array the empty text nodes are removed."
  [text-nodes {:keys [offset node-index]}]
  (let [[before [at-index & after]]      (split-at node-index text-nodes)
        [at-index-before at-index-after] (split-text-node-at at-index offset)]
    (->> [(conj (vec before) at-index-before)
          (vec (cons at-index-after after))]
         (mapv (fn [text-nodes]
                 (cond->> text-nodes
                   (not= (count text-nodes) 1)
                   (filterv #(not-empty (text-node->text %)))))))))

(tests
 (let [text-nodes [{:attrs {}, :content ["Type some"], :tag :span, :type :element}
                   {:attrs {:style {:font-weight "bold"}},
                    :content ["thing awesome"],
                    :tag :span,
                    :type :element}]]
   (split-text-nodes-at text-nodes {:offset 9 :node-index 0}))
 := [[{:attrs {}, :content ["Type some"], :tag :span, :type :element}]
     [{:attrs {:style {:font-weight "bold"}},
       :content ["thing awesome"],
       :tag :span,
       :type :element}]]

 (let [text-nodes [{:attrs {:style {:font-weight "bold"}}, :content ["Type some"], :tag :span, :type :element}]]
   (split-text-nodes-at text-nodes {:offset 9 :node-index 0}))
 := [[{:attrs {:style {:font-weight "bold"}},
       :content ["Type some"],
       :tag :span,
       :type :element}]
     [{:attrs {:style {:font-weight "bold"}},
       :content [""],
       :tag :span,
       :type :element}]])

(defn hickory-zip
  "Returns a zipper for hickory maps given a root element."
  [root]
  (zip/zipper (fn [node]
                (not (text-node? node)))
              (comp seq :content)
              (fn [node children]
                (assoc node :content (and children (apply vector children))))
              root))

(defn hickory-path
  "Returns the path to a node in a hickory map,
   imagining the node tree as a vector of vectors,
   where each node is a vector of its children."
  [path]
  (vec (interleave (repeat :content) path)))

(defn hickory-update-in
  "Updates a node at this path with a function f.
   If the path is empty, we update the root."
  [hickory path f]
  (if (seq path)
    (update-in hickory (hickory-path path) f)
    (f hickory)))

(defn hickory-assoc-in
  "Assoc's a node at this path with this value.
   If the path is empty, we replace the root."
  [hickory path v]
  (if (seq path)
    (assoc-in hickory (hickory-path path) v)
    v))

(defn hickory-get-in
  "Gets a node at this path."
  [hickory path]
  (get-in hickory (hickory-path path)))

(defn hickory-dissoc-in
  "Removes node at this path."
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

(defn hickory-insert-node-right
  "Inserts this node to the right of the node at this path."
  [hickory {:keys [path node]}]
  (hickory-update-in hickory
                     (vec (butlast path))
                     (fn [parent-node]
                       (update parent-node :content vec-insert (inc (last path)) node))))

(defn browser-compatible-hickory
  "Returns Hickory HTML compatible between browsers"
  [hickory]
  (walk/prewalk (fn [node]
                  ;; COMPAT: Browsers will collapse trailing new lines at the end of blocks,
                  ;; so we need to add an extra trailing new lines to prevent that.
                  (if (and (string? node)
                           (= (last node) "\n"))
                    (str node "\n")
                    node))
                hickory))

(defn minimized-hickory
  "Returns minimized hickory HTML"
  [hickory]
  (walk/prewalk (fn [node]
                  (cond
                    ;; If the node is a span with no attributes, replace it with its text content.
                    (and (= (:tag node) :span)
                         (empty? (:attrs node)))
                    (first (:content node))
                    :else
                    node))
                hickory))

(defn hickory->hiccup
  "Converts a hickory map to hiccup."
  [hickory]
  (walk/prewalk (fn [node]
                  (if (:tag node)
                    (-> [(:tag node)]
                        (cond-> (not-empty (:attrs node))
                          (conj (:attrs node)))
                        (cond-> (:content node)
                          (into (:content node))))
                    node))
                hickory))

;;;;;;;;;;;;;;;;;;;;
;; MARKED TEXT

(defn marked-text
  "Returns marked text from this string, where every charater has these attrs.
   Marked text is represented by a vector of marked characters, where a marked character
   is a 2-vec of the character and the attrs the character has."
  [text attrs]
  (map vector (seq text) (repeat attrs)))

(defn text-node->marked-text
  "Returns marked text from this hickory text node."
  [text-node]
  (let [characters (seq (text-node->text text-node))]
    (marked-text characters (:attrs text-node))))

(defn text-nodes->marked-text
  "Returns marked text from these hickory text nodes."
  [text-nodes]
  (vec (mapcat text-node->marked-text text-nodes)))

(defn marked-text->text-nodes
  "Returns a vector of text-nodes from this marked text"
  [marked-text]
  (->> marked-text
       (partition-by second)
       (map (fn [part]
              {:attrs   (second (first part))
               :content [(apply str (map first part))]
               :tag     :span
               :type    :element}))
       vec))

(tests
 (let [text-nodes [{:attrs {:style {:font-weight "bold"}}, :content ["bold"], :tag :span, :type :element}
                   {:attrs {}, :content [" and "], :tag :span, :type :element}
                   {:attrs {:style {:font-style "italic"}}, :content ["italic"], :tag :span, :type :element}]]
   (text-nodes->marked-text text-nodes))
 := [["b" {:style {:font-weight "bold"}}]
     ["o" {:style {:font-weight "bold"}}]
     ["l" {:style {:font-weight "bold"}}]
     ["d" {:style {:font-weight "bold"}}]
     [" " {}]
     ["a" {}]
     ["n" {}]
     ["d" {}]
     [" " {}]
     ["i" {:style {:font-style "italic"}}]
     ["t" {:style {:font-style "italic"}}]
     ["a" {:style {:font-style "italic"}}]
     ["l" {:style {:font-style "italic"}}]
     ["i" {:style {:font-style "italic"}}]
     ["c" {:style {:font-style "italic"}}]]

 (let [marked-text [["b" {:style {:font-weight "bold"}}]
                    ["o" {:style {:font-weight "bold"}}]
                    ["l" {:style {:font-weight "bold"}}]
                    ["d" {:style {:font-weight "bold"}}]
                    [" " {}]
                    ["a" {}]
                    ["n" {}]
                    ["d" {}]
                    [" " {}]
                    ["i" {:style {:font-style "italic"}}]
                    ["t" {:style {:font-style "italic"}}]
                    ["a" {:style {:font-style "italic"}}]
                    ["l" {:style {:font-style "italic"}}]
                    ["i" {:style {:font-style "italic"}}]
                    ["c" {:style {:font-style "italic"}}]]]
   (marked-text->text-nodes marked-text))
 := [{:attrs {:style {:font-weight "bold"}}
      :content ["bold"]
      :tag :span,
      :type :element}
     {:attrs {}
      :content [" and "]
      :tag :span
      :type :element}
     {:attrs {:style {:font-style "italic"}}
      :content ["italic"]
      :tag :span
      :type :element}])

(defn update-marked-text-attrs
  "Updates the attrs of each character with this function."
  [marked-text f]
  (mapv (fn [[char attrs]]
          [char (f attrs)])
        marked-text))

(defn update-text-nodes
  "Updates this vector of text nodes with this function.
   Can work on a subvector of the characters in the text, if start and end indices are provided."
  ([text-nodes update-fn]
   (update-text-nodes text-nodes update-fn 0 nil))
  ([text-nodes update-fn start-text-offset]
   (update-text-nodes text-nodes update-fn start-text-offset nil))
  ([text-nodes update-fn start-text-offset end-text-offset]
   (let [marked-text (-> text-nodes
                         (text-nodes->marked-text))]
     (if (empty? marked-text)
       (mapv (fn [text-node] (update text-node :attrs update-fn))
             text-nodes)
       (-> text-nodes
           (text-nodes->marked-text)
           (update-subvec (fn [marked-text]
                            (update-marked-text-attrs marked-text update-fn))
                          start-text-offset
                          end-text-offset)
           (marked-text->text-nodes))))))

(tests
 "Updating text nodes with empty content should preserve the empty content, but still update the node."
 (update-text-nodes [{:attrs {}, :content [""], :tag :span, :type :element}]
                    #(assoc % :attrs {:style {:font-weight "bold"}}))
 := [{:attrs {:attrs {:style {:font-weight "bold"}}},
      :content [""],
      :tag :span,
      :type :element}])

;; The following two functions map a point in a vector of text nodes
;; to an index in an array of characters, and vice versa.
;;
;; node-index-and-offset <=> text-index + node-side-of-index
;;     [[a b c|][d e f]] <=> ([a b c | d e f], [:node-side-of-index :left])
;;                 [0 3] <=> {:text-index 3, :node-side-of-index :left}
;;
;; :node-side-of-index is needed to recover the original node-index-and-offset from a text-index.

(defn node-index-and-offset->text-index [text-nodes {:keys [node-index node-offset]}]
  (let [cumulative-offsets   (reductions + 0 (map (fn [node] (count (text-node->text node))) text-nodes))
        start-offset-of-node (nth cumulative-offsets node-index)
        node-side-of-index   (if (and (< 0 node-offset)
                                      (= (count (text-node->text (nth text-nodes node-index)))
                                         node-offset))
                               :left ; left is the exception. It's only to the left if the node-offset is equal to the count of characters in the node, i.e. the cursor is at the end of the node.
                               :right)]
    {:text-index         (+ start-offset-of-node
                            node-offset)
     ;; Node side of cursor is :left if converting back to a point, the point should be in the node to the left of the cursor.
     :node-side-of-index node-side-of-index}))

(defn text-index->node-index-and-offset
  [text-nodes {:keys [text-index node-side-of-index]}]
  (let [start-offsets          (reductions + 0 (map (fn [node] (count (text-node->text node))) text-nodes))
        [start-offset-of-node
         [node-index _node]]   (->> text-nodes
                                    (map-indexed vector)
                                    (map vector start-offsets)
                                    (take-while (fn [[start-offset _indexed-node]]
                                                  (if (= node-side-of-index :left)
                                                    (< start-offset text-index)
                                                    (<= start-offset text-index))))
                                    (last))]
    {:node-index  node-index
     :node-offset (- text-index
                     start-offset-of-node)}))

(tests
 (node-index-and-offset->text-index [{:attrs {},
                                      :content [""],
                                      :tag :span,
                                      :type :element}]
                                    {:node-index 0, :node-offset 0})
 :=
 {:node-side-of-index :right, :text-index 0}

 (def text-nodes [{:attrs {}, :content ["Type "], :tag :span, :type :element}
                  {:attrs {:style {:font-weight "bold"}},
                   :content ["something"],
                   :tag :span,
                   :type :element}
                  {:attrs {}, :content ["awesome"], :tag :span, :type :element}])

 (node-index-and-offset->text-index text-nodes {:node-index 0 :node-offset 5})
 := {:text-index 5, :node-side-of-index :left}

 (text-index->node-index-and-offset text-nodes {:text-index 5, :node-side-of-index :left})
 := {:node-index 0, :node-offset 5}

 (node-index-and-offset->text-index text-nodes {:node-index 1 :node-offset 0})
 := {:text-index 5, :node-side-of-index :right}

 (text-index->node-index-and-offset text-nodes {:text-index 5, :node-side-of-index :right})
 := {:node-index 1, :node-offset 0})

;;;;;;;;;;;;;;;;;;;;
;; ZIP UTILS

(defn zip-next-seq
  "Given a clojure.zip zipper location loc return a lazy sequence of all
  clojure.zip/next locations from loc."
  [loc]
  (if (zip/end? loc)
    ()
    (lazy-seq (cons loc (zip-next-seq (zip/next loc))))))

(defn zip-nth-child [zipper n]
  (nth (iterate zip/right (zip/down zipper)) n))

(defn zip-get-in [zipper path]
  (if (seq path)
    (zip-get-in (zip-nth-child zipper (first path)) (rest path))
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

(defn branch-zips-after
  "Returns all leaf nodes after loc, inclusive."
  [loc]
  (filter zip/branch?
          (take-while (complement zip/end?)
                      (iterate zip/next loc))))

(defn leaf-zips-between
  "Returns all leaf zippers between start-path and end-path, inclusive."
  [zipper start-path end-path]
  (take-until (fn [z] (= (path-to-zip z) end-path)) (leaf-zips-after (zip-get-in zipper start-path))))

(defn branch-zips-between
  "Returns all branch zippers between start-path and end-path, exclusive of the end-path."
  [zipper start-path end-path]
  (take-until (fn [z] (lexicographic-less-than (path-to-zip z) end-path)) (branch-zips-after (zip-get-in zipper start-path))))

(defn paths-between
  "All paths between start-path and end-path"
  [zipper start-path end-path]
  (map path-to-zip (leaf-zips-between zipper start-path end-path)))

(tests
 (def example-zip (zip/vector-zip [1 [2 [0 1] 3] 4]))
 (path-to-zip example-zip) := []
 (zip/node (zip-nth-child example-zip 2)) := 4
 (zip/node (zip-get-in example-zip [1 1 0])) := 0
 (mapv zip/node (leaf-zips-before (zip-get-in example-zip [1 1 0]))) := [0 2 1]
 (mapv zip/node (leaf-zips-after (zip-get-in example-zip [1 1 0]))) := [0 1 3 4]
 (path-to-zip (zip-get-in example-zip [1 1 0])) := [1 1 0]
 (mapv zip/node (leaf-zips-between example-zip [1 1 0] [1 2])) := [0 1 3]
 (paths-between example-zip [1 1 0] [1 2]) := '([1 1 0] [1 1 1] [1 2])
 (path-to-zip (first (branch-zips-between example-zip [1 0] [1 2])))
 (branch-paths-between example-zip [1 0] [1 2]))



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

(defn maybe-merge-text-nodes [first-text-node second-text-node]
  (if (= (dissoc first-text-node :content) (dissoc second-text-node :content))
    [(assoc first-text-node :content [(str (text-node->text first-text-node)
                                           (text-node->text second-text-node))])]
    (if (empty? (text-node->text second-text-node))
      [first-text-node]
      [first-text-node second-text-node])))

(defn merge-backwards
  "Recursively merges the the node at this path with the previous sibling node.
   Adds the children of the current block node to the previous block node, and merges them.
   Assumes there are two consecutive block nodes."
  [state {:keys [path]}]
  (let [content      (:content state)
        prev-path    (path-left path)
        node         (hickory-get-in content path)
        node-content (:content node)
        prev-node    (hickory-get-in content prev-path)
        prev-node    (update prev-node :content (fn [prev-node-content]
                                                  ;; Merge the last leaf node with the previous node if it has the same attrs, or it's empty
                                                  ;; Assumes the children are all leaf nodes
                                                  (vec (concat (butlast prev-node-content)
                                                               (maybe-merge-text-nodes (last prev-node-content) (first node-content))
                                                               (rest node-content)))))
        new-content  (-> content
                         (hickory-assoc-in prev-path prev-node)
                         (hickory-dissoc-in path))]
    (assoc state :content new-content)))

(defn search-backwards
  "Returns the first index where (pred item) returns true, starting from an index and searching backwards.
   The search does not include the item at the start index."
  [xs pred start]
  (loop [i (dec start)]
    (cond
      (neg? i)
      nil
      (pred (nth xs i))
      i
      :else
      (recur (dec i)))))

(tests
 (let [xs    [1 0 0 4 0 5]
       start 4
       pred  #(= % 0)]
   (search-backwards xs pred start))
 := 2)

(defn delete-backwards
  "Returns a new state after deleting a unit of characters, starting from a given point."
  [state {:keys [path offset unit] :as start-point}]
  (if (= offset 0)
    ;; We are deleting from the start of the leaf, so we need to delete into the previous leaf
    (let [prev-zip (second (leaf-zips-before (zip-get-in (hickory-zip (:content state)) path)))]
      (if (seq prev-zip)
        ;; Delete backwards from the end of the previous leaf.
        (let [prev-path    (path-to-zip prev-zip)
              prev-node    (zip/node prev-zip)
              text-content (text-node->text prev-node)
              new-point    {:path   prev-path
                            :offset (count text-content)}]
          (if (path-left path)
            ;; Delete from the previous sibling node node
            (delete-backwards (-> state
                                  (assoc :anchor new-point)
                                  (assoc :focus new-point))
                              {:path   prev-path
                               :offset (count text-content)
                               :unit   unit})
            ;; If there is no text-node to the left, merge the current parent (assumed to be a block) with the previous (another block)
            (let [parent-path (vec (butlast path))]
              (-> state
                  (merge-backwards {:path parent-path})
                  (assoc :anchor new-point)
                  (assoc :focus new-point)))))
        ;; If we are at the first leaf, and we can't delete further, do nothing.
        state))
    (let [content           (:content state)
          parent-path       (vec (butlast (:path start-point)))
          parent-node       (hickory-get-in content parent-path)
          sibling-nodes     (:content parent-node)
          start-index       (last (:path start-point))
          {start-text-index :text-index} (node-index-and-offset->text-index sibling-nodes {:node-index  start-index
                                                                                           :node-offset (:offset start-point)})
          marked-text       (text-nodes->marked-text sibling-nodes)
          delete-to-index   (case unit
                              :char (dec start-text-index) ; start-text-index > 0 as offset > 0
                              :word (let [index (search-backwards marked-text
                                                                  (fn [[char _]] (= char " "))
                                                                  (dec start-text-index))]
                                      (if (nil? index)
                                        0
                                        (inc index)))
                              :line  0)
          new-marked-text   (remove-subvec marked-text delete-to-index start-text-index)
          new-sibling-nodes (marked-text->text-nodes new-marked-text)
          new-sibling-nodes (if (empty? new-sibling-nodes)
                              [empty-text-node]
                              new-sibling-nodes)
          {new-start-index  :node-index
           new-start-offset :node-offset} (text-index->node-index-and-offset new-sibling-nodes {:text-index         delete-to-index
                                                                                                :node-side-of-index :right})
          new-start-point   {:path   (conj parent-path new-start-index)
                             :offset new-start-offset}
          new-content       (hickory-update-in content parent-path
                                               (fn [parent-node]
                                                 (assoc parent-node :content new-sibling-nodes)))]
      (merge state {:content new-content
                    :anchor  new-start-point
                    :focus   new-start-point}))))

(tests
 "Deleting at the start of a block should merge text nodes together"
 (let [state initial-test-state
       commands [[:set-selection
                  {:anchor {:path [0 0], :offset 9}, :focus {:path [0 0], :offset 9}}]
                 [:set-selection
                  {:anchor {:path [0 0], :offset 5},
                   :focus {:path [0 0], :offset 14}}]
                 [:selection-toggle-attribute [:style :font-weight] "bold"]
                 [:set-selection
                  {:anchor {:path [0 1], :offset 4}, :focus {:path [0 1], :offset 4}}]
                 [:insert-paragraph]
                 [:delete-content-backward]]]
   (-> (do-commands state commands)
       (select-keys [:anchor :content :focus])))
 := {:anchor {:offset 4, :path [0 1]},
     :content
     {:attrs {},
      :content
      [{:attrs {},
        :content
        [{:attrs {}, :content ["Type "], :tag :span, :type :element}
         {:attrs {:style {:font-weight "bold"}},
          :content ["something"],
          :tag :span,
          :type :element}
         {:attrs {}, :content [" awesome"], :tag :span, :type :element}],
        :tag :div,
        :type :element}],
      :tag :div,
      :type :element},
     :focus {:offset 4, :path [0 1]}})

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

(defn update-selection-attrs
  "Updates all the text in the current selected range with this function."
  [state update-fn]
  (let [content             (:content state)
        {:keys [start-point end-point]} (range-from-selection state)
        start-parent-path   (vec (butlast (:path start-point)))
        start-parent-node   (hickory-get-in content start-parent-path)
        end-parent-path     (vec (butlast (:path end-point)))
        end-parent-node     (hickory-get-in content end-parent-path)
        start-sibling-nodes (:content start-parent-node)
        end-sibling-nodes   (:content end-parent-node)
        start-index         (last (:path start-point))
        end-index           (last (:path end-point))
        start-text-offset   (node-index-and-offset->text-index start-sibling-nodes {:node-index  start-index
                                                                                    :node-offset (:offset start-point)})
        end-text-offset     (node-index-and-offset->text-index (p end-sibling-nodes) (q {:node-index  end-index
                                                                                         :node-offset (:offset end-point)}))]
    (if (= end-parent-path start-parent-path)
      (let [new-sibling-nodes   (update-text-nodes start-sibling-nodes
                                                   update-fn
                                                   (:text-index start-text-offset)
                                                   (:text-index end-text-offset))
            {new-start-index  :node-index
             new-start-offset :node-offset} (text-index->node-index-and-offset new-sibling-nodes start-text-offset)
            {new-end-index    :node-index
             new-end-offset   :node-offset} (text-index->node-index-and-offset new-sibling-nodes end-text-offset)
            new-start-point   {:path   (conj start-parent-path new-start-index)
                               :offset new-start-offset}
            new-end-point     {:path   (conj end-parent-path new-end-index)
                               :offset new-end-offset}
            [anchor focus]    (if (backwards-selection? state)
                                [new-end-point new-start-point]
                                [new-start-point new-end-point])
            new-content       (hickory-update-in content start-parent-path
                                                 (fn [parent-node]
                                                   (assoc parent-node :content new-sibling-nodes)))]
        (merge state {:content new-content
                      :anchor  anchor
                      :focus   focus}))
      ;; start and end nodes have different parents
      (let [new-start-sibling-nodes (update-text-nodes start-sibling-nodes
                                                       update-fn
                                                       (:text-index start-text-offset))
            between-branch-zippers  (branch-zips-between (hickory-zip content) (:path start-point) (:path end-point))
            between-paths-and-nodes (for [zip between-branch-zippers]
                                      (let [text-nodes (:content (zip/node zip))]
                                        {:path       (path-to-zip zip)
                                         :text-nodes (update-text-nodes text-nodes
                                                                        update-fn)}))
            new-end-sibling-nodes   (update-text-nodes end-sibling-nodes
                                                       update-fn
                                                       0
                                                       (:text-index end-text-offset))
            {new-start-index  :node-index
             new-start-offset :node-offset} (text-index->node-index-and-offset new-start-sibling-nodes start-text-offset)
            {new-end-index    :node-index
             new-end-offset   :node-offset} (text-index->node-index-and-offset new-end-sibling-nodes end-text-offset)
            new-start-point   {:path   (conj start-parent-path new-start-index)
                               :offset new-start-offset}
            new-end-point     {:path   (conj end-parent-path new-end-index)
                               :offset new-end-offset}
            [anchor focus]    (if (backwards-selection? state)
                                [new-end-point new-start-point]
                                [new-start-point new-end-point])
            new-paths-and-nodes   (into #{{:path start-parent-path
                                           :text-nodes new-start-sibling-nodes}
                                          {:path end-parent-path
                                           :text-nodes new-end-sibling-nodes}}
                                        between-paths-and-nodes)
            new-content       (reduce (fn [content {:keys [path text-nodes]}]
                                        (hickory-update-in content
                                                           path
                                                           (fn [parent-node]
                                                             (assoc parent-node :content text-nodes))))
                                      content
                                      new-paths-and-nodes)]
        (merge state {:content new-content
                      :anchor  anchor
                      :focus   focus})))))

(tests
 "Select a word and make it bold"
 (let [commands [[:set-selection
                  {:anchor {:path [0 0], :offset 8}, :focus {:path [0 0], :offset 8}}]
                 [:set-selection
                  {:anchor {:path [0 0], :offset 5},
                   :focus {:path [0 0], :offset 14}}]
                 [:selection-toggle-attribute [:style :font-weight] "bold"]]
       state (do-commands initial-test-state commands)]
   (select-keys state [:anchor :content :focus]))
 := {:anchor {:offset 0, :path [0 1]},
     :content
     {:attrs {},
      :content
      [{:attrs {},
        :content
        [{:attrs {}, :content ["Type "], :tag :span, :type :element}
         {:attrs {:style {:font-weight "bold"}},
          :content ["something"],
          :tag :span,
          :type :element}
         {:attrs {}, :content [" awesome"], :tag :span, :type :element}],
        :tag :div,
        :type :element}],
      :tag :div,
      :type :element},
     :focus {:offset 0, :path [0 2]}})

(defn insert-marked-text
  "Inserts this marked text at the current cursor."
  [state marked-text]
  (if (seq marked-text)
    (let [content           (:content state)
          {:keys [start-point _end-point]} (range-from-selection state) ;; Asume start-point and end-point are equal
          parent-path       (vec (butlast (:path start-point)))
          parent-node       (hickory-get-in content parent-path)
          sibling-nodes     (:content parent-node)
          start-index       (last (:path start-point))
          start-text-offset (node-index-and-offset->text-index sibling-nodes {:node-index  start-index
                                                                              :node-offset (:offset start-point)})
          new-sibling-nodes (-> sibling-nodes
                                (text-nodes->marked-text)
                                (update-subvec (fn [_]
                                                 marked-text)
                                               (:text-index start-text-offset)
                                               (:text-index start-text-offset))
                                (marked-text->text-nodes))
          new-character-index (+ (count marked-text)
                                 (:text-index start-text-offset))
          {new-start-index  :node-index
           new-start-offset :node-offset} (text-index->node-index-and-offset new-sibling-nodes {:text-index         new-character-index
                                                                                                :node-side-of-index :left})
          new-start-point   {:path   (conj parent-path new-start-index)
                             :offset new-start-offset}
          new-content       (hickory-update-in content parent-path
                                               (fn [parent-node]
                                                 (assoc parent-node :content new-sibling-nodes)))]
      (merge state {:content new-content
                    :anchor  new-start-point
                    :focus   new-start-point}))
    ;; if there's no marked-text
    state))

(tests
 "Insert text after making the cursor bold"
 (let [commands [[:set-selection
                  {:anchor {:path [0 0], :offset 15},
                   :focus {:path [0 0], :offset 15}}]
                 [:selection-toggle-attribute [:style :font-weight] "bold"]]
       state (do-commands initial-test-state commands)
       marked-text [["b" {:style {:font-weight "bold"}}]]]
   (-> (insert-marked-text state marked-text)
       (select-keys [:anchor :content :focus])))
 := {:anchor {:offset 1, :path [0 1]},
     :content
     {:attrs {},
      :content
      [{:attrs {},
        :content
        [{:attrs {}, :content ["Type something "], :tag :span, :type :element}
         {:attrs {:style {:font-weight "bold"}},
          :content ["b"],
          :tag :span,
          :type :element}
         {:attrs {}, :content ["awesome"], :tag :span, :type :element}],
        :tag :div,
        :type :element}],
      :tag :div,
      :type :element},
     :focus {:offset 1, :path [0 1]}})

(defn things-in-both
  "Recursively diffs a and b to find the common values. Maps are subdiffed where keys match and values differ."
  [a b]
  (nth (data/diff a b) 2))

(defn universal-leaf-attrs
  "Returns the common attributes of all nodes between two paths, inclusive."
  [content start-path end-path]
  (let [leaf-nodes (->> (leaf-zips-between (hickory-zip content) start-path end-path)
                        (map (comp :attrs zip/node)))]
    (reduce things-in-both leaf-nodes)))

(defn universal-leaf-attrs-in-selection
  "Returns the common attributes of all nodes in the selection."
  [state]
  (let [content                         (:content state)
        {:keys [start-point end-point]} (range-from-selection state)
        end-path                        (if (zero? (:offset end-point))
                                          (path-left (:path end-point))
                                          (:path end-point))]
    (universal-leaf-attrs content (:path start-point) end-path)))

(tests
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
 := {:style {:font-size "1em"}}
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
 := {:style {:font-size "1em", :font-weight "bold"}})


;;;;;;;;;;;;;;;;;;;;;
;; EDITABLE COMPONENT

(def default-initial-state
  {:content {:attrs   {},
             :content [{:tag     :div
                        :type    :element
                        :attrs   {}
                        :content [{:attrs   {}
                                   :content [""],
                                   :tag     :span,
                                   :type    :element}]}],
             :tag     :div,
             :type    :element}})

(def initial-test-state
  {:content {:attrs   {},
             :content [{:tag     :div
                        :type    :element
                        :attrs   {}
                        :content [{:attrs   {}
                                   :content ["Type something awesome"],
                                   :tag     :span,
                                   :type    :element}]}],
             :tag     :div,
             :type    :element}})

(defmulti command-handler (fn [_state command-v]
                           (first command-v)))

(defn selection-toggle-attribute
  "Toggles an attribute in the selection."
  [state attr-path value]
  (if (selection? state)
    (update-selection-attrs state (if (= (get-in (universal-leaf-attrs-in-selection state) attr-path) value)
                                    (fn [attrs] (dissoc-in attrs attr-path))
                                    (fn [attrs] (assoc-in attrs attr-path value))))
    (let [content            (:content state)
          attrs-under-cursor (:attrs (hickory-get-in content (get-in state [:anchor :path])))]
      (if (or
           ;; The attribute is already in active-attrs
           (= (get-in (:active-attrs state) attr-path)
              value)
           ;; The attribute is already applied to the text under the cursor, and we are not removing attrs
           (and (= (-> (hickory-get-in content (get-in state [:anchor :path]))
                       (get-in (into [:attrs] attr-path)))
                   value)
                (not (:remove-attrs state))))
        ;; Toggle off
        (let [new-active-attrs (dissoc-in attrs-under-cursor attr-path)]
          (if (empty? new-active-attrs)
            (-> state
                (dissoc :active-attrs)
                (cond-> (not-empty attrs-under-cursor)
                  (assoc :remove-attrs true)))
            (-> state
                (assoc :active-attrs new-active-attrs))))
        ;; Toggle on
        (-> state
            (dissoc :remove-attrs)
            (update :active-attrs deep-merge (assoc-in attrs-under-cursor attr-path value)))))))

(tests
 "Set two active attributes under the cursor and start typing"
 (let [commands [[:set-selection
                  {:anchor {:path [0 0], :offset 15},
                   :focus {:path [0 0], :offset 15}}]
                 [:selection-toggle-attribute [:style :font-weight] "bold"]
                 [:selection-toggle-attribute [:style :font-style] "italic"]
                 [:insert-text "b"]]
       state (do-commands initial-test-state commands)]
   (select-keys state [:anchor :content :focus]))
 := {:anchor {:offset 1, :path [0 1]},
     :content
     {:attrs {},
      :content
      [{:attrs {},
        :content
        [{:attrs {}, :content ["Type something "], :tag :span, :type :element}
         {:attrs {:style {:font-style "italic", :font-weight "bold"}},
          :content ["b"],
          :tag :span,
          :type :element}
         {:attrs {}, :content ["awesome"], :tag :span, :type :element}],
        :tag :div,
        :type :element}],
      :tag :div,
      :type :element},
     :focus {:offset 1, :path [0 1]}})

(def do-command
  (fn [state command-v]
    (-> state
        (update :command-history conj command-v)
        (update :history conj (select-keys state [:content :anchor :focus]))
        (command-handler command-v))))

(defn do-commands
  "Decodes a list of commands, executing them on this state"
  [state commands]
  (reduce do-command
          state
          commands))

(defn undo [state]
  (-> state
      (merge (second (:history state)))
      (update :history (comp rest rest))))

(defmethod command-handler :undo
  [state [_]]
  (undo state))

(defmethod command-handler :selection-toggle-attribute
  [state [_ attr-path value]]
  (selection-toggle-attribute state attr-path value))

(defn paste
  [state text]
  (-> state
      (cond-> (selection? state) delete-selection)
      (update :content hickory-insert-text {:text   text
                                            :path   (get-in state [:focus :path])
                                            :offset (get-in state [:focus :offset])})
      (update-in [:anchor :offset] + (count text))
      (update-in [:focus :offset] + (count text))))

(defmethod command-handler :paste
  [state [_ text]]
  (paste state text))

(defn insert-text [state text]
  (cond
    (:remove-attrs state)
    (-> state
        (insert-marked-text (marked-text text {}))
        (dissoc :remove-attrs))
    (:active-attrs state)
    (-> state
        (insert-marked-text (marked-text text (:active-attrs state)))
        (dissoc :active-attrs))
    :else
    (let [state (delete-selection state)]
      (-> state
          (update :content hickory-insert-text {:text   text
                                                :path   (get-in state [:focus :path])
                                                :offset (get-in state [:focus :offset])})
          (update-in [:anchor :offset] + (count text))
          (update-in [:focus :offset] + (count text))))))

(defmethod command-handler :insert-text
  [state [_ text]]
  (insert-text state text))

(defn insert-paragraph-at-selection-start
  "Inserts a paragraph at the start of the selection.
   Assumes the selection is collapsed."
  [state]
  (let [content            (:content state)
        {:keys [start-point _end-point]} (range-from-selection state)
        parent-path        (vec (butlast (:path start-point)))
        parent-node        (hickory-get-in content parent-path)
        parent-parent-path (vec (butlast parent-path))
        parent-index       (last parent-path)
        parent-parent      (hickory-get-in content parent-parent-path)
        parent-siblings    (:content parent-parent)
        sibling-nodes      (:content parent-node)
        start-index        (last (:path start-point))
        ;; split the text nodes in the parent
        [first-nodes second-nodes] (split-text-nodes-at sibling-nodes {:offset     (:offset start-point)
                                                                       :node-index start-index})
        ;; clone the parent
        first-parent-node   (assoc parent-node :content first-nodes)
        second-parent-node  (assoc parent-node :content second-nodes)
        ;; replace parent with two new nodes
        new-parent-siblings (replace-subvec parent-siblings parent-index (inc parent-index) [first-parent-node second-parent-node])
        new-content         (hickory-update-in content parent-parent-path
                                               (fn [parent-parent]
                                                 (assoc parent-parent :content new-parent-siblings)))
        ;; selection is collapsed at the beginning of the second parent
        new-point {:path   (conj (path-right parent-path) 0)
                   :offset 0}]
    ;; replace the parent node with two new parents
    (-> state
        (assoc :content new-content)
        (assoc :anchor new-point)
        (assoc :focus new-point))))

(tests
 (let [state {:anchor {:offset 4, :path [0 1]},
              :content
              {:attrs {},
               :content
               [{:attrs {},
                 :content
                 [{:attrs {}, :content ["Type "], :tag :span, :type :element}
                  {:attrs {:style {:font-weight "bold"}},
                   :content ["something"],
                   :tag :span,
                   :type :element}
                  {:attrs {}, :content [" awesome"], :tag :span, :type :element}],
                 :tag :div,
                 :type :element}],
               :tag :div,
               :type :element},
              :focus {:offset 4, :path [0 1]}}]
   (insert-paragraph-at-selection-start state))
 := {:anchor {:offset 0, :path [1 0]},
     :content
     {:attrs {},
      :content
      [{:attrs {},
        :content [{:attrs {}, :content ["Type "], :tag :span, :type :element}
                  {:attrs {:style {:font-weight "bold"}},
                   :content ["some"],
                   :tag :span,
                   :type :element}],
        :tag :div,
        :type :element}
       {:attrs {},
        :content
        [{:attrs {:style {:font-weight "bold"}},
          :content ["thing"],
          :tag :span,
          :type :element}
         {:attrs {}, :content [" awesome"], :tag :span, :type :element}],
        :tag :div,
        :type :element}],
      :tag :div,
      :type :element},
     :focus {:offset 0, :path [1 0]}})

(defn insert-paragraph
  [state]
  (-> state
      (delete-selection)
      (insert-paragraph-at-selection-start)))

(defmethod command-handler :insert-paragraph
  [state [_]]
  (insert-paragraph state))

(defn delete-content-backward [state unit]
  (if (selection? state)
    (delete-selection state)
    (delete-backwards state {:unit   unit
                             :path   (get-in state [:focus :path])
                             :offset (get-in state [:focus :offset])})))

(defmethod command-handler :delete-content-backward
  [state [_]]
  (delete-content-backward state :char))

(defmethod command-handler :delete-soft-line-backward
  [state [_]]
  (delete-content-backward state :line))

(defmethod command-handler :delete-word-backward
  [state [_]]
  (delete-content-backward state :word))

(defn set-selection [state selection]
  (merge state selection))

(defmethod command-handler :set-selection
  [state [_ selection]]
  (set-selection state selection))

;;;;;;;;
;; DOM

(defn editable-hickory
  "Returns hickory HTML for the editable component"
  [content]
  (-> (walk/prewalk (fn [node]
                      (cond
                        (text-node? node)
                        (-> node
                            (assoc-in [:attrs :data-rich-text] true)
                            (update :content (fn [content]
                                               (if (= content [""])
                                                 ["\uFEFF"]
                                                 content))))
                        :else
                        node))
                    content)
      (assoc-in [:attrs :data-rich-root] true)))

;; FIXME: needs to take an editor identifier, so there can be more than one editor on the page
(defn get-root-element []
  (js/document.getElementById "rich-editable"))

;; FIXME: needs to take an editor identifier, so there can be more than one editor on the page
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

(defn element-in-editable?
  "Returns the root element of the editable component"
  [element]
  (.closest element "[data-rich-root]"))

(defn root-element?
  "Returns true if element is the root of the editable component's content"
  [element]
  (.hasAttribute element "data-rich-root"))

(defn index-from-parent
  "Returns the index of the element in its parent's children."
  [element]
  (let [children (-> element .-parentElement .-children array-seq)]
    (.indexOf children element)))

(defn path-to-element
  "Returns the path to the first ascendent element for which this predicate returns true.
   The path assumes each node is represented by a vector of its children."
  [element pred]
  (if (pred element)
    []
    (conj (path-to-element (.-parentElement element) pred) (index-from-parent element))))

(defn get-selection
  "Returns the current selection in the editor from the DOM."
  []
  (let [selection (.getSelection js/window)]
    (when (some-> selection .-anchorNode .-parentElement element-in-editable?)
      {:anchor {:path   (let [node (.-anchorNode selection)
                              node (if (= (.-nodeType node) 3)
                                     ;; If the selection is a DOM text node, select the parent
                                     (.-parentNode node)
                                     ;; Find the first descendant DOM text node parent
                                     (.querySelector node "[data-rich-text]"))]
                          (path-to-element node root-element?))
                :offset (if (= (.-textContent (.-anchorNode selection)) "\uFEFF")
                          ;; If the text at the point is an empty character, the offset is zero, not one.
                          0
                          (.-anchorOffset selection))}
       :focus  {:path   (let [node (.-focusNode selection)
                              node (if (= (.-nodeType node) 3)
                                     (.-parentNode node)
                                     (.querySelector node "[data-rich-text]"))]
                          (path-to-element node root-element?))
                :offset (if (= (.-textContent (.-focusNode selection)) "\uFEFF")
                          ;; If the text at the point is an empty character, the offset is zero, not one.
                          0
                          (.-focusOffset selection))}})))

(defn editable
  [{:keys [state
           on-key-down
           on-paste]}]
  (when (nil? @state)
    (reset! state default-initial-state))
  (let [on-selection-change        (throttle (fn []
                                               (when (or (:active-attrs @state)
                                                         (:remove-attrs @state))
                                                 (swap! state dissoc :active-attrs :remove-attrs))
                                               (when-let [selection (get-selection)]
                                                 (when (not= selection (select-keys @state [:anchor :focus]))
                                                   (swap! state do-command [:set-selection selection]))))
                                             100)
        debounced-selection-change (debounce on-selection-change 0)
        on-before-input            (fn [e]
                                     (.preventDefault e)

                                     ;; Some IMEs/Chrome extensions like e.g. Grammarly set the selection immediately before
                                     ;; triggering a `beforeinput` expecting the change to be applied to the immediately before
                                     ;; set selection.
                                     (.flush on-selection-change)
                                     (.flush debounced-selection-change)

                                     (swap!
                                      state
                                      (fn [state]
                                        (let [data       (.-data e)
                                              input-type (.-inputType e)]
                                          (case input-type
                                            "insertText"
                                            (do-command state [:insert-text data])
                                            "insertParagraph"
                                            (do-command state [:insert-paragraph])
                                            "deleteContentBackward"
                                            (do-command state [:delete-content-backward])
                                            "deleteSoftLineBackward"
                                            (do-command state [:delete-soft-line-backward])
                                            "deleteWordBackward"
                                            (do-command state [:delete-word-backward]))))))]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (js/document.addEventListener "selectionchange" debounced-selection-change)
        (.addEventListener (rdom/dom-node this) "beforeinput" on-before-input))
      :component-will-unmount
      (fn [this]
        (js/document.removeEventListener "selectionchange" debounced-selection-change)
        (.removeEventListener (rdom/dom-node this) "beforeinput" on-before-input))
      :component-did-update
      (fn [this]
        (let [selection        (get-selection)
              selection-values (fn [s]
                                 (select-paths s [[:anchor :path] [:anchor :offset]
                                                  [:focus :path] [:focus :offset]]))]
            ;; DOM selection is out of sync, so update it.
          (when (not= (selection-values selection) (selection-values @state))
            (let [{:keys [start-point end-point]} (range-from-selection @state)
                  dom-range                       (js/window.document.createRange)
                  dom-selection                   (js/window.getSelection)]
              (.setStart dom-range (find-element (conj (:path start-point) 0)) (:offset start-point))
              (.setEnd dom-range (find-element (conj (:path end-point) 0)) (:offset end-point))
              (.setBaseAndExtent dom-selection
                                 (.-startContainer dom-range)
                                 (.-startOffset dom-range)
                                 (.-endContainer dom-range)
                                 (.-endOffset dom-range))))))
      :reagent-render
      (fn []
        [:div
         {:id                                "rich-editable"
          :content-editable                  true
          :suppress-content-editable-warning true
          :style                             {:width         "100%"
                                              :height        "100%"
                                              :white-space   "pre-wrap"
                                              :overflow-wrap "break-word"}
          :on-key-down                       on-key-down
          :on-paste                          on-paste}
         (-> @state
             :content
             editable-hickory
             minimized-hickory
             browser-compatible-hickory
             hickory->hiccup)])})))
