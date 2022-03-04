(ns core)

(def content {:children [{:tag "div"
                          :children [{:text  "Some text"
                                      :style {:font-size "1em"}}]}]})

(defn update-node [content path f]
  (if (seq path)
    (update-in content [:children (first path)] update-node (vec (rest path)) f)
    (f content)))

(defn write-text [content path text]
  (update-node content path (fn [content]
                              (update content :text #(str % text)))))

(write-text content [0 0] "....")
(update-node content [0 0] (fn [content]
                             (update content :text #(str % "....."))))
