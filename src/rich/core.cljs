(ns rich.core
  (:require [clojure.walk :as walk]
            [applied-science.js-interop :as j]
            [com.wotbrew.relic :as rel]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(defn p [x] (prn x) x)

(def content
  {:children [{:tag "div"
               :children [{:text  "Some text. "
                           :style {:font-size "1em"}}
                          {:text  "More text"
                           :style {:font-size "1em"}}]}]})

(defn update-node [content path f]
  (if (seq path)
    (update-in content [:children (first path)] update-node (vec (rest path)) f)
    (f content)))

(defn write-text [content path text]
  (update-node content path (fn [content]
                              (update content :text #(str % text)))))

(def state (atom {}))

(defn render-content [content]
  (walk/prewalk (fn [node]
                  (cond
                    (:children node)
                    (into [:div]
                          (:children node))
                    (vector? node)
                    node
                    (:text node)
                    [:span (:text node)]
                    :else
                    (p node)))
                content))

(comment
  (render-content content)
  )

(defn app []
  [render-content content])

(defn ^:dev/after-load start []
  (js/console.log "Starting...")
  (rdom/render [app] (js/document.getElementById "root")))

(defn ^:export init []
  (start))

(comment
  (write-text content [0 0] "....")
  (update-node content [0 0] (fn [content]
                               (update content :text #(str % "....."))))
  )
