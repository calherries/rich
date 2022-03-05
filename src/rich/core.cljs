(ns rich.core
  (:require [helix.core :refer [<> defnc $]]
            [helix.dom :as d]
            [helix.hooks :as hooks]
            ["react-dom" :as rdom]))

(def content
  {:children [{:tag "div"
               :children [{:text  "Some text"
                           :style {:font-size "1em"}}]}]})

(defn update-node [content path f]
  (if (seq path)
    (update-in content [:children (first path)] update-node (vec (rest path)) f)
    (f content)))

(defn write-text [content path text]
  (update-node content path (fn [content]
                              (update content :text #(str % text)))))

(def state (atom {}))

;; define components using the `defnc` macro
(defnc greeting
  "A component which greets a user."
  [{:keys [name]}]
  ;; use helix.dom to create DOM elements
  (d/div "Hello, " (d/strong name) "!"))

(defnc app []
  (let [[state set-state] (hooks/use-state {:name "Helix User"})]
    (d/div
     (d/h1 "Welcome!")
      ;; create elements out of components
     ($ greeting {:name (:name state)})
     (d/input {:value (:name state)
               :on-change #(set-state assoc :name (.. % -target -value))}))))

(defn ^:dev/after-load start []
  (js/console.log "Starting...")
  (rdom/render ($ app)
               (js/document.getElementById "root")))

(defn ^:export init []
  (start))

(comment
  (write-text content [0 0] "....")
  (update-node content [0 0] (fn [content]
                               (update content :text #(str % "....."))))
  )
