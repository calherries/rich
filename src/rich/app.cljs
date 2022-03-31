(ns rich.app
  (:require [rich.core :as rich]
            [reagent.dom :as rdom]
            [clojure.pprint :as pprint]))

(defn app []
  [:div {:style {:padding "10px"
                 :display "flex"
                 :flex-direction "row"}}
   [:div {:style {:width       "50%"
                  :min-height  "100px"
                  :border      "2px solid black"
                  :white-space "pre-wrap"}}
    [rich/editable]]
   [:div {:style {:width "50%"
                  :min-height "100px"
                  :border "2px solid black"
                  :white-space "pre-wrap"}}
    [:p (with-out-str (pprint/pprint @rich/state))]]])


(defn ^:dev/after-load start []
  (js/console.log "Starting...")
  (rdom/render [app] (js/document.getElementById "app")))

(defn ^:export init []
  (start))
