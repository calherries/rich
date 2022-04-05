(ns rich.app
  (:require [rich.core :as rich]
            [reagent.dom :as rdom]
            [clojure.pprint :as pprint]))

(defn app []
  [:div {:style {:padding "10px"
                 :min-height    "100px"
                 :display "flex"
                 :flex-direction "row"}}
   [:div {:style {:width  "50%"
                  :border "2px solid black"}}
    [rich/editable]]
   [:div {:style {:width "50%"
                  :border "2px solid black"
                  :white-space "pre-wrap"
                  :overflow-wrap "break-word"}}
    [:p (with-out-str (pprint/pprint (update @rich/state :content rich/as-hiccup)))]]])


(defn ^:dev/after-load start []
  (js/console.log "Starting...")
  (rdom/render [app] (js/document.getElementById "app")))

(defn ^:export init []
  (start))
