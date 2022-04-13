(ns rich.app
  (:require [hyperfiddle.rcf]
            [rich.core :as rich]
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
    (with-out-str (pprint/pprint (update @rich/state :content rich/hickory->hiccup)))]])

; Enable tests after app namespaces are loaded (intended for subsequent REPL interactions)
(set! hyperfiddle.rcf/*enabled* true)

(defn ^:dev/after-load start []
; prevent test execution during cljs hot code reload
  (set! hyperfiddle.rcf/*enabled* true)
  (js/console.log "Starting...")
  (rdom/render [app] (js/document.getElementById "app")))

(defn ^:dev/before-load stop []
;;   (remove-watch rich/state :state-logger)
  (set! hyperfiddle.rcf/*enabled* false))

(defn ^:export init []
  (start))
