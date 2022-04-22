(ns editors.rich-text
  (:require [hyperfiddle.rcf]
            [rich.core :as rich]
            [reagent.dom :as rdom]
            [clojure.pprint :as pprint]))

(defn editor []
  [:div {:style {:padding        "10px"
                 :min-height     "100px"
                 :display        "flex"
                 :flex-direction "row"}}
   [:div {:style {:width  "50%"
                  :border "2px solid black"}}
    [rich/editable]]
   [:div {:style {:width         "50%"
                  :font-family   "monospace"
                  :font-size     "1.2em"
                  :border        "2px solid black"
                  :white-space   "pre-wrap"
                  :overflow-wrap "break-word"}}
    (-> @rich/state
        :content
        rich/minimized-hickory
        rich/hickory->hiccup
        pprint/pprint
        with-out-str)]])
