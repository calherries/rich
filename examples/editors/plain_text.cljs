(ns editors.plain-text
  (:require [rich.core :as rich]
            [reagent.core :as r]
            [clojure.pprint :as pprint]))

(defonce state (r/atom nil))

(defn plain-text-editor []
  [rich/editable
   {:state       state
    :on-key-down (fn [e]
                   (when (and (= (.-key e) "z") (.-metaKey e))
                     (.preventDefault e)
                     (swap! state rich/do-command [:undo])))
    :on-paste    (fn [e]
                   (.preventDefault e)
                   (let [text (-> e .-clipboardData (.getData "Text"))]
                     (swap! state rich/do-command [:paste text])))}])

(defn plain-text-example []
  [:div {:style {:padding        "10px"
                 :min-height     "100px"
                 :display        "flex"
                 :flex-direction "row"}}
   [:div {:style {:width  "50%"
                  :border "2px solid black"}}
    [plain-text-editor]]
   [:div {:style {:width         "50%"
                  :font-family   "monospace"
                  :font-size     "1.2em"
                  :border        "2px solid black"
                  :white-space   "pre-wrap"
                  :overflow-wrap "break-word"}}
    (-> @state
        :content
        rich/minimized-hickory
        rich/hickory->hiccup
        pprint/pprint
        with-out-str)]])
