(ns editors.rich-text
  (:require [rich.core :as rich]
            [reagent.core :as r]
            [clojure.pprint :as pprint]))

(defonce state (r/atom nil))

(defn rich-text-editor []
  [rich/editable
   {:state       state
    :on-key-down (fn [e]
                   (when (and (= (.-key e) "b") (.-metaKey e))
                     (.preventDefault e)
                     (swap! state rich/do-command [:selection-toggle-attribute [:style :font-weight] "bold"]))
                   (when (and (= (.-key e) "i") (.-metaKey e))
                     (.preventDefault e)
                     (swap! state rich/do-command [:selection-toggle-attribute [:style :font-style] "italic"]))
                   (when (and (= (.-key e) "u") (.-metaKey e))
                     (.preventDefault e)
                     (swap! state rich/do-command [:selection-toggle-attribute [:style :text-decoration] "underline"]))
                   (when (and (= (.-key e) "z") (.-metaKey e))
                     (.preventDefault e)
                     (swap! state rich/do-command [:undo])))
    :on-paste    (fn [e]
                   (.preventDefault e)
                   (let [text (-> e .-clipboardData (.getData "Text"))]
                     (swap! state rich/do-command [:paste text])))}])

(defn rich-text-example []
  [:div {:style {:padding        "10px"
                 :min-height     "100px"
                 :display        "flex"
                 :flex-direction "row"}}
   [:div {:style {:width  "50%"
                  :border "2px solid black"}}
    [rich-text-editor]]
   [:div {:style {:width         "50%"
                  :font-family   "monospace"
                  :font-size     "1.2em"
                  :border        "2px solid black"
                  :white-space   "pre-wrap"
                  :overflow-wrap "break-word"}}
    (-> @state
        (update :content (fn [content] (-> content
                                           rich/minimized-hickory
                                           rich/hickory->hiccup)))
        (dissoc :history :command-history)
        pprint/pprint
        with-out-str)]])
