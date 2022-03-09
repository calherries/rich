(ns rich.core
  (:require [clojure.walk :as walk]
            [hickory.core :as hick]
            [hickory.zip :as hzip]
            [clojure.string :as str]
            [applied-science.js-interop :as j]
            [com.wotbrew.relic :as rel]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(defn p [x] (def x x) (prn x) x)
(enable-console-print!)

(def state
  (r/atom {:selection [[0 0 0] [0 0 0]]
           :value     [{:attrs   nil,
                        :content [{:attrs   {:style {:font-size "1em"
                                                     :color "brown"}},
                                   :content ["Some text"],
                                   :tag     :span,
                                   :type    :element}
                                  {:attrs   {:style {:font-size "1em"}},
                                   :content ["More text"],
                                   :tag     :span,
                                   :type    :element}],
                        :tag     :div,
                        :type    :element}]}))

(defn at-path [path]
  (vec (interpose :content path)))

(defn update-node [content path f]
  (if (seq path)
    (update-in content (at-path path) f)))

(comment
  (at-path [0 0])
  (vec (interpose :content [0 0 0]))
  [0 :content 0])

(defn insert-text [content {:keys [path text offset]}]
  (update-in content (into (at-path path) [:content 0])
             (fn [old-text]
               (let [[before after] (split-at offset old-text)]
                 (str/join (concat before (seq text) after))))))


(comment
  (def content (:value @state))
  (insert-text content {:path [0 0] :text "...!" :offset 0})
  )

(defn as-hiccup [content]
  (walk/prewalk (fn [node]
                  (cond
                    (:tag node)
                    (-> [(:tag node)]
                        (cond-> (:attrs node) (conj (assoc (:attrs node) :data-rich-node true)))
                        (cond-> (:content node) (into (:content node))))
                    :else
                    node))
                content))

(defn find-rich-node [target]
  (.closest target "[data-rich-node]"))

(defn rich-node? [target]
  (.hasAttribute target "data-rich-node"))

(defn index-of-child [element]
  (.indexOf (array-seq (.-children (.-parentElement element))) element))

(defn path-to-node [element]
  (if (not (rich-node? element))
    []
    (concat [(index-of-child element)] (path-to-node (.-parentElement element)))))

(comment
  (as-hiccup (:value @state)))

(defn editable []
  (r/create-class
   {:component-did-update
    (fn [_]
      (let [selection (.getSelection js/window)]
        ; WIP
        ))
    :reagent-render
    (fn []
      (let [content (:value @state)]
        [:div
         {:content-editable true
          :on-click (fn [e]
                      (let [element (find-rich-node (.-target e))
                            path    (vec (path-to-node element))]
                        (swap! state assoc :selection path)))
          :on-before-input (fn [e]
                             (.preventDefault e)
                             (let [text (.-data e)]
                               (swap! state update :value (fn [content]
                                                            (insert-text content {:text   text
                                                                                  :path   [0 0]
                                                                                  :offset 0})))))}
         (into [:<>] (as-hiccup content))]))}))

(def parsed-doc (hick/parse-fragment (.-outerHTML (js/document.getElementById "app"))))

(defn app []
  [editable])

(defn ^:dev/after-load start []
  (js/console.log "Starting...")
  (rdom/render [app] (js/document.getElementById "app")))

(defn ^:export init []
  (start))

(comment
  (insert-text content {:path [0 0 0] :text "...."})
  (update-node content [0 0] (fn [content]
                               (update content :text #(str % ".....")))))
