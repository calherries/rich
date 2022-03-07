(ns rich.core
  (:require [clojure.walk :as walk]
            [hickory.core :as hick]
            [hickory.zip :as hzip]
            [clojure.string :as str]
            [applied-science.js-interop :as j]
            [com.wotbrew.relic :as rel]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(defn p [x] (prn x) x)
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
                        (cond-> (:attrs node) (conj (:attrs node)))
                        (cond-> (:content node) (into (:content node))))
                    :else
                    node))
                content))

(defn editable []
  (let [content (:value @state)]
    [:div
     {:content-editable true
      :on-before-input (fn [e]
                         (.preventDefault e)
                         (let [text (-> e .-data)]
                           (swap! state update :value (fn [content]
                                                        (insert-text content {:text   text
                                                                              :path   [0 0]
                                                                              :offset 0})))))}
     (into [:<>] (as-hiccup content))]))

(def parsed-doc (hick/parse-fragment (.-outerHTML (js/document.getElementById "root"))))

(map hick/as-hickory parsed-doc)

(comment
  (as-hiccup (:value @state)))

(defn app []
  [editable])

(defn ^:dev/after-load start []
  (js/console.log "Starting...")
  (rdom/render [app] (js/document.getElementById "root")))

(defn ^:export init []
  (start))

(comment
  (insert-text content {:path [0 0 0] :text "...."})
  (update-node content [0 0] (fn [content]
                               (update content :text #(str % ".....")))))
