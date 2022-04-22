(ns examples
  (:require [editors.rich-text]
            [hyperfiddle.rcf]
            [rich.core :as rich]
            [reagent.dom :as rdom]))

(defn root []
  [editors.rich-text/rich-text-example])

; Enable tests after app namespaces are loaded (intended for subsequent REPL interactions)
(set! hyperfiddle.rcf/*enabled* true)

(defn ^:dev/after-load start []
  ; prevent test execution during cljs hot code reload
  (set! hyperfiddle.rcf/*enabled* true)
  (js/console.log "Starting...")
  (rdom/render [root] (js/document.getElementById "app")))

(defn ^:dev/before-load stop []
  (set! hyperfiddle.rcf/*enabled* false))

(defn ^:export init []
  (enable-console-print!)
  (start))
