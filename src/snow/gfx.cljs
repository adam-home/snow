(ns snow.gfx
  (:require [clojure.core.async :as async :refer [>!]]))

(defrecord Canvas [elem ctx])

(defn create-canvas
  [elem-id & args]
    
  (let [elem   (.getElementById js/document elem-id)
        parent (.-parentElement elem)]
    (println args)
    (when (some #(= :fullscreen %) args)
      (println "fullscreen!")
      (println (.-clientWidth parent) (.-clientHeight parent))
      (set! (.. elem -width)  (.-clientWidth parent))
      (set! (.. elem -height) (.-clientHeight parent)))

    (->Canvas elem (.getContext elem "2d"))))

(defn create-offscreen-canvas
  [w h]
  (let [elem (js/OffscreenCanvas. w h)]
    (->Canvas elem (.getContext elem "2d"))))

(defn clear
  [canvas]
  (.save (:ctx canvas))
  (.beginPath (:ctx canvas))
  (.rect (:ctx canvas)
         0 0
         (.-width (:elem canvas)) (.-height (:elem canvas)))
  (set! (.-fillStyle (:ctx canvas)) "black")
  (.fill (:ctx canvas)))

(defn- render-with-attrs
  [ctx {:keys [colour fill linewidth]
        :or {colour      "white"
             fill        nil
             linewidth   1}}]
  (when fill
    (set! (.-fillStyle ctx) fill)
    (.fill ctx))
  (when colour
    (set! (.-strokeStyle ctx) colour))
  (when linewidth
    (set! (.-lineWidth ctx) linewidth)
    (.stroke ctx)))

(defn line
  [canvas x1 y1 x2 y2 & attrs]
  (let [{ctx :ctx} canvas]
    (.save ctx)
    (.beginPath ctx)
    (.moveTo ctx x1 y1)
    (.lineTo ctx x2 y2)
    (.closePath ctx)
    (render-with-attrs ctx attrs)
    (.restore ctx)))

(defn rect
  [canvas x y w h & attrs]
  (let [{ctx :ctx} canvas
        {linewidth :linewidth :or {linewidth 0}} attrs]
    (.save ctx)
    (.beginPath ctx)
    (.moveTo ctx (+ x linewidth) (+ y linewidth))
    (.lineTo ctx (+ x (- w linewidth)) (+ y linewidth))
    (.lineTo ctx (+ x (- w linewidth)) (+ y (- h linewidth)))
    (.lineTo ctx (+ x linewidth) (+ y (- h linewidth)))
    (.closePath ctx)

    (render-with-attrs ctx attrs)
          
    (.restore ctx)))

(defn circle
  [canvas x y r & attrs]
  (let [{ctx :ctx} canvas]
    (.save ctx)

    (.beginPath ctx)
    (.arc ctx x y r 0 (* 2 Math/PI))
    (.closePath ctx)

    (render-with-attrs ctx attrs)))

(defn draw-vertexes
  ([canvas vertexes & attrs]
   (let [{ctx :ctx}    canvas]
    (doseq [[x y] vertexes]
     (apply (partial circle canvas x y 4)
            attrs)))))

(defn draw-edges
  ([canvas vertexes & attrs]
   (let [{ctx :ctx}    canvas]
    (.save ctx)
    (.beginPath ctx)
    (doseq [[x y] vertexes]
      (.lineTo ctx x y))
    (.closePath ctx)
    (render-with-attrs ctx attrs))))

(defn flip
  [backing-canvas screen-canvas & opts]
  (let [{backing-elem :elem} backing-canvas
        {screen-ctx :ctx} screen-canvas]
    (.drawImage screen-ctx
                backing-elem
                0 0)
    (when-not (some #{:noclear} opts)
      (clear backing-canvas))))

(defn load-image [url loaded-chan]
  (let [img (js/Image.)]
    (set! (.-onload img) (fn []
                           ;; once image is loaded, call the callback
                           (async/go (>! loaded-chan img))))
    (set! (.-onerror img) (fn [err]
                            (js/console.error "Failed to load image" err)))
    (set! (.-src img) url)))

(defn draw-image
  [canvas image x y size]
  (.drawImage (:ctx canvas) image x y size size))
