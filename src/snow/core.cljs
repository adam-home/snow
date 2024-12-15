(ns snow.core
  (:require [clojure.core.async :as async :refer [chan timeout go go-loop <! >!]]
            [snow.gfx :as gfx]))

(def iterations (.-MAX_SAFE_INTEGER js/Number))
;; (def iterations 1000)
(def snowflakes (atom []))
(def canvas (atom {}))
(def num-snowflakes (atom 0))
(def wind-strength (atom 2))

(defrecord Snowflake [x y r attrs offset])

(defn create-snowflake [max-x max-y]
  (Snowflake. (rand-int max-x)            ;; x
              (rand-int max-y)            ;; y
              (+ 1 (rand-int 8))          ;; radius
              {:fill "white" :colour "white"} ;; attrs
              (- 0.5 (rand))))            ;; offset

(defn draw-snowflake [canvas snowflake]
  (gfx/circle canvas
              (:x snowflake)
              (:y snowflake)
              (:r snowflake)
              (:attrs snowflake)))

(defn off-bottom?
  [snowflake canvas]
  (let [y      (:y snowflake)
        height (.-height (:elem canvas))]
    (> y height)))

(defn move-snowflake
  [snowflake]
  (let [
        new-x (+ (:x snowflake)                 ; current position
                 (Math/sin (:offset snowflake)) ; natural flutter
                 @wind-strength)                ; wind strength
        new-y (+ 1 (/ (:r snowflake) 4) (:y snowflake)) ; larger flakes fall faster
        new-o (+ 0.05 (:offset snowflake))]
    
    (assoc snowflake
           :x new-x
           :y new-y
           :offset new-o)))

(defn wrap-snowflake
  [snowflake]
  (let [w (.-width (:elem (:screen @canvas)))]
    (cond
      (< (:x snowflake) 0) (assoc snowflake :x w)
      (> (:x snowflake) w) (assoc snowflake :x 0)
      :else snowflake)))

(defn main-loop
  [buffer ground screen]
  (run! (fn [s] (draw-snowflake buffer s)) @snowflakes)
  (reset! snowflakes (map move-snowflake @snowflakes))

  ;; Snowflakes "die" at the bottom of the screen, and a replaced with a new one randomly somewhere at the top
  (let [w                (.-width  (:elem screen))
        h                (.-height (:elem screen))
        alive-snowflakes (remove (fn [s] (off-bottom? s screen)) @snowflakes)
        dead-snowflakes  (remove (fn [s] (not (off-bottom? s screen))) @snowflakes)
        num-to-make      (- @num-snowflakes (count alive-snowflakes))
        new-snowflakes   (map (fn [_] (create-snowflake w 0)) (range num-to-make))]

    (run! (fn [s] (draw-snowflake ground s)) dead-snowflakes)

    (reset! snowflakes (into [] (concat alive-snowflakes new-snowflakes)))
    (reset! snowflakes (mapv wrap-snowflake @snowflakes)))
    
  (gfx/flip buffer screen)
  (gfx/flip ground buffer :noclear))
;;  (gfx/clear buffer))

(defn main
  []
  (let [screen (gfx/create-canvas "canvas" :fullscreen)
        ground (gfx/create-offscreen-canvas (.-width  (:elem screen))
                                            (.-height (:elem screen)))
        buffer (gfx/create-offscreen-canvas (.-width  (:elem screen))
                                            (.-height (:elem screen)))]

    (reset! num-snowflakes (/ (* (.-width (:elem screen))
                                 (.-height (:elem screen)))
                              10000))
    
    (swap! canvas assoc :screen screen :buffer buffer :ground ground)
    
    (gfx/clear buffer)
    (gfx/clear ground)
    (gfx/clear screen)

    (let [slider (.getElementById js/document "windspeed")]
      (set! (.-value slider) 0)
      (reset! wind-strength 0)
      (set! (.-oninput slider) (fn [evt] (reset! wind-strength (int (.-value (.-target evt)))))))

    (reset! snowflakes [])
    (dotimes [_ @num-snowflakes]
      (swap! snowflakes conj (create-snowflake (.-width  (:elem screen))
                                               (.-height (:elem screen)))))

    (go-loop [counter iterations]
      (when (> counter 0)
        (<! (timeout 10))
        (.requestAnimationFrame js/window
                                (fn [] (main-loop buffer ground screen)))
        (recur (dec counter))))))
