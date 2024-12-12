(ns snow.core
  (:require [clojure.core.async :as async :refer [chan timeout go go-loop <! >!]]
            [snow.gfx :as gfx]))

(def iterations (.-MAX_SAFE_INTEGER js/Number))
(def snowflakes (atom []))
(def canvas (atom {}))
(def num-snowflakes (atom 0))
(def wind-strength (atom 2))

(defrecord Snowflake [x y r attrs offset])

(defn create-snowflake [max-x max-y]
  (Snowflake. (rand-int max-x)            ;; x
              (rand-int max-y)            ;; y
              (+ 1 (rand-int 8))          ;; radius
              {:fill "white" :colour nil} ;; attrs
              (- 0.5 (rand))))            ;; offset

(defn draw-snowflake [buffer snowflake]
  (gfx/circle buffer
              (:x snowflake)
              (:y snowflake)
              (:r snowflake)
              (:attrs snowflake)))

(defn off-bottom?
  [snowflake]
  (> (:y snowflake) (.-height (:elem (:screen @canvas)))))

(defn off-sides?
  [snowflake]
  (or (< (:x snowflake) 0)
      (> (:x snowflake) (.-width (:elem (:screen @canvas))))))

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
  [buffer screen]
  (run! (partial draw-snowflake buffer) @snowflakes)
  (reset! snowflakes (map move-snowflake @snowflakes))

  ;; Snowflakes "die" at the bottom of the screen, and a replaced with a new one randomly somewhere at the top
  (let [alive-snowflakes (remove off-bottom? @snowflakes)
        num-to-make      (- @num-snowflakes (count alive-snowflakes))
        new-snowflakes   (map (fn [_] (create-snowflake (.-width (:elem (:screen @canvas))) 0)) (range num-to-make))]
    (reset! snowflakes (into [] (concat alive-snowflakes new-snowflakes)))
    (reset! snowflakes (mapv wrap-snowflake @snowflakes)))
    
  
  (gfx/flip buffer screen)
  (gfx/clear buffer))

(defn main
  []
  (let [screen (gfx/create-canvas "canvas" :fullscreen)
        buffer (gfx/create-offscreen-canvas (.-width  (:elem screen))
                                            (.-height (:elem screen)))]

    (reset! num-snowflakes (/ (* (.-width (:elem screen))
                                 (.-height (:elem screen)))
                              10000))
    
    (swap! canvas assoc :screen screen :buffer buffer)
    
    (gfx/clear buffer)
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
                                (fn [] (main-loop buffer screen)))
        (recur (dec counter))))))
