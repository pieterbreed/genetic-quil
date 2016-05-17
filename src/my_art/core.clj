(ns my-art.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn load-image [path]
  nil)

(defn make-fitness-function [image-path]
  (let [target-img (q/load-image image-path)
        dest-pixels (q/pixels target-img)
        nr-pixels (count dest-pixels)]
    [(fn [image]
       (let [child-pixels (q/pixels image)
             child-nr-pixels (count child-pixels)]
         (when (not= nr-pixels child-nr-pixels)
           (throw (Exception. "The images do not have the same number of pixels")))
         (as-> (map #(Math/abs (- %1 %2)) dest-pixels child-pixels) $
               (reduce + $)
               (Math/sqrt $))))
     (.-width target-img)
     (.-height target-img)]))


(defn setup []
  (q/frame-rate 30)
  (q/smooth))

(defn make-perc-of-height-fn [height-perc]
  (fn []
    (* (q/height) height-perc)))

(defn make-perc-of-width-fn [width-perc]
  (fn []
    (* (q/width) width-perc)))

(defn make-perc-of-radians-fn [angle-perc]
  (fn []
    (q/radians (* 360 angle-perc))))

(defn get-adjustment-values []
  (repeatedly #(/ (- 50 (rand-int 100))
                  100)))

(defn adjust-values [vs error_rate]
  (loop [result []
         avs (get-adjustment-values)
         x (first vs)
         rst (rest vs)]
    (if (nil? x) result
        (let [v (if (>= error_rate
                        (/ (rand-int 100) 100))
                  x
                  (as-> (first avs) $
                        (+ x $)
                        (min 1.0 $)
                        (max 0.0 $)))]
          (recur (conj result v)
                 (drop 1 avs)
                 (first rst)
                 (rest rst))))))

(defn make-ellipse [x y width height]
  {:type :ellipse
   :draw-fn q/ellipse
   :make-child (fn [er_perc]
                 (apply make-ellipse (adjust-values [x y width height] er_perc)))
   :args [(make-perc-of-width-fn x)
          (make-perc-of-height-fn y)
          (make-perc-of-width-fn width)
          (make-perc-of-height-fn height)]})

(defn make-arc [x y width height start stop]
  {:type :arc
   :draw-fn q/arc
   :make-child (fn [ep] (apply make-arc (adjust-values [x y width height start stop] ep)))
   :args [(make-perc-of-width-fn x)
          (make-perc-of-height-fn y)
          (make-perc-of-width-fn width)
          (make-perc-of-height-fn height)
          (make-perc-of-radians-fn start)
          (make-perc-of-radians-fn stop)]})

(defn make-triangle [x1 y1 x2 y2 x3 y3]
  {:type :triangle
   :draw-fn q/triangle
   :make-child (fn [ep] (apply make-triangle (adjust-values [x1 y1 x2 y2 x3 y3] ep)))
   :args [(make-perc-of-width-fn x1)
          (make-perc-of-height-fn y1)
          (make-perc-of-width-fn x2)
          (make-perc-of-height-fn y2)
          (make-perc-of-width-fn x3)
          (make-perc-of-height-fn y3)]})

(defn make-fill-color [r g b]
  {:type :fill-color
   :draw-fn q/fill
   :make-child (fn [ep] (apply make-fill-color (adjust-values [r g b] ep)))
   :args [(constantly (* r 255 ))
          (constantly (* g 255))
          (constantly (* b 255))]})

(defn make-random-shape []
  (let [stream (repeatedly #(/ (rand-int 100) 100))
        shapes [(apply make-fill-color (take 3 stream))
                (apply make-ellipse (take 4 stream))
                (apply make-arc (take 6 stream))
                (apply make-triangle (take 6 stream))]]
    (get shapes (rand-int (count shapes)))))

(defn random-drawing [nr]
  (take nr (repeatedly make-random-shape)))

(defn draw-shape [shape]
  (apply (:draw-fn shape)
         (map #(%1) (:args shape))))

(defn make-child [p error-rate]
  (map #((:make-child %1) error-rate) p))

(defn make-offspring [parent nr-children]
  (let [children-copies (for [_ (range nr-children)]
                          (make-child parent))]))

(defn make-drawer [drawing]
  (fn []
    (q/no-stroke)
    (loop [c (first drawing)
           nxt (rest drawing)]
      (when (not (nil? c))
        (draw-shape c)
        (recur (first nxt) (rest nxt))))))

(doseq [drawing (loop [counter 0
                       parent (random-drawing 500)
                       result []]
                  (if (<= counter 0) result
                      (recur (dec counter)
                             (make-child parent 0.75)
                             (conj result parent))))]
  (q/defsketch example
    :title "Example"
    :setup setup
    :draw (make-drawer drawing)
    :size [200 200]))

(defn -main [& args])
