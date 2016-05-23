(ns my-art.picture
  (:require [mikera.image.core :as imagez]
            [mikera.image.colours :as col])
  (:import [java.awt Polygon Color]))

(defn create-image-environment [path]
  (let [dest-img (imagez/load-image path)]
    {:target-image dest-img}))

(defn draw-creature [environment creature]
  (let [target (:target-image environment)
        target-height (imagez/height target)
        target-width (imagez/width target)
        
        perc->height (fn [p] (int (* p target-height)))
        perc->width (fn [p] (int (* p target-width)))

        img (imagez/new-image target-width target-height true)
        gfx (imagez/graphics img)]
    (loop [genes creature
           gene (first genes)]
      (if-not (nil? gene)
        (do
          (condp = (:type gene)
            :triangle (let [p->x (fn [i] (-> gene :data (get i) perc->width))
                            p->y (fn [i] (-> gene :data (get i) perc->height))
                            xs (int-array (map p->x [0 2 4]))
                            ys (int-array (map p->y [1 3 5]))]
                        (.fill gfx (new Polygon xs ys 3)))
            :color (let [i->color (fn [i] (-> gene :data (get i) int))]
                     (.setColor gfx (new Color
                                         (i->color 0)
                                         (i->color 1)
                                         (i->color 2)
                                         (i->color 3))))
            nil nil)
          (recur (rest genes)
                 (first (rest genes))))))
    img))

(defn measure-creature-fitness [environment creature-img]
  (let [target-img (-> environment :target-image)]
    (->> [target-img creature-img]
         (map imagez/get-pixels)
         (apply map #(java.lang.Math/abs (- %1 %2)))
         (reduce +)
         java.lang.Math/sqrt)))
  
