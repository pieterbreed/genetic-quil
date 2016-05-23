(ns my-art.core
  (:require [my-art.genes :as genes]
            [my-art.picture :as pic]
            [clojure.core.async :as async]))

(defn measure-population [environment population]
  (let [result-chs (vec
                    (for [creature population]
                      (async/go
                        (let [img (pic/draw-creature environment
                                                     creature)
                              fitness (pic/measure-creature-fitness environment
                                                                    img)]
                          (vector fitness
                                  img
                                  creature)))))]
    (loop [ch (first result-chs)
           chs (rest result-chs)
           fittest nil]
      (if (nil? ch)
        fittest
        (let [[fitness img creature] (async/<! ch)
              new-fittest (if (and (not (nil? fittest))
                                   (> fitness (:fitness fittest)))
                            fittest
                            {:fitness fitness
                             :img img
                             :creature creature})]
          (recur (first chs)
                 (rest chs)
                 new-fittest))))))
          

(defn find-picture-genetically [path population-size creature-genes-nr]
  (let [environment (pic/create-image-environment path)
        seed-population (take population-size
                              (repeatedly #(genes/make-random-creature
                                            creature-genes-nr)))]
    
    ))
    
    

(defn -main [& args])
