(ns my-art.core
  (:require [my-art.genes :as genes]
            [my-art.picture :as pic]
            [clojure.core.async :as async]))

(defn start-quit-listener [population-ch quit-ch]
  (async/go (let [_ (async/<! quit-ch)]
              (println "closing population-ch")
              (async/close! population-ch))))

(defn start-creature-measurer [environment population-ch measured-creature-ch]
  (async/go
    (loop [cr (async/<! population-ch)]
      (when (not (nil? cr))
        (async/go (let [img (pic/draw-creature environment
                                               cr)
                        fitness (pic/measure-creature-fitness environment
                                                              img)]
                    (async/>! measured-creature-ch
                              {:fitness fitness
                               :img img
                               :creature cr})))
        (recur (async/<! population-ch))))
    (println "closing measure-creature-ch")
    (async/close! measured-creature-ch)))

(defn start-measurement-listener [measured-creature-ch fittest-atom]
  (async/go
    (println "waiting for the first measured creature")
    (loop [measurement (async/<! measured-creature-ch)]
      (if (not (nil? measurement))
        (let [{:keys [fitness img creature]} measurement]
          (if (or (nil? @fittest-atom)
                  (< fitness (:fitness @fittest-atom)))
            (do
              (println (str "found a new fittest (" fitness ")"))
              (pic/show img "fittest")
              (swap! fittest-atom (fn [fittest new-measurement]
                                    (if (or (nil? fittest)
                                            (< (:fitness new-measurement)
                                               (:fitness fittest)))
                                      new-measurement
                                      fittest))
                     measurement)))
          (recur (async/<! measured-creature-ch)))))))

(defn start-population-feeder [population-ch fittest]
  (async/go
    (println "starting a feeding cycle")
    (loop [population (if (nil? @fittest)
                        (repeatedly
                         100
                         #(genes/make-random-creature 100))
                        (repeatedly
                         100
                         #(genes/make-mutated-creature (:creature @fittest))))]
      (let [x (first population)]
        (if (nil? x)
          (start-population-feeder population-ch fittest)
          (when-let [_ (async/>! population-ch x)]
            (recur (rest population))))))
    (println "feeder cycle stopped")))

(defn fittest-seeker [environment
                      quit-ch]
  (let [fittest-atom (atom nil)
        measured-creature-ch (async/chan)
        population-ch (async/chan)]
    
    ;; ensure we'll stop this engine when something comes
    ;; over the quit-ch
    (start-quit-listener population-ch quit-ch)
    
    ;; listen for a new child happening in the population channel
    ;; and spin up a go channel to measure it
    (start-creature-measurer environment population-ch measured-creature-ch)
    
    ;; listen for measured creatures and keep track of the
    ;; best one we've got so far
    (start-measurement-listener measured-creature-ch fittest-atom)

    ;; this guy keeps the population-ch filled up with good guesses
    (start-population-feeder population-ch fittest-atom)))
      

(defn -main [& args])
