(ns sbr.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

;;sling <blade> runner
;;license <to kill> a mockingbird

(defn join-movies
  [movie1 movie2]
  (let [[beginning middle end] (reduce (fn [[beginning end] word]
                                         (let [s (take (count end) movie2)
                                               r (drop (count end) movie2)]
                                           (if (= s end)
                                             (reduced [beginning s r])
                                             [(conj beginning (first end)) (rest end)])))
                                       [() movie1]
                                       movie1)]
    (when-not (empty? middle)
      [beginning middle end])))

(defn movies
  []
  (with-open [r (io/reader (io/resource "movies.lst"))]
    (doall (map (fn [movie]
                  (str/split movie #" "))
                (line-seq r)))))

;; (movies)

;; (join-movies ["SLING" "BLADE"] ["BLADE" "RUNNER"])
;; (join-movies ["SLING" "BLADE"]["SLING" "BLADE"])
;; (join-movies ["license" "to" "kill"] ["to" "kill" "a" "mockingbird"])
;; (join-movies ["pretty" "woman"] ["superman" "2"])

(defn make-movie-graph
  [movies]
  (reduce (fn [graph movie1]
            (reduce (fn [graph movie2]
                      (if (= movie1 movie2)
                        graph
                        (let [overlap (join-movies movie1 movie2)]
                          (if (nil? overlap)
                            graph
                            (assoc graph movie1 (conj (graph movie1) movie2))))))
                    graph
                    movies))
          {}
          movies))

;; ((make-movie-graph (movies) ["SLING" "BLADE"])

(defn dfs
  [start neighbors-fn visited-fn]
  (loop [stack (list [start #{} []])]
    (if (empty? stack)
      nil ;; done
      (let [[vert visited parents] (first stack)
            stack (pop stack)]
        (if-not (visited vert)
          (do (visited-fn (conj parents vert))
              (recur (reduce (fn [stack neighbor]
                               (conj stack
                                     [neighbor (conj visited vert) (conj parents vert)]))
                             stack
                             (neighbors-fn vert))))
          (recur stack))))))



(defn- test-neighbors
  [[x y]]
  (reduce (fn [neighbors [ox oy]]
            (let [nx (+ ox x)
                  ny (+ oy y)]
              (if (or (< nx 0)
                      (>= nx 3)
                      (< ny 0)
                      (>= ny 3))
                neighbors
                (conj neighbors [nx ny]))))
          []
          (list [0 1] [1 0] [1 1] [0 -1] [-1 0] [-1 1] [1 -1] [-1 -1])))


(defn- test-graph
  []
  (let [graph [[0 1 2]
               [3 4 5]
               [6 7 8]]]
    (dfs [0 0]
         test-neighbors
         (fn [parents]
           (prn (mapv (fn [[x y]]
                        ((graph y) x))
                      parents))))))


(defn find-chains
  [found-chain-fn]
  (let [movies (movies)
        graph (make-movie-graph movies)]
    (reduce (fn [[best-n best-chain] movie]
              (dfs movie
                   graph
                   ))
            [0 nil]
            movies)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
