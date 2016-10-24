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

;; (join-movies ["SLING" "BLADE"] ["BLADE" "RUNNER"])
;; (join-movies ["SLING" "BLADE"]["SLING" "BLADE"])
;; (join-movies ["license" "to" "kill"] ["to" "kill" "a" "mockingbird"])
;; (join-movies ["pretty" "woman"] ["superman" "2"])

(defn movies
  []
  (with-open [r (io/reader (io/resource "movies.lst"))]
    (doall (map (fn [movie]
                  (str/split movie #" "))
                (line-seq r)))))

;; (movies)

(defn sort-by-most-neighbors
  "sorts a list of movies by how many movies they connect to"
  [graph movies]
  (sort (fn [a b] (> (count (graph a))
                     (count (graph b)))) movies))

(defn make-movie-graph
  [movies]
  (let [initial-graph
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
                movies)]
    (reduce (fn [graph movie]
              (assoc graph movie
                     (sort-by-most-neighbors graph (graph movie))))
            graph
            (keys graph))))

;; ((make-movie-graph (movies) ["SLING" "BLADE"])

(defn dfs
  [start neighbors-fn acc visited-fn]
  (loop [stack (list [start #{} []])
         acc acc]
    (if (empty? stack)
      acc ;; done
      (let [[vert visited parents] (first stack)
            stack (pop stack)]
        (if-not (visited vert)
          (let [acc (visited-fn acc (conj parents vert))]
            (recur (reduce (fn [stack neighbor]
                             (conj stack
                                   [neighbor (conj visited vert) (conj parents vert)]))
                           stack
                           (neighbors-fn vert))
                   acc))
          (recur stack acc))))))

;; (defn- test-neighbors
;;   [[x y]]
;;   (reduce (fn [neighbors [ox oy]]
;;             (let [nx (+ ox x)
;;                   ny (+ oy y)]
;;               (if (or (< nx 0)
;;                       (>= nx 3)
;;                       (< ny 0)
;;                       (>= ny 3))
;;                 neighbors
;;                 (conj neighbors [nx ny]))))
;;           []
;;           (list [0 1] [1 0] [1 1] [0 -1] [-1 0] [-1 1] [1 -1] [-1 -1])))

;; (defn- test-graph
;;   []
;;   (let [graph [[0 1 2]
;;                [3 4 5]
;;                [6 7 8]]]
;;     (dfs [0 0]
;;          test-neighbors
;;          (fn [parents]
;;            (prn (mapv (fn [[x y]]
;;                         ((graph y) x))
;;                       parents))))))

(defn print-overlapping-movies
  [movies]
  (second
   (reduce (fn [[prev-movie combined] movie]
             (if (empty? prev-movie)
               [movie (concat combined movie)]
               (let [[start middle end] (join-movies prev-movie movie)]
                 [movie (concat combined end)])))
           [[] []]
           movies)))

;; (print-overlapping-movies [["SLING" "BLADE"]["BLADE"]["BLADE" "RUNNER"]])
;; (print-overlapping-movies [["DUST" "TO" "GLORY"] ["GLORY" "ROAD"] ["ROAD" "GAMES"]])
;; (print-overlapping-movies [["Q" "AND" "A"] ["A" "HANDFUL" "OF" "DUST"] ["DUST" "TO" "GLORY"]])

(defn find-chains
  ([]
   (find-chains (make-movie-graph movies)))
  ([graph]
   (let [movies (movies)]
     (reduce (fn [[best-count best-chain] movie]
               (prn "Best count: " best-count)
               (print-overlapping-movies best-chain)
               (dfs movie
                    (partial sorted-best-neighbors graph)
                    [best-count best-chain]
                    (fn [[best-count best-chain] chain]
                      (let [chain-count (count chain)]
                        (if (> chain-count best-count)
                          (do
                            (prn chain-count chain)
                            [chain-count chain])
                          [best-count best-chain])))))
             [0 nil]
             (sort-by-most-neighbors graph movies)))))

(defn -main
  ""
  [& args]
  (find-chains))
