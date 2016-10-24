(ns sbr.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

;;sling <blade> runner
;;license <to kill> a mockingbird

;; best chain: 241 movie titles:

;; Q AND A HANDFUL OF DUST TO GLORY ROAD GAMES PEOPLE PLAY NEW YORK COP LAND OF
;; THE DEAD END OF DAYS OF HEAVEN CAN WAIT UNTIL DARK BLUE WORLD TRADE CENTER
;; STAGE FRIGHT NIGHT FALLS ON MANHATTAN MURDER MYSTERY ALASKA SPIRIT OF THE
;; WILD IN THE STREETS OF FIRE ON THE MOUNTAIN MEN CRY BULLETS OVER BROADWAY
;; DANNY ROSE RED DAWN OF THE DEAD MAN ON CAMPUS MAN TROUBLE EVERY DAY OF THE
;; WOMAN ON TOP GUN CRAZY PEOPLE I KNOW WHERE IM GOING HOME ALONE 3 NINJAS
;; KNUCKLE UP CLOSE AND PERSONAL BEST FRIENDS AND LOVERS AND OTHER STRANGERS
;; WHEN WE MEET JOE BLACK RAIN MAN ON FIRE IN THE SKY HIGH SPIRITS OF THE DEAD
;; BANG BANG YOURE DEAD MAN WALKING AND TALKING ABOUT SEX AND THE OTHER MAN OF
;; THE HOUSE OF DRACULA DEAD AND LOVING IT HAPPENED AT THE WORLDS FAIR GAME OF
;; DEATH SHIP OF FOOLS RUSH IN GODS HANDS ON A HARD BODY AND SOUL FOOD OF LOVE
;; IN THE TIME OF MONEY FOR NOTHING BUT TROUBLE IN PARADISE ROAD HOUSE OF
;; FRANKENSTEIN AND THE MONSTER FROM HELL UP IN HARLEM RIVER DRIVE ME CRAZY AS
;; HELL NIGHT ON EARTH GIRLS ARE EASY COME EASY GO NOW YOU SEE HIM NOW YOU DONT
;; BOTHER TO KNOCK OFF THE MAP OF THE HUMAN HEART CONDITION RED RIVER OF NO
;; RETURN TO ME WITHOUT YOU CAN COUNT ON ME MYSELF I SPY HARD TIMES SQUARE DANCE
;; WITH A STRANGER IN THE HOUSE PARTY MONSTER IN A BOX OF MOON LIGHT OF DAY FOR
;; NIGHT AND DAY OF THE DEAD OF NIGHT MOTHER NIGHT AND THE CITY OF JOY RIDE THE
;; HIGH COUNTRY LIFE IS BEAUTIFUL PEOPLE WILL TALK OF ANGELS WITH DIRTY FACES OF
;; DEATH 4 LITTLE GIRLS GIRLS GIRLS WILL BE GIRLS OF SUMMER SCHOOL OF ROCK N
;; ROLL HIGH SCHOOL HIGH CRIMES OF PASSION IN THE DESERT BLUE CAR 54 WHERE ARE
;; YOU CANT TAKE IT WITH YOU LIGHT UP MY LIFE SO FAR FROM HOME THE ADVENTURES OF
;; YELLOW DOG RUN SILENT RUN DEEP BLUE SEA OF LOVE LIFE WITH FATHER OF THE BRIDE
;; OF THE WIND AND THE LION KING OF THE JUNGLE 2 JUNGLE BOOK OF LIFE OR
;; SOMETHING LIKE IT COULD HAPPEN TO YOU ONLY LIVE ONCE AROUND THE BEND OF THE
;; RIVER WILD THINGS TO COME AND GET IT HAPPENED ONE NIGHT WITH THE KING AND I
;; WANT TO LIVE AND LET DIE MOMMIE DIE MONSTER DIE HARD EIGHT AND A HALF WOMEN
;; IN LOVE WALKED IN OLD CALIFORNIA SPLIT SECOND BEST MEN WITH GUNS OF THE
;; MAGNIFICENT SEVEN RIDE WITH THE DEVIL RIDES OUT COLD FEVER PITCH BLACK HAWK
;; DOWN WITH LOVE AND DEATH WISH V THE FACE OF DEATH WISH UPON A STAR IS BORN
;; AMERICAN HISTORY X THE MAN WITH THE X RAY EYES OF AN ANGEL BABY SECRET OF THE
;; LOST LEGEND OF THE LOST BOYS LIFE AS A HOUSE PARTY 3 NINJAS KICK BACK TO THE
;; BEACH PARTY GIRL IN THE CADILLAC MAN OF THE YEAR OF THE DRAGON SEED OF CHUCKY

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

(defn overlap-movies
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
               (dfs movie
                    (partial sorted-best-neighbors graph)
                    [best-count best-chain]
                    (fn [[best-count best-chain] chain]
                      (let [chain-count (count chain)]
                        (if (> chain-count best-count)
                          (do
                            (prn "Best count: " best-count)
                            (doseq [word (overlap-movies best-chain)]
                              (print word ""))
                            (prn)
                            [chain-count chain])
                          [best-count best-chain])))))
             [0 nil]
             (sort-by-most-neighbors graph movies)))))

(defn -main
  ""
  [& args]
  (find-chains))
