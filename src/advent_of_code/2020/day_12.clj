(ns advent-of-code.2020.day-12
  (:require
   [advent-of-code.2020.util :refer [file-lines]]))

(defn manhattan-magnitude
  [v]
  (+ (Math/abs (nth v 0)) (Math/abs (nth v 1))))

(def ship-start {:pos [0.0 0.0]
                 :facing 0.0})

(def vec-add (partial mapv +))

(def directions
  {\F (fn [ship dist]
        (update ship :pos vec-add  [(* dist (Math/cos (:facing ship)))
                                    (* dist (Math/sin (:facing ship)))]))
   \L (fn [ship angle]
        (update ship :facing + (Math/toRadians angle)))
   \R (fn [ship angle]
        (update ship :facing - (Math/toRadians angle)))
   \N (fn [ship dist]
        (update ship :pos vec-add [0 dist]))
   \S (fn [ship dist]
        (update ship :pos vec-add [0 (- dist)]))
   \E (fn [ship dist]
        (update ship :pos vec-add [dist 0]))
   \W (fn [ship dist]
        (update ship :pos vec-add [(- dist) 0]))})

(defn parse-direction
  [s]
  (let [[_ [dir] dist] (re-matches #"([FLRNSEW])(\d+)" s)
        dist (Long/parseLong dist)]
    [dir dist]))

(defn parse-input
  [lines]
  (map parse-direction lines))

(defn part-1
  []
  (let [input (file-lines "day-12.txt")
        input (parse-input input)
        ship (reduce (fn [ship instr]
                       ((directions (nth instr 0)) ship (nth instr 1)))
                     ship-start
                     input)]
    (manhattan-magnitude (:pos ship))))

(def waypoint-start
  [10.0 1.0])

(defn magnitude
  [v]
  (Math/sqrt
   (transduce (map #(Math/pow % 2.0)) + 0 v)))

(defn angle
  [v]
  (Math/atan2 (nth v 1) (nth v 0)))

(defn rotate
  [v amnt]
  (let [mag (magnitude v)
        old-angle (angle v)
        new-angle (+ old-angle (Math/toRadians amnt))]
    (mapv (partial * mag) [(Math/cos new-angle) (Math/sin new-angle)])))

(def waypoint-mods
  {\F (fn [ship times]
        (if (zero? times)
          ship
          (recur (update ship :pos (partial mapv +) (:waypoint ship)) (dec times))))
   \L (fn [ship amnt]
        (update ship :waypoint rotate amnt))
   \R (fn [ship amnt]
        (update ship :waypoint rotate (- amnt)))
   \N (fn [ship dist]
        (update ship :waypoint vec-add [0 dist]))
   \S (fn [ship dist]
        (update ship :waypoint vec-add [0 (- dist)]))
   \E (fn [ship dist]
        (update ship :waypoint vec-add [dist 0]))
   \W (fn [ship dist]
        (update ship :waypoint vec-add [(- dist) 0]))})

(defn part-2
  []
  (let [input (file-lines "day-12.txt")
        input (parse-input input)
        ship (reduce (fn [ship instr]
                       ((waypoint-mods (nth instr 0)) ship (nth instr 1)))
                     (assoc ship-start :waypoint waypoint-start)
                     input)]
    (manhattan-magnitude (:pos ship))))
