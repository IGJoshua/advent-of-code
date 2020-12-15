(ns advent-of-code.2020.day-14
  (:require
   [advent-of-code.2020.util :refer [file-lines]])
  (:refer-clojure
   :exclude [eval]))

(defn long->bit-seq
  [n]
  (map #(bit-test n %) (reverse (range 36))))

(defn bit-seq->long
  [s]
  (reduce #(if (second %2)
             (bit-set %1 (first %2))
             %1)
          0
          (map-indexed #(vector %1 (nth s %2))
                       (reverse (range 36)))))

(defn apply-mask
  [m n]
  (bit-seq->long
   (map #(cond
           (nil? %1) %2
           :otherwise %1)
        m
        (long->bit-seq n))))

(def mask-chars
  {\X nil
   \1 true
   \0 false})

(defn parse-mask
  [s]
  (map mask-chars s))

(defn write
  [mem mask idx val]
  (assoc mem idx (apply-mask mask val)))

(def mask-re
  #"^mask = ([X10]{36})$")

(def mem-write-re
  #"^mem\[(\d+)\] = (\d+)$")

(defn eval
  [writer {:keys [mem mask] :as state} s]
  (if-let [[_ new-mask] (re-matches mask-re s)]
    (assoc state :mask (parse-mask new-mask))
    (when-let [[_ addr val] (re-matches mem-write-re s)]
      (update state :mem writer mask (Long/parseLong addr) (Long/parseLong val)))))

(def initial-state
  {:mem {}
   :mask (repeat 36 nil)})

(defn part-1
  []
  (let [input (file-lines "day-14.txt")]
    (reduce +' 0 (vals (:mem (reduce (partial eval write) initial-state input))))))

(defn decode-address
  [m n]
  (letfn [(merge-addr [addr floating]
            (bit-seq->long
             (:bit-seq
              (reduce
               (fn [{:keys [float-idx bit-seq] :as acc} [mask-bit orig]]
                 (assoc acc
                        :bit-seq (conj bit-seq
                                       (cond
                                         (false? mask-bit) orig
                                         (true? mask-bit) mask-bit
                                         :otherwise (bit-test floating float-idx)))
                        :float-idx (if (nil? mask-bit) (inc float-idx) float-idx)))
               {:bit-seq []
                :float-idx 0}
               (map vector
                    m
                    (long->bit-seq addr))))))
          (addr-seq [num-floating floating]
            (lazy-seq
             (when (< floating num-floating)
               (cons (merge-addr n floating)
                     (addr-seq num-floating (inc floating))))))]
    (addr-seq (Math/pow 2 (count (filter nil? m))) 0)))

(defn write-all
  [mem addrs val]
  (reduce #(assoc %1 %2 val) mem addrs))

(defn part-2
  []
  (let [input (file-lines "day-14.txt")]
    (reduce +' 0
            (vals (:mem (reduce
                         (partial eval #(write-all %1 (decode-address %2 %3) %4))
                         initial-state input))))))
