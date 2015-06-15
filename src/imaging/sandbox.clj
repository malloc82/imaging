(ns imaging.sandbox
  (:use clojure.core
        [clojure.core.matrix :as mat]))

(defn array-max [^doubles arr]
  (let [len (alength arr)]
    (loop [m Double/NEGATIVE_INFINITY indx 0]
      (if (< indx len)
        (recur (max m (aget arr indx)) (unchecked-inc indx))
        m))))

(let [vs (amap (double-array 1280000) idx ret (Math/random))] (time (array-max vs)))

(let [v (mat/array :vectorz (range 1280000))] (time (emax v)))
