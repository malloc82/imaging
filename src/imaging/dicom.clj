(ns imaging.dicom
  (:use clojure.core
        clojure.java.io
        [clojure.core.matrix :as mat]
        [clojure.pprint :only [pprint]])
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.io File FileInputStream DataInputStream]
           [ij IJ]
           ij.plugin.DICOM
           (java.awt.image BufferedImage Raster WritableRaster)
           (javax.swing JFrame JLabel JPanel)
           (java.awt Graphics Dimension Color)
           [javax.swing JScrollPane ImageIcon]
           java.awt.BorderLayout))

(def test_samples "resources/PCT/CTP404_merged")

(defn dicomread [filename]
  (let [img (IJ/openImage filename)
        row (.getHeight img)
        col (.getWidth img)]
    (reshape (mat/matrix :vectorz (double-array (.getPixels (.getProcessor img)))) [col row])))

(defn dicominfo [filename]
  (let [img (IJ/openImage filename)
        info (.getProperty img "Info")]
    (apply hash-map (map #(str/trim %) (str/split info #"[\n:]")))))

(def dicom_meta_list (keys (dicominfo "resources/dicom_template.dcm")))

(def dicom_meta_map  (reduce
                      (fn [s [k v]]
                        (conj s {k v}))
                      {}
                      (map #(list (subs % 0 9) (subs % 11))
                           (keys (dicominfo "resources/dicom_template.dcm")))))

(defn min-max-by-columns [s]
  (reduce (fn [[smallest largest] y]
            [(min smallest y) (max largest y)]
            [(doall (map min smallest y)) (doall (map max largest y))])
          [(first s) (first s)]
          s))

(defn load-txt-image [src_fname & {:keys [show debug]
                                   :or   {show  false
                                          debug false}}]
  (let [[row col] (with-open [rdr (reader src_fname)]
                    (let [stream (line-seq rdr)]
                      [(count stream)
                       (count (str/split (first (take 1 stream)) #"\s"))]))
        data      (timer "loading txt data"
                         (map #(Double. %) (str/split (slurp "resources/PCT/CTP404_merged/x_10.txt") #"[\s]+")))
        image     (BufferedImage. col row BufferedImage/TYPE_USHORT_GRAY)]
    (when debug
      (println "row =" row "  col =" col " count =" (count data) (class data)))
    (let [max_val (timer "max" (apply max data))
          min_val (timer "min" (apply min data))
          ;; [min_val max_val] (timer "min max : " (reduce (fn [[_min _max] elem]
          ;;                                                 [(if (< elem _min) elem _min)
          ;;                                                  (if (> elem _max) elem _max)])
          ;;                                               [0.0 0.0]
          ;;                                               data))
          ]
      (when debug
        (println "min =" min_val " max =" max_val))
      (timer "update pixel values"
             (let [scaled_range (- max_val min_val)]
               (.setPixels ^WritableRaster (.getRaster image) 0 0 col row
                           (double-array (map #(* (/ (- % min_val) scaled_range) 0xffff) data))))))
    (timer "displaying"
           (let [canvas (JLabel. (ImageIcon. image))]
             (doto (JFrame.)
               (.setSize (Dimension. (+ col 40) (* col 5)))
               ;; (-> .getContentPane
               ;;     (.add (doto (JPanel.)
               ;;             (.setLayout (BorderLayout.))
               ;;             (.add (doto (JScrollPane.)
               ;;                     (-> .getViewport (.add canvas))
               ;;                     (.setHorizontalScrollBarPolicy JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)
               ;;                     (.setVerticalScrollBarPolicy JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED))
               ;;                   BorderLayout/CENTER))))
               (-> .getContentPane (.add (doto (JScrollPane.)
                                           (-> .getViewport (.add canvas))
                                           (.setHorizontalScrollBarPolicy JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)
                                           (.setVerticalScrollBarPolicy JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED))))
               (.setVisible true)
               (.show))))
    data))

(defmacro timer [msg call]
  `(let [startTime# (System/nanoTime)]
     (let [retval#    ~call]
       (println "  ==> timing:  " ~msg " " (/ (- (System/nanoTime) startTime#) 1000000.0) "ms")
       retval#)))

;; (doto (ij.io.FileInfo.)
;;   (-> .fileName (set! "x_0.txt"))
;;   (-> .directory (set! "resources/PCT")))

;; (DataInputStream. (FileInputStream. "resources/PCT/x_0.txt"))


