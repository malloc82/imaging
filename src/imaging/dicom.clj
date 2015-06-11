(ns imaging.dicom
  (:use clojure.core
        ;; clojure.java.io
        [clojure.core.matrix :as mat]
        [clojure.pprint :only [pprint]])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.matrix.operators :as op])
  (:import [java.io File FileInputStream DataInputStream]
           [ij IJ]
           ij.plugin.DICOM
           (java.awt.image BufferedImage Raster WritableRaster)
           (javax.swing JFrame JLabel JPanel)
           (java.awt Graphics Dimension Color)
           [javax.swing JScrollPane ImageIcon]
           java.awt.BorderLayout))

(defmacro timer [msg call]
  `(let [startTime# (System/nanoTime)]
     (let [retval#    ~call]
       (println (format "  ==> timing:  %20s" ~msg) (/ (- (System/nanoTime) startTime#) 1000000.0) "ms")
       retval#)))

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

(defn load-txt-image [src_fname & {:keys [show debug]
                                   :or   {show  false
                                          debug false}}]
  (let [[row col] (with-open [rdr (io/reader src_fname)]
                    (let [stream (line-seq rdr)]
                      [(count stream)
                       (count (str/split (first (take 1 stream)) #"\s"))]))
        data      (timer "loading txt data"
                         (mapv #(Double. ^String %) (str/split (slurp src_fname) #"[\s]+")))
        image     (BufferedImage. ^long col ^long row BufferedImage/TYPE_USHORT_GRAY)]
    (when debug
      (println "row =" row "  col =" col " count =" (count data) (class data)))
    (let [max_val ^double (timer "max" (apply max data))
          min_val ^double (timer "min" (apply min data))]
      (when debug
        (println "min =" min_val " max =" max_val))
      (timer "update pixel values"
             (let [scaled_range ^double (- max_val min_val)]
               (.setPixels ^WritableRaster (.getRaster image) 0 0 ^long col ^long row
                           ^doubles (double-array (mapv #(* (/ (- % min_val) scaled_range) 0xffff) data))))))
    (when show
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
                 (.show)))))
    data))

(defn load-txt-image_array [src_fname & {:keys [show debug]
                                         :or   {show  false
                                                debug false}}]
  (let [[row col] (with-open [rdr (io/reader src_fname)]
                    (let [stream (line-seq rdr)]
                      [(count stream)
                       (count (str/split (first (take 1 stream)) #"\s"))]))
        data      (timer "loading txt data"
                         (double-array
                          (mapv #(Double. ^String %) (str/split (slurp src_fname) #"[\s]+"))))
        image     (BufferedImage. col row BufferedImage/TYPE_USHORT_GRAY)]
    (when debug
      (println "row =" row "  col =" col " count =" (count data) (class data)))
    (let [;; max_val (timer "max" (apply max (vec data))
          ;; min_val (timer "min" (apply min (vec data))
          [^double min_val ^double max_val] (timer "min max : "
                                                   (areduce ^doubles data i
                                                            min-max [0.0 0.0]
                                                            (let [val  (aget ^doubles data i)
                                                                  minv ^double (min-max 0)
                                                                  maxv ^double (min-max 1)]
                                                              [(if (< val minv) val minv)
                                                               (if (> val maxv) val maxv)])))]
      (when debug
        (println "min =" min_val " max =" max_val))
      (timer "update pixel values"
             (let [scaled_range ^double (- max_val min_val)]
               (doseq [i (range (* row col))]
                 (aset ^doubles data ^long i (* (/ (- (aget ^doubles data i) min_val) scaled_range) 0xffff)))
               (.setPixels ^WritableRaster (.getRaster image) 0 0 ^long col ^long row ^doubles data))))
    (when show
      (timer "displaying"
             (let [canvas (JLabel. (ImageIcon. image))]
               (doto (JFrame.)
                 (.setSize (Dimension. (+ col 40) (* col 5)))
                 (-> .getContentPane (.add (doto (JScrollPane.)
                                             (-> .getViewport (.add canvas))
                                             (.setHorizontalScrollBarPolicy JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)
                                             (.setVerticalScrollBarPolicy JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED))))
                 (.setVisible true)
                 (.show)))))
    data))

(defn load-txt-image_matrix [src_fname & {:keys [show debug]
                                          :or   {show  false
                                                 debug false}}]
  (let [[row col] (with-open [rdr (io/reader src_fname)]
                    (let [stream (line-seq rdr)]
                      [(count stream)
                       (count (str/split (first (take 1 stream)) #"\s"))]))
        data      (timer "loading txt data"
                         (mat/array :vectorz (mapv #(Double. ^String %) (str/split (slurp src_fname) #"[\s]+"))))
        image     (BufferedImage. ^long col ^long row BufferedImage/TYPE_USHORT_GRAY)]
    (when debug
      (println "row =" row "  col =" col " count =" (count data) (class data)))
    (let [max_val (timer "max" (mat/emax data))
          min_val (timer "min" (mat/emin data))]
      (when debug
        (println "min =" min_val " max =" max_val))
      (timer "update pixel values"
             (let [scaled_range ^double (- max_val min_val)]
               (.setPixels ^WritableRaster (.getRaster image) 0 0 ^long col ^long row
                           ^doubles (to-double-array (op/* (op// (op/- data min_val) scaled_range) 0xffff))))))
    (when show
      (timer "displaying"
             (let [canvas (JLabel. (ImageIcon. image))]
               (doto (JFrame.)
                 (.setSize (Dimension. (+ col 40) (* col 5)))
                 (-> .getContentPane (.add (doto (JScrollPane.)
                                             (-> .getViewport (.add canvas))
                                             (.setHorizontalScrollBarPolicy JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)
                                             (.setVerticalScrollBarPolicy JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED))))
                 (.setVisible true)
                 (.show)))))
    data))

;; (doto (ij.io.FileInfo.)
;;   (-> .fileName (set! "x_0.txt"))
;;   (-> .directory (set! "resources/PCT")))

;; (DataInputStream. (FileInputStream. "resources/PCT/x_0.txt"))


