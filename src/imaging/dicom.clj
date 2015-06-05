(ns imaging.dicom
  (:use clojure.core
        clojure.java.io
        [clojure.core.matrix :as mat]
        [clojure.pprint :only [pprint]])
  (:require [clojure.string :as str])
  (:import [java.io File FileInputStream DataInputStream]
           [ij IJ]
           ij.plugin.DICOM
           (java.awt.image BufferedImage Raster WritableRaster)
           (javax.swing JFrame JLabel JPanel)
           (java.awt Graphics Dimension Color)
           [javax.swing JScrollPane ImageIcon]
           java.awt.BorderLayout))


(defn dicomread [filename]
  (let [img (IJ/openImage filename)
        row (.getHeight img)
        col (.getWidth img)]
    (reshape (mat/matrix :vectorz (double-array (.getPixels (.getProcessor img)))) [col row])))

(defn dicominfo [filename]
  (let [img (IJ/openImage filename)
        info (.getProperty img "Info")]
    (apply hash-map (map #(str/trim %) (str/split info #"[\n:]")))))

(defn txt2dicom [src_fname]
  (let [[row col] (with-open [rdr (reader src_fname)]
                    (let [stream (line-seq rdr)]
                      [(count stream)
                       (count (str/split (first (take 1 stream)) #"\s"))]))
        v_data    (str/split (IJ/openAsString src_fname) #"[\s]")
        s_data    (double-array (* row col))
        image     (BufferedImage. col row BufferedImage/TYPE_USHORT_GRAY)]
    (println "row =" row "  col =" col)
    (doseq [r (range row)
            c (range col)]
      (let [idx (+ (* r col) c)]
        (aset s_data idx (Double. (v_data idx)))))
    (.setPixels ^WritableRaster (.getRaster image) 0 0 col row s_data)
    (let [canvas (JLabel. (ImageIcon. image))]
      (doto (JFrame.)
        (.setSize (Dimension. col (* col 2)))
        ;; (-> .getContentPane (.add (doto (JPanel.)
        ;;                             (.setLayout (BorderLayout.))
        ;;                             (.add (doto (JScrollPane.)
        ;;                                     (-> .getViewport (.add canvas))
        ;;                                     (.setHorizontalScrollBarPolicy JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)
        ;;                                     (.setVerticalScrollBarPolicy JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED))
        ;;                                   BorderLayout/CENTER))))
        (-> .getContentPane (.add (doto (JScrollPane.)
                                    (-> .getViewport (.add canvas))
                                    (.setHorizontalScrollBarPolicy JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)
                                    (.setVerticalScrollBarPolicy JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED))))
        (.setVisible true)
        (.show))
      (println (.getSize canvas))
      canvas)))

(defmacro timer [call]
  `(let [startTime# (System/nanoTime)
         retval#    ~call]
     (println (/ (- (System/nanoTime) startTime#) 1000000.0) "ms")
     retval#))

;; (doto (ij.io.FileInfo.)
;;   (-> .fileName (set! "x_0.txt"))
;;   (-> .directory (set! "resources/PCT")))

;; (DataInputStream. (FileInputStream. "resources/PCT/x_0.txt"))


