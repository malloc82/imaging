(ns imaging.dicom
  (:use clojure.core
        [clojure.core.matrix :as mat]
        [clojure.pprint :only [pprint]])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.matrix.operators :as op])
  (:import [mikera.matrixx Matrix]
           [ij IJ]
           ij.plugin.DICOM
           ij.io.ImageWriter

           [org.dcm4che3.io DicomInputStream DicomOutputStream]
           [org.dcm4che3.io.DicomInputStream.IncludeBulkData]
           [org.dcm4che3.data Attributes Tag UID VR]
           [org.dcm4che3.util UIDUtils]

           [java.io File InputStream FileInputStream FileOutputStream DataInputStream]
           java.io.InputStream
           (java.awt.image BufferedImage Raster WritableRaster)
           (javax.swing JFrame JLabel JPanel)
           (java.awt Graphics Dimension Color)
           [javax.swing JScrollPane ImageIcon]
           java.awt.BorderLayout))

(mat/set-current-implementation :vectorz)

(defmacro timer [msg call]
  `(let [startTime# (System/nanoTime)]
     (let [retval#    ~call]
       (println "  ==> timing:  " ~msg " " (/ (- (System/nanoTime) startTime#) 1000000.0) "ms")
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

(defn imshow [^Matrix data]
  (let [[row col] (mat/shape data)
        image     ^BufferedImage (BufferedImage. ^long col ^long row BufferedImage/TYPE_USHORT_GRAY)
        max_val   ^double (timer "emax" (mat/emax data))
        min_val   ^double (timer "emin" (mat/emin data))
        scaled_range ^double (double (- max_val min_val))]
    (timer "update pixel values"
           (.setPixels ^WritableRaster (.getRaster ^BufferedImage image) 0 0 ^long col ^long row
                       ^doubles (mat/to-double-array (op/* (op// (op/- data min_val) scaled_range) 0xffff))))
    (let [canvas (JLabel. (ImageIcon. image))]
      (doto (JFrame.)
        (.setSize (Dimension. (+ col 50) (+ row 50)))
        (-> .getContentPane
            (.add (doto (JScrollPane.)
                    (-> .getViewport (.add canvas))
                    (.setHorizontalScrollBarPolicy JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)
                    (.setVerticalScrollBarPolicy JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED))))
        (.setVisible true)
        (.show)))))

(defn load-txt-data [src_fname & {:keys [show debug]
                                   :or   {show  false
                                          debug false}}]
  (let [[row col] (with-open [rdr (io/reader src_fname)]
                    (let [stream (line-seq rdr)]
                      [(count stream)
                       (count (str/split (first (take 1 stream)) #"\s"))]))
        data      (timer "loading txt data"
                         (Matrix/wrap row col
                                      (double-array (mapv #(Double. ^String %)
                                                          (str/split (slurp src_fname) #"[\s]+")))))]
    (when show
      (timer "displaying" (imshow data)))
    data))

(defn load-txt-image [src_fname & {:keys [show debug]
                                   :or   {show  false
                                          debug false}}]
  (let [[row col] (with-open [rdr (io/reader src_fname)]
                    (let [stream (line-seq rdr)]
                      [(count stream)
                       (count (str/split (first (take 1 stream)) #"\s"))]))
        data      (timer "loading txt data"
                         (Matrix/wrap row col
                                      (double-array (mapv #(Double. ^String %)
                                                          (str/split (slurp src_fname) #"[\s]+")))))
        image     (BufferedImage. ^long col ^long row BufferedImage/TYPE_USHORT_GRAY)]
    (when debug
      (println "row =" row "  col =" col " count =" (count data) (class data)))
    (let [max_val (timer "max" (mat/emax data))
          min_val (timer "min" (mat/emin data))
          scaled_range ^double  (- max_val min_val)]
      (when debug
        (println "min =" min_val " max =" max_val))
      (timer "update pixel values"
             (.setPixels ^WritableRaster (.getRaster image) 0 0 ^long col ^long row
                         ^doubles (mat/to-double-array (op/* (op// (op/- data min_val) scaled_range) 0xffff))))
      (when show
        (timer "displaying"
               (let [canvas (JLabel. (ImageIcon. image))]
                 (doto (JFrame.)
                   (.setSize (Dimension. (+ col 40) (* col 5)))
                   (-> .getContentPane
                       (.add (doto (JScrollPane.)
                               (-> .getViewport (.add canvas))
                               (.setHorizontalScrollBarPolicy JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)
                               (.setVerticalScrollBarPolicy JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED))))
                   (.setVisible true)
                   (.show)))))
      (mat/to-double-array data))))

(defn dcm4che_read [filename]
  (let [stream (DicomInputStream. (File. filename))
        attributes (.readDataset stream -1 -1)]
    (pprint (str/split (.toString attributes 300 80) #"[\n]"))))

(defn dcm4che_write [mat_data]
  (let [[row col]  (shape mat_data)
        attributes (doto (Attributes.)
                     (.setString Tag/StudyInstanceUID  VR/UI (UIDUtils/createUID))
                     (.setString Tag/SeriesInstanceUID VR/UI (UIDUtils/createUID))
                     (.setString Tag/SOPInstanceUID    VR/UI (UIDUtils/createUID))
                     (.setString Tag/SOPClassUID       VR/UI (UIDUtils/createUID))

                     (.setDouble Tag/PixelSpacing  VR/DS (double-array [1.0 1.0]))
                     (.setInt    Tag/Rows          VR/US (int-array    [row]))
                     (.setInt    Tag/Columns       VR/US (int-array    [col]))
                     (.setInt    Tag/BitsAllocated VR/US (int-array    [16]))
                     (.setInt    Tag/BitsStored    VR/US (int-array    [16]))
                     (.setInt    Tag/HighBit       VR/US (int-array    [15]))
                     (.setInt    Tag/PixelRepresentation VR/US (int-array [1]))
                     (.setInt    Tag/SamplesPerPixel     VR/US (int-array [1]))
                     (.setInt    Tag/PixelData     VR/OW (into-array Integer/TYPE (to-double-array mat_data))))
        ostream    (DicomOutputStream. (doto (File. "resources/dcm_out.dcm")
                                        (.createNewFile)))]
    (.writeDataset ostream  (.createFileMetaInformation attributes UID/CTImageStorage) attributes)
    (.writeHeader  ostream  Tag/SequenceDelimitationItem nil 0)
    (.close ostream)
    attributes))

;; (doto (ij.io.FileInfo.)
;;   (-> .fileName (set! "x_0.txt"))
;;   (-> .directory (set! "resources/PCT")))

;; (DataInputStream. (FileInputStream. "resources/PCT/x_0.txt"))

