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

           [org.dcm4che3.io DicomInputStream DicomOutputStream DicomEncodingOptions]
           [org.dcm4che3.io.DicomInputStream.IncludeBulkData]
           [org.dcm4che3.data Attributes Tag DatePrecision UID VR]
           [org.dcm4che3.util UIDUtils]
           ;; [org.dcm4che3.imageio.codec Compressor Decompressor TransferSyntaxType]
           ;; [org.dcm4che3.image PhotometricInterpretation]

           [java.io File InputStream FileInputStream FileOutputStream DataInputStream]
           java.io.InputStream
           [java.util Date TimeZone Scanner]
           [java.time ZoneId]
           [java.text SimpleDateFormat]
           (java.awt.image BufferedImage Raster WritableRaster)
           (javax.swing JFrame JLabel JPanel)
           (java.awt Graphics Dimension Color)
           [javax.swing JScrollPane ImageIcon]
           java.awt.BorderLayout))


(set! *warn-on-reflection* true)
;; (set! *unchecked-math* true)

(def ^:dynamic *timezone* (TimeZone/getTimeZone "America/Chicago"))
(def ^:dynamic *debug*    true)

(mat/set-current-implementation :vectorz)

(defmacro timer [msg call & {:keys [on]
                             :or   {on false}}]
  (if-not on
    `(let [startTime# (System/nanoTime)
           retval#  ~call
           endTime#   (System/nanoTime)]
       (println "  ==> timing:  " ~msg " " (/ (- endTime# startTime#) 1000000.0) "ms")
       retval#)
    call))

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

(defn load-txt-data ^Matrix [^String src_fname & {:keys [show debug]
                                                  :or   {show  false
                                                         debug false}}]
  (let [[^long col ^long row] (with-open [rdr (io/reader src_fname)]
                                (let [stream (line-seq rdr)]
                                  [^long (count (str/split (first stream) #"[\s]+"))
                                   ^long (count stream)]))
        data_mat  ^Matrix (let [data_str  (str/split (slurp src_fname) #"[\s]+")
                                array_len ^long (* row col)
                                data      ^doubles (double-array array_len)]
                            (loop [i ^long (long 0)]
                              (when (< i array_len)
                                (aset data i ^double (Double. ^String (data_str i)))
                                (recur (unchecked-inc i))))
                            (Matrix/wrap ^long row ^long col ^doubles data))]
    (when show
      (timer "displaying" (imshow data_mat)))
    data_mat))

;; (loop [i ^long (long 0)]
;;   (when (< i 10)
;;     (let [data (time (load-txt-data "resources/PCT/CTP404_merged/x_11.txt"))]
;;       (recur (unchecked-inc i)))))


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

(defn dcm4che_read [^String filename & {:keys [print]
                                        :or [print false]}]
  (let [stream (DicomInputStream. (File. filename))
        attributes (.readDataset stream -1 -1)]
    (when print
      (pprint (str/split (.toString attributes 300 100) #"[\n]")))
    attributes))

(defn print_attribute [^Attributes attr]
  (pprint (str/split (.toString attr 300 100) #"[\n]")))

(defn write_attribute ^Attributes [^Attributes attr ^String filename]
  (let [fmi (.createFileMetaInformation attr UID/ExplicitVRLittleEndian)]
    (doto (DicomOutputStream. (doto (File. filename)
                                (.createNewFile)))
      (.setEncodingOptions DicomEncodingOptions/DEFAULT)
      (.writeDataset fmi attr)
      (.close))
    fmi))

(defn dcm4che_write [mat_data ^String filename]
  (let [[row col]  (shape mat_data)
        curr_date  #^"[Ljava.util.Date;" (into-array Date [(Date.)])
        birthday   #^"[Ljava.util.Date;" (into-array Date [(.parse (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss Z") "1982-12-14T12:00:00 +0800")])

        attributes (doto (Attributes.)
                     (.setTimezone *timezone*)
                     (.setString Tag/SOPInstanceUID      VR/UI (UIDUtils/createUID))
                     (.setString Tag/SOPClassUID         VR/UI (UIDUtils/createUID))
                     ;; 0008
                     (.setString Tag/SpecificCharacterSet VR/CS "ISO_IR 100")
                     (.setString Tag/ImageType            VR/CS "Original\\PRIMARY\\AXIAL")
                     (.setDate   Tag/InstanceCreationDate VR/DA curr_date)
                     (.setDate   Tag/InstanceCreationTime VR/TM curr_date)

                     (.setDate   Tag/StudyDate            VR/DA curr_date)
                     (.setDate   Tag/SeriesDate           VR/DA curr_date)
                     (.setDate   Tag/AcquisitionDate      VR/DA curr_date)
                     (.setDate   Tag/ContentDate          VR/DA curr_date)

                     (.setDate   Tag/StudyTime            VR/TM curr_date)
                     (.setDate   Tag/SeriesTime           VR/TM curr_date)
                     (.setDate   Tag/AcquisitionTime      VR/TM curr_date)
                     (.setDate   Tag/ContentTime          VR/TM curr_date)

                     (.setString Tag/Manufacturer           VR/LO "Baylor Scientific Computation Laboratory")
                     (.setString Tag/InstitutionName        VR/LO "Baylor University")

                     (.setString Tag/ReferringPhysicianName VR/PN "Jesus Christ")
                     (.setString Tag/StationName            VR/SH "tardis-student1")

                     (.setString Tag/StudyDescription             VR/LO "DICOM construction test")
                     (.setString Tag/SeriesDescription            VR/LO "DICOM construction test")
                     (.setString Tag/NameOfPhysiciansReadingStudy VR/PN "")
                     (.setString Tag/OperatorsName                VR/PN "")
                     (.setString Tag/ManufacturerModelName        VR/LO "Clojure + dcm4che 3.3.7")
                     ;; 0010
                     (.setString Tag/PatientName              VR/PN "PCT")
                     (.setString Tag/PatientID                VR/LO "0000000000000")
                     (.setDate   Tag/PatientBirthDate         VR/DA birthday)
                     (.setString Tag/PatientSex               VR/CS "Male")
                     (.setString Tag/PatientAge               VR/AS "100")
                     (.setString Tag/AdditionalPatientHistory VR/LT "N/A")
                     ;; 0018
                     (.setString Tag/ScanOptions              VR/CS "whatever")
                     (.setDouble Tag/SliceThickness           VR/DS (double-array [1.0]))
                     (.setDouble Tag/KVP                      VR/DS (double-array [120.0]))
                     (.setDouble Tag/DataCollectionDiameter   VR/DS (double-array [500.0]))
                     (.setString Tag/SoftwareVersions         VR/LO "v0.001")
                     (.setString Tag/ProtocolName             VR/LO "DICOM test")
                     (.setDouble Tag/ReconstructionDiameter   VR/DS (double-array [100.0]))
                     (.setDouble Tag/DistanceSourceToDetector VR/DS (double-array [100.0]))
                     (.setDouble Tag/DistanceSourceToPatient  VR/DS (double-array [100.0]))
                     (.setDouble Tag/GantryDetectorTilt       VR/DS (double-array [0.0]))
                     (.setDouble Tag/TableHeight              VR/DS (double-array [100.0]))
                     (.setString Tag/RotationDirection        VR/CS "CW")
                     (.setString Tag/ExposureTime             VR/IS "1000")
                     (.setString Tag/XRayTubeCurrent          VR/IS "200")
                     (.setString Tag/Exposure                 VR/IS "3")
                     (.setString Tag/FilterType               VR/SH "Some Filter")
                     (.setString Tag/GeneratorPower           VR/IS "24000")
                     (.setDouble Tag/FocalSpots               VR/DS (double-array [0.7]))
                     (.setString Tag/ConvolutionKernel        VR/SH "STANDARD")
                     (.setString Tag/PatientPosition          VR/CS "HFS")
                     (.setDouble Tag/RevolutionTime           VR/FD (double-array [1.0]))
                     (.setDouble Tag/SingleCollimationWidth   VR/FD (double-array [0.625]))
                     (.setDouble Tag/TotalCollimationWidth    VR/FD (double-array [40.0]))
                     (.setDouble Tag/TableSpeed               VR/FD (double-array [39.375]))
                     (.setDouble Tag/TableFeedPerRotation     VR/FD (double-array [39.375]))
                     (.setDouble Tag/SpiralPitchFactor        VR/FD (double-array [0.984375]))
                     ;; 0020
                     (.setString Tag/StudyInstanceUID           VR/UI (UIDUtils/createUID))
                     (.setString Tag/SeriesInstanceUID          VR/UI (UIDUtils/createUID))
                     (.setString Tag/StudyID                    VR/SH "00000")
                     (.setString Tag/SeriesNumber               VR/IS "11111")
                     (.setString Tag/AcquisitionNumber          VR/IS "0")
                     (.setString Tag/InstanceNumber             VR/IS "0")
                     (.setDouble Tag/ImagePositionPatient       VR/DS  (double-array [0.0 0.0 0.0]))
                     (.setDouble Tag/ImageOrientationPatient    VR/DS  (double-array [0.0 1.0
                                                                                      0.0 0.0
                                                                                      0.0 -1.0]))
                     (.setString Tag/FrameOfReferenceUID        VR/UI (UIDUtils/createUID))
                     (.setString Tag/PositionReferenceIndicator VR/LO "IC")
                     (.setDouble Tag/SliceLocation              VR/DS (double-array [0.0]))
                     ;; 0028
                     (.setDouble Tag/PixelSpacing        VR/DS (double-array [1.0 1.0]))
                     (.setInt    Tag/Rows                VR/US (int-array    [row]))
                     (.setInt    Tag/Columns             VR/US (int-array    [col]))
                     (.setInt    Tag/BitsAllocated       VR/US (int-array    [16]))
                     (.setInt    Tag/BitsStored          VR/US (int-array    [16]))
                     (.setInt    Tag/HighBit             VR/US (int-array    [15]))
                     (.setInt    Tag/PixelRepresentation VR/US (int-array    [1]))
                     (.setInt    Tag/SamplesPerPixel     VR/US (int-array    [1]))
                     ;; 7FE0
                     (.setInt    Tag/PixelData           VR/OW (into-array Integer/TYPE (to-double-array mat_data))))]
    (doto (DicomOutputStream. (doto (File. filename)
                                (.createNewFile)))
      (.writeDataset (.createFileMetaInformation attributes UID/CTImageStorage) attributes)
      (.writeHeader  Tag/SequenceDelimitationItem nil 0)
      (.close))
    attributes))

;; (doto (ij.io.FileInfo.)
;;   (-> .fileName (set! "x_0.txt"))
;;   (-> .directory (set! "resources/PCT")))

;; (DataInputStream. (FileInputStream. "resources/PCT/x_0.txt"))

