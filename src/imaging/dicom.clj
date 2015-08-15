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

(defn print_attribute
  "Pretty print an attribute in REPL."
  [^Attributes attr]
  (pprint (str/split (.toString attr 300 100) #"[\n]")))

(defn write_attribute
  "Write existing DICOM attribute to file to create DICOM file.
   Returning newly created File Meta Information."
  ^Attributes [^Attributes attr ^String path]
  (let [fmi (.createFileMetaInformation attr UID/ExplicitVRLittleEndian)]
    (doto (DicomOutputStream. (doto (File. filename)
                                (.createNewFile)))
      (.setEncodingOptions DicomEncodingOptions/DEFAULT)
      (.writeDataset fmi attr)
      (.close))
    fmi))

(defn mk_attribute
  "Creating a new dcm4che attribute object from hash map."
  ^Attributes [attr_map]
  )

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
      (.writeDataset (.createFileMetaInformation attributes UID/ExplicitVRLittleEndian) attributes)
      (.writeHeader  Tag/SequenceDelimitationItem nil 0)
      (.close))
    attributes))

;; (doto (ij.io.FileInfo.)
;;   (-> .fileName (set! "x_0.txt"))
;;   (-> .directory (set! "resources/PCT")))

;; (DataInputStream. (FileInputStream. "resources/PCT/x_0.txt"))

{;; 0008
 :SpecificCharacterSet         (fn [^Attributes attr ^String ch-set]             (.setString attr Tag/SpecificCharacterSet         VR/CS ch-set))
 :ImageType                    (fn [^Attributes attr ^String im-type]            (.setString attr Tag/ImageType                    VR/CS im-type))
 :InstanceCreationDate         (fn [^Attributes attr #^"[Ljava.util.Date;" date] (.setDate   attr Tag/InstanceCreationDate         VR/DA date))
 :InstanceCreationTime         (fn [^Attributes attr #^"[Ljava.util.Date;" date] (.setDate   attr Tag/InstanceCreationTime         VR/TM date))
 :StudyDate                    (fn [^Attributes attr #^"[Ljava.util.Date;" date] (.setDate   attr Tag/StudyDate                    VR/DA date))
 :SeriesDate                   (fn [^Attributes attr #^"[Ljava.util.Date;" date] (.setDate   attr Tag/SeriesDate                   VR/DA date))
 :AcquisitionDate              (fn [^Attributes attr #^"[Ljava.util.Date;" date] (.setDate   attr Tag/AcquisitionDate              VR/DA date))
 :ContentDate                  (fn [^Attributes attr #^"[Ljava.util.Date;" date] (.setDate   attr Tag/ContentDate                  VR/DA date))
 :StudyTime                    (fn [^Attributes attr #^"[Ljava.util.Date;" date] (.setDate   attr Tag/StudyTime                    VR/TM date))
 :SeriesTime                   (fn [^Attributes attr #^"[Ljava.util.Date;" date] (.setDate   attr Tag/SeriesTime                   VR/TM date))
 :AcquisitionTime              (fn [^Attributes attr #^"[Ljava.util.Date;" date] (.setDate   attr Tag/AcquisitionTime              VR/TM date))
 :ContentTime                  (fn [^Attributes attr #^"[Ljava.util.Date;" date] (.setDate   attr Tag/ContentTime                  VR/TM date))
 :Manufacturer                 (fn [^Attributes attr ^String manufacturer]       (.setString attr Tag/Manufacturer                 VR/LO manufacturer))
 :InstitutionName              (fn [^Attributes attr ^String institution]        (.setString attr Tag/InstitutionName              VR/LO institution))
 :ReferringPhysicianName       (fn [^Attributes attr ^String physician]          (.setString attr Tag/ReferringPhysicianName       VR/PN physician))
 :StationName                  (fn [^Attributes attr ^String station]            (.setString attr Tag/StationName                  VR/SH station))
 :StudyDescription             (fn [^Attributes attr ^String study-desc]         (.setString attr Tag/StudyDescription             VR/LO study-desc))
 :SeriesDescription            (fn [^Attributes attr ^String series-desc]        (.setString attr Tag/SeriesDescription            VR/LO series-desc))
 :NameOfPhysiciansReadingStudy (fn [^Attributes attr ^String reading-study]      (.setString attr Tag/NameOfPhysiciansReadingStudy VR/PN reading-study))
 :OperatorsName                (fn [^Attributes attr ^String operator]           (.setString attr Tag/OperatorsName                VR/PN operator))
 :ManufacturerModelName        (fn [^Attributes attr ^String model-name]         (.setSTring attr Tag/ManufacturerModelName        VR/LO model-name))
 ;; 0010
 :PatientName                  (fn [^Attributes attr ^String pname]              (.setString attr Tag/PatientName                  VR/PN pname))
 :PatientID                    (fn [^Attributes attr ^String ID]                 (.setString attr Tag/PatientID                    VR/PN ID))
 :PatientBirthDate             (fn [^Attributes attr #^"[Ljava.util.Date;" bday] (.setDate   attr Tag/PatientBirthDate             VR/DA bday))
 :PatientSex                   (fn [^Attributes attr ^String sex]                (.setString attr Tag/PatientSex                   VR/CS sex))
 :PatientAge                   (fn [^Attributes attr ^String age]                (.setString attr Tag/PatientAge                   VR/AS age))
 :AdditionalPatientHistory     (fn [^Attributes attr ^String history]            (.setString attr Tag/AdditionalPatientHistory     VR/LT history))
 ;; 0018
 :ScanOptions                  (fn [^Attributes attr ^String options]            (.setString attr Tag/ScanOptions                  VR/CS options))
 :SliceThickness               (fn [^Attributes attr ^Double thickness]          (.setDouble attr Tag/SliceThickness               VR/DS (double-array [thickness])))
 :KVP                          (fn [^Attributes attr ^Double kvp]                (.setDouble attr Tag/KVP                          VR/DS (double-array [kvp])))
 :DataCollectionDiameter       (fn [^Attributes attr ^Double diameter]           (.setDouble attr Tag/DataCollectionDiameter       VR/DS diameter))
 :SoftwareVersions             (fn [^Attributes attr ^String version]            (.setString attr Tag/SoftwareVersions             VR/LO version))
 :ProtocolName                 (fn [^Attributes attr ^String protocol]           (.setString attr Tag/ProtocolName                 VR/LO protocol))
 :ReconstructionDiameter       (fn [^Attributes attr ^Double diameter]           (.setDouble attr Tag/ReconstructionDiameter       VR/DS (double-array [diameter])))
 :DistanceSourceToDetector     (fn [^Attributes attr ^Double distance]           (.setDouble attr Tag/DistanceSourceToDetector     VR/DS (double-array [distance])))
 :DistanceSourceToPatient      (fn [^Attributes attr ^Double distance]           (.setDouble attr Tag/DistanceSourceToPatient      VR/DS (double-array [distance])))
 :GantryDetectorTilt           (fn [^Attributes attr ^Double tilt]               (.setDouble attr Tag/GantryDetectorTilt           VR/DS (double-array [tilt])))
 :TableHeight                  (fn [^Attributes attr ^Double height]             (.setDouble attr Tag/TableHeight                  VR/DS (double-array [height])))
 :RotationDirection            (fn [^Attributes attr ^String direction]          (.setString attr Tag/RotationDirection            VR/CS direction))
 :ExposureTime                 (fn [^Attributes attr ^String exposure-time]      (.setString attr Tag/ExposureTime                 VR/IS exposure-time))
 :XRayTubeCurrent              (fn [^Attributes attr ^String current]            (.setString attr Tag/XRayTubeCurrent              VR/IS current))
 :Exposure                     (fn [^Attributes attr ^String exposure]           (.setString attr Tag/Exposure                     VR/IS exposure))
 :FilterType                   (fn [^Attributes attr ^String filter-type]        (.setString attr Tag/FilterType                   VR/SH filter-type))
 :GeneratorPower               (fn [^Attributes attr ^String generator-power]    (.setString attr Tag/GeneratorPower               VR/IS generator-power))
 :FocalSpots                   (fn [^Attributes attr ^Double focalspots]         (.setDouble attr Tag/FocalSpots                   VR/DS (double-array [focalspots])))
 :ConvolutionKernel            (fn [^Attributes attr ^String kernel]             (.setString attr Tag/ConvolutionKernel            VR/SH kernel))
 :PatientPosition              (fn [^Attributes attr ^String position]           (.setString attr Tag/PatientPosition              VR/CS position))
 :RevolutionTime               (fn [^Attributes attr ^Double revo-time]          (.setDouble attr Tag/RevolutionTime               VR/FD (double-array [revo-time])))
 :SingleCollimationWidth       (fn [^Attributes attr ^Double width]              (.setDouble attr Tag/SingleCollimationWidth       VR/FD (double-array [width])))
 :TotalCollimationWidth        (fn [^Attributes attr ^Double width]              (.setDouble attr Tag/TotalCollimationWidth        VR/FD (double-array [width])))
 :TableSpeed                   (fn [^Attributes attr ^Double speed]              (.setDouble attr Tag/TableSpeed                   VR/FD (double-array [speed])))
 :TableFeedPerRotation         (fn [^Attributes attr ^Double feed]               (.setDouble attr Tag/TableFeedPerRotation         VR/FD (double-array [feed])))
 :SpiralPitchFactor            (fn [^Attributes attr ^Double pitch]              (.setDouble attr Tag/SpiralPitchFactor            VR/FD (double-array [pitch])))
 ;; 0020
 :StudyInstanceUID             (fn [^Attributes attr ^String uid]                (.setString attr Tag/StudyInstanceUID             VR/UI uid))
 :SeriesInstanceUID            (fn [^Attributes attr ^String uid]                (.setString attr Tag/SeriesInstanceUID            VR/UI uid))
 :StudyID                      (fn [^Attributes attr ^String study-id]           (.setString attr Tag/StudyID                      VR/SH study-id))
 :SeriesNumber                 (fn [^Attributes attr ^String snumber]            (.setString attr Tag/SeriesNumber                 VR/IS snumber))
 :AcquisitionNumber            (fn [^Attributes attr ^String anumber]            (.setString attr Tag/AcquisitionNumber            VR/IS anumber))
 :InstanceNumber               (fn [^Attributes attr ^String inumber]            (.setString attr Tag/InstanceNumber               VR/IS inumber))
 :ImagePositionPatient         (fn [^Attributes attr ^doubles image-position]    (.setDouble attr Tag/ImagePositionPatient         VR/DS image-position))
 :ImageOrientationPatient      (fn [^Attributes attr ^doubles image-orientation] (.setDouble attr Tag/ImageOrientationPatient      VR/DS image-orientation))
 :FrameOfReferenceUID          (fn [^Attributes attr ^String referenceUID]       (.setString attr Tag/FrameOfReferenceUID          VR/UI (UIDUtils/createUID)))
 :PositionReferenceIndicator   (fn [^Attributes attr ^String pos-ref]            (.setString attr Tag/PositionReferenceIndicator   VR/LO pos-ref))
 :SliceLocation                (fn [^Attributes attr ^Double slice-location]     (.setDouble attr Tag/SliceLocation                VR/DS (double-array [slice-location])))
 ;; 0028
 :PixelSpacing                 (fn [^Attributes attr ^doubles spacing]           (.setDouble attr Tag/PixelSpacing                 VR/DS spacing))
 :Rows                         (fn [^Attributes attr ^Integer row]               (.setInt    attr Tag/Rows                         VR/US (int-array    [row])))
 :Columns                      (fn [^Attributes attr ^Integer col]               (.setInt    attr Tag/Columns                      VR/US (int-array    [col])))
 :BitsAllocated                (fn [^Attributes attr ^Integer ballocated]        (.setInt    attr Tag/BitsAllocated                VR/US (int-array    [ballocated])))
 :BitsStored                   (fn [^Attributes attr ^Integer bstored]           (.setInt    attr Tag/BitsStored                   VR/US (int-array    [bstored])))
 :HighBit                      (fn [^Attributes attr ^Integer highbit]           (.setInt    attr Tag/HighBit                      VR/US (int-array    [15])))
 :PixelRepresentation          (fn [^Attributes attr ^Integer pixel-rep]         (.setInt    attr Tag/PixelRepresentation          VR/US (int-array    [pixel-rep])))
 :SamplesPerPixel              (fn [^Attributes attr ^Integer samples-pixel]     (.setInt    attr Tag/SamplesPerPixel              VR/US (int-array    [samples-pixel])))
 :PixelData                    (fn [^Attributes attr ^integers pixeldata]        (.setInt    attr Tag/PixelData                    VR/OW pixeldata))
 }

(defn new-dicom
  "Create new DICOM image from list of given attributes."
  [tags]
  (let [attr (Attributes.)]
    (if (:PixelData tags) (.setInt attr Tag/PixelData VR/OW (into-array Integer/TYPE (to-double-array (:PixelData tags)))))))
