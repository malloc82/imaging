(ns imaging.core
  ;; (:use clojure.core.matrix clojure.core.matrix.operators)
  (:use [clojure.core.matrix :as mat])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :as mo])
  (:import (java.io File FileInputStream)
           javax.imageio.ImageIO
           (java.awt.image BufferedImage Raster WritableRaster)
           (javax.swing JFrame JLabel)
           (java.awt Graphics Dimension Color)
           ij.IJ))

(defn imread [filename]
  (let [image  (ImageIO/read (File. filename))
        raster (.getData image)
        w      (.getWidth  raster)
        h      (.getHeight raster)
        bands  (.getNumBands raster)]
    (cond
      (= bands 1) {:matrix (reshape (matrix :vectorz (.getPixels  ^Raster raster 0 0 w h (double-array (* w h)))) [w h])
                   :type (.getType image)}
      (= bands 3) {:matrix [(reshape (matrix :vectorz (.getSamples ^Raster raster 0 0 w h 0 (double-array (* w h)))) [w h])
                            (reshape (matrix :vectorz (.getSamples ^Raster raster 0 0 w h 1 (double-array (* w h)))) [w h])
                            (reshape (matrix :vectorz (.getSamples ^Raster raster 0 0 w h 2 (double-array (* w h)))) [w h])]
                   :type (.getType image)}
      :else {})))

(defn imshow [matrix & {:keys [type]
                        :or   {type BufferedImage/TYPE_BYTE_GRAY}}]
  (let [[row col] (mat/shape matrix)
        image (BufferedImage. col row type)
        canvas (proxy [JLabel] []
                 (paint [g] (.drawImage g image 0 0 this)))]
    ;; paint canvas
    (.setPixels ^WritableRaster (.getRaster image) 0 0 col row (mat/to-double-array matrix))
    (doto (JFrame.)
      (.add canvas)
      (.setSize (Dimension. col row))
      (.show))
    image))



(defn imsave [matrix filename & {:keys [imtype ftype]
                                 :or   [imtype BufferedImage/TYPE_BYTE_GRAY
                                        ftype  "png"]}]
  (let [[row col] (if (mat/matrix? matrix)
                    (mat/shape matrix)
                    (mat/shape (matrix 0)))
        image  (BufferedImage. col row imtype)
        raster (.getRaster image)]
    (if (cond
          (= imtype BufferedImage/TYPE_BYTE_GRAY)
          (do
            (.setPixels ^WritableRaster raster 0 0 col row (mat/to-double-array matrix))
            true)
          (= imtype BufferedImage/TYPE_INT_RGB)
          (do
            (.setSamples ^WritableRaster raster 0 0 col row 0 (mat/to-double-array (matrix 0)))
            (.setSamples ^WritableRaster raster 0 0 col row 1 (mat/to-double-array (matrix 1)))
            (.setSamples ^WritableRaster raster 0 0 col row 2 (mat/to-double-array (matrix 2)))
            true)
          :else false)
      (do

        (ImageIO/write ^BufferedImage image ftype (File. filename)) true)
      false)))

