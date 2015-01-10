(ns runmap.gpxparser
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as jio]))

(def ext ".gpx")

(defn gpx-file->latlngs [file]
  (->> file
       jio/reader
       xml/parse
       xml-seq
       (filter #(= (:tag %) :trkpt))
       (map :attrs)
       (map #(into {} (for [[k v] %] [k (Double/parseDouble v)])))
       (filter #(= (count %) 2))
       (map #(assoc {} :lat (:lat %) :lng (:lon %)))))

(defn files->latlngs [files]
  (mapcat gpx-file->latlngs files))
