(ns runmap.core
  (:require [runmap.fitparser :as fitparser]
            [clojure.java.io :as jio]))

(defrecord LatLng [lat lng])

(defn degrees->radians [dg]
  (Math/toRadians dg))

(defn haversine [ll1 ll2]
  (let [earth-radius 6371
        lat1 (degrees->radians (:lat ll1))
        lat2 (degrees->radians (:lat ll2))
        dellat (degrees->radians (- (:lat ll2) (:lat ll1)))
        dellng (degrees->radians (- (:lng ll2) (:lng ll1)))
        a (+ (Math/pow (Math/sin (/ dellat 2)) 2) (* (Math/cos lat1) (Math/cos lat2) (Math/pow (Math/sin (/ dellng 2)) 2)))
        c (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a))))]
    (* earth-radius c)))

(defn runmap-limit []
  {:min (LatLng. 51.323264 -0.396881) :max (LatLng. 51.729952 0.379028)})

(defn bounds [lls]
  "bounding box for lat lngs"
  (let [lats (map :lat lls)
        lngs (map :lng lls)
        latmin (apply min lats)
        latmax (apply max lats)
        lngmin (apply min lngs)
        lngmax (apply max lngs)]
    {:min (LatLng. latmin lngmin) :max (LatLng. latmax lngmax)}))

(defn scale [val domain range]
  (let [[dommin dommax] domain
        dom (- dommax dommin)
        [rngmin rngmax] range
        rng (double (- rngmax rngmin))]
    (+ (* (- val dommin) (/ rng dom)) rngmin)))

(defn latlngs-within [lls bnds]
  (->> lls
       (filter #(> (:lat %) (get-in bnds [:min :lat])))
       (filter #(< (:lat %) (get-in bnds [:max :lat])))
       (filter #(> (:lng %) (get-in bnds [:min :lng])))
       (filter #(< (:lng %) (get-in bnds [:max :lng])))))

(defn latlngs-domain [lls]
  (let [bnds (bounds lls)
        latdom [(get-in bnds [:min :lat]) (get-in bnds [:max :lat])]
        lngdom [(get-in bnds [:min :lng]) (get-in bnds [:max :lng])]]
    [latdom lngdom]))

(defn runmap-scale [lls size]
  (let [[latdom lngdom] (latlngs-domain lls)
        latlen (- (apply max latdom) (apply min latdom))
        lnglen (- (apply max lngdom) (apply min lngdom))
        maxlen (max latlen lnglen)]
    {:min (LatLng. 0 0) :max (LatLng. (* size (/ latlen maxlen)) (* size (/ lnglen maxlen)))}))

(defn distances-scale [lls]
  (let [minll (:min (bounds lls))
        maxll (:max (bounds lls))
        latdist (haversine minll (LatLng. (:lat maxll) (:lng minll)))
        lngdist (haversine minll (LatLng. (:lat minll) (:lng maxll)))]
    {:min (LatLng. 0 0) :max (LatLng. latdist lngdist)}))

(defn scale-latlngs [lls scl]
  (let [[latdom lngdom] (latlngs-domain lls)
        latrng [(get-in scl [:min :lat]) (get-in scl [:max :lat])]
        lngrng [(get-in scl [:min :lng]) (get-in scl [:max :lng])]]
    (map #(LatLng. (scale (:lat %1) latdom latrng) (scale (:lng %1) lngdom lngrng)) lls)))

(defn runmap [lls size]
  (let [lls (latlngs-within lls (runmap-limit))
        lls (scale-latlngs lls (distances-scale lls))]
    (scale-latlngs lls (runmap-scale lls size))))

(defn runmap->bitmap [rm]
  (let [xys (map #(vector (:lng %) (:lat %)) rm)
        maxx (apply max (map first xys))
        maxy (apply max (map second xys))
        img (java.awt.image.BufferedImage. maxx maxy java.awt.image.BufferedImage/TYPE_BYTE_GRAY)
        g (.createGraphics img)]
    (doall (map (fn [[x y]] (.drawRect g x (- maxy y) 1 1)) xys))
    img))

(defn fit-files [dir]
  (filter (memfn isFile) (file-seq (jio/file dir))))

(defn -main [& args]
  (let [dir "resources/fit-files"
        files (fit-files dir)
        lls (fitparser/fit-files->latlngs files)
        size 1024
        rm (runmap lls size)
        bmp (runmap->bitmap rm)]
    (javax.imageio.ImageIO/write bmp "png" (jio/file "runmap.png"))))
