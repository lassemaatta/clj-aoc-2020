(ns aoc-2020.day11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample-data "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL")

(defn- input->vals
  [input]
  (->> input
       (str/split-lines)
       (mapv (comp vec seq))))

(def empty-seat \L)
(def occupied-seat \#)
(def no-seat \.)

(defn is-empty?
  [seat]
  (= seat empty-seat))

(defn is-occupied?
  [seat]
  (= seat occupied-seat))

(defn is-floor?
  [seat]
  (= seat no-seat))

(defn is-seat?
  [seat]
  (or (is-empty? seat)
      (is-occupied? seat)))

(defn inside-floormap?
  [{:keys [width height]} w h]
  (and (< -1 w width)
       (< -1 h height)))

(defn build-vector
  [{:keys [^long max-length] :as ctx} xdir ydir]
  (let [valid? (partial inside-floormap? ctx)]
    (for [^long r (range 1 (inc max-length))
          :let [w (xdir r)
                h (ydir r)]
          :when (valid? w h)]
      [h w])))

(defn build-vectors
  [ctx ^long x ^long y]
  (let [up     (partial - y)
        down   (partial + y)
        keep-y (constantly y)
        left   (partial - x)
        right  (partial + x)
        keep-x (constantly x)]
    [(build-vector ctx keep-x down)                         ; S
     (build-vector ctx right down)                          ; SE
     (build-vector ctx right keep-y)                        ; E
     (build-vector ctx right up)                            ; NE
     (build-vector ctx keep-x up)                           ; N
     (build-vector ctx left up)                             ; NW
     (build-vector ctx left keep-y)                         ; W
     (build-vector ctx left down)]))                        ; SW

(defn adjacent
  [{:keys [floormap] :as ctx} ^long x ^long y]
  (let [vectors          (build-vectors ctx x y)
        points-to-floor? (fn [coord]
                           (is-floor? (get-in floormap coord no-seat)))]
    (->> vectors
         (map (fn map-coords [coords] (->> coords
                                           (drop-while points-to-floor?)
                                           (take 1)
                                           (first))))
         (remove nil?))))

(defn find-adjacent
  [{:keys [width height floormap] :as ctx}]
  (let [coordinates (->> (for [w (range width)
                               h (range height)]
                           [h w])
                         (map (fn [[h w]] [h w (get-in floormap [h w])]))
                         (filter (fn [[h w seat]] (is-seat? seat)))
                         (map (fn [[h w _seat]] [h w]))
                         (into []))
        adjacent    (->> coordinates
                         (map (fn [[h w _seat]]
                                [[h w] (adjacent ctx w h)]))
                         (into {}))]
    (assoc ctx :coordinates coordinates
               :adjacent adjacent)))

(defn apply-ctx
  [floormap ctx]
  (let [height (count floormap)
        width  (count (first floormap))]
    (-> ctx
        (assoc :floormap floormap
               :height height
               :width width)
        (find-adjacent))))

(defn update-seat
  [seat {:keys [^long max-occupied adjacent floormap] :as ctx} w h]
  (if (is-seat? seat)
    (let [adjacent-seats    (->> (get adjacent [h w])
                                 (map #(get-in floormap %)))
          adjancent-grouped (frequencies adjacent-seats)
          occupied-seats    ^long (get adjancent-grouped occupied-seat 0)]
      (cond
        ; An occupied seat may turn into an empty seat
        (is-occupied? seat) (if (>= occupied-seats max-occupied)
                              empty-seat
                              seat)
        ; An empty seat may turn into an occupied seat
        (is-empty? seat) (if (zero? occupied-seats)
                           occupied-seat
                           seat)
        :else seat))
    seat))

(defn update-floormap
  [{:keys [coordinates] :as ctx}]
  (reduce
    (fn [m [h w]]
      (update-in m [:floormap h w] update-seat ctx w h))
    ctx
    coordinates))

(defn iterations
  [ctx]
  (iterate update-floormap ctx))

(defn iterate-until-stabilized
  [ctx]
  (reduce
    (fn [{:keys [last-value] :as m} {:keys [floormap] :as ctx}]
      (if (= last-value floormap)
        (reduced floormap)
        (-> m
            (update :iteration inc)
            (assoc :last-value floormap))))
    {:iteration  0
     :last-value []}
    (iterations ctx)))

(defn count-seats
  [floormap]
  (->> floormap
       (mapcat identity)
       (frequencies)))

(comment
  (let [data sample-data
        ctx  {:max-occupied 4
              :max-length   1}]
    (-> data
        (input->vals)
        (apply-ctx ctx)
        (iterate-until-stabilized)
        (count-seats)))
  (let [data (slurp (io/resource "day11.txt"))
        ctx  {:max-occupied 5
              :max-length   100}]
    (-> data
        (input->vals)
        (apply-ctx ctx)
        (iterate-until-stabilized)
        (count-seats))))
