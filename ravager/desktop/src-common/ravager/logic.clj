(ns ravager.logic
  (:use ravager.herringbone); primitive-math)
	(:require [hiphip.double :as hiphip]
            [hiphip.array :as harray])
  (:import [squid.squidgrid.fov TranslucenceWrapperFOV BresenhamLOS BasicRadiusStrategy])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:const wide 123)
(def ^:const high 103)
(def ^:const iw (- wide 2)) ;inner width
(def ^:const ih (- high 2)) ;inner height

(def ^:const wall 9999.0)
(def ^:const floor 2000.0)
(def ^:const dark 11111.0)

(def ^:const GOAL 0.0)

(def cleared-levels (atom {}))
(def dlevel (atom 0))
(def res (atom (make-array Float/TYPE (* wide high))))
(defn init-full-seen ^booleans [] (let [^booleans res1d (make-array Boolean/TYPE (* wide high))]
                   (doseq [i (range (* wide high))]
                     (aset ^booleans res1d i false))
                     res1d))

(def player (atom {:pos 0 :show \@ :hp 999 :vision 8 :dijkstra nil :fov nil :full-seen (init-full-seen)}))

(def monster-names
  ["dragon"
   "tyrant"
   "displacer"
   "migo"
   "goldbot"
   "frostbite"
   "ogre"
   "shoggoth"
   "scather"
   "wolverine"
   "discord"
   "glutton"
   "zombie"
   "jabberwock"
   "cerberus"])

(defn create-monsters [] (atom (vec (for [i (range 1000)]
                                      (atom {:pos 0 :show \M
                                             :hp 10 :vision 7
                                             :dijkstra nil :wander 1
                                             :tile (rand-int (count monster-names))})))))
;(def monster-hash (atom {}))
(def ^TranslucenceWrapperFOV fov (TranslucenceWrapperFOV. ))
(def ^BresenhamLOS los (BresenhamLOS. ))

(defn make-bones []
  (let [seed (rand-int (count horiz))
        initial (horiz seed)
        hvec (mapv #(mapv vec %) horiz)
        vvec (mapv #(mapv vec %) vert)
        oh (+ 20 ih)
        ow (+ 20 iw)
        initial (hiphip/amake [i (* ow oh)] wall)
        shown (char-array (* ow oh) \#)]
    (loop [next-fill 0 started-indent 0]
      (if (>= (+ (* 10 ow ) next-fill) (* ow oh))
          initial
          (let [hofull (rand-nth hvec)
                    ho (mapv #(replace {\# wall \. floor \$ floor \~ floor \% floor \+ floor} %) hofull)]
            (when (< (+ 20 (+ (* 10 ow) next-fill)) (* ow oh))
                (doseq [nf (range 10)]
                                       (hiphip/afill! [[i eh] initial :range [(+ (* ow (long nf)) next-fill) (+ 20 (+ (* ow (long nf)) next-fill))]]
                                                                  (nth (nth ho nf) (- (- i (* ow (long nf))) next-fill)))
                                       (harray/afill! Character/TYPE [[i eh] shown :range [(+ (* ow (long nf)) next-fill) (+ 20 (+ (* ow (long nf)) next-fill))]]
                                                                  (nth (nth hofull nf) (- (- i (* ow (long nf))) next-fill)))))
                (recur
                 (long
                   (if (< (rem (+ 40 next-fill) ow) (rem next-fill ow))
                     (condp = started-indent
                       0 (+ (+ (* ow 10) 10) (- next-fill (rem next-fill ow )))
                       1 (+ (+ (* ow 10) 20) (- next-fill (rem next-fill ow )))
                       2 (+ (+ (* ow 10) 30) (- next-fill (rem next-fill ow )))
                       3 (+     (* ow 10)     (- next-fill (rem next-fill ow )))
                       )
                     (+ 40 next-fill) ) )
                 (long (if (< (rem (+ 40 next-fill) ow) (rem next-fill ow))
                   (rem (+ 1 started-indent) 4)
                   started-indent))
                 ))))
    (loop [next-fill (* 10 ow) started-indent 1]
      (if (>= (+ 10 (rem next-fill ow)) ow)
          initial
        (let [vefull (rand-nth vvec)
                    ve (mapv #(replace {\# wall \. floor \$ floor \~ floor \% floor \+ floor} %) vefull)]
                (when (< (+ (+ (* 19 ow) 10) next-fill) (* ow oh))
                  (doseq [nf (range 20)] (hiphip/afill! [[i eh] initial :range [(+ (* ow (long nf)) next-fill) (+ (+ 10 (* ow (long nf))) next-fill)]]
                                                                  (nth (nth ve nf) (- (- i (* ow (long nf))) next-fill)))
                      (harray/afill! Character/TYPE [[i eh] shown :range [(+ (* ow (long nf)) next-fill) (+ (+ 10 (* ow (long nf))) next-fill)]]
                                                                  (nth (nth vefull nf) (- (- i (* ow (long nf))) next-fill)))))
                (recur
                 (long
                   (if (< (rem (+ 40 (quot next-fill ow)) oh) (quot next-fill ow))
                     (condp = started-indent
                       0 (+ (+  (* ow 10) 10) (rem next-fill ow))
                       1 (+ (+  (* ow 20) 10) (rem next-fill ow))
                       2 (+ (+  (* ow 30) 10) (rem next-fill ow))
                       3 (+                10  (rem next-fill ow))
                       )
                       (+ (* 40 ow) next-fill) ) )
                 (long (if (< (rem (+ 40 (quot next-fill ow)) oh) (quot next-fill ow))
                   (rem (+ 1 started-indent) 4)
                   started-indent))
                 ))))
    ;(doall (map #(println (apply str %)) (partition ow (vec shown))))
    [(hiphip/amake [i (* wide high)] (if (or
			(= (rem i wide) 0)
			(= (rem i wide) (+ -1 wide))
			(< i wide)
			(> i (- (* wide high) wide)))
		 wall
		 (hiphip/aget ^doubles initial (+ (+ (+ (* 10 ow) -10) (* 20 (quot i wide))) (- (- i (+ -1 wide)) (* 2 (quot i wide)))))))

     (harray/amake Character/TYPE [i (* wide high)] (if (or
			(= (rem i wide) 0)
			(= (rem i wide) (+ -1 wide))
			(< i wide)
			(> i (- (* wide high) wide)))
		 \#
		 (aget ^chars shown (+ (+ (+ (* 10 ow) -10) (* 20 (quot i wide))) (- (- i (+ -1 wide)) (* 2 (quot i wide)))))))
                                       ]))

(defn dungeon-resistances ^floats [^doubles dungeon]
                 (let [res1d (make-array Float/TYPE (* wide high))]
                   (doseq [i (range (* wide high))]
                     (aset ^floats res1d i (if
                                       (= (hiphip/aget ^doubles dungeon i) (double wall))
                                       (float 1.0)
                                       (float 0.0))))
                   res1d))

(defn run-fov-player
  ^floats [entity dungeon]
    (let [^floats calculated
  ;        (let [res2d (make-array Float/TYPE wide high)]
  ;                 (doseq [x (range wide) y (range high)]
  ;                   (aset res2d x y
  ;                                     (float 1.0)
  ;                                     ))
  ;                 res2d)]
          (. fov calculateFOV @res wide (int (rem (:pos @entity) wide)) (int (quot (:pos @entity) wide)) (float 1.0) (float (/ 1.0 (double (:vision @entity)))) BasicRadiusStrategy/DIAMOND)]
      (doseq [idx (range (* wide high))]
         (aset ^booleans (:full-seen @entity) idx
            (boolean (or (aget ^booleans (:full-seen @entity) idx)
                (if (> (double (aget ^floats calculated idx)) 0.0)
                  true
                  false)

                   ))))
      calculated)
  )

(defn run-fov
  ([entity dd]
    (let [^floats calculated (. fov calculateFOV @res wide (rem (:pos @entity) wide) (quot (:pos @entity) wide) 1.0 (/ 1.0 (double (:vision @entity))) BasicRadiusStrategy/DIAMOND)]
      calculated))
  ([entity dd distance]
    (let [^floats calculated (. fov calculateFOV @res wide (rem (:pos @entity) wide) (quot (:pos @entity) wide) 1.0 (/ 1.0 (double distance)) BasicRadiusStrategy/DIAMOND)]
      calculated)))
(defn check-los
  [start end distance]
     (. los isReachable @res wide (mod start wide) (quot start wide) (mod end wide) (quot end wide) distance 1.0 BasicRadiusStrategy/DIAMOND)) ; (/ 1.0 distance)

(defn init-dungeon ([dngn] (loop [ctr 0] (if (>= ctr  1) (hiphip/aclone dngn) (let [rand-loc (rand-int (* wide high))] (if (= (hiphip/aget ^doubles dngn rand-loc) floor)
			                                                        (recur (do (hiphip/aset dngn rand-loc GOAL) (inc ctr))) (recur ctr)))))))
(defn place-creature
  ([dun entity] (loop [ctr 0] (if (>= ctr 1) entity (let [rand-loc (rand-int (* wide high))] (if (and
                                                                                    (apply distinct? (concat (filter (complement nil?)
                                                                                                             (map (fn [atm] (if (= (:pos @atm) 0) nil (:pos @atm))) @(:monsters @dun)))
                                                                                                        [rand-loc (:pos @player)]))
                                                                                                 (= (hiphip/aget ^doubles (:dungeon @dun) rand-loc) floor))
			                                                        (recur (do
                                                                       (swap! entity assoc :pos rand-loc)
                                                                       ;(when (= \# (aget ^chars shown (:pos @entity)))
                                                                       ;   (println "Monster intersecting with wall"))

                                                                       (inc ctr)))
                                                              (recur ctr))))))
  ([dun entity starting-cell] (loop [ctr 0] (if (>= ctr 1) entity (let [rand-loc (rand-nth (keep-indexed #(if (= %2 starting-cell) %1) (vec (:dungeon @dun))))]
                                                                   (if (and (apply distinct? (concat (filter (complement nil?)
                                                                                                             (map (fn [atm] (if (= (:pos @atm) 0) nil (:pos @atm))) @(:monsters @dun)))
                                                                                                        [rand-loc (:pos @player)]))
                                                                                                 (= (hiphip/aget ^doubles (:dungeon @dun) rand-loc) starting-cell))
			                                                        (recur (do (swap! entity assoc :pos rand-loc) (inc ctr))) (recur ctr)))))))
(defn alter-dungeon
  ([dngn cell] (loop [ctr 0] (if (>= ctr  1) dngn (let [rand-loc (rand-int (* wide high))]
                                                    (if (= (hiphip/aget ^doubles dngn rand-loc) floor)
			                                                        (recur (do (hiphip/aset dngn rand-loc cell) (inc ctr))) (recur ctr))))))
  ([dngn shown cell shown-cell filt] (loop [ctr 0] (if (>= ctr  1) dngn (let [rand-loc (rand-int (* wide high))]
                                                         (if (filt (hiphip/aget ^doubles dngn rand-loc))
			                                                        (recur (do (aset ^chars shown ^int rand-loc ^char shown-cell) (hiphip/aset ^doubles dngn rand-loc cell) (inc ctr))) (recur ctr)))))))

(defn find-cells [^doubles a cell-kind]
    (persistent! (areduce ^doubles a i ret (transient {})
                          (if (= (hiphip/aget ^doubles a i) cell-kind) (assoc! ret i cell-kind) ret))))

(defn find-goals [^doubles a]
  (find-cells a GOAL))

(defn find-walls [^doubles a]
    (persistent! (areduce ^doubles a i ret (transient {})
                          (if (>= (hiphip/aget ^doubles a i) (double wall)) (assoc! ret i wall) ret))))

(defn find-floors [^doubles a]
  (find-cells a floor))

(defn find-lowest [^doubles a]
  (let [low-val (hiphip/amin a)]
    (find-cells a low-val)))

(defn find-monsters [m]
    (into {} (for [mp (map #(:pos @%) m)] [mp 1.0])))

(defn dijkstra
  ([a]
     (dijkstra a (find-walls a) (find-lowest a)))
  ([dun _]
     (dijkstra (:dungeon dun) (merge (find-walls (:dungeon dun)) (find-monsters @(:monsters dun))) (find-lowest (:dungeon dun))))
  ([a closed open-cells]
     (loop [open open-cells]
       (when (seq open)
         (recur (reduce (fn [newly-open [^long i ^double v]]
                          (reduce (fn [acc dir]
                                    (if (or (closed dir) (open dir)
                                            (>= (+ 1.0 v) (hiphip/aget ^doubles a dir)))
                                      acc
                                      (do (hiphip/aset ^doubles a dir (+ 1.0 v))
                                          (assoc acc dir (+ 1.0 v)))))
                                  newly-open, [(- i wide)
                                               (+ i wide)
                                               (- i 1)
                                               (+ i 1)]))
                        {}, open))))
     a))
(comment
(defn local-dijkstra
  ([a center radius]
     (local-dijkstra a (find-walls a) {center 0} center radius))
  ([a ent center radius]
     (local-dijkstra a (dissoc (merge (find-walls a) (find-monsters @monsters)) (:pos @ent)) {center 0} center radius))
  ([a closed open-cells center radius]
     (loop [open open-cells ctr 0]
       (if (and (seq open) (< ctr (long radius)))
         (recur (reduce (fn [newly-open [^long i ^double v]]
                          (reduce (fn [acc dir]
                                    (if (or (closed dir) (open dir)
                                            (>= (+ 1.0 v) (hiphip/aget ^doubles a dir)))
                                      acc
                                      (do
                                          (hiphip/aset ^doubles a dir (+ 1.0 v))
                                          (assoc acc dir (+ 1.0 v)))))
                                  newly-open, [(- i wide)
                                               (+ i wide)
                                               (- i 1)
                                               (+ i 1)]))
                        {}, open) (inc ctr))
         a))
     )))

(defn local-dijkstra-map
  ([a center radius]
     (local-dijkstra-map a (find-walls a) {center 0} center radius))
;  ([a ent center radius]
;     (local-dijkstra-map a (dissoc (merge (find-walls a) (find-monsters @monsters)) (:pos @ent)) {center 0} center radius))
  ([a closed open-cells center radius]
     (loop [open open-cells result (atom {}) ctr 0]
       (if (and (seq open) (< ctr (long radius)))
         (recur (reduce (fn [newly-open [^long i ^double v]]
                          (reduce (fn [acc dir]
                                    (if (or (closed dir) (open dir)
                                            (>= (+ 1.0 v) (double (get @result dir 22222.0))))
                                      acc
                                      (do
                                          (swap! result assoc dir (+ 1.0 v))
                                          (assoc acc dir (+ 1.0 v)))))
                                  newly-open, [(- i wide)
                                               (+ i wide)
                                               (- i 1)
                                               (+ i 1)]))
                        {}, open) result (inc ctr))
         @result))
     ))

(defn init-ambush-entity [dun entity choke] (loop [ctr 0] (let [rand-loc (rand-int (* wide high))]
                                                                                 (if (and
                                                                                    (= (hiphip/aget ^doubles (:dungeon @dun) rand-loc) floor)
                                                                                    (apply distinct? (concat (filter (complement nil?)
                                                                                                             (map (fn [atm] (if (= (:pos @atm) 0) nil (:pos @atm))) @(:monsters @dun)))
                                                                                                        [rand-loc (:pos @player)]))
                                                                                                 (> (hiphip/aget ^doubles choke rand-loc) 10.0))
			                                                        (do
                                                                       (swap! entity assoc :pos rand-loc)
                                                                       (swap! entity assoc :wander 0)
                                                                       entity)
                                                              (if (>= ctr 10) (place-creature dun entity) (recur (inc ctr)))))))
(defn init-monsters
  [dd]
;    (let [chokepoints (amap ^doubles dungeon idx _ (double (if (>= (hiphip/aget ^doubles dungeon idx) wall) wall (reduce (fn [base [k v]] (+ base v)) 0 (local-dijkstra ^doubles dungeon idx 2)))))]
  (let [chokepoints (amap ^doubles dd idx _ (double (if (>= (hiphip/aget ^doubles dd idx) (double wall))
                                                           0.0
                                                           (reduce (fn [^long base [^long k ^double v]]
                                                                     (if (>= v 5.0)
                                                                       (+ base (if (check-los idx k 1.0) 0 1)) base)) 0 (local-dijkstra-map ^doubles dd idx 7)))))
        dun0 (atom {:dungeon dd :monsters (create-monsters)})]
    (atom (swap! (:monsters @dun0) (fn [ms] (mapv #(do (init-ambush-entity dun0 % chokepoints) %) ms))))))

(defn prepare-bones []
         (let [dungeon-z (make-bones)
               dungeon (first dungeon-z)
               dngn-clone (init-dungeon dungeon)]
                      (loop [
                          start   (double-array (map #(if (< (double %) (double wall)) floor %) (replace {floor dark} (vec (dijkstra dungeon)))))
                          scanned dngn-clone
                          worst   (apply clojure.core/max (filter #(< (double %) (double floor)) (vec (dijkstra (hiphip/aclone dngn-clone)))))
                          shown   (second dungeon-z)]
                       ; (println (str "In loop, worst is " worst
                        ;              "\nstart is \n" (clojure.string/join "\n" (for [l (partition wide start)] (apply str (mapv {wall \# dark \space floor \. GOAL \@ 10001.0 \< 10002.0 \>} l))))
                         ;             "\n\nclone is \n" (clojure.string/join "\n" (for [l (partition wide scanned)]  (apply str (mapv {wall \# dark \space floor \. GOAL \@ 10001.0 \< 10002.0 \>} l))))))
                        (if (and (< (double worst) (double floor)) (> (long worst) (/ (+ wide high) 4)))
                              [(double-array (map-indexed #(if (= (double %2) (double GOAL))
                                                                       (do (aset ^chars shown %1 \<) 10001.0)
                                                                       (if (< (double %2) (double wall)) (double floor) (double %2)))
                                                                    (alter-dungeon (dijkstra scanned) shown 10002.0 \> #(and (>= (double %) (double (/ (+ wide high) 4))) (< (double %) (double floor))))))
                                                                shown]
                                    (let [d0 (make-bones)
                                          d_ (first d0)
                                          d1 (init-dungeon d_)
                                          d2 (double-array (map #(if (< (double %) (double wall)) floor %) (replace {floor dark} (vec (dijkstra d_)))))
                                          w2 (apply clojure.core/max (filter #(< (double %) (double wall)) (vec (dijkstra (hiphip/aclone d1)))))]
                                      (recur d2 d1 w2 (harray/afill! char [[i x] ^chars (second d0)] (if (= (hiphip/aget ^doubles d2 i) (double wall)) \# x))))))))

(defn damage-player
  ([entity dd]
  (do (swap! entity assoc :hp (- (long (:hp @entity)) (inc (long (rand-int 4)))))
    (if (<= (long (:hp @player)) 0)
      (do
        (println (str "GAME OVER.  You explored "
                          (count (filter true?
                                         (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @player))))))
                          " squares and reached floor " (+ 1 (long @dlevel)) "."
                          )) (System/exit 0)))))
  ([entity dd amt]
  (do (swap! entity assoc :hp amt)
    (if (<= (long (:hp @player)) 0)
      (do
        (println (str "GAME OVER.  You explored "
                          (count (filter true?
                                         (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @player))))))
                          " squares and reached floor " (+ 1 (long @dlevel)) "."
                          )) (System/exit 0))))))

(defn damage-monster
  ([entity dun]
  (do (swap! entity assoc :hp (- (long (:hp @entity)) (+ 2 (long (rand-int 6)))))
    (when (<= (long (:hp @entity)) 0)
      (swap! (:monsters @dun) (fn [ms] (remove #(= % entity) ms)))
      (swap! (:mon-hash @dun) dissoc (:pos @entity)))
     ))
  ([entity dun dice die-size]
  (do (swap! entity assoc :hp (- (long (:hp @entity))
                                 (long (apply clojure.core/+ (repeatedly dice #(inc (long (rand-int die-size))))))))
    (when (<= (long (:hp @entity)) 0)
      (swap! (:monsters @dun) (fn [ms] (remove #(= % entity) ms)))
      (swap! (:mon-hash @dun) dissoc (:pos @entity)))
     )))


(defn move-monster [dun]
  (let [first-dun (assoc @dun :dungeon (hiphip/aclone ^doubles (:dungeon @dun)))
        first-d ^doubles (:dungeon first-dun)
        player-pos (:pos @player)
        _ (mapv #(if (< (aget ^doubles first-d ^long %) wall)
                   (aset ^doubles first-d ^long % ^double GOAL))
                [(- player-pos wide)
                 (+ player-pos wide)
                 (- player-pos 1)
                 (+ player-pos 1)])
        find-map ^doubles (dijkstra (hiphip/aclone ^doubles first-d))
        find-map-allies ^doubles (dijkstra first-dun nil)
       ; _ (println (reduce #(str %1 "\n" (vec %2)) "" (partition wide (vec find-map))))
       ; _ (println (reduce #(str %1 "\n" (vec %2)) "" (partition wide (mapv - (vec find-map-allies)(vec find-map)))))
        new-d (hiphip/afill! [[idx x] (hiphip/aclone ^doubles find-map)]
                             (if (= (:pos @player) idx)
                               10007.0
                               (if (>= (double x) (double wall))
                                 wall
                                 (Math/floor (* -1.4 (double x))))))
         flee-map (dijkstra (hiphip/aclone ^doubles new-d))
         flee-map-allies (dijkstra (assoc @dun :dungeon new-d) nil)
         player-fov-extended ^floats (run-fov player dun 9)
         pc-x (rem (long (:pos @player)) wide)
         pc-y (quot (long (:pos @player)) wide)]
                             (doseq [monster @(:monsters @dun)]
                               (let [mon-pos (long (:pos @monster))
                                     mon-x (rem mon-pos wide)
                                     mon-y (quot mon-pos wide)
                                     monster-sees-player (and (> (double (aget ^floats player-fov-extended mon-pos)) 0.0)
                                                              (<= (+ (Math/abs (double (- mon-x pc-x)))
                                                                     (Math/abs (double (- mon-y pc-y))))
                                                                  (double (:vision @monster))))]
                                ; (println "monster " monster "\n @dun " @dun)
                                 (swap! (:mon-hash @dun) dissoc mon-pos)
                                 ;(println monster-sees-player)
                                 (if (> (long (:hp @monster)) 2)
                                   (when monster-sees-player
                                     (swap! monster assoc :dijkstra find-map)
                                     (swap! monster assoc :dijkstra-allies find-map-allies)
                                          ;     (let [new-d (hiphip/aclone ^doubles (:dungeon @dun))]
                                          ;       (aset ^doubles new-d (long (:pos @player)) ^double GOAL)
                                          ;       (dijkstra (assoc @dun :dungeon new-d) monster)))
                                     (swap! monster assoc :wander 1))
                                   (when monster-sees-player
                                     (swap! monster assoc :dijkstra flee-map)
                                     (swap! monster assoc :dijkstra-allies flee-map-allies)
                                     (swap! monster assoc :wander 1)))))
                             ;(println)
                             (doseq [mon @(:monsters @dun)]
                                (let [oldpos (long (:pos @mon))
                                      mons @(:monsters @dun)]
                                (if (:dijkstra @mon) (let [orig-pos (long (:pos @mon))
                                                          adjacent (shuffle [
                                                                    (- orig-pos wide)
                                                                    (+ orig-pos wide)
                                                                    (- orig-pos 1)
                                                                    (+ orig-pos 1)])
                                                          lowest;-ignoring-monsters
                                                            (long (reduce #(if (and
                                                                               (apply distinct? (concat (map (fn [atm] (:pos @atm)) @(:monsters @dun)) [%2 (:pos @player)])) ;(map (fn [[_ atm]] (:pos @atm)) (dissoc @(:mon-hash @dun) orig-pos))
                                                                               (< (hiphip/aget ^doubles (:dijkstra-allies @mon) %2) (hiphip/aget ^doubles (:dijkstra-allies @mon) %1))
                                                                               (= (hiphip/aget ^doubles (:dungeon @dun) %2) (double floor)))
                                                                            %2
                                                                            %1)
                                                                         orig-pos
                                                                         adjacent))
                                                         ; lowest-respecting-monsters
                                                         ;   (long (reduce #(if (and
                                                         ;                      (apply distinct? (concat (map (fn [[_ atm]] (:pos @atm)) (dissoc @(:mon-hash @dun) orig-pos)) [%2 (:pos @player)]))
                                                         ;                      (< (hiphip/aget ^doubles (:dijkstra-allies @mon) %2) (hiphip/aget ^doubles (:dijkstra-allies @mon) %1))
                                                         ;                      (= (hiphip/aget ^doubles (:dungeon @dun) %2) (double floor)))
                                                         ;                   %2
                                                         ;                   %1)
                                                                         ;orig-pos
                                                         ;                adjacent))
                                                         ; lowest (long (if (@(:mon-hash @dun) lowest-ignoring-monsters)
                                                         ;                lowest-respecting-monsters
                                                         ;                lowest-ignoring-monsters))
                                                          ]
                                                      (swap! mon assoc :pos lowest)
                                                      (when (or (= (- (long (:pos @player)) wide) lowest)
                                                                (= (+ (long (:pos @player)) wide) lowest)
                                                                (= (- (long (:pos @player)) 1   ) lowest)
                                                                (= (+ (long (:pos @player)) 1   ) lowest))
                                                        (damage-player player dun)))

                                 (when (not= (:wander @mon) 0)
                                   ((rand-nth [
                                          #(when (and (apply distinct? (concat (map (fn [atm] (long (:pos @atm))) mons) [(- (long (:pos @%)) wide) (long (:pos @player))]))
                                                      (= (hiphip/aget ^doubles (:dungeon @dun) (- (long (:pos @%)) wide)) floor)) (swap! % assoc :pos (- (long (:pos @%)) wide)))
                                          #(when (and (apply distinct? (concat (map (fn [atm] (long (:pos @atm))) mons) [(+ (long (:pos @%)) wide) (long (:pos @player))]))
                                                      (= (hiphip/aget ^doubles (:dungeon @dun) (+ (long (:pos @%)) wide)) floor)) (swap! % assoc :pos (+ (long (:pos @%)) wide)))
                                          #(when (and (apply distinct? (concat (map (fn [atm] (long (:pos @atm))) mons) [(- (long (:pos @%)) 1) (long (:pos @player))]))
                                                      (= (hiphip/aget ^doubles (:dungeon @dun) (- (long (:pos @%)) 1)) floor)) (swap! % assoc :pos (- (long (:pos @%)) 1)))
                                          #(when (and (apply distinct? (concat (map (fn [atm] (long (:pos @atm))) mons) [(+ (long (:pos @%)) 1) (long (:pos @player))]))
                                                      (= (hiphip/aget ^doubles (:dungeon @dun) (+ (long (:pos @%)) 1)) floor)) (swap! % assoc :pos (+ (long (:pos @%)) 1)))]
                                            ) mon)))
                                  (swap! (:mon-hash @dun) assoc (:pos @mon) mon))
                                 )
    flee-map))

(defn ascend
  [pc dun]
  (swap! cleared-levels assoc @dlevel (assoc @dun :full-seen (aclone ^booleans (:full-seen @pc))))
  (swap! dlevel clojure.core/dec)
  (let [
                                dd1 (:dungeon (get @cleared-levels @dlevel))
                                ;dungeon-res  (dungeon-resistances dd1)
                                shown (:shown (get @cleared-levels @dlevel))
                                monsters (:monsters (get @cleared-levels @dlevel))
                                mon-hash (:mon-hash (get @cleared-levels @dlevel))
                                ]
    
    (reset! dun {:dungeon dd1
                 :shown shown
                 :monsters monsters
                 :mon-hash mon-hash})
    ;monster-calc (doall (map #(do (init-dungeon dd1 %)(swap! % assoc :dijkstra nil)) @monsters))]
    (place-creature dun pc 10002.0)
    (harray/afill! boolean [[i x] ^booleans (:full-seen @pc)] (aget ^booleans (:full-seen (get @cleared-levels ^long @dlevel)) i))
    (reset! res (dungeon-resistances dd1))
                            ;(init-monsters dd1 @(:monsters @dun))
                            ))

(defn descend
  [pc dun]
  (swap! cleared-levels assoc @dlevel (assoc @dun :full-seen (aclone ^booleans (:full-seen @pc))))
  (swap! dlevel clojure.core/inc)
  (if (contains? @cleared-levels @dlevel)
    (let [
      dd1 (:dungeon (get @cleared-levels @dlevel))
      shown (:shown (get @cleared-levels @dlevel))
      monsters (:monsters (get @cleared-levels @dlevel))
      mon-hash (:mon-hash (get @cleared-levels @dlevel))
      
          ]
          ;monster-calc (doall (map #(do (init-dungeon dd1 %) (swap! % assoc :dijkstra nil)) @mons))]
      (reset! dun {:dungeon dd1 :shown shown :monsters monsters :mon-hash mon-hash})
      (place-creature dun pc 10001.0)
      (harray/afill! Boolean/TYPE [[i x] ^booleans (:full-seen @pc)] (aget ^booleans (:full-seen (get @cleared-levels ^long @dlevel)) i))
      (reset! res (dungeon-resistances dd1))
      
      
      )
    (let [
      dd0 (prepare-bones)
      dd1 (first dd0)
      shown (last dd0)
      _ (let [monsters (init-monsters dd1)
            mon-hash (atom (into {} (map (fn [entry] [(:pos @entry) entry]) @monsters)))]
        (reset! dun {:dungeon dd1 :shown shown :monsters monsters :mon-hash mon-hash}))
      player-calc  (place-creature dun pc 10001.0)

          ;monster-calc (doall (map #(init-dungeon dd1 %) @mons))
      blank-seen (init-full-seen)]
      (harray/afill! Boolean/TYPE [[i x] ^booleans (:full-seen @pc)] (aget ^booleans blank-seen i))
      (reset! res (dungeon-resistances dd1))
      )))
(comment
(defn shoot [pc mons dd target monhash]
    (let [mon-list (drop-while #(not= target (:ident @%)) @mons)]
      (when (seq mon-list)
        (let [tgt (first mon-list)]
      (when (> (double (aget ^floats (:fov @pc) (:pos @tgt))) 0.0)
        (damage-monster tgt dd monhash 1 2)
        (move-monster @mons dd monhash))
    ))))
)
