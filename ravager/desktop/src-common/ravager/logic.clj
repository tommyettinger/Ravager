(ns ravager.logic
  (:use ravager.herringbone); primitive-math)
	(:require [hiphip.double :as hiphip]
            [hiphip.array :as harray])
  (:import [squid.squidgrid.fov TranslucenceWrapperFOV BresenhamLOS BasicRadiusStrategy])
  (:gen-class))

(set! *warn-on-reflection* true)

(defmacro +_ [a b] `(+ (long ~a) (long ~b)))
(defmacro -_ [a b] `(- (long ~a) (long ~b)))
(defmacro *_ [a b] `(* (long ~a) (long ~b)))
(defmacro div_ [a b] `(/ (long ~a) (long ~b)))
(defmacro rem_  [a b] `(rem  (long ~a) (long ~b)))
(defmacro quot_ [a b] `(quot (long ~a) (long ~b)))
(defmacro >_ [a b] `(> (long ~a) (long ~b)))
(defmacro <_ [a b] `(< (long ~a) (long ~b)))
(defmacro >=_ [a b] `(>= (long ~a) (long ~b)))
(defmacro <=_ [a b] `(<= (long ~a) (long ~b)))

(def wide (long 33))
(def high (long 33))
(def iw (-_ wide 2)) ;inner width
(def ih (-_ high 2)) ;inner height

(def wall (double 9999.0))
(def floor (double 2000.0))
(def dark (double 11111.0))

(def GOAL (double 0.0))

(def cleared-levels (atom {}))
(def dlevel (atom 0))
(def res (atom (make-array Float/TYPE wide high)))
(defn ^"[Z" init-full-seen [] (let [ ^"[Z" res1d (make-array Boolean/TYPE (*_ wide high))]
                   (doseq [i (range (*_ wide high))]
                     (aset ^"[Z" res1d i false))
                     res1d))

(def player (atom {:pos 0 :show \@ :hp 100 :vision 8 :dijkstra nil :fov nil :full-seen (init-full-seen)}))
(def monsters (atom (vec (for [i (range 1 25)] (atom {:pos 0 :show \M :hp 8 :vision 7 :dijkstra nil :wander 1})))))
(def monster-hash (atom {}))
(def ^TranslucenceWrapperFOV fov (TranslucenceWrapperFOV. ))
(def ^BresenhamLOS los (BresenhamLOS. ))

(defn make-bones []
  (let [seed (rand-int (count horiz))
        initial (horiz seed)
        hvec (mapv #(mapv vec %) horiz)
        vvec (mapv #(mapv vec %) vert)
        oh (+_ 20 ih)
        ow (+_ 20 iw)
        initial (hiphip/amake [i (*_ ow oh)] wall)
        shown (char-array (*_ ow oh) \#)]
    (loop [next-fill 0 started-indent 0]
      (if (>=_ (+_ (*_ 10 ow ) next-fill) (*_ ow oh))
          initial
          (let [hofull (rand-nth hvec)
                    ho (mapv #(replace {\# wall \. floor \$ floor \~ floor \% floor \+ floor} %) hofull)]
            (when (<_ (+_ 20 (+_ (*_ 10 ow) next-fill)) (*_ ow oh))
                (doseq [nf (range 10)]
                                       (hiphip/afill! [[i eh] initial :range [(+_ (*_ ow nf) next-fill) (+_ 20 (+_ (*_ ow nf) next-fill))]]
                                                                  (nth (nth ho nf) (-_ (-_ i (*_ ow nf)) next-fill)))
                                       (harray/afill! Character/TYPE [[i eh] shown :range [(+_ (*_ ow nf) next-fill) (+_ 20 (+_ (*_ ow nf) next-fill))]]
                                                                  (nth (nth hofull nf) (-_ (-_ i (*_ ow nf)) next-fill)))))
                (recur
                 (long
                   (if (<_ (rem_ (+_ 40 next-fill) ow) (rem_ next-fill ow))
                     (condp = started-indent
                       0 (+_ (+_ (*_ ow 10) 10) (-_ next-fill (rem_ next-fill ow )))
                       1 (+_ (+_ (*_ ow 10) 20) (-_ next-fill (rem_ next-fill ow )))
                       2 (+_ (+_ (*_ ow 10) 30) (-_ next-fill (rem_ next-fill ow )))
                       3 (+_     (*_ ow 10)     (-_ next-fill (rem_ next-fill ow )))
                       )
                     (+_ 40 next-fill) ) )
                 (long (if (<_ (rem_ (+_ 40 next-fill) ow) (rem_ next-fill ow))
                   (rem_ (+_ 1 started-indent) 4)
                   started-indent))
                 ))))
    (loop [next-fill (*_ 10 ow) started-indent 1]
      (if (>=_ (+_ 10 (rem_ next-fill ow)) ow)
          initial
        (let [vefull (rand-nth vvec)
                    ve (mapv #(replace {\# wall \. floor \$ floor \~ floor \% floor \+ floor} %) vefull)]
                (when (<_ (+_ (+_ (*_ 19 ow) 10) next-fill) (*_ ow oh))
                  (doseq [nf (range 20)] (hiphip/afill! [[i eh] initial :range [(+_ (*_ ow nf) next-fill) (+_ (+_ 10 (*_ ow nf)) next-fill)]]
                                                                  (nth (nth ve nf) (-_ (-_ i (*_ ow nf)) next-fill)))
                      (harray/afill! Character/TYPE [[i eh] shown :range [(+_ (*_ ow nf) next-fill) (+_ (+_ 10 (*_ ow nf)) next-fill)]]
                                                                  (nth (nth vefull nf) (-_ (-_ i (*_ ow nf)) next-fill)))))
                (recur
                 (long
                   (if (<_ (rem_ (+_ 40 (quot_ next-fill ow)) oh) (quot_ next-fill ow))
                     (condp = started-indent
                       0 (+_ (+_  (* ow 10) 10) (rem_ next-fill ow))
                       1 (+_ (+_  (* ow 20) 10) (rem_ next-fill ow))
                       2 (+_ (+_  (* ow 30) 10) (rem_ next-fill ow))
                       3 (+_                10  (rem_ next-fill ow))
                       )
                       (+_ (*_ 40 ow) next-fill) ) )
                 (long (if (<_ (rem_ (+_ 40 (quot_ next-fill ow)) oh) (quot_ next-fill ow))
                   (rem_ (+_ 1 started-indent) 4)
                   started-indent))
                 ))))
    ;(doall (map #(println (apply str %)) (partition ow (vec shown))))
    [(hiphip/amake [i (*_ wide high)] (if (or
			(= (rem_ i wide) 0)
			(= (rem_ i wide) (+_ -1 wide))
			(<_ i wide)
			(>_ i (-_ (*_ wide high) wide)))
		 wall
		 (hiphip/aget initial (+_ (+_ (+_ (*_ 10 ow) -10) (*_ 20 (quot_ i wide))) (-_ (-_ i (+_ -1 wide)) (*_ 2 (quot_ i wide)))))))

     (harray/amake Character/TYPE [i (*_ wide high)] (if (or
			(= (rem_ i wide) 0)
			(= (rem_ i wide) (+_ -1 wide))
			(<_ i wide)
			(>_ i (-_ (*_ wide high) wide)))
		 \#
		 (aget shown (+_ (+_ (+_ (*_ 10 ow) -10) (*_ 20 (quot_ i wide))) (-_ (-_ i (+_ -1 wide)) (*_ 2 (quot_ i wide)))))))
                                       ]))

(defn ^"[[F" dungeon-resistances [^doubles dungeon]
                 (let [res2d (make-array Float/TYPE wide high)]
                   (doseq [x (range wide) y (range high)]
                     (aset res2d x y (if
                                       (= (hiphip/aget ^doubles dungeon (+_ x (*_ wide y))) (double wall))
                                       (float 1.0)
                                       (float 0.0))))
                   res2d))

(defn run-fov-player
  [entity dungeon]
    (let [^"[[F" calculated
  ;        (let [res2d (make-array Float/TYPE wide high)]
  ;                 (doseq [x (range wide) y (range high)]
  ;                   (aset res2d x y
  ;                                     (float 1.0)
  ;                                     ))
  ;                 res2d)]
          (. fov calculateFOV @res (int (rem_ (:pos @entity) wide)) (int (quot_ (:pos @entity) wide)) (float 1.0) (float (/ 1.0 (double (:vision @entity)))) BasicRadiusStrategy/DIAMOND)]
      (doseq [ idx (range (*_ wide high))]
         (aset ^"[Z" (:full-seen @entity) idx
            (boolean (or (aget ^"[Z" (:full-seen @entity) idx)
                (if (> (double (aget ^"[[F" calculated (rem_ idx wide) (quot_ idx wide))) 0.0)
                  true
                  false)

                   ))))
      calculated)
  )

(defn run-fov
  ([entity dd]
    (let [^"[[F" calculated (. fov calculateFOV @res (rem_ (:pos @entity) wide) (quot_ (:pos @entity) wide) 1.0 (/ 1.0 (double (:vision @entity))) BasicRadiusStrategy/DIAMOND)]
      calculated))
  ([entity dd distance]
    (let [^"[[F" calculated (. fov calculateFOV @res (rem_ (:pos @entity) wide) (quot_ (:pos @entity) wide) 1.0 (/ 1.0 (double distance)) BasicRadiusStrategy/DIAMOND)]
      calculated)))
(defn check-los
  [start end distance]
     (. los isReachable @res (mod start wide) (quot start wide) (mod end wide) (quot end wide) distance 1.0 BasicRadiusStrategy/DIAMOND)) ; (/ 1.0 distance)

(defn init-dungeon ([dngn] (loop [ctr 0] (if (>= ctr  1) (hiphip/aclone dngn) (let [rand-loc (rand-int (*_ wide high))] (if (= (hiphip/aget dngn rand-loc) floor)
			                                                        (recur (do (hiphip/aset dngn rand-loc GOAL) (inc ctr))) (recur ctr))))))
  ([dngn entity] (loop [ctr 0] (if (>= ctr 1) entity (let [rand-loc (rand-int (*_ wide high))] (if (and
                                                                                    (apply distinct? (concat (filter (complement nil?)
                                                                                                             (map (fn [atm] (if (= (:pos @atm) 0) nil (:pos @atm))) @monsters))
                                                                                                        [rand-loc (:pos @player)]))
                                                                                                 (= (hiphip/aget ^doubles dngn rand-loc) floor))
			                                                        (recur (do
                                                                       (swap! entity assoc :pos rand-loc)
                                                                       ;(when (= \# (aget ^chars shown (:pos @entity)))
                                                                       ;   (println "Monster intersecting with wall"))

                                                                       (inc ctr)))
                                                              (recur ctr))))))
  ([dngn entity starting-cell] (loop [ctr 0] (if (>= ctr 1) dngn (let [rand-loc (rand-nth (keep-indexed #(if (= %2 starting-cell) %1) (vec dngn)))]
                                                                   (if (and (apply distinct? (concat (filter (complement nil?)
                                                                                                             (map (fn [atm] (if (= (:pos @atm) 0) nil (:pos @atm))) @monsters))
                                                                                                        [rand-loc (:pos @player)]))
                                                                                                 (= (hiphip/aget dngn rand-loc) starting-cell))
			                                                        (recur (do (swap! entity assoc :pos rand-loc) (inc ctr))) (recur ctr)))))))

(defn alter-dungeon
  ([dngn cell] (loop [ctr 0] (if (>= ctr  1) dngn (let [rand-loc (rand-int (*_ wide high))]
                                                    (if (= (hiphip/aget dngn rand-loc) floor)
			                                                        (recur (do (hiphip/aset dngn rand-loc cell) (inc ctr))) (recur ctr))))))
  ([dngn shown cell shown-cell filt] (loop [ctr 0] (if (>= ctr  1) dngn (let [rand-loc (rand-int (*_ wide high))]
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

(defn find-_owest [^doubles a]
  (let [low-val (hiphip/amin a)]
    (find-cells a low-val)))

(defn find-monsters [m]
    (into {} (for [mp (map #(:pos @%) m)] [mp wall])))

(defn dijkstra
  ([a]
     (dijkstra a (find-walls a) (find-_owest a)))
  ([a ent]
     (dijkstra a (dissoc (merge (find-walls a) (find-monsters @monsters)) (:pos @ent)) (find-_owest a)))
  ([a closed open-cells]
     (loop [open open-cells]
       (when (seq open)
         (recur (reduce (fn [newly-open [i ^double v]]
                          (reduce (fn [acc dir]
                                    (if (or (closed dir) (open dir)
                                            (>= (+ 1.0 v) (hiphip/aget ^doubles a dir)))
                                      acc
                                      (do (hiphip/aset ^doubles a dir (+ 1.0 v))
                                          (assoc acc dir (+ 1.0 v)))))
                                  newly-open, [(-_ i wide)
                                               (+_ i wide)
                                               (-_ i 1)
                                               (+_ i 1)]))
                        {}, open))))
     a))

(defn local-dijkstra
  ([a center radius]
     (local-dijkstra a (find-walls a) {center 0} center radius))
  ([a ent center radius]
     (local-dijkstra a (dissoc (merge (find-walls a) (find-monsters @monsters)) (:pos @ent)) {center 0} center radius))
  ([a closed open-cells center radius]
     (loop [open open-cells ctr 0]
       (if (and (seq open) (< ctr (long radius)))
         (recur (reduce (fn [newly-open [i ^double v]]
                          (reduce (fn [acc dir]
                                    (if (or (closed dir) (open dir)
                                            (>= (+ 1.0 v) (hiphip/aget ^doubles a dir)))
                                      acc
                                      (do
                                          (hiphip/aset ^doubles a dir (+ 1.0 v))
                                          (assoc acc dir (+ 1.0 v)))))
                                  newly-open, [(-_ i wide)
                                               (+_ i wide)
                                               (-_ i 1)
                                               (+_ i 1)]))
                        {}, open) (inc ctr))
         a))
     ))

(defn local-dijkstra-map
  ([a center radius]
     (local-dijkstra-map a (find-walls a) {center 0} center radius))
  ([a ent center radius]
     (local-dijkstra-map a (dissoc (merge (find-walls a) (find-monsters @monsters)) (:pos @ent)) {center 0} center radius))
  ([a closed open-cells center radius]
     (loop [open open-cells result (atom {}) ctr 0]
       (if (and (seq open) (< ctr (long radius)))
         (recur (reduce (fn [newly-open [i ^double v]]
                          (reduce (fn [acc dir]
                                    (if (or (closed dir) (open dir)
                                            (>= (+ 1.0 v) (double (get @result dir 22222.0))))
                                      acc
                                      (do
                                          (swap! result assoc dir (+ 1.0 v))
                                          (assoc acc dir (+ 1.0 v)))))
                                  newly-open, [(-_ i wide)
                                               (+_ i wide)
                                               (-_ i 1)
                                               (+_ i 1)]))
                        {}, open) result (inc ctr))
         @result))
     ))

(defn init-ambush-entity [dngn entity choke] (loop [ctr 0] (let [rand-loc (rand-int (*_ wide high))]
                                                                                 (if (and
                                                                                    (= (hiphip/aget ^doubles dngn rand-loc) floor)
                                                                                    (apply distinct? (concat (filter (complement nil?)
                                                                                                             (map (fn [atm] (if (= (:pos @atm) 0) nil (:pos @atm))) @monsters))
                                                                                                        [rand-loc (:pos @player)]))
                                                                                                 (> (hiphip/aget choke rand-loc) 10.0))
			                                                        (do
                                                                       (swap! entity assoc :pos rand-loc)
                                                                       (swap! entity assoc :wander 0)
                                                                       entity)
                                                              (if (>= ctr 10) (init-dungeon dngn entity) (recur (inc ctr)))))))
(defn init-monsters
  [dungeon mons]
;    (let [chokepoints (amap ^doubles dungeon idx _ (double (if (>= (hiphip/aget ^doubles dungeon idx) wall) wall (reduce (fn [base [k v]] (+ base v)) 0 (local-dijkstra ^doubles dungeon idx 2)))))]
  (let [chokepoints (amap ^doubles dungeon idx _ (double (if (>= (hiphip/aget ^doubles dungeon idx) (double wall))
                                                           0.0
                                                           (reduce (fn [base [k ^double v]] (if (>= v 5.0) (+_ base (if (check-los idx k 1.0) 0 1)) base)) 0 (local-dijkstra-map ^doubles dungeon idx 7)))))]
    (mapv #(do (init-ambush-entity dungeon % chokepoints) %) @monsters)))

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
                        (if (and (< (double worst) (double floor)) (> (long worst) (div_ (+_ wide high) 4)))
                              [(double-array (map-indexed #(if (= (double %2) (double GOAL))
                                                                       (do (aset ^chars shown %1 \<) 10001.0)
                                                                       (if (< (double %2) (double wall)) (double floor) (double %2)))
                                                                    (alter-dungeon (dijkstra scanned) shown 10002.0 \> #(and (>=_ % (div_ (+_ wide high) 4)) (< (double %) (double floor))))))
                                                                shown]
                                    (let [d0 (make-bones)
                                          d_ (first d0)
                                          d1 (init-dungeon d_)
                                          d2 (double-array (map #(if (< (double %) (double wall)) floor %) (replace {floor dark} (vec (dijkstra d_)))))
                                          w2 (apply clojure.core/max (filter #(< (double %) (double wall)) (vec (dijkstra (hiphip/aclone d1)))))]
                                      (recur d2 d1 w2 (harray/afill! char [[i x] ^chars (second d0)] (if (= (hiphip/aget d2 i) (double wall)) \# x))))))))

(defn damage-player
  ([entity dd]
  (do (swap! entity assoc :hp (-_ (:hp @entity) (+_ 1 (rand-int 4))))
    (if (<=_ (:hp @player) 0)
      (do
        (println (str "GAME OVER.  You explored "
                          (count (filter true?
                                         (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @player))))))
                          " squares and reached floor " (+_ 1 @dlevel) "."
                          )) (System/exit 0)))))
  ([entity dd amt]
  (do (swap! entity assoc :hp amt)
    (if (<=_ (:hp @player) 0)
      (do
        (println (str "GAME OVER.  You explored "
                          (count (filter true?
                                         (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @player))))))
                          " squares and reached floor " (+_ 1 @dlevel) "."
                          )) (System/exit 0))))))

(defn damage-monster
  ([entity dd monhash]
  (do (swap! entity assoc :hp (-_ (:hp @entity) (+_ 2 (rand-int 6))))
    (when (<=_ (:hp @entity) 0)
      (reset! monsters (remove #(= % entity) @monsters))
      (swap! monhash dissoc (:pos @entity)))
     ))
  ([entity dd monhash dice die-size]
  (do (swap! entity assoc :hp (-_ (:hp @entity) (apply clojure.core/+ (repeatedly dice #(+_ 1 (rand-int die-size))))))
    (when (<=_ (:hp @entity) 0)
      (reset! monsters (remove #(= % entity) @monsters))
      (swap! monhash dissoc (:pos @entity)))
     )))


(defn move-monster [mons dd monhash]
  (let [flee-map (let [first-d (hiphip/aclone ^doubles (:dungeon @dd))
                                                     d-eh (aset ^doubles first-d ^long (:pos @player) ^double GOAL)
                                                     new-d (hiphip/afill! [[idx x] (dijkstra first-d)]
                                                                        (if (= (:pos @player) idx)
                                                                          10007.0
                                                                          (if (>= (double x) (double wall))
                                                                            wall
                                                                            (Math/floor (* -1.4 (double x))))
                                                                          ))]
                   (dijkstra new-d))
         ^"[[F" player-fov-extended (run-fov player dd 9)
         pc-x (rem_ (:pos @player) wide)
         pc-y (quot_ (:pos @player) wide)]
                             (doseq [monster mons]
                               (let [mon-x (rem_ (:pos @monster) wide)
                                     mon-y (quot_ (:pos @monster) wide)
                                     monster-sees-player (and (> (double (aget ^"[[F" player-fov-extended mon-x mon-y)) 0.0)
                                                              (<=_ (+_ (Math/abs (double (-_ mon-x pc-x)))
                                                                     (Math/abs (double (-_ mon-y pc-y))))
                                                                  (long (:vision @monster))))]
                                 (swap! monhash dissoc (:pos @monster))
                                 ;(println monster-sees-player)
                                 (if (>_ (:hp @monster) 2)
                                   (when monster-sees-player
                                     (swap! monster assoc :dijkstra
                                               (let [new-d (hiphip/aclone ^doubles (:dungeon @dd))] (aset ^doubles new-d ^long (:pos @player) ^double GOAL) (dijkstra new-d monster)))
                                     (swap! monster assoc :wander 1))
                                   (when monster-sees-player
                                     (swap! monster assoc :dijkstra flee-map)
                                     (swap! monster assoc :wander 1)))))
                             ;(println)
                             (doseq [mon mons]
                                (let [oldpos (:pos @mon)]
                                (if (:dijkstra @mon) (let [orig-pos (:pos @mon)
                                                          adjacent (shuffle [
                                                                    (-_ orig-pos wide)
                                                                    (+_ orig-pos wide)
                                                                    (-_ orig-pos 1)
                                                                    (+_ orig-pos 1)])
                                                          lowest (reduce #(if (and
                                                                               (apply distinct? (concat (map (fn [atm] (:pos @atm)) mons) [%2 (:pos @player)]))
                                                                               (< (hiphip/aget ^doubles (:dijkstra @mon) %2) (hiphip/aget ^doubles (:dijkstra @mon) %1))
                                                                               (= (hiphip/aget ^doubles (:dungeon @dd) %2) (double floor)))
                                                                            %2
                                                                            %1)
                                                                         orig-pos
                                                                         adjacent)]
                                                      (swap! mon assoc :pos lowest)
                                                      (when (or (= (-_ (:pos @player) wide) (:pos @mon))
                                                                (= (+_ (:pos @player) wide) (:pos @mon))
                                                                (= (-_ (:pos @player) 1   ) (:pos @mon))
                                                                (= (+_ (:pos @player) 1   ) (:pos @mon)))
                                                        (damage-player player dd)))

                                 (when (not= (:wander @mon) 0)
                                   ((rand-nth [
                                          #(when (and (apply distinct? (concat (map (fn [atm] (:pos @atm)) mons) [(-_ (:pos @%) wide) (:pos @player)]))
                                                      (= (hiphip/aget ^doubles (:dungeon @dd) (-_ (:pos @%) wide)) floor)) (swap! % assoc :pos (-_ (:pos @%) wide)))
                                          #(when (and (apply distinct? (concat (map (fn [atm] (:pos @atm)) mons) [(+_ (:pos @%) wide) (:pos @player)]))
                                                      (= (hiphip/aget ^doubles (:dungeon @dd) (+_ (:pos @%) wide)) floor)) (swap! % assoc :pos (+_ (:pos @%) wide)))
                                          #(when (and (apply distinct? (concat (map (fn [atm] (:pos @atm)) mons) [(-_ (:pos @%) 1) (:pos @player)]))
                                                      (= (hiphip/aget ^doubles (:dungeon @dd) (-_ (:pos @%) 1)) floor)) (swap! % assoc :pos (-_ (:pos @%) 1)))
                                          #(when (and (apply distinct? (concat (map (fn [atm] (:pos @atm)) mons) [(+_ (:pos @%) 1) (:pos @player)]))
                                                      (= (hiphip/aget ^doubles (:dungeon @dd) (+_ (:pos @%) 1)) floor)) (swap! % assoc :pos (+_ (:pos @%) 1)))]
                                            ) mon)))
                                  (swap! monhash assoc (:pos @mon) mon))
                                 )
    flee-map))

(defn ascend
  [pc mons dd]
  (swap! cleared-levels assoc @dlevel (assoc @dd :full-seen (aclone ^"[Z" (:full-seen @pc))))
  (swap! dlevel clojure.core/dec)
  (let [
                                dd1 (:dungeon (get @cleared-levels @dlevel))
                                ;dungeon-res  (dungeon-resistances dd1)
                                shown (:shown (get @cleared-levels @dlevel))
                                player-calc  (init-dungeon dd1 pc 10002.0)
                                ]
;monster-calc (doall (map #(do (init-dungeon dd1 %)(swap! % assoc :dijkstra nil)) @monsters))]
                            (harray/afill! boolean [[i x] ^"[Z" (:full-seen @pc)] (aget ^"[Z" (:full-seen (get @cleared-levels ^long @dlevel)) i))
                            (reset! res (dungeon-resistances dd1))
                            (init-monsters dd1 @mons)
                            (reset! dd {:dungeon dd1 :shown shown})
                            ))

(defn descend
  [pc mons dd]
  (swap! cleared-levels assoc @dlevel (assoc @dd :full-seen (aclone ^"[Z" (:full-seen @pc))))
  (swap! dlevel clojure.core/inc)
  (if (contains? @cleared-levels @dlevel)
    (let [
      dd1 (:dungeon (get @cleared-levels @dlevel))
      shown (:shown (get @cleared-levels @dlevel))
      player-calc  (init-dungeon dd1 pc 10001.0)
          ]
          ;monster-calc (doall (map #(do (init-dungeon dd1 %) (swap! % assoc :dijkstra nil)) @mons))]
      (harray/afill! Boolean/TYPE [[i x] ^"[Z" (:full-seen @pc)] (aget ^"[Z" (:full-seen (get @cleared-levels ^int @dlevel)) i))
      (reset! res (dungeon-resistances dd1))
      (init-monsters dd1 @mons)
      (reset! dd {:dungeon dd1 :shown shown})
      )
    (let [
      dd0 (prepare-bones)
      dd1 (first dd0)
      shown (last dd0)
      player-calc  (init-dungeon dd1 pc 10001.0)

          ;monster-calc (doall (map #(init-dungeon dd1 %) @mons))
      blank-seen (init-full-seen)]
      (harray/afill! Boolean/TYPE [[i x] ^"[Z" (:full-seen @pc)] (aget ^"[Z" blank-seen i))
      (reset! res (dungeon-resistances dd1))
      (init-monsters dd1 @mons)
      (reset! dd {:dungeon dd1 :shown shown})
      )))

(defn shoot [pc mons dd target monhash]
    (let [mon-list (drop-while #(not= target (:ident @%)) @mons)]
      (when (seq mon-list)
        (let [tgt (first mon-list)]
      (when (> (double (aget ^"[[F" (:fov @pc) (rem_ (:pos @tgt) wide) (quot_ (:pos @tgt) wide))) 0.0)
        (damage-monster tgt dd monhash 1 2)
        (move-monster @mons dd monhash))
    ))))
