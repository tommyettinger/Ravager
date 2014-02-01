(ns ravager.core
  (:use ravager.logic) ; primitive-math)
  (:require [clojure.java.io :as io]
            [hiphip.double :as hiphip]
            [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.utils :as u])
  (:import [java.io FileWriter]
            [com.badlogic.gdx Gdx Files Game]
            [com.badlogic.gdx.math Rectangle Vector3]
            [com.badlogic.gdx.scenes.scene2d Stage Actor]
            [com.badlogic.gdx.scenes.scene2d.utils ScissorStack]
            [com.badlogic.gdx.graphics.g2d BitmapFont TextureAtlas TextureAtlas$AtlasSprite TextureRegion SpriteBatch Sprite NinePatch]
            [com.badlogic.gdx.graphics Camera OrthographicCamera Color]))
(set! *warn-on-reflection* true)
(comment
  (defn restore-arr [vv]
  (let [arr (make-array Float/TYPE (count vv) (count (nth vv 0)))]
    (doseq [a2 (range (count vv))] (aset ^"[[F" arr a2 (float-array (nth vv a2)))) arr)))

(defmethod print-dup (Class/forName "[D") [a out] (.write ^java.io.FileWriter out (str "#=" `(double-array ~(vec a)))))
(defmethod print-dup (Class/forName "[F") [a out] (.write ^java.io.FileWriter out (str "#=" `(float-array ~(vec a)))))
(defmethod print-dup (Class/forName "[C") [a out] (.write ^java.io.FileWriter out (str "#=" `(char-array ~(vec a)))))
(defmethod print-dup (Class/forName "[Z") [a out] (.write ^java.io.FileWriter out (str "#=" `(boolean-array ~(vec a)))))
;(defmethod print-dup (Class/forName "[[F") [a out] (.write ^java.io.FileWriter out (str "#=" `(restore-arr ~(mapv (partial mapv double) a)))))
(declare sv sh ; straight vert/horiz
         gr da ; ground/dark
         ul ur ; up left/right (pointing these ways)
         dl dr ; down left/right
         cw    ; crosswall
         Tu Td Tl Tr) ; T-shaped pointing up/down/left/right
(declare dd shown nucky monster-tiles monster-tile-sizes health-pc health-mob health-red)
(declare clean-bones)
(def dun (atom nil))
(def map-ents (atom nil))
(def presses (atom nil))
(def cursor (atom nil))
(def mode (atom :act))
(def colors (vec (repeatedly (* wide high) #(color 1 1 1 1))))
(defn disposal []
  (if (> ^long (:hp @player) 0)
        (do
        ;  (println @cleared-levels)
          (binding [*print-dup* true]
            (swap! cleared-levels #(mapv (fn [[_ lvl]] (assoc lvl
                                                          :monsters (mapv deref @(:monsters lvl))
                                                          :mon-hash (into {} (map (fn [[k v]] [k @v]) @(:mon-hash lvl)))))
                                         %))
            (spit "Savefile.edn" (pr-str [wide high (:dungeon @dun) (:shown @dun) @cleared-levels @dlevel @player (mapv deref @(:monsters @dun))]))))
        (io/delete-file "Savefile.edn" true)))

(defn make-entity
  ([tex hue ^long pos ^long d]
    (assoc (u/create-entity tex)
         :x (double (+ (* 32 (rem pos wide)) (* 16 (- wide (quot pos wide)))))
         :y (- 64.0 (double (* 32 (quot pos wide)))) ;(double (game :height))
         :d d
         :width 48.0
         :height 64.0
         :pos pos
         :hue ^Color (if (coll? hue) (color (hue 0) (hue 1) (hue 2) (hue 3)) (color :white)) )))
(comment '([tex hue ^long x ^long y ^long d]
    (assoc (u/create-entity tex)
         :x (double (+ (* 32 x) (* 16 (- wide y))))
         :y (- 64.0 (double (* 32 y)))
         :d d
         :width 48.0
         :height 64.0
         :pos (+ x (* y wide))
         :hue ^Color (if (coll? hue) (color (hue 0) (hue 1) (hue 2) (hue 3)) (color :white)))))
(defn make-player
  ([tex ^long pos sizes]
    [(assoc (u/create-entity tex)
         :x (double (+ (* 32 (rem pos wide)) (* 16 (- wide (quot pos wide)))))
         :y (- 64.0 (double (* 32 (quot pos wide))))
         :d 2
         :width 48.0
         :height 64.0
         :pos pos
         :is-me? true
         :hue (color 1 0.9 0.45 1))
     (assoc (u/create-entity tex)
         :x (double (+ (* 32 (rem pos wide)) (* 16 (- wide (quot pos wide)))))
         :y (- 64.0 (double (* 32 (quot pos wide))))
         :d 3
         :width 48.0
         :height 64.0
         :pos pos
         :is-me? true
         :hue (color 1 0 0 1)
         :clip (+ (long (:creature-start sizes))
                  (- (long (:creature-height sizes))
                     (* (long (:creature-height sizes))
                        (/ (long (:hp @player)) 999)))))
;    (assoc (u/create-entity health-red)
;         :x (double (+ (* 32 (rem pos wide)) (* 16 (- wide (quot pos wide)))))
;         :y (- (double (game :height)) (double (* 32 (quot pos wide))))
;         :d (+ d 1)
;         :width 20.0
;         :height 20.0
;         :pos pos
;         :hue (color :white))
;    (assoc (u/create-entity (nth health-pc (quot 20 (/ 100 (long (:hp @player))))))
;         :x (double (+ (* 32 (rem pos wide)) (* 16 (- wide (quot pos wide)))))
;         :y (- (double (game :height)) (double (* 32 (quot pos wide))))
;         :d (+ d 2)
;         :width 20.0
;         :height 20.0
;         :pos pos
;         :hue (color :white))
]))
(defn make-mob
  ([tex hue ^long pos sizes]
    [(assoc (u/create-entity tex)
         :x (double (+ (* 32 (rem pos wide)) (* 16 (- wide (quot pos wide)))))
         :y (- 64.0 (double (* 32 (quot pos wide))))
         :d 2
         :width 48.0
         :height 64.0
         :pos pos
         :is-npc? true
         :hue ^Color (if (coll? hue) (color (hue 0) (hue 1) (hue 2) (hue 3)) (color 1 0.2 0.3 1)) )
     (assoc (u/create-entity tex)
         :x (double (+ (* 32 (rem pos wide)) (* 16 (- wide (quot pos wide)))))
         :y (- 64.0 (double (* 32 (quot pos wide))))
         :d 3
         :width 48.0
         :height 64.0
         :pos pos
         :hue (color 0.8 0 0 1)
         :clip (+ (long (:creature-start sizes))
                  (- (long (:creature-height sizes))
                     (* (long (:creature-height sizes))
                        (/ (long (:hp @(get @(:mon-hash @dun) pos))) 10)))))]))
(comment '[
    (assoc (u/create-entity health-red)
         :x (double (+ (* 32 (rem pos wide)) (* 16 (- wide (quot pos wide)))))
         :y (- (double (game :height)) (double (* 32 (quot pos wide))))
         :d (+ d 1)
         :width 20.0
         :height 20.0
         :pos pos
         :hue (color :white))
    (assoc (u/create-entity (nth health-mob (quot 20 (/ 8 (long (:hp @(get @monster-hash pos)))))))
         :x (double (+ (* 32 (rem pos wide)) (* 16 (- wide (quot pos wide)))))
         :y (- (double (game :height)) (double (* 32 (quot pos wide))))
         :d (+ d 2)
         :width 20.0
         :height 20.0
         :pos pos
         :hue (color :white))])
(defn sort-entities [ents]
  (sort #(cond
           (< (quot (long (:pos %1)) wide) (quot (long (:pos %2)) wide)) -1
           (= (quot (long (:pos %1)) wide) (quot (long (:pos %2)) wide)) (cond
                                 (> (long (:d %1)) (long (:d %2))) 1
                                 (= (long (:d %1)) (long (:d %2))) (if (< (double (:x %1)) (double (:x %2))) -1 1)
                                 :else -1)
           :else 1)
        ents))

(defn center-camera
  ([screen]
    (position! screen
      (float (+ (* 32 (rem (long (:pos @player)) wide)) (* 16 (- wide (quot (long (:pos @player)) wide)))))
      (float (- 64.0 (double (* 32 (quot (long (:pos @player)) wide)))))) ;(double (game :height))
    ;(.apply (u/get-obj s :camera) (Gdx/gl10))
    )
  ([screen _]
    (position! screen
      (float (+ (* 32 (rem (long @cursor) wide)) (* 16 (- wide (quot (long @cursor) wide)))))
      (float (- 64.0 (double (* 32 (quot (long @cursor) wide)))))) ; (double (game :height))
    ;(.apply (u/get-obj s :camera) (Gdx/gl10))
    ))
(defn update-screen!
  [screen entities]
  (doseq [{:keys [x y is-me?]} entities]
    (when is-me?
      (position! screen x y)))
  entities)

(defn draw-texture!
  [^SpriteBatch batch {:keys [^TextureRegion object x y width height pos hue clip]}]
  (assert object)
  (let [x (float (or x 0))
        y (float (or y 0))
        width (float (or width (.getRegionWidth object)))
        height (float (or height (.getRegionHeight object)))
        hue (or hue (color :white))]
    (if clip
      (let [new-obj (TextureRegion. object 0 (int (- 64 clip)) 48 (int clip))]
        (.setColor ^SpriteBatch batch ^Color hue)
        (.draw batch new-obj x y width (float clip)))
      (do
        (.setColor ^SpriteBatch batch ^Color (if (> (double (aget ^floats (:fov @player) pos)) 0.0)
                                      ^Color (.lerp (color :black)
                                       hue
                                       (aget ^floats (:fov @player) pos))
                                     ^Color (nth colors pos)))
        (.draw batch object x y width height))
    ))
  nil)

(defn draw-all!
  [^SpriteBatch batch {:keys [renderer ^Camera camera] :as screen} entities]
  (assert renderer)
  (.setProjectionMatrix ^SpriteBatch batch (.combined ^OrthographicCamera camera))
  (.begin batch)
  (.setColor ^SpriteBatch batch ^Color Color/WHITE)
  (doseq [entity entities]
    (draw-texture! batch entity))
  (.end batch)
  entities)

(defn render-proper-stage!
  [{:keys [^Stage renderer ^Camera camera]}]
  (when camera
    (.setCamera renderer camera)
    
    (.setViewport renderer
      (float (game :width))
      (float (game :height))
      false
      (. (. camera position) x)
      (. (. camera position) y)
      (float (game :width))
      (float (game :height)))
    (.setCullingArea (.getRoot renderer) (Rectangle. 
      (. (. camera position) x)
      (. (. camera position) y)
      (float (game :width))
      (float (game :height)))))
  (doto renderer .act) ; .draw
  )

(defn render-proper!
  ([{:keys [renderer] :as screen}]
    (render-proper-stage! screen))
  ([^SpriteBatch batch {:keys [^Stage renderer] :as screen} entities]
    (render-proper! screen)
    (draw-all! batch screen entities)))

(defn update-fov [dun]
  (swap! player assoc :fov (run-fov-player player dun))
  (doall (map-indexed (fn [^long idx ^Color item]
                          (.set ^Color item
                            ^Color (if (> (double (aget ^floats (:fov @player) idx)) 0.0)
                                     (.lerp (color :black)
                                       (color 1.0 1.0 0.8 1.0)
                                       (aget ^floats (:fov @player) idx))
                                     (if
                                       (aget ^booleans (:full-seen @player) idx)
                                       (color 0.2 0.2 0.2 1)
                                       (color 0 0 0 0))))) colors)))
(defn shift-player [pc dun newpos]
 ; (let [remake? (atom false)]
  (if
    (and (apply distinct? (conj (map (fn [atm] (:pos @atm)) @(:monsters @dun)) newpos))
         (or (= (hiphip/aget ^doubles (:dungeon @dun) newpos) floor)
             (= (hiphip/aget ^doubles (:dungeon @dun) newpos) 10001.0)
             (= (hiphip/aget ^doubles (:dungeon @dun) newpos) 10002.0)))
    (do
      (swap! pc assoc :pos newpos)
      (condp = (hiphip/aget ^doubles (:dungeon @dun) newpos)
        floor (do
                (move-monster dun))
        10001.0 (do
                  (if (= @dlevel 0)
                    (println (str "YOU ESCAPED.  You explored "
                                  (count (filter true?
                                                 (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @pc))))))
                                  " squares."))
                    (do (ascend pc dun)
                      (reset! map-ents (clean-bones))
                      ;(reset! monster-hash (into {} (map (fn [entry] [(:pos @entry) entry]) @monsters)))
                      (update-fov dun)
                      (println
                        (str "YOU ASCEND TO FLOOR " (inc (long @dlevel)) "...  You explored "
                             (count (filter true?
                                            (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @pc))))))
                             " squares.")))))
        10002.0 (do
                  (descend pc dun)
                  (mapv #(do
                    (swap! % assoc :sizes (nth monster-tile-sizes (:tile @%)))
                    (swap! % assoc :hue
                            (condp = (:tile @%)
                                     0  [(rand) (rand) (rand) 1]
                                     1  [1   0   1   1]
                                     2  [0.8 0   0.9 0.6]
                                     3  [0.7 0   0.9 0.7]
                                     4  [1   0.9 0.4 1]
                                     5  [0.5 0.9 1   1]
                                     6  [1   0.8 0.6 1]
                                     7  [0.4 1   0.9 0.7]
                                     8  [1   0.7 0.5 1]
                                     9  [0.9 0.5 0.3 1]
                                     10 [0.6 0.9 0.7 1]
                                     11 [0.9 0.8 0.1 1]
                                     12 [0.8 0.75 0.75 1]
                                     13 [0.9 0.2 0.9 1]
                                        [1   0.9 0.4 1]
                                     ))) @(:monsters @dun))
                  (reset! map-ents (clean-bones))
                  ;(reset! monster-hash (into {} (map (fn [entry] [(:pos @entry) entry]) @monsters)))
                  (update-fov dun)
                  (println
                    (str "YOU DESCEND TO FLOOR " (inc (long @dlevel)) "...  You explored "
                         (count (filter true?
                                        (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @pc))))))
                         " squares.")))
        (println "Something's wrong."))
      )
    (when (= (hiphip/aget ^doubles (:dungeon @dun) newpos) (double floor))
      (when
        (@(:mon-hash @dun) newpos)
        (damage-monster (@(:mon-hash @dun) newpos) dun))
      (move-monster dun)))
  (update-fov dun))
    
(defn process-key [keycode screen entities]
  (let [pp (long (:pos @player))]
    (if (= @mode :act)
      (condp = keycode
        (key-code :up)    (do (shift-player player dun (- pp wide)) (center-camera screen))
        (key-code :down)  (do (shift-player player dun (+ pp wide)) (center-camera screen))
        (key-code :left)  (do (shift-player player dun (- pp 1)) (center-camera screen))
        (key-code :right) (do (shift-player player dun (+ pp 1)) (center-camera screen))
       ; Input$Keys/L     (do (reset! cursor pp) (reset! mode :look))
        (key-code :q)     (do (disposal)  (System/exit 0))
        true))
      (comment
        (condp = keycode
        Input$Keys/UP    (do (shift-cursor :UP)    (center-camera screen 1) )
        Input$Keys/DOWN  (do (shift-cursor :DOWN)  (center-camera screen 1) )
        Input$Keys/LEFT  (do (shift-cursor :LEFT)  (center-camera screen 1) )
        Input$Keys/RIGHT (do (shift-cursor :RIGHT) (center-camera screen 1) )
        Input$Keys/L     (do (center-camera) (reset! mode :act))
        Input$Keys/Q     (do (-dispose this)  (System/exit 0))
        true))
      nil))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [packed (TextureAtlas. (.internal ^Files Gdx/files "mono-packed2/pack.atlas"))
          trimmed (TextureAtlas. (.internal ^Files Gdx/files "trimmed-packed/pack.atlas"))
          healths (TextureAtlas. (.internal ^Files Gdx/files "health-stars/pack.atlas"))
          grab (fn ^TextureRegion [sprite] (.findRegion ^TextureAtlas packed sprite))
          grab-dimensions (fn [sprite] {:creature-height (.getRegionHeight (.findRegion ^TextureAtlas trimmed (str "all/" sprite)))
                                        :creature-start (- 64 (.getRegionHeight (.findRegion ^TextureAtlas trimmed (str "bottom/" sprite))))})
          _ (def health-pc  (mapv #(.findRegion ^TextureAtlas healths (str "health-pc/"  %)) (range 21))) ; :set-color 0.0 1.0 0.3 0.5
          _ (def health-mob (mapv #(.findRegion ^TextureAtlas healths (str "health-mob/" %)) (range 21))) ; :set-color 0.0 1.0 0.3 0.5
          _ (def health-red (.findRegion ^TextureAtlas healths "health-red"))
          _ (def s-batch (SpriteBatch.))
          _ (def monster-tiles (mapv grab monster-names))
          _ (def monster-tile-sizes (mapv grab-dimensions monster-names))
          __ (def nucky (grab "king"))
          __ (def nucky-sizes (grab-dimensions "king"))
          highlight-back  (grab "system-tile-back")
          highlight-front (grab "system-tile-front")
          sv (grab "cmap-wall-vertical")
          sh (grab "cmap-wall-horizontal")
          gr (grab "cmap-floor of a room")
          da (grab "cmap-dark part of a room")
          ul (grab "cmap-wall-bottom right corner")
          ur (grab "cmap-wall-bottom left corner")
          dl (grab "cmap-wall-top right corner")
          dr (grab "cmap-wall-top left corner")
          cw (grab "cmap-wall-crosswall")
          Tu (grab "cmap-wall-tee up")
          Td (grab "cmap-wall-tee down")
          Tl (grab "cmap-wall-tee left")
          Tr (grab "cmap-wall-tee right")
          stairs-up (grab "cmap-staircase up")
          stairs-down (grab "cmap-staircase down")
          water  (grab "cmap-water") ; ~
          ice    (grab "cmap-ice") ; ^
          stones (grab "object-venoms-splash of venom-blinding venom") ; &
          statue (grab "object-large stones-statue") ; $
          leaf   (grab "object-food-eucalyptus leaf") ; %
          ___ (def clean-bones (fn [] ;;^doubles dd ^chars shown
                        (let [ents (transient [])
                              dd ^doubles (:dungeon @dun)
                              shown ^chars (:shown @dun)
                              wide (long wide) high (long high)
                              wall (double wall) dark (double dark)]
                          (amap ^doubles dd i ret
                                (let [t (hiphip/aget ^doubles dd i)
                                      x (rem i wide)
                                      y (quot i wide)]
                                  (if (= t wall)
                                    (conj! ents
                                          (make-entity 
                                            (let [
                                                i (long i)
                                                left  (if (> (rem i wide) 0)
                                                        (and (not= (aget ^doubles dd (- i 1)) dark)
                                                             (= (aget ^doubles dd (- i 1)) wall))
                                                        false)
                                                right (if (< (rem i wide) (dec wide))
                                                        (and (not= (aget ^doubles dd (+ i 1)) dark)
                                                             (= (aget ^doubles dd (+ i 1)) wall))
                                                        false)
                                                top   (if (> (quot i wide) 0)
                                                        (and (not= (aget ^doubles dd (- i wide)) dark)
                                                             (= (aget ^doubles dd (- i wide)) wall))
                                                        false)
                                                down  (if (< (quot i wide) (dec high))
                                                        (and (not= (aget ^doubles dd (+ i wide)) dark)
                                                             (= (aget ^doubles dd (+ i wide)) wall))
                                                        false)
                                                
                                                downleft  (if (and (< (quot i wide) (dec high)) (> (rem i wide) 0))
                                                            (and (not= (aget ^doubles dd (dec (+ i wide))) dark)
                                                                 (= (aget ^doubles dd (dec (+ i wide))) wall))
                                                            false)
                                                downright (if (and (< (quot i wide) (dec high)) (< (rem i wide) (dec wide)))
                                                            (and (not= (aget ^doubles dd (inc (+ i wide))) dark)
                                                                 (= (aget ^doubles dd (inc (+ i wide))) wall))
                                                            false)
                                                upleft    (if (and (> (quot i wide) 0) (> (rem i wide) 0))
                                                            (and (not= (aget ^doubles dd (dec  (- i wide))) dark)
                                                                 (= (aget ^doubles dd (dec (- i wide))) wall))
                                                            false)
                                                upright   (if (and (> (quot i wide) 0) (< (rem i wide) (dec wide)))
                                                            (and (not= (aget ^doubles dd (inc (- i wide))) dark)
                                                                 (= (aget ^doubles dd (inc (- i wide))) wall))
                                                            false)]
                                            (cond
                                              left (cond
                                                     right (cond
                                                             top (if (not down)
                                                                   (if (and upleft upright) sh Tu)
                                                                   (cond
                                                                     (and upleft upright downleft downright) da
                                                                     (and (not upleft) upright downleft downright) ul
                                                                     (and upleft (not upright) downleft downright) ur
                                                                     (and upleft upright (not downleft) downright) dl
                                                                     (and upleft upright downleft (not downright)) dr
                                                                     
                                                                     (and (not upleft) (not upright) downleft downright) Tu
                                                                     (and upleft (not upright) downleft (not downright)) Tr
                                                                     (and upleft upright (not downleft) (not downright)) Td
                                                                     (and (not upleft) upright (not downleft) downright) Tl
                                                                     :else cw))
                                                             down (if (and downleft downright) sh Td)
                                                             :else sh)
                                                     top (if down (if (and upleft downleft) sv Tl) ul)
                                                     down dl
                                                     :else sh
                                                     )
                                              right (cond
                                                      top (if down (if (and downright upright) sv Tr) ur)
                                                      down dr
                                                      :else sh)
                                              (or top down) sv
                                              :else sh
                                              ))
                                            [0.8 0.8 0.8 1]
                                            i 2))
                                    (do (if (condp = t
                                            10001.0 1
                                            10002.0 1
                                            (condp = (aget ^chars shown i)
                                              \~ nil
                                              \^ nil
                                              \$ 1
                                              \% 1
                                              nil))
                                          (conj! ents (make-entity gr [0.8 0.8 0.8 1] i 0)))
                                      (conj! ents
                                          (make-entity (condp = t
                                            10001.0 stairs-up
                                            10002.0 stairs-down
                                            (condp = (aget ^chars shown i)
                                              \~ water
                                              \^ ice
                                              \$ statue
                                              \% leaf
                                              gr))
                                                       (condp = t
                                            10001.0 [1 1 0.5 1]
                                            10002.0 [0.5 1 1 1]
                                            (condp = (aget ^chars shown i)
                                              \~ [0 0.6 1 1]
                                              \^ [0.7 1 1 1]
                                              \$ [0.7 0.7 0.7 1]
                                              \% [0.5 0.8 0.3 1]
                                                 [0.8 0.8 0.8 1]))
                                          i
                                          (condp = t
                                            10001.0 1
                                            10002.0 1
                                            (condp = (aget ^chars shown i)
                                              \~ 0
                                              \^ 0
                                              \$ 1
                                              \% 1
                                              0))))
                                    ))
                                  0.0))
                          (persistent! ents))))
          ]
   (if (.exists (io/file "Savefile.edn"))
     (let [
         sav (read-string (slurp "Savefile.edn"))
         sav-wide (nth sav 0)
         sav-high (nth sav 1)
         ;dd0 (prepare-bones)
         _ (def shown (nth sav 3))
         monsters (mapv atom (nth sav 7))
         mon-hash (atom (into {} (map (fn [entry] [(:pos @entry) entry]) monsters)))]
         (def dd (nth sav 2))
         (reset! res (dungeon-resistances dd))
         (reset! cleared-levels (nth sav 4))
         (swap! cleared-levels #(into {} (map (fn [idx lvl] [idx (assoc lvl
                                                           :monsters (atom (mapv atom (:monsters lvl)))
                                                           :mon-hash (atom (into {} (map (fn [[k v]] [k (atom v)]) (:mon-hash lvl)))))])
                                      (range) %)))
         (reset! dlevel (nth sav 5))
         (reset! player (nth sav 6))
         
         (io/delete-file "Savefile.edn" true)
;;         (def dd dd1)
         
         (mapv #(swap! % assoc :label
                                   (condp = (:tile @%)
                                     0 "Dragon"
                                     1 "Tyrant"
                                     2 "Displacer"
                                     3 "Mi-Go"
                                     4 "Golem"
                                     5 "Winter Devil"
                                     6 "Ogre"
                                     7 "Shoggoth"
                                     8 "Scathe Seer"
                                     9 "Mist Wolf"
                                     10 "Discord"
                                     "The Kingpin")) monsters)
         (reset! dun {:dungeon dd :shown shown :monsters (atom monsters) :mon-hash mon-hash})
;;         (def colors (vec (repeatedly (* wide high) #(Color. (float 1.0) (float 1.0) (float 1.0) (float 1.0)))))
         
         (swap! player assoc :label "The Kingpin")
         (reset! map-ents (clean-bones))
         )
      (let [
         dd0 (prepare-bones)
         _   (def dd (first dd0))
         dungeon-res (dungeon-resistances dd)
         __  (def shown (last dd0))
         monsters (init-monsters dd)
         mon-hash (atom (into {} (map (fn [entry] [(:pos @entry) entry]) @monsters)))]
         (reset! res (dungeon-resistances dd))
;;         (def dd dd1)
         
         (mapv #(do
                  (swap! % assoc :sizes (nth monster-tile-sizes (:tile @%)))
                  (swap! % assoc :hue
                            (condp = (:tile @%)
                                     0  [(rand) (rand) (rand) 1]
                                     1  [1   0   1   1]
                                     2  [0.8 0   0.9 0.6]
                                     3  [0.7 0   0.9 0.7]
                                     4  [1   0.9 0.4 1]
                                     5  [0.5 0.9 1   1]
                                     6  [1   0.8 0.6 1]
                                     7  [0.4 1   0.9 0.7]
                                     8  [1   0.7 0.5 1]
                                     9  [0.9 0.5 0.3 1]
                                     10 [0.6 0.9 0.7 1]
                                     11 [0.9 0.8 0.1 1]
                                     12 [0.8 0.75 0.75 1]
                                     13 [0.9 0.2 0.9 1]
                                        [1   0.9 0.4 1]
                                     ))) @monsters)
         (reset! dun {:dungeon dd :shown shown :monsters monsters :mon-hash mon-hash})
;;         (def colors (vec (repeatedly (* wide high) #(Color. (float 1.0) (float 1.0) (float 1.0) (float 1.0)))))

         (place-creature dun player)
         (swap! player assoc :fov (run-fov-player player dun))
         (swap! player assoc :label "The Kingpin")
         ;(append-being player)
         (reset! map-ents (clean-bones))
         ))
      (update! screen :camera (orthographic) :renderer (stage))
      (update-fov dun);; (OrthographicCamera. (game :width) (game :height))
      (sort-entities (concat
                       entities
                       @map-ents
                       (make-player nucky (:pos @player) nucky-sizes)
                       (reduce into [] (mapv #(make-mob (monster-tiles (:tile @%)) (:hue @%) (:pos @%) (:sizes @%))
                             (filter #(> (double (aget ^floats (:fov @player) (:pos @%))) 0.0)
                                     @(:monsters @dun))))))))
  :on-render
  (fn [screen entities]
    (when @presses
      (swap! presses #(process-key % screen entities)))
    (clear! 0 0 0 1)
    ;(center-camera screen)
;    (y! screen (rand-int 200))
    ;(position! screen
    ;           (double (+ (* 32 (rem (long (:pos @player)) wide)) (* 16 (- wide (quot (long (:pos @player)) wide)))))
    ;           (- (double (game :height)) 64.0 (double (* 32 (quot (long (:pos @player)) wide)))))
               ;; (game :height)
    (->> (sort-entities (concat
                       @map-ents
                       (make-player nucky (:pos @player) nucky-sizes)
                       (reduce into [] (mapv #(make-mob (monster-tiles (:tile @%)) (:hue @%) (:pos @%) (:sizes @%))
                             (filter #(> (double (aget ^floats (:fov @player) (:pos @%))) 0.0)
                                     @(:monsters @dun))))))
      (render-proper! s-batch screen) (update-screen! screen)))
  :on-resize
  (fn [screen entities]
    (size! screen (game :width) (game :height))
    (->> (sort-entities (concat
                       @map-ents
                       (make-player nucky (:pos @player) nucky-sizes)
                       (reduce into [] (mapv #(make-mob (monster-tiles (:tile @%)) (:hue @%) (:pos @%) (:sizes @%))
                             (filter #(> (double (aget ^floats (:fov @player) (:pos @%))) 0.0)
                                     @(:monsters @dun))))))
      (update-screen! screen)))
  :on-key-down
  (fn [{:keys [keycode]} entities]
    (do (reset! presses keycode) true)
;    (move-monster @monsters dun monster-hash)
    entities)
  )

(defonce ravager
  (proxy [Game] []
    (create []
      (set-screen! this main-screen))
    (dispose []
      (disposal))
    ))
