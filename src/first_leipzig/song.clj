(ns first-leipzig.song
  (:require [overtone.live :refer :all]
            [overtone.sc.machinery.server.connection :as conn]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]))

; Instruments
(definst bass [freq 110 volume 1.0]
  (-> (saw freq)
      (* (env-gen (perc 0.1 0.4) :action FREE))
      (* volume)))

(definst organ [freq 440 dur 1 volume 1.0]
  (-> (square freq)
      (* (env-gen (adsr 0.01 0.8 0.1) (line:kr 1 0 dur) :action FREE))
      (* 1/10 volume)))

; Arrangement
(defmethod live/play-note :bass [{hertz :pitch}] (bass hertz))
(defmethod live/play-note :accompaniment [{hertz :pitch seconds :duration}] (organ hertz seconds))

; Composition
(def progression [0 4, 3 0 4 0 ])

(defn bassline [root]
  (->> (phrase (cycle [1 1/2 1/2 1 1]) [0 -3 -1 0 2 0 2 3 2 0])
       (where :pitch (scale/from root))
       (where :pitch (comp scale/lower scale/lower))
       (all :part :bass)))

(defn accompaniment [root]
  (->>
    (phrase [8] [(-> chord/seventh (chord/root root))])
    (all :part :accompaniment)))

; Track
(def track
  (->>
    (mapthen bassline progression)
    (with (mapthen accompaniment progression))
    (where :pitch (comp temperament/equal scale/A scale/minor))
    (tempo (bpm 90))))

(defn -main []
  (live/play track))

(comment
  ; Loop the track, allowing live editing.
  (live/jam (var track))
)
(comment
  ; Loop the track, allowing live editing.
  (live/stop)
)
;(chord/inversion chord/triad)

;(all :part :bass (bassline 0))
(def fifth-and-octave
  {:i 0, :v 4, :xii 11})

(def second-inversion (chord/inversion chord/triad 2) )
(def first-inversion (chord/inversion chord/triad 1) )
(where :pitch (comp scale/A scale/major) first-inversion)


(bassline 0)
(defn bassclef [root]
  (->> (where :pitch scale/lower (phrase (cycle [2]) [fifth-and-octave chord/triad]) )
       (where :pitch (scale/from root))
       (where :pitch (comp scale/lower scale/lower))
       (all :part :bass)))

;
(take 10 (->> 
           (phrase [4] [[fifth-and-octave  ]])
           (all :part :accompaniment)
           (where :pitch (comp temperament/equal scale/A scale/major) [{:pitch 60}])
           (tempo (bpm 90)) 
           ;(live/play)
         )
      )

(comment 
  (conn/shutdown-server)
  (conn/conect "127.0.0.1" 57110)
  k)
