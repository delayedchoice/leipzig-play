(ns first-leipzig.song
  (:require [overtone.live :refer :all]
            [overtone.sc.machinery.server.connection :as conn]
            [leipzig.melody :as melody]
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

(def aea [{:root 0
           :g-clef j}])

;(defn bassline [{:root root {:shape shape :duration duration} }]
;  (->> (melody/phrase (cycle [1 1/2 1/2 1 1]) [0 -3 -1 0 2 0 2 3 2 0])
;       (melody/where :pitch (scale/from root))
;       (melody/where :pitch (comp scale/lower scale/lower))
;       (melody/all :part :bass)))
(defn bassline [{root :root {shape :shape duration :duration} :f-clef }]
  (println-str root " " shape " " duration)
)

(defn accompaniment [root]
  (->>
    (melody/phrase [8] [(-> chord/seventh (chord/root root))])
    (melody/all :part :accompaniment)))

; Track
(def track
  (->>
    (melody/mapthen bassline progression)
    (melody/with (melody/mapthen accompaniment progression))
    (melody/where :pitch (comp temperament/equal scale/A scale/minor))
    (melody/tempo (melody/bpm 90))))

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
(def fifth-and-octave {:i 0, :v 4, :xii 11})
(def third-and-octave {:i 0, :iii 3, :xii 11})
(def fifth-and-second  {:i 4, :ii 1})
(def root {:i 0})
(def second-inversion (chord/inversion chord/triad 2) )
(def first-inversion (chord/inversion chord/triad 1) )
(def second-inversion-augmented-fourth (-> (chord/inversion chord/triad 1) (chord/augment :iii 1)) )
;(melody/where :pitch (comp melody/utter scale/A scale/major ) (melody/utter first-inversion))

(def an-ending [
     {:root 0 :g-clef {:shape second-inversion                  :quavers 2} :f-clef {:shape fifth-and-octave :quavers 2} }
     {:root 4 :g-clef {:shape fifth-and-octave                  :quavers 2} :f-clef {:shape chord/triad      :quavers 2} }
     {:root 0 :g-clef {:shape :rest                             :quavers 2} :f-clef {:shape :rest            :quavers 2}}
     {:root 3 :g-clef {:shape first-inversion                   :quavers 2} :f-clef {:shape fifth-and-octave :quavers 2} }
     {:root 0 :g-clef {:shape second-inversion                  :quavers 2} :f-clef {:shape fifth-and-octave :quavers 2} }
     {:root 4 :g-clef {:shape chord/triad                       :quavers 2} :f-clef {:shape chord/triad      :quavers 2} }
     {:root 0 :g-clef {:shape :rest                             :quavers 2} :f-clef {:shape :rest            :quavers 2}}
     {:root 3 :g-clef {:shape fifth-and-octave                  :quavers 2} :f-clef {:shape second-inversion :quavers 2} }
     {:root 0 :g-clef {:shape second-inversion                  :quavers 2} :f-clef {:shape fifth-and-octave :quavers 2} }
     {:root 4 :g-clef {:shape fifth-and-octave                  :quavers 2} :f-clef {:shape chord/triad      :quavers 2} }
     {:root 0 :g-clef {:shape :rest                             :quavers 2} :f-clef {:shape :rest            :quavers 2}}
     {:root 3 :g-clef {:shape second-inversion-augmented-fourth :quavers 2} :f-clef {:shape fifth-and-octave :quavers 2} }
     {:root 0 :g-clef {:shape third-and-octave                  :quavers 2} :f-clef {:shape fifth-and-octave :quavers 2} }
     {:root 4 :g-clef {:shape fifth-and-second                  :quavers 1} :f-clef {:shape chord/triad      :quavers 2} }
     {:root 4 :g-clef {:shape root                              :quavers 1} :f-clef {:shape :rest            :quavers 1} }
     ])

(defn bassline [{root :root {shape :shape duration :quavers} :f-clef }]
  (println-str "root: " root " shape: " shape " duration: " duration)
)
(bassline (first an-ending))
(println an-ending)

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
