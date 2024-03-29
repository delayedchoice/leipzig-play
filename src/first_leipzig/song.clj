(ns first-leipzig.song
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}
                                 :invalid-arity {:level :off}}}}
  (:require ;[overtone.live :as live]
            [overtone.sc.machinery.server.connection :as conn]
            [overtone.sc.ugens :as u]
            [overtone.sc.server :as srv]
            [overtone.sc.synth :as sy]
            [overtone.sc.cgens.oscillators :as osc]
            [overtone.sc.envelope :as envel]
            [overtone.studio.inst :as inst]
            [overtone.repl.ugens :as ru]
            ;[overtone.algo.scaling :as scale]
            [overtone.core]
            [overtone.sc.cgens.mix :as mix]
            [overtone.sc.cgens.line :as line]
            [leipzig.melody :as melody]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]))

; Instruments
(inst/definst organ [freq 440 dur 1 volume 1.0]
  (-> (osc/square freq)
      (* (u/env-gen (envel/adsr 0.01 0.8 0.1) (u/line:kr 1 0 dur) :action u/FREE))
      (* 1/10 volume)))

(comment 
  (meta #'overtone.core/signal->wavetable)
  (ru/odoc mix/mix)
 )

(inst/definst cs80lead
  [freq 880
   dur 6
   amp 0.5
   att 0.75
   decay 10
   sus 0.8
   rel 8.0
   fatt 0.75
   fdecay 0.5
   fsus 0.8
   frel 1.0
   cutoff 200
   dtune 0.002
   vibrate 4
   vibdepth 0.015
   gate 1
   ratio 1
   cbus 1
   freq-lag 0.1]
  (let [freq (u/lag freq freq-lag)
        cuttoff (u/in:kr cbus)
        env     (u/env-gen (envel/adsr att decay sus rel) (u/line:kr 1 0 dur) :action u/FREE)
        fenv    (u/env-gen (envel/adsr fatt fdecay fsus frel 2) gate)

        vib     (+ 1 (line/lin-lin:kr (u/sin-osc:kr vibrate) -1 1 (- vibdepth) vibdepth))

        freq    (* freq vib)
        sig     (* env amp (u/saw [freq (* freq (+ dtune 1))]))]
    sig))

(def major-third    (float 81/64))
(def perfect-fifth  (float 3/2))
(def major-sixth    (float 27/16))


(inst/definst mysynth [
                       freq 220
                       sines 5
                       speed 2
                       att 0.95
                       decay 1 
                       sus 0.8
                       rel 0.01 
                       ]
  (let [sig (* (mix/mix
             (concat (map #(u/pan2 (* (* 0.1 (u/sin-osc (* % freq))) 
                                      (u/lf-noise1:kr speed))
                                   (- (clojure.core/rand 2) 1))
                          (range 5))
                     (map #(u/pan2 
                             (* (* 0.05 (u/sin-osc (* % (* 2 freq major-sixth))))
                                (u/lf-noise1:kr speed) )
                             (- (clojure.core/rand 2) 1))
                          (range 5))
                     (map #(u/pan2 
                             (* (* 0.05 (u/sin-osc (* % (* 2 freq major-third))))
                                (u/lf-noise1:kr speed) )
                             (- (clojure.core/rand 2) 1))
                          (range 5))
                     (map #(u/pan2 
                             (* (* 0.05 (u/sin-osc (* % (* 2 freq perfect-fifth))))
                                (u/lf-noise1:kr speed) )
                             (- (clojure.core/rand 2) 1))
                          (range 5))))
           (/ 1 (*  5 )))]
    (* 20 sig (u/env-gen (envel/adsr att decay sus rel) (u/line:kr 1 0 2) :action u/FREE))))

; Arrangement
;(defmethod live/play-note :arrangement [{hertz :pitch seconds :duration}] (organ hertz seconds))
(defmethod live/play-note :arrangement [{hertz :pitch }] (mysynth hertz ))

(def fifth-and-octave {:i 0, :v 4, :xii 11})
(def fifth-and-octave-lower {:i -11, :v -7 , :xii 0})
(def third-and-octave {:i 0, :iii 2, :xii 11})
(def fifth-and-second  {:v 4, :ii 8})
(def root {:i 0})
(def second-inversion (chord/inversion chord/triad 2) )
(def first-inversion (chord/inversion chord/triad 1) )
(def second-inversion-augmented-fourth (-> second-inversion (chord/augment :iii 1)) )

(def fourth-and-sixth {:iv 3, :vi 12 }) ;c
(def octave-sixth {:vi 12 }) ;c
(def octave-third {:iii 9 }) ;g
(def first-and-octave-third {:i 0, :iii 9 }) ;g
(def fifth-and-octave-d {:v 4, :v2 11 }) ;d
(def octave-fifth {:v 11 }) ;d
(def fourth-and-octave-c {:iv 3, :iv2 10 }) ;c
(def octave-fourth {:iv 10 }) ;c
(def octave-second  {:ii 8})
(def octave-seventh  {:xiii 13})
(def two-octave-first  {:xiv 14})
(def first {:i 0})
(def third {:iii 2 })
(def second {:ii 1 })
(def first {:ii 0 })
(def seventh {:vii 6})
(def fourth-and-seventh {:iv 3, :vii 13 }) ;c -diminished
(def first-and-octave {:vi 1, :vii 14 }) ;g
(def fifth-and-seventh {:iv 4, :vii 13 }) ;d
(def fifth-and-first {:iv 4, :vii 8 }) ;g
(def seventh {:vii 6})

;(melody/where :pitch (comp melody/utter scale/A scale/major ) (melody/utter first-inversion))
(def an-ending-stops [
     {:root 0  :f-clef {:shape octave-sixth :quavers 2} }
     {:root 0  :f-clef {:shape octave-third :quavers 2} }
     {:root 0  :f-clef {:shape octave-fifth :quavers 2} }
     {:root 0  :f-clef {:shape nil            :quavers 2}}
     {:root 0  :f-clef {:shape octave-fourth :quavers 2} }
     {:root 0  :f-clef {:shape octave-third :quavers 2} }
     {:root 0  :f-clef {:shape octave-second      :quavers 2} }
     {:root 0  :f-clef {:shape nil            :quavers 2}}
     {:root 0  :f-clef {:shape octave-sixth :quavers 2} }
     {:root 0  :f-clef {:shape octave-third :quavers 2} }
     {:root 0  :f-clef {:shape octave-fifth :quavers 2} }
     {:root 0  :f-clef {:shape octave-seventh      :quavers 2}}
     {:root 0  :f-clef {:shape two-octave-first      :quavers 2}}
     {:root 0  :f-clef {:shape octave-seventh      :quavers 1}}
     {:root 0  :f-clef {:shape octave-sixth            :quavers 1}}
     {:root 0  :f-clef {:shape octave-fifth            :quavers 4}}
     ;{:root 0  :f-clef {:shape nil            :quavers 4}}
     ;{:root 0  :f-clef {:shape third            :quavers 1}}
     ;{:root 0  :f-clef {:shape second            :quavers 3}}
     ;{:root 0  :f-clef {:shape first            :quavers 4}}
     ;{:root 0  :f-clef {:shape nil            :quavers 2}}
     ;{:root 0  :f-clef {:shape first            :quavers 1}}
     ;{:root 0  :f-clef {:shape second            :quavers 1}}

;     {:root 3  :f-clef {:shape second-inversion :quavers 2} }
;     {:root 0  :f-clef {:shape fifth-and-octave :quavers 2} }
;     {:root 4  :f-clef {:shape chord/triad      :quavers 2} }
;     {:root 0  :f-clef {:shape nil            :quavers 2}}
;     {:root 3  :f-clef {:shape fifth-and-octave :quavers 2} }
;     {:root 0  :f-clef {:shape fifth-and-octave :quavers 2} }
;     {:root 4  :f-clef {:shape chord/triad      :quavers 2} }
;     {:root 4  :f-clef {:shape nil            :quavers 1} }
     ])


(def an-ending [
     {:root 3 :g-clef {:shape fifth-and-octave                  :quavers 2} :f-clef {:shape second-inversion :quavers 2} }
     {:root 0 :g-clef {:shape second-inversion                  :quavers 2} :f-clef {:shape fifth-and-octave :quavers 2} }
     {:root 4 :g-clef {:shape fifth-and-octave-lower                  :quavers 2} :f-clef {:shape chord/triad      :quavers 2} }
     {:root 0 :g-clef {:shape nil                             :quavers 2} :f-clef {:shape nil            :quavers 2}}
     {:root 3 :g-clef {:shape first-inversion                   :quavers 2} :f-clef {:shape fifth-and-octave :quavers 2} }
     {:root 0 :g-clef {:shape second-inversion                  :quavers 2} :f-clef {:shape fifth-and-octave :quavers 2} }
     {:root 4 :g-clef {:shape chord/triad                       :quavers 2} :f-clef {:shape chord/triad      :quavers 2} }
     {:root 0 :g-clef {:shape nil                             :quavers 2} :f-clef {:shape nil            :quavers 2}}
     {:root 3 :g-clef {:shape fifth-and-octave                  :quavers 2} :f-clef {:shape second-inversion :quavers 2} }
     {:root 0 :g-clef {:shape second-inversion                  :quavers 2} :f-clef {:shape fifth-and-octave :quavers 2} }
     {:root 4 :g-clef {:shape fifth-and-octave-lower                  :quavers 2} :f-clef {:shape chord/triad      :quavers 2} }
     {:root 0 :g-clef {:shape nil                             :quavers 2} :f-clef {:shape nil            :quavers 2}}
     {:root 3 :g-clef {:shape second-inversion-augmented-fourth :quavers 2} :f-clef {:shape fifth-and-octave :quavers 2} }
     {:root 0 :g-clef {:shape third-and-octave                  :quavers 2} :f-clef {:shape fifth-and-octave :quavers 2} }
     {:root 4 :g-clef {:shape fifth-and-second                  :quavers 1} :f-clef {:shape chord/triad      :quavers 2} }
     {:root 4 :g-clef {:shape root                              :quavers 1} :f-clef {:shape nil            :quavers 1} }
     ])

(defn translate [thing]
  (let [
        {
         root :root 
         {f-shape :shape f-duration :quavers} :f-clef 
         ;{g-shape :shape g-duration :quavers} :g-clef
         
         } thing 
        ] 
    (let [f-notes (->> (melody/phrase [f-duration] [f-shape])
                       (melody/where :pitch (scale/from root) )
                       (melody/where :pitch (comp scale/lower scale/lower))
                       (melody/all :part :arrangement))

                 ;g-notes (->> (melody/phrase [g-duration] [g-shape])
                 ;             (melody/where :pitch (scale/from root) )
                 ;             (melody/all :part :arrangement))
                 ]
     (melody/with f-notes)
     )))
(melody/mapthen translate [ {:root 0 :g-clef {:shape nil :quavers 2} :f-clef {:shape nil :quavers 2}} ])

(def track
  (->>
    (melody/mapthen translate an-ending-stops)
    (melody/where :pitch (comp temperament/equal scale/A scale/major))
    (melody/tempo (melody/bpm 60))))

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

(defn reset-server-connection []
 (conn/shutdown-server)
 (conn/connect "127.0.0.1" 57110)
)

(comment 
  (conn/shutdown-server))
