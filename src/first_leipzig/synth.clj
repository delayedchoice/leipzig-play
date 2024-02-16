(ns first-leipzig.synth
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}
                                 :invalid-arity {:level :off}}}}
  (:require ;[overtone.live :as live]
            [overtone.sc.machinery.server.connection :as conn]
            [overtone.sc.ugens :as u]
            [overtone.sc.buffer :as buffer]
            [overtone.sc.server :as srv]
            [overtone.sc.synth :as sy]
            [overtone.sc.cgens.oscillators :as osc]
            [overtone.sc.envelope :as envel]
            [overtone.studio.inst :as inst]
            [overtone.repl.ugens :as ru]
            [overtone.core]
            [overtone.sc.cgens.mix :as mix]
            [first-leipzig.song :as app]
            [kixi.stats.distribution :as stats]
            [first-leipzig.plot :as plot]
            [overtone.studio.wavetable :as wave]))

; Instruments
(defn exprange [min max]
  (let [
        avg (/ (+ min max) 2)
        rate (/ avg)
        qualify #(and (> % min) (< % max))
        ]
   (filter qualify (stats/exponential {:rate rate}))))

(defn zero-or-exp [min max]
  (let [t-or-f (stats/bernoulli {:p 0.5})
        amps (exprange min max)]
  (map #(if %1 0 %2) t-or-f amps)))

(inst/definst buffer-synth 
  [
   buf (buffer/buffer 2048)
   amp 0.2
  ]
  (as-> (u/mouse-x 10 10000 1) $
        (* amp (u/osc:ar buf $ ))))

(defn restart-simple-wavtable []
  (let [buf (buffer/buffer 2048)
        amplist [1 (float 1/4) (float 1/6) (float 1/2)] ;(cons 1 (take 31 (zero-or-exp 0.1 0.9)))
        _ (buffer/buffer-wave-fill! buf "sine1" 6 amplist)
        ]
        (srv/stop)
        (sy/demo (buffer-synth :freq 220 :buf buf))))

;wavetable synth
(defn line-between [[y1 y2] num-intervals]
  (let [
        begin (min y1 y2)
        end   (max y1 y2)
        points (range begin end (/ (- end begin) num-intervals)) 
        ]
    (if (> y2 y1)
        points 
        (reverse points))))

(defn create-curve-offsets [curve size]
  (let [
        ;end   (/ size 2 )
        ;begin (* -1 end)
        step-size (/ size)
        a (- (abs curve))
        c (abs curve)
        ] 
    (for [x (range -1 1 (* 2 step-size))]
     (+ (* x x a) c))))
 (plot/plot (create-curve-offsets 0.5 1024 ))

(defn curve-line [line-points curve-points curve]
  (let [op (if (pos? curve) - +)] 
    (map op line-points curve-points)))

(defn env->signal [size levels durations curves]
 (let [
       total-time-span (apply + durations)
       endpoints (map #(vector %1 %2) levels (rest levels))
       num-points-per-segment (map #(int (/ (* %1 size) total-time-span)) durations)
       line-segments (map line-between endpoints num-points-per-segment)
       curve-segements (map create-curve-offsets curves num-points-per-segment)
       segments (map curve-line line-segments curve-segements curves)
       ]
   (flatten segments)) )

(defn signal->wavetable->buf [signal size]
  (let [wt (wave/signal->wavetable signal)
        buf (buffer/buffer size) 
        _ (buffer/buffer-write! buf wt)        
        ]
    buf))

(def signal (env->signal 1024 [0 0.6 -0.9 0.3 0] [4 3 2 1] [-0.5 0.1 -0.2 0.2]))

(plot/plot signal)
(comment 
  (sy/demo  5 (buffer-synth (signal->wavetable->buf signal 2048))))


(comment 
  (def *major-third*    (float 81/64))
 (def *perfect-fifth*  (float 3/2))
 (def *major-sixth*    (float 27/16))
 ;additive synth
 (sy/demo 5 (let [sines 5
                  speed 2
                  freq 220]
              (* (mix/mix
                   (concat (map #(u/pan2 (* (* 1 (u/sin-osc (* % freq))) 
                                            (u/lf-noise1:kr speed))
                                         (- (clojure.core/rand 2) 1))
                                (range sines))
                           (map #(u/pan2 
                                   (* (* 0.05 (u/sin-osc (* % (* 2 freq *major-sixth*))))
                                      (u/lf-noise1:kr speed) )
                                   (- (clojure.core/rand 2) 1))
                                (range sines))
                           (map #(u/pan2 
                                   (* (* 0.05 (u/sin-osc (* % (* 2 freq *major-third*))))
                                      (u/lf-noise1:kr speed) )
                                   (- (clojure.core/rand 2) 1))
                                (range sines))
                           (map #(u/pan2 
                                   (* (* 0.05 (u/sin-osc (* % (* 2 freq *perfect-fifth*))))
                                      (u/lf-noise1:kr speed) )
                                   (- (clojure.core/rand 2) 1))
                                (range sines))))
                 (/ 1 (*  sines ))))))

(comment 
  (restart-simple-wavtable)
)

(comment 
  (meta #'overtone.core/osc)
  (ru/odoc mix/mix)
  (sy/demo (organ))
  (sy/demo (buffer-synth))
 )

(comment 
  (srv/stop)
  (app/reset-server-connection)
)
