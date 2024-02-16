(ns first-leipzig.an-ending-synth
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
            ;[overtone.algo.scaling :as scale]
            [overtone.core]
            [overtone.sc.cgens.mix :as mix]
            [overtone.sc.cgens.line :as line]
            [leipzig.melody :as melody]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [first-leipzig.song :as app]
            [kixi.stats.distribution :as stats]
            [oz.core :as oz]
            [overtone.studio.wavetable :as wave]))

; Instruments
(inst/definst organ [freq 440 dur 1 volume 1]
  (-> (osc/square freq)
      (* (u/env-gen (envel/adsr 0.01 0.8 0.1) (u/line:kr 1 0 dur) :action u/FREE))
      (* 1/10 volume)))

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
  ]
  (as-> (u/mouse-x 10 10000 1) $
        (* 0.2 (u/osc:ar buf $ ))))

(defn exprange [min max]
  (let [
        avg (/ (+ min max) 2)
        rate (/ avg)
        qualify #(and (> % min) (< % max))
        ]
   (filter qualify (stats/exponential {:rate rate}))))

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


(defn env->signal [size levels durations]
 (let [
       total-time-span (apply + durations)
       endpoints (map #(vector %1 %2) levels (rest levels))
       num-points-per-interval (map #(int (/ (* %1 size) total-time-span)) durations)
       line-segments (map line-between endpoints num-points-per-interval)
       ]
   (flatten line-segments)) )
(def signal (env->signal 1024 [0 0.6 -0.9 0.3 0] [4 3 2 1] ))
(def wt (wave/signal->wavetable signal)) ;fix so env-signal only gives size
(def buf (buffer/buffer 2048))
(buffer/buffer-write! buf wt)
(sy/demo  5 (buffer-synth buf))

;graphing stuff
(comment 
  (def plot-data (for [y (map-indexed #(vector %1 %2) (env->signal 1024 [0 0.6 -0.9 0.3 0] [4 3 2 1] ))]
                  {:time (first y) :item "amplitude" :quantity (second y)}))
 (def line-plot
   {:data {:values plot-data}
    :encoding {:x {:field "time" :type "quantitative"}
               :y {:field "quantity" :type "quantitative"}
               :color {:field "item" :type "nominal"}}
    :mark "line"})

 (oz/view! line-plot)
)

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
                 (/ 1 (*  sines )))))

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
