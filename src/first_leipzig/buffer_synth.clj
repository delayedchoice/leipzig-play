(ns first-leipzig.buffer-synth
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}
                                 :invalid-arity {:level :off}}}}
  (:require 
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
;asWavetableNoWrap {
;  // Shaper requires wavetables without wrap.
;  // This method returns a wavetable in that format.
;  //To generate size N wavetable need N/2+1 signal values rather than N/2
;  //because Buffer's add_wchebyshev calculates N/2+1 values whilst
;  //Signal's _SignalAddChebyshev calculates N/2!
;  
;  var newsig = Signal.newClear((this.size-1)*2);
;  var next, cur;
;  
;  cur= this[0];
;  (this.size-1).do{|i|
;  var index= 2*i;
;  next= this[i+1];
;  
;  newsig[index]= 2*cur -next;
;  newsig[index+1]= next-cur;
;  cur=next;
;  
;  };
;  
;  ^newsig
;}
;waveFill { arg function, start = 0.0, end = 1.0;
;  var i = 0, step, size, val, x;
;  
;  // evaluate a function for every sample over the interval from
;  // start to end.
;  size = this.size;
;  if (size <= 0, { ^this });
;  
;  x = start;
;  step = (end - start) / size;
;  while ({ i < size }, {
;  	val = function.value(x, this.at(i), i);
;  	this.put(i, val);
;  	x = x + step;
;  	i = i + 1;
;  });
;  ^this
;}

(inst/definst buffer-synth 
  [
   buf (buffer/buffer 2048)
   freq 440
   amp 0.2
  ]
  (as-> freq #_(u/mouse-x 220 880 1) $
        (* amp (u/osc:ar buf $ ))))

(defn line-between [[y1 y2] num-intervals]
  (let [
        begin (min y1 y2)
        end   (max y1 y2)
        _ (println (str "begin: " begin " end: " end " num-intervals: " num-intervals))
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
;(plot/plot (create-curve-offsets 0.5 1024 ))

(defn curve-line [line-points curve-points curve]
  (let [op (if (pos? curve) + -)] 
    (map op line-points curve-points)))

(defn env->signal [size levels durations curves]
 (let [
       _ (println (str "size: " size " levels: " levels " durations: " durations " curves: " curves))
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

(defn random-synth [num-segments] 
  (signal->wavetable->buf 
    (env->signal 1024 
                (take num-segments (repeatedly #(- (rand 2) 1)) ) 
                (take (- num-segments 1) (repeatedly #(+ 1 (rand-int 10)))) 
                (take (- num-segments 1) (repeatedly #(- (rand 1) 0.5)) )
                )))

(def signal (env->signal 1024 [0 0.6 -0.9 0.3 0] [4 3 2 1] [-0.5 0.1 -0.2 0.2]))

(plot/plot (let [num (+ (rand-int 7) 3)] 
             (env->signal 1024 
                         (take num (repeatedly #(- (rand 2) 1)) ) 
                         (take (- num 1) (repeatedly #(+ 1 (rand-int 10)))) 
                         (take (- num 1) (repeatedly #(- (rand 2) 1)) )
                         )))

(plot/plot signal)
(comment 
  (sy/demo  5 (buffer-synth (signal->wavetable->buf signal 2048))))

(defn recreate-synth-and-play []
  (srv/stop)
  (sy/demo  5 (buffer-synth (random-synth 10) 2048 (+ (rand-int 440) 220)))
)
(comment 
  (recreate-synth-and-play)
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
