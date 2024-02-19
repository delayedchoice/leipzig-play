(ns first-leipzig.plot
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}
                                 :invalid-arity {:level :off}}}}
  (:require [oz.core :as oz]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import   [java.io BufferedReader StringReader]))

(defn plot [data]
  (let [formatted-data (for [y (map-indexed #(vector %1 %2) data)]
                          {:time (first y) :amplitude (second y)})
        graph-data {:data {:values formatted-data}
                    :encoding {:x {:field "time" :type "quantitative"}
                               :y {:field "amplitude" :type "quantitative"}
                              ; :color {:field "item" :type "nominal"}
                               }
                    :mark "line"}]
    (oz/view! graph-data)))

(comment
  (plot [1 2 3 4 5]))

(comment
  (defn run [cmd args] 
   (let [output-as-lines-of-strings  (-> (shell/sh cmd args)
                                         :out
                                         StringReader.
                                         BufferedReader.
                                         line-seq )

         output-as-fields (map #(-> (str/split %1 #"\s+") rest) output-as-lines-of-strings)
         ]
     output-as-fields))

 (defn end-process [pid]
   (let [_ (print (str "killing: " pid))] 
     (shell/sh "kill" "-9" pid)))

 (defn get-processes-by-name [name]
   (let [procs (run "ps" "-ef")] 
     (filter #(str/includes? (last %1) name) procs)))

 (defn end-processes-by-name [name]
   (doseq [p (get-processes-by-name name)]
     (let [_ (print (str "killing: " (second p)))] 
       (end-process (second p)))))
 (let [procs (run "ps" "-ef")] 
   (filter #(str/includes? (last %1) "tail") procs))
 (end-processes-by-name "tail")
 (def processes (run "ps" "-ef"))

 (filter #(str/includes? (last %1) "System") processes)
 (count ps)
 (println (last ps))
 #_(str/split (-> (shell/sh "ps" "-ef")
                  :out
                  StringReader.
                  BufferedReader.
                  line-seq )))
