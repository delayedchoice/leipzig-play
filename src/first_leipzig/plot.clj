(ns first-leipzig.plot
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}
                                 :invalid-arity {:level :off}}}}
  (:require [oz.core :as oz]))

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



