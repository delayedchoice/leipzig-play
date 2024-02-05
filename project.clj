(defproject first-leipzig "0.1.0-SNAPSHOT"
  :main ^{:skip-aot true} first-leipzig.song
  :jvm-opts ^:replace []
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [overtone "0.13.3177"]
                 [leipzig "0.10.0"]])
