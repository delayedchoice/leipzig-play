(defproject first-leipzig "0.1.0-SNAPSHOT"
  :main ^{:skip-aot true} first-leipzig.song
  :jvm-opts ^:replace []
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [overtone "0.13.3177"]
                 [kixi/stats "0.5.5"]
                 [leipzig "0.10.0"]
                 [metasoarous/oz "2.0.0-alpha5"]])
