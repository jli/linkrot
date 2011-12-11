(defproject linkrot "1.0.0-SNAPSHOT"
  :description "Check a site for linkrot."
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/tools.cli "0.2.1"]
                 [clj-http "0.2.5"]
                 [enlive "1.0.0-SNAPSHOT"]]
  :main linkrot.core)
