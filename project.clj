(defproject learn-macro "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [lein-light-nrepl "0.0.10"]]
  :repl-options {:nrepl-middleware [lighttable.nrepl.handler/lighttable-ops]})
