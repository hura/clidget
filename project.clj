(defproject jarohen/clidget "0.2.0"
  :description "An ultra-lightweight ClojureScript state utility"
  :url "https://github.com/james-henderson/clidget"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]]

  :plugins [[com.keminglabs/cljx "0.3.2"]]

  :source-paths ["src/clojure" "target/generated/clj" "target/generated/cljs"]
  
  :hooks [cljx.hooks]

  :cljx {:builds [{:source-paths ["src"]
                   :output-path "target/generated/clj"
                   :rules :clj}

                  {:source-paths ["src"]
                   :output-path "target/generated/cljs"
                   :rules :cljs}]})
