(defproject imaging "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  ;; :repositories [["org.slf4j" "http://mvnrepository.com/artifact/org.slf4j"]]
  :dependencies [[org.clojure/clojure    "1.7.0"]
                 [net.mikera/core.matrix "0.34.0"]
                 [clatrix "0.5.0"]
                 [net.mikera/vectorz     "0.47.1-SNAPSHOT"]
                 [net.mikera/vectorz-clj "0.29.0"]
                 [org.slf4j/slf4j-api    "1.7.12"]]
  :jvm-opts ^:replace ["-server" "-XX:+UseCompressedOops" "-XX:+AggressiveOpts"
                       "-XX:+UnlockCommercialFeatures"
                       "-XX:+FlightRecorder"]
  :resource-paths ["lib/ij.jar"
                   "lib/dcm4che-core-3.3.7.jar"]
  :main imaging.core)
