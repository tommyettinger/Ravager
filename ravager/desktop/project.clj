(defproject ravager "0.0.1"
  :description "A Tactical RPG and/or Roguelike"
  :url "https://github.com/tommyettinger/ravager"
  :license {:name "GPL License v2"
            :url "http://opensource.org/licenses/GPL-2.0"}
  :dependencies [[com.badlogicgames.gdx/gdx "0.9.9"]
                 [com.badlogicgames.gdx/gdx-backend-lwjgl "0.9.9"]
                 [com.badlogicgames.gdx/gdx-platform "0.9.9"
                  :classifier "natives-desktop"]
                 [org.clojure/clojure "1.5.1"]
                 [hiphip-aot "0.1.2"]
                 [com.squid/cuttlebone "1.95.2"]
                 [play-clj "0.1.0"]
;                 [primitive-math "0.1.3"]
                 [com.taoensso/timbre "3.0.0"]]
  :repositories [["sonatype"
                  "https://oss.sonatype.org/content/repositories/snapshots/"]]

  :jvm-opts ^:replace ["-Xshare:off" "-server"]
  :source-paths ["src" "src-common"]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :uberjar-name "App.jar"
  :aot [ravager.herringbone ravager.logic ravager.core ravager.core.desktop-launcher]
  :main ravager.core.desktop-launcher)

