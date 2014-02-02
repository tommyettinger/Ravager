(ns ravager.core.desktop-launcher
  (:require [gaunt.ravager.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))
(set! *warn-on-reflection* true)
(defn -main
  []
  (LwjglApplication. ravager "Ravager" 1200 640 true)
  (Keyboard/enableRepeatEvents true))

