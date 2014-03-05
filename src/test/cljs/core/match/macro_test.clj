(ns cljs.core.match.macro-test)

(defmacro test1 []
  `(fn [& args#]
     (cljs.core.match.macros/match [1 2 3] [1 2 3] :match)))
