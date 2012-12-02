{:namespaces
 ({:source-url
   "https://github.com/clojure/core.match/blob/cbcc6e5fa070a7025e72f5eab4e83eaec100332b/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match-api.html",
   :name "clojure.core.match",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.match/blob/ce66e9385bdfed0d8572fe706c950ecdb3d748c7/src/main/clojure/clojure/core/match/java.clj",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match.java-api.html",
   :name "clojure.core.match.java",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.match/blob/17075bb1cd0cbdab080fbc98691080a5f07f88fc/src/main/clojure/clojure/core/match/regex.clj",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match.regex-api.html",
   :name "clojure.core.match.regex",
   :doc nil}),
 :vars
 ({:name "*backtrack-with-errors*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/cbcc6e5fa070a7025e72f5eab4e83eaec100332b/src/main/clojure/clojure/core/match.clj#L58",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/cbcc6e5fa070a7025e72f5eab4e83eaec100332b/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*backtrack-with-errors*",
   :doc "Enable backtracking diagnostics",
   :var-type "var",
   :line 58,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:name "*breadcrumbs*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/cbcc6e5fa070a7025e72f5eab4e83eaec100332b/src/main/clojure/clojure/core/match.clj#L50",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/cbcc6e5fa070a7025e72f5eab4e83eaec100332b/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*breadcrumbs*",
   :doc "Enable breadcrumb diagnostics with fail nodes",
   :var-type "var",
   :line 50,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:name "*syntax-check*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/cbcc6e5fa070a7025e72f5eab4e83eaec100332b/src/main/clojure/clojure/core/match.clj#L46",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/cbcc6e5fa070a7025e72f5eab4e83eaec100332b/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*syntax-check*",
   :doc "Enable syntax check of match macros",
   :var-type "var",
   :line 46,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:name "*trace*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/cbcc6e5fa070a7025e72f5eab4e83eaec100332b/src/main/clojure/clojure/core/match.clj#L54",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/cbcc6e5fa070a7025e72f5eab4e83eaec100332b/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*trace*",
   :doc "Enable pattern compile time tracing",
   :var-type "var",
   :line 54,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([vars & clauses]),
   :name "match",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/cbcc6e5fa070a7025e72f5eab4e83eaec100332b/src/main/clojure/clojure/core/match.clj#L1740",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/cbcc6e5fa070a7025e72f5eab4e83eaec100332b/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/match",
   :doc
   "Pattern match a row of occurrences. Take a vector of occurrences, vars.\nClause question-answer syntax is like `cond`. Questions must be\nwrapped in a vector, with same arity as vars. Last question can be :else,\nwhich expands to a row of wildcards.\n\nExample:\n(let [x 1\n      y 2]\n    (match [x y 3]\n           [1 2 3] :answer1\n           :else :default-answer))",
   :var-type "macro",
   :line 1740,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/BindNode",
   :namespace "clojure.core.match",
   :var-type "record",
   :name "BindNode"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/FailNode",
   :namespace "clojure.core.match",
   :var-type "record",
   :name "FailNode"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/GuardPattern",
   :namespace "clojure.core.match",
   :var-type "type",
   :name "GuardPattern"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/LeafNode",
   :namespace "clojure.core.match",
   :var-type "record",
   :name "LeafNode"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/LiteralPattern",
   :namespace "clojure.core.match",
   :var-type "type",
   :name "LiteralPattern"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/MapPattern",
   :namespace "clojure.core.match",
   :var-type "type",
   :name "MapPattern"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/OrPattern",
   :namespace "clojure.core.match",
   :var-type "type",
   :name "OrPattern"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/PatternMatrix",
   :namespace "clojure.core.match",
   :var-type "type",
   :name "PatternMatrix"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/PatternRow",
   :namespace "clojure.core.match",
   :var-type "type",
   :name "PatternRow"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/PredicatePattern",
   :namespace "clojure.core.match",
   :var-type "type",
   :name "PredicatePattern"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/RestPattern",
   :namespace "clojure.core.match",
   :var-type "type",
   :name "RestPattern"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/SeqPattern",
   :namespace "clojure.core.match",
   :var-type "type",
   :name "SeqPattern"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/SwitchNode",
   :namespace "clojure.core.match",
   :var-type "record",
   :name "SwitchNode"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/VectorPattern",
   :namespace "clojure.core.match",
   :var-type "type",
   :name "VectorPattern"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/WildcardPattern",
   :namespace "clojure.core.match",
   :var-type "type",
   :name "WildcardPattern"}
  {:file "src/main/clojure/clojure/core/match.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/cbcc6e5fa070a7025e72f5eab4e83eaec100332b/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/cbcc6e5fa070a7025e72f5eab4e83eaec100332b/src/main/clojure/clojure/core/match.clj#L116",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/IMatchLookup",
   :namespace "clojure.core.match",
   :line 116,
   :var-type "protocol",
   :doc
   "Allows arbitrary objects to act like a map-like object when pattern\nmatched. Avoid extending this directly for Java Beans, see\n`match.java/bean-match`.",
   :name "IMatchLookup"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/val-at",
   :namespace "clojure.core.match",
   :var-type "function",
   :arglists ([this k not-found]),
   :doc nil,
   :name "val-at"}
  {:arglists ([class]),
   :name "bean-match",
   :namespace "clojure.core.match.java",
   :source-url
   "https://github.com/clojure/core.match/blob/ce66e9385bdfed0d8572fe706c950ecdb3d748c7/src/main/clojure/clojure/core/match/java.clj#L20",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/ce66e9385bdfed0d8572fe706c950ecdb3d748c7/src/main/clojure/clojure/core/match/java.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.java/bean-match",
   :doc
   "Generate an implementation of match.core/IMatchLookup for a Java bean.\nAccessor method names are mapped to keys like this:\n\n  isVisible       -> :visible?\n  getText         -> :text\n  getAbsolutePath -> :absolute-path \n  isFUD           -> :fud?\n  getFUDFactor    -> :fud-factor\n\n",
   :var-type "macro",
   :line 20,
   :file "src/main/clojure/clojure/core/match/java.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.regex/RegexPattern",
   :namespace "clojure.core.match.regex",
   :var-type "record",
   :name "RegexPattern"})}
