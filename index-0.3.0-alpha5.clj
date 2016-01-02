{:namespaces
 ({:doc nil,
   :name "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match-api.html",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj"}
  {:doc nil,
   :name "clojure.core.match.java",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match.java-api.html",
   :source-url
   "https://github.com/clojure/core.match/blob/ff6c839d1551a57e2b15033c256324343680fe92/src/main/clojure/clojure/core/match/java.clj"}
  {:doc nil,
   :name "clojure.core.match.protocols",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match.protocols-api.html",
   :source-url
   "https://github.com/clojure/core.match/blob/0545c6af9d545dcf1bc0a3ca792771b9a678a030/src/main/clojure/clojure/core/match/protocols.clj"}
  {:doc nil,
   :name "clojure.core.match.regex",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match.regex-api.html",
   :source-url
   "https://github.com/clojure/core.match/blob/0545c6af9d545dcf1bc0a3ca792771b9a678a030/src/main/clojure/clojure/core/match/regex.clj"}),
 :vars
 ({:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "*match-lookup*",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L60",
   :dynamic true,
   :line 60,
   :var-type "var",
   :doc "Allow map matching syntax to check for IMatchLookup",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*match-lookup*"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "*no-backtrack*",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L74",
   :dynamic true,
   :line 74,
   :var-type "var",
   :doc "Flag to optimize performance over code size.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*no-backtrack*"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "*recur-present*",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L70",
   :dynamic true,
   :line 70,
   :var-type "var",
   :doc
   "In the presence of recur we cannot apply code size optimizations",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*recur-present*"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "*syntax-check*",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L49",
   :dynamic true,
   :line 49,
   :var-type "var",
   :doc "Enable syntax check of match macros",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*syntax-check*"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "*vector-type*",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L64",
   :dynamic true,
   :line 64,
   :var-type "var",
   :doc
   "Default vector type. Can be rebound allowing emission of\ncustom inline code for vector patterns, for example\ntype-hinted primitive array operations",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*vector-type*"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->AppPattern",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1577",
   :line 1577,
   :var-type "function",
   :arglists ([p form _meta]),
   :doc
   "Positional factory function for class clojure.core.match.AppPattern.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->AppPattern"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->BindNode",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L422",
   :line 422,
   :var-type "function",
   :arglists ([bindings node]),
   :doc
   "Positional factory function for class clojure.core.match.BindNode.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->BindNode"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->FailNode",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L406",
   :line 406,
   :var-type "function",
   :arglists ([]),
   :doc
   "Positional factory function for class clojure.core.match.FailNode.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->FailNode"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->GuardPattern",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1490",
   :line 1490,
   :var-type "function",
   :arglists ([p gs _meta]),
   :doc
   "Positional factory function for class clojure.core.match.GuardPattern.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->GuardPattern"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->LeafNode",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L373",
   :line 373,
   :var-type "function",
   :arglists ([value bindings]),
   :doc
   "Positional factory function for class clojure.core.match.LeafNode.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->LeafNode"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->LiteralPattern",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L908",
   :line 908,
   :var-type "function",
   :arglists ([l _meta]),
   :doc
   "Positional factory function for class clojure.core.match.LiteralPattern.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->LiteralPattern"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->MapKeyPattern",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1093",
   :line 1093,
   :var-type "function",
   :arglists ([p]),
   :doc
   "Positional factory function for class clojure.core.match.MapKeyPattern.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->MapKeyPattern"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->MapPattern",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1195",
   :line 1195,
   :var-type "function",
   :arglists ([m _meta]),
   :doc
   "Positional factory function for class clojure.core.match.MapPattern.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->MapPattern"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->OrPattern",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1433",
   :line 1433,
   :var-type "function",
   :arglists ([ps _meta]),
   :doc
   "Positional factory function for class clojure.core.match.OrPattern.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->OrPattern"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->PatternMatrix",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L815",
   :line 815,
   :var-type "function",
   :arglists ([rows ocrs]),
   :doc
   "Positional factory function for class clojure.core.match.PatternMatrix.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->PatternMatrix"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->PatternRow",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L267",
   :line 267,
   :var-type "function",
   :arglists ([ps action bindings]),
   :doc
   "Positional factory function for class clojure.core.match.PatternRow.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->PatternRow"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->PredicatePattern",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1654",
   :line 1654,
   :var-type "function",
   :arglists ([p gs _meta]),
   :doc
   "Positional factory function for class clojure.core.match.PredicatePattern.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->PredicatePattern"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->RestPattern",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1069",
   :line 1069,
   :var-type "function",
   :arglists ([p]),
   :doc
   "Positional factory function for class clojure.core.match.RestPattern.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->RestPattern"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->SeqPattern",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1012",
   :line 1012,
   :var-type "function",
   :arglists ([s _meta]),
   :doc
   "Positional factory function for class clojure.core.match.SeqPattern.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->SeqPattern"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->SwitchNode",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L450",
   :line 450,
   :var-type "function",
   :arglists ([occurrence cases default]),
   :doc
   "Positional factory function for class clojure.core.match.SwitchNode.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->SwitchNode"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->VectorPattern",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1334",
   :line 1334,
   :var-type "function",
   :arglists ([v t size offset rest? _meta]),
   :doc
   "Positional factory function for class clojure.core.match.VectorPattern.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->VectorPattern"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "->WildcardPattern",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L857",
   :line 857,
   :var-type "function",
   :arglists ([sym named _meta]),
   :doc
   "Positional factory function for class clojure.core.match.WildcardPattern.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->WildcardPattern"}
  {:name "backtrack",
   :doc "Pre-allocated exception used for backtracing",
   :var-type "var",
   :line 78,
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/backtrack",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L78",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :file "src/main/clojure/clojure/core/match.clj"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "emit-matrix",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L2039",
   :line 2039,
   :var-type "function",
   :arglists ([vars clauses] [vars clauses default]),
   :doc
   "Take the list of vars and sequence of unprocessed clauses and\nreturn the pattern matrix. The pattern matrix contains the processed\npattern rows and the list of vars originally specified. Inserts\na last match - :else if provided by the user or a default match that\nthrows.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/emit-matrix"}
  {:name "emit-pattern",
   :doc
   "Returns the corresponding pattern for the given syntax. Dispatches\non the class of its argument. For example, `[(:or 1 2) 2]` is dispatched\nas clojure.lang.IPersistentVector",
   :var-type "multimethod",
   :line 1743,
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/emit-pattern",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1743",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :file "src/main/clojure/clojure/core/match.clj"}
  {:name "emit-pattern-for-syntax",
   :doc
   "Handles patterns wrapped in the special list syntax. Dispatches\non the first or second keyword in the list. For example, the pattern \n`(:or 1 ...) is dispatches as :or, and `(1 :as a)` is dispatched by :as.",
   :var-type "multimethod",
   :line 1819,
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/emit-pattern-for-syntax",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1819",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :file "src/main/clojure/clojure/core/match.clj"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "empty-rows-case",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L713",
   :line 713,
   :var-type "function",
   :arglists ([]),
   :doc
   "Case 1: If there are no pattern rows to match, then matching always fails",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/empty-rows-case"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "first-column-chosen-case",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L760",
   :line 760,
   :var-type "function",
   :arglists ([matrix col ocrs]),
   :doc
   "Case 3a: The first column is chosen. Compute and return a\nswitch/bind node with a default matrix case",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/first-column-chosen-case"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "first-row-empty-case",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L718",
   :line 718,
   :var-type "function",
   :arglists ([rows ocr]),
   :doc
   "Case 2: If the first row is empty then matching always succeeds \nand yields the first action.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/first-row-empty-case"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "first-row-wildcards-case",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L728",
   :line 728,
   :var-type "function",
   :arglists ([rows ocrs]),
   :doc
   "Case 2: If the first row is constituted by wildcards then matching\nmatching always succeeds and yields the first action.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/first-row-wildcards-case"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "group-keywords",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1903",
   :line 1903,
   :var-type "function",
   :arglists ([pattern]),
   :doc
   "Returns a pattern with pattern-keywords (:when and :as) properly\ngrouped.  The original pattern may use the 'flattened' syntax.\nFor example, a 'flattened' pattern row like [a b :when even?] is\ngrouped as [a (b :when even?)].",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/group-keywords"}
  {:name "groupable?",
   :doc
   "Determine if two patterns may be grouped together for simultaneous\ntesting.",
   :var-type "multimethod",
   :line 249,
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/groupable?",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L249",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :file "src/main/clojure/clojure/core/match.clj"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "map->BindNode",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L422",
   :line 422,
   :var-type "function",
   :arglists ([m__5818__auto__]),
   :doc
   "Factory function for class clojure.core.match.BindNode, taking a map of keywords to field values.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->BindNode"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "map->FailNode",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L406",
   :line 406,
   :var-type "function",
   :arglists ([m__5818__auto__]),
   :doc
   "Factory function for class clojure.core.match.FailNode, taking a map of keywords to field values.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->FailNode"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "map->LeafNode",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L373",
   :line 373,
   :var-type "function",
   :arglists ([m__5818__auto__]),
   :doc
   "Factory function for class clojure.core.match.LeafNode, taking a map of keywords to field values.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->LeafNode"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "map->MapKeyPattern",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1093",
   :line 1093,
   :var-type "function",
   :arglists ([m__5818__auto__]),
   :doc
   "Factory function for class clojure.core.match.MapKeyPattern, taking a map of keywords to field values.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->MapKeyPattern"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "map->PatternMatrix",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L815",
   :line 815,
   :var-type "function",
   :arglists ([m__5818__auto__]),
   :doc
   "Factory function for class clojure.core.match.PatternMatrix, taking a map of keywords to field values.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->PatternMatrix"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "map->RestPattern",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1069",
   :line 1069,
   :var-type "function",
   :arglists ([m__5818__auto__]),
   :doc
   "Factory function for class clojure.core.match.RestPattern, taking a map of keywords to field values.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->RestPattern"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "map->SwitchNode",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L450",
   :line 450,
   :var-type "function",
   :arglists ([m__5818__auto__]),
   :doc
   "Factory function for class clojure.core.match.SwitchNode, taking a map of keywords to field values.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->SwitchNode"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "match",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L2101",
   :line 2101,
   :var-type "macro",
   :arglists ([vars & clauses]),
   :doc
   "Pattern match a row of occurrences. Take a vector of occurrences, vars.\nClause question-answer syntax is like `cond`. Questions must be\nwrapped in a vector, with same arity as vars. Last question can be :else,\nwhich expands to a row of wildcards. Optionally may take a single\nvar not wrapped in a vector, questions then need not be wrapped in a\nvector.\n\nExample:\n(let [x 1\n      y 2]\n  (match [x y 3]\n    [1 2 3] :answer1\n    :else :default-answer))",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/match"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "matchm",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L2135",
   :line 2135,
   :var-type "macro",
   :arglists ([vars & clauses]),
   :doc "Same as match but supports IMatchLookup when\nmatching maps.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/matchm"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "other-column-chosen-case",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L783",
   :line 783,
   :var-type "function",
   :arglists ([matrix col]),
   :doc
   "Case 3b: A column other than the first is chosen. Swap column \ncol with the first column and compile the result",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/other-column-chosen-case"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "process-vars",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L2027",
   :line 2027,
   :var-type "function",
   :arglists ([vars]),
   :doc
   "Process the vars for the pattern matrix. If user provides an\nexpression, create a var and annotate via metadata with the\noriginal expression.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/process-vars"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "to-pattern-row",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1911",
   :line 1911,
   :var-type "function",
   :arglists ([pat action]),
   :doc
   "Take an unprocessed pattern expression and an action expression and return\na pattern row of the processed pattern expression plus the action epxression.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/to-pattern-row"}
  {:name "to-source",
   :doc
   "Returns a Clojure form that, when executed, is truthy if the\npattern matches the occurrence. Dispatches on the `type` of the\npattern. For instance, a literal pattern might return `(= ~(:pattern\npattern) ~ocr)`, using `=` to test for a match.",
   :var-type "multimethod",
   :line 1736,
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/to-source",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1736",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :file "src/main/clojure/clojure/core/match.clj"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj",
   :name "wildcards-and-duplicates",
   :file "src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/1c6b2b522990ed9c78fa3499d2797d0fab87d114/src/main/clojure/clojure/core/match.clj#L1918",
   :line 1918,
   :var-type "function",
   :arglists ([patterns]),
   :doc
   "Returns a vector of two elements: the set of all wildcards and the \nset of duplicate wildcards.  The underbar _ is excluded from both.",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/wildcards-and-duplicates"}
  {:name "AppPattern",
   :var-type "type",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/AppPattern",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "BindNode",
   :var-type "record",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/BindNode",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "FailNode",
   :var-type "record",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/FailNode",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "GuardPattern",
   :var-type "type",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/GuardPattern",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "LeafNode",
   :var-type "record",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/LeafNode",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "LiteralPattern",
   :var-type "type",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/LiteralPattern",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "MapKeyPattern",
   :var-type "record",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/MapKeyPattern",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "MapPattern",
   :var-type "type",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/MapPattern",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "OrPattern",
   :var-type "type",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/OrPattern",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "PatternMatrix",
   :var-type "record",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/PatternMatrix",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "PatternRow",
   :var-type "type",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/PatternRow",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "PredicatePattern",
   :var-type "type",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/PredicatePattern",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "RestPattern",
   :var-type "record",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/RestPattern",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "SeqPattern",
   :var-type "type",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/SeqPattern",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "SwitchNode",
   :var-type "record",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/SwitchNode",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "VectorPattern",
   :var-type "type",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/VectorPattern",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "WildcardPattern",
   :var-type "type",
   :namespace "clojure.core.match",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/WildcardPattern",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/ff6c839d1551a57e2b15033c256324343680fe92/src/main/clojure/clojure/core/match/java.clj",
   :name "bean-match",
   :file "src/main/clojure/clojure/core/match/java.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/ff6c839d1551a57e2b15033c256324343680fe92/src/main/clojure/clojure/core/match/java.clj#L21",
   :line 21,
   :var-type "macro",
   :arglists ([class]),
   :doc
   "Generate an implementation of match.core/IMatchLookup for a Java bean.\nAccessor method names are mapped to keys like this:\n\n  isVisible       -> :visible?\n  getText         -> :text\n  getAbsolutePath -> :absolute-path \n  isFUD           -> :fud?\n  getFUDFactor    -> :fud-factor\n\n",
   :namespace "clojure.core.match.java",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.java/bean-match"}
  {:name "IMatchLookup",
   :doc
   "Allows arbitrary objects to act like a map-like object when pattern\nmatched. Avoid extending this directly for Java Beans, see\n`match.java/bean-match`.",
   :var-type "protocol",
   :line 12,
   :namespace "clojure.core.match.protocols",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.protocols/IMatchLookup",
   :source-url
   "https://github.com/clojure/core.match/blob/0545c6af9d545dcf1bc0a3ca792771b9a678a030/src/main/clojure/clojure/core/match/protocols.clj#L12",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/0545c6af9d545dcf1bc0a3ca792771b9a678a030/src/main/clojure/clojure/core/match/protocols.clj",
   :file "src/main/clojure/clojure/core/match/protocols.clj"}
  {:name "val-at",
   :doc nil,
   :arglists ([this k not-found]),
   :var-type "function",
   :namespace "clojure.core.match.protocols",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.protocols/val-at",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/0545c6af9d545dcf1bc0a3ca792771b9a678a030/src/main/clojure/clojure/core/match/regex.clj",
   :name "->RegexPattern",
   :file "src/main/clojure/clojure/core/match/regex.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/0545c6af9d545dcf1bc0a3ca792771b9a678a030/src/main/clojure/clojure/core/match/regex.clj#L15",
   :line 15,
   :var-type "function",
   :arglists ([regex]),
   :doc
   "Positional factory function for class clojure.core.match.regex.RegexPattern.",
   :namespace "clojure.core.match.regex",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.regex/->RegexPattern"}
  {:raw-source-url
   "https://github.com/clojure/core.match/raw/0545c6af9d545dcf1bc0a3ca792771b9a678a030/src/main/clojure/clojure/core/match/regex.clj",
   :name "map->RegexPattern",
   :file "src/main/clojure/clojure/core/match/regex.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/0545c6af9d545dcf1bc0a3ca792771b9a678a030/src/main/clojure/clojure/core/match/regex.clj#L15",
   :line 15,
   :var-type "function",
   :arglists ([m__5818__auto__]),
   :doc
   "Factory function for class clojure.core.match.regex.RegexPattern, taking a map of keywords to field values.",
   :namespace "clojure.core.match.regex",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.regex/map->RegexPattern"}
  {:name "RegexPattern",
   :var-type "record",
   :namespace "clojure.core.match.regex",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.regex/RegexPattern",
   :source-url nil,
   :raw-source-url nil,
   :file nil})}
