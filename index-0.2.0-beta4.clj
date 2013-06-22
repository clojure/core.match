{:namespaces
 ({:source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match-api.html",
   :name "clojure.core.match",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.match/blob/ff6c839d1551a57e2b15033c256324343680fe92/src/main/clojure/clojure/core/match/java.clj",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match.java-api.html",
   :name "clojure.core.match.java",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.match/blob/ec5823744b3073cbbc66e575620b8c2ebd41f2df/src/main/clojure/clojure/core/match/pred.clj",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match.pred-api.html",
   :name "clojure.core.match.pred",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.match/blob/e1fb4c67f1f76afc8bd74c342c96015407ea51f7/src/main/clojure/clojure/core/match/protocols.clj",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match.protocols-api.html",
   :name "clojure.core.match.protocols",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.match/blob/4f89cd06670a51ed533cd143d7f8fcba05f765db/src/main/clojure/clojure/core/match/regex.clj",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match.regex-api.html",
   :name "clojure.core.match.regex",
   :doc nil}),
 :vars
 ({:name "*recur-present*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L62",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*recur-present*",
   :doc
   "In the presence of recur we cannot apply code size optimizations",
   :var-type "var",
   :line 62,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:name "*syntax-check*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L45",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*syntax-check*",
   :doc "Enable syntax check of match macros",
   :var-type "var",
   :line 45,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:name "*vector-type*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L56",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*vector-type*",
   :doc
   "Default vector type. Can be rebound allowing emission of\ncustom inline code for vector patterns, for example\ntype-hinted primitive array operations",
   :var-type "var",
   :line 56,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([bindings node]),
   :name "->BindNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L391",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->BindNode",
   :doc
   "Positional factory function for class clojure.core.match.BindNode.",
   :var-type "function",
   :line 391,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([]),
   :name "->FailNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L377",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->FailNode",
   :doc
   "Positional factory function for class clojure.core.match.FailNode.",
   :var-type "function",
   :line 377,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([p gs _meta]),
   :name "->GuardPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1424",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->GuardPattern",
   :doc
   "Positional factory function for class clojure.core.match.GuardPattern.",
   :var-type "function",
   :line 1424,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([value bindings]),
   :name "->LeafNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L339",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->LeafNode",
   :doc
   "Positional factory function for class clojure.core.match.LeafNode.",
   :var-type "function",
   :line 339,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([l _meta]),
   :name "->LiteralPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L848",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->LiteralPattern",
   :doc
   "Positional factory function for class clojure.core.match.LiteralPattern.",
   :var-type "function",
   :line 848,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([p]),
   :name "->MapKeyPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1018",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->MapKeyPattern",
   :doc
   "Positional factory function for class clojure.core.match.MapKeyPattern.",
   :var-type "function",
   :line 1018,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([m _meta]),
   :name "->MapPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1120",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->MapPattern",
   :doc
   "Positional factory function for class clojure.core.match.MapPattern.",
   :var-type "function",
   :line 1120,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([ps _meta]),
   :name "->OrPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1355",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->OrPattern",
   :doc
   "Positional factory function for class clojure.core.match.OrPattern.",
   :var-type "function",
   :line 1355,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([rows ocrs]),
   :name "->PatternMatrix",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L755",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->PatternMatrix",
   :doc
   "Positional factory function for class clojure.core.match.PatternMatrix.",
   :var-type "function",
   :line 755,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([ps action bindings]),
   :name "->PatternRow",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L233",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->PatternRow",
   :doc
   "Positional factory function for class clojure.core.match.PatternRow.",
   :var-type "function",
   :line 233,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([p gs _meta]),
   :name "->PredicatePattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1504",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->PredicatePattern",
   :doc
   "Positional factory function for class clojure.core.match.PredicatePattern.",
   :var-type "function",
   :line 1504,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([p]),
   :name "->RestPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L994",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->RestPattern",
   :doc
   "Positional factory function for class clojure.core.match.RestPattern.",
   :var-type "function",
   :line 994,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([s _meta]),
   :name "->SeqPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L938",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->SeqPattern",
   :doc
   "Positional factory function for class clojure.core.match.SeqPattern.",
   :var-type "function",
   :line 938,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([occurrence cases default]),
   :name "->SwitchNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L419",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->SwitchNode",
   :doc
   "Positional factory function for class clojure.core.match.SwitchNode.",
   :var-type "function",
   :line 419,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([v t size offset rest? _meta]),
   :name "->VectorPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1257",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->VectorPattern",
   :doc
   "Positional factory function for class clojure.core.match.VectorPattern.",
   :var-type "function",
   :line 1257,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([sym named _meta]),
   :name "->WildcardPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L797",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->WildcardPattern",
   :doc
   "Positional factory function for class clojure.core.match.WildcardPattern.",
   :var-type "function",
   :line 797,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:file "src/main/clojure/clojure/core/match.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L66",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/backtrack",
   :namespace "clojure.core.match",
   :line 66,
   :var-type "var",
   :doc "Pre-allocated exception used for backtracing",
   :name "backtrack"}
  {:arglists ([vars clauses] [vars clauses default]),
   :name "emit-matrix",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1869",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/emit-matrix",
   :doc
   "Take the list of vars and sequence of unprocessed clauses and\nreturn the pattern matrix. The pattern matrix contains the processed\npattern rows and the list of vars originally specified. Inserts\na last match - :else if provided by the user or a default match that\nthrows.",
   :var-type "function",
   :line 1869,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:file "src/main/clojure/clojure/core/match.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1591",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/emit-pattern",
   :namespace "clojure.core.match",
   :line 1591,
   :var-type "multimethod",
   :doc
   "Returns the corresponding pattern for the given syntax. Dispatches\non the class of its argument. For example, `[(:or 1 2) 2]` is dispatched\nas clojure.lang.IPersistentVector",
   :name "emit-pattern"}
  {:file "src/main/clojure/clojure/core/match.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1652",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/emit-pattern-for-syntax",
   :namespace "clojure.core.match",
   :line 1652,
   :var-type "multimethod",
   :doc
   "Handles patterns wrapped in the special list syntax. Dispatches\non the first or second keyword in the list. For example, the pattern \n`(:or 1 ...) is dispatches as :or, and `(1 :as a)` is dispatched by :as.",
   :name "emit-pattern-for-syntax"}
  {:arglists ([]),
   :name "empty-rows-case",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L670",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/empty-rows-case",
   :doc
   "Case 1: If there are no pattern rows to match, then matching always fails",
   :var-type "function",
   :line 670,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([matrix col ocrs]),
   :name "first-column-chosen-case",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L707",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/first-column-chosen-case",
   :doc
   "Case 3a: The first column is chosen. Compute and return a\nswitch/bind node with a default matrix case",
   :var-type "function",
   :line 707,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([rows ocr]),
   :name "first-row-empty-case",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L675",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/first-row-empty-case",
   :doc
   "Case 2: If the first row is empty then matching always succeeds \nand yields the first action.",
   :var-type "function",
   :line 675,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([rows ocrs]),
   :name "first-row-wildcards-case",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L685",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/first-row-wildcards-case",
   :doc
   "Case 2: If the first row is constituted by wildcards then matching\nmatching always succeeds and yields the first action.",
   :var-type "function",
   :line 685,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([pattern]),
   :name "group-keywords",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1733",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/group-keywords",
   :doc
   "Returns a pattern with pattern-keywords (:when and :as) properly\ngrouped.  The original pattern may use the 'flattened' syntax.\nFor example, a 'flattened' pattern row like [a b :when even?] is\ngrouped as [a (b :when even?)].",
   :var-type "function",
   :line 1733,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:file "src/main/clojure/clojure/core/match.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L215",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/groupable?",
   :namespace "clojure.core.match",
   :line 215,
   :var-type "multimethod",
   :doc
   "Determine if two patterns may be grouped together for simultaneous\ntesting.",
   :name "groupable?"}
  {:arglists ([m__5818__auto__]),
   :name "map->BindNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L391",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->BindNode",
   :doc
   "Factory function for class clojure.core.match.BindNode, taking a map of keywords to field values.",
   :var-type "function",
   :line 391,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([m__5818__auto__]),
   :name "map->FailNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L377",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->FailNode",
   :doc
   "Factory function for class clojure.core.match.FailNode, taking a map of keywords to field values.",
   :var-type "function",
   :line 377,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([m__5818__auto__]),
   :name "map->LeafNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L339",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->LeafNode",
   :doc
   "Factory function for class clojure.core.match.LeafNode, taking a map of keywords to field values.",
   :var-type "function",
   :line 339,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([m__5818__auto__]),
   :name "map->MapKeyPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1018",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->MapKeyPattern",
   :doc
   "Factory function for class clojure.core.match.MapKeyPattern, taking a map of keywords to field values.",
   :var-type "function",
   :line 1018,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([m__5818__auto__]),
   :name "map->PatternMatrix",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L755",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->PatternMatrix",
   :doc
   "Factory function for class clojure.core.match.PatternMatrix, taking a map of keywords to field values.",
   :var-type "function",
   :line 755,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([m__5818__auto__]),
   :name "map->RestPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L994",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->RestPattern",
   :doc
   "Factory function for class clojure.core.match.RestPattern, taking a map of keywords to field values.",
   :var-type "function",
   :line 994,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([m__5818__auto__]),
   :name "map->SwitchNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L419",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->SwitchNode",
   :doc
   "Factory function for class clojure.core.match.SwitchNode, taking a map of keywords to field values.",
   :var-type "function",
   :line 419,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([vars & clauses]),
   :name "match",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1919",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/match",
   :doc
   "Pattern match a row of occurrences. Take a vector of occurrences, vars.\nClause question-answer syntax is like `cond`. Questions must be\nwrapped in a vector, with same arity as vars. Last question can be :else,\nwhich expands to a row of wildcards.\n\nExample:\n(let [x 1\n      y 2]\n  (match [x y 3]\n    [1 2 3] :answer1\n    :else :default-answer))",
   :var-type "macro",
   :line 1919,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([matrix col]),
   :name "other-column-chosen-case",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L723",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/other-column-chosen-case",
   :doc
   "Case 3b: A column other than the first is chosen. Swap column \ncol with the first column and compile the result",
   :var-type "function",
   :line 723,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([vars]),
   :name "process-vars",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1857",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/process-vars",
   :doc
   "Process the vars for the pattern matrix. If user provides an\nexpression, create a var and annotate via metadata with the\noriginal expression.",
   :var-type "function",
   :line 1857,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([pat action]),
   :name "to-pattern-row",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1741",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/to-pattern-row",
   :doc
   "Take an unprocessed pattern expression and an action expression and return\na pattern row of the processed pattern expression plus the action epxression.",
   :var-type "function",
   :line 1741,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:file "src/main/clojure/clojure/core/match.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1584",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/to-source",
   :namespace "clojure.core.match",
   :line 1584,
   :var-type "multimethod",
   :doc
   "Returns a Clojure form that, when executed, is truthy if the\npattern matches the occurrence. Dispatches on the `type` of the\npattern. For instance, a literal pattern might return `(= ~(:pattern\npattern) ~ocr)`, using `=` to test for a match.",
   :name "to-source"}
  {:arglists ([patterns]),
   :name "wildcards-and-duplicates",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj#L1748",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/59f6fbfad289bc6064b775fc9a5a6934658ee855/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/wildcards-and-duplicates",
   :doc
   "Returns a vector of two elements: the set of all wildcards and the \nset of duplicate wildcards.  The underbar _ is excluded from both.",
   :var-type "function",
   :line 1748,
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
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/MapKeyPattern",
   :namespace "clojure.core.match",
   :var-type "record",
   :name "MapKeyPattern"}
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
   :var-type "record",
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
   :var-type "record",
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
  {:arglists ([class]),
   :name "bean-match",
   :namespace "clojure.core.match.java",
   :source-url
   "https://github.com/clojure/core.match/blob/ff6c839d1551a57e2b15033c256324343680fe92/src/main/clojure/clojure/core/match/java.clj#L21",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/ff6c839d1551a57e2b15033c256324343680fe92/src/main/clojure/clojure/core/match/java.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.java/bean-match",
   :doc
   "Generate an implementation of match.core/IMatchLookup for a Java bean.\nAccessor method names are mapped to keys like this:\n\n  isVisible       -> :visible?\n  getText         -> :text\n  getAbsolutePath -> :absolute-path \n  isFUD           -> :fud?\n  getFUDFactor    -> :fud-factor\n\n",
   :var-type "macro",
   :line 21,
   :file "src/main/clojure/clojure/core/match/java.clj"}
  {:arglists ([dispatches]),
   :name "->DispMatrix",
   :namespace "clojure.core.match.pred",
   :source-url
   "https://github.com/clojure/core.match/blob/ec5823744b3073cbbc66e575620b8c2ebd41f2df/src/main/clojure/clojure/core/match/pred.clj#L40",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/ec5823744b3073cbbc66e575620b8c2ebd41f2df/src/main/clojure/clojure/core/match/pred.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.pred/->DispMatrix",
   :doc
   "Positional factory function for class clojure.core.match.pred.DispMatrix.",
   :var-type "function",
   :line 40,
   :file "src/main/clojure/clojure/core/match/pred.clj"}
  {:arglists ([super sub]),
   :name "subsumes",
   :namespace "clojure.core.match.pred",
   :source-url
   "https://github.com/clojure/core.match/blob/ec5823744b3073cbbc66e575620b8c2ebd41f2df/src/main/clojure/clojure/core/match/pred.clj#L74",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/ec5823744b3073cbbc66e575620b8c2ebd41f2df/src/main/clojure/clojure/core/match/pred.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.pred/subsumes",
   :doc "Declares super as a strict superset of sub",
   :var-type "function",
   :line 74,
   :file "src/main/clojure/clojure/core/match/pred.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.pred/DispMatrix",
   :namespace "clojure.core.match.pred",
   :var-type "type",
   :name "DispMatrix"}
  {:file "src/main/clojure/clojure/core/match/protocols.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/e1fb4c67f1f76afc8bd74c342c96015407ea51f7/src/main/clojure/clojure/core/match/protocols.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/e1fb4c67f1f76afc8bd74c342c96015407ea51f7/src/main/clojure/clojure/core/match/protocols.clj#L12",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.protocols/IMatchLookup",
   :namespace "clojure.core.match.protocols",
   :line 12,
   :var-type "protocol",
   :doc
   "Allows arbitrary objects to act like a map-like object when pattern\nmatched. Avoid extending this directly for Java Beans, see\n`match.java/bean-match`.",
   :name "IMatchLookup"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.protocols/val-at",
   :namespace "clojure.core.match.protocols",
   :var-type "function",
   :arglists ([this k not-found]),
   :doc nil,
   :name "val-at"}
  {:arglists ([regex]),
   :name "->RegexPattern",
   :namespace "clojure.core.match.regex",
   :source-url
   "https://github.com/clojure/core.match/blob/4f89cd06670a51ed533cd143d7f8fcba05f765db/src/main/clojure/clojure/core/match/regex.clj#L10",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/4f89cd06670a51ed533cd143d7f8fcba05f765db/src/main/clojure/clojure/core/match/regex.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.regex/->RegexPattern",
   :doc
   "Positional factory function for class clojure.core.match.regex.RegexPattern.",
   :var-type "function",
   :line 10,
   :file "src/main/clojure/clojure/core/match/regex.clj"}
  {:arglists ([m__5818__auto__]),
   :name "map->RegexPattern",
   :namespace "clojure.core.match.regex",
   :source-url
   "https://github.com/clojure/core.match/blob/4f89cd06670a51ed533cd143d7f8fcba05f765db/src/main/clojure/clojure/core/match/regex.clj#L10",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/4f89cd06670a51ed533cd143d7f8fcba05f765db/src/main/clojure/clojure/core/match/regex.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.regex/map->RegexPattern",
   :doc
   "Factory function for class clojure.core.match.regex.RegexPattern, taking a map of keywords to field values.",
   :var-type "function",
   :line 10,
   :file "src/main/clojure/clojure/core/match/regex.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.regex/RegexPattern",
   :namespace "clojure.core.match.regex",
   :var-type "record",
   :name "RegexPattern"})}
