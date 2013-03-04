{:namespaces
 ({:source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
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
   "https://github.com/clojure/core.match/blob/ec5823744b3073cbbc66e575620b8c2ebd41f2df/src/main/clojure/clojure/core/match/pred.clj",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match.pred-api.html",
   :name "clojure.core.match.pred",
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
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L58",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*backtrack-with-errors*",
   :doc "Enable backtracking diagnostics",
   :var-type "var",
   :line 58,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:name "*breadcrumbs*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L50",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*breadcrumbs*",
   :doc "Enable breadcrumb diagnostics with fail nodes",
   :var-type "var",
   :line 50,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:name "*syntax-check*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L46",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*syntax-check*",
   :doc "Enable syntax check of match macros",
   :var-type "var",
   :line 46,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:name "*trace*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L54",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*trace*",
   :doc "Enable pattern compile time tracing",
   :var-type "var",
   :line 54,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([bindings node]),
   :name "->BindNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L417",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->BindNode",
   :doc
   "Positional factory function for class clojure.core.match.BindNode.",
   :var-type "function",
   :line 417,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([]),
   :name "->FailNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L400",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->FailNode",
   :doc
   "Positional factory function for class clojure.core.match.FailNode.",
   :var-type "function",
   :line 400,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([p gs _meta]),
   :name "->GuardPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L1286",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->GuardPattern",
   :doc
   "Positional factory function for class clojure.core.match.GuardPattern.",
   :var-type "function",
   :line 1286,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([value bindings]),
   :name "->LeafNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L363",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->LeafNode",
   :doc
   "Positional factory function for class clojure.core.match.LeafNode.",
   :var-type "function",
   :line 363,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([l _meta]),
   :name "->LiteralPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L888",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->LiteralPattern",
   :doc
   "Positional factory function for class clojure.core.match.LiteralPattern.",
   :var-type "function",
   :line 888,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([m _meta]),
   :name "->MapPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L1017",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->MapPattern",
   :doc
   "Positional factory function for class clojure.core.match.MapPattern.",
   :var-type "function",
   :line 1017,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([ps _meta]),
   :name "->OrPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L1231",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->OrPattern",
   :doc
   "Positional factory function for class clojure.core.match.OrPattern.",
   :var-type "function",
   :line 1231,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([rows ocrs _meta]),
   :name "->PatternMatrix",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L687",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->PatternMatrix",
   :doc
   "Positional factory function for class clojure.core.match.PatternMatrix.",
   :var-type "function",
   :line 687,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([ps action bindings]),
   :name "->PatternRow",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L291",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->PatternRow",
   :doc
   "Positional factory function for class clojure.core.match.PatternRow.",
   :var-type "function",
   :line 291,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([p gs _meta]),
   :name "->PredicatePattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L1348",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->PredicatePattern",
   :doc
   "Positional factory function for class clojure.core.match.PredicatePattern.",
   :var-type "function",
   :line 1348,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([p _meta]),
   :name "->RestPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L989",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->RestPattern",
   :doc
   "Positional factory function for class clojure.core.match.RestPattern.",
   :var-type "function",
   :line 989,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([s _meta]),
   :name "->SeqPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L926",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->SeqPattern",
   :doc
   "Positional factory function for class clojure.core.match.SeqPattern.",
   :var-type "function",
   :line 926,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([occurrence cases default]),
   :name "->SwitchNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L453",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->SwitchNode",
   :doc
   "Positional factory function for class clojure.core.match.SwitchNode.",
   :var-type "function",
   :line 453,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([v t size offset rest? _meta]),
   :name "->VectorPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L1125",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->VectorPattern",
   :doc
   "Positional factory function for class clojure.core.match.VectorPattern.",
   :var-type "function",
   :line 1125,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([sym _meta]),
   :name "->WildcardPattern",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L848",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/->WildcardPattern",
   :doc
   "Positional factory function for class clojure.core.match.WildcardPattern.",
   :var-type "function",
   :line 848,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:file "src/main/clojure/clojure/core/match.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L266",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/comparable?",
   :namespace "clojure.core.match",
   :line 266,
   :var-type "var",
   :doc
   "Returns true if it is possible to tell at compile time whether two\ndifferent versions of the same object can never match the same\nobject.",
   :name "comparable?"}
  {:file "src/main/clojure/clojure/core/match.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L1473",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/emit-pattern",
   :namespace "clojure.core.match",
   :line 1473,
   :var-type "var",
   :doc
   "Returns the corresponding pattern for the given syntax. Dispatches\non the class of its argument. For example, `[(:or 1 2) 2]` is dispatched\nas clojure.lang.IPersistentVector",
   :name "emit-pattern"}
  {:file "src/main/clojure/clojure/core/match.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L1535",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/emit-pattern-for-syntax",
   :namespace "clojure.core.match",
   :line 1535,
   :var-type "var",
   :doc
   "Handles patterns wrapped in the special list syntax. Dispatches\non the first or second keyword in the list. For example, the pattern \n`(:or 1 ...) is dispatches as :or, and `(1 :as a)` is dispatched by :as.",
   :name "emit-pattern-for-syntax"}
  {:arglists ([m__5828__auto__]),
   :name "map->BindNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L417",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->BindNode",
   :doc
   "Factory function for class clojure.core.match.BindNode, taking a map of keywords to field values.",
   :var-type "function",
   :line 417,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([m__5828__auto__]),
   :name "map->FailNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L400",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->FailNode",
   :doc
   "Factory function for class clojure.core.match.FailNode, taking a map of keywords to field values.",
   :var-type "function",
   :line 400,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([m__5828__auto__]),
   :name "map->LeafNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L363",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->LeafNode",
   :doc
   "Factory function for class clojure.core.match.LeafNode, taking a map of keywords to field values.",
   :var-type "function",
   :line 363,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([m__5828__auto__]),
   :name "map->SwitchNode",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L453",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/map->SwitchNode",
   :doc
   "Factory function for class clojure.core.match.SwitchNode, taking a map of keywords to field values.",
   :var-type "function",
   :line 453,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([vars & clauses]),
   :name "match",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L1740",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/match",
   :doc
   "Pattern match a row of occurrences. Take a vector of occurrences, vars.\nClause question-answer syntax is like `cond`. Questions must be\nwrapped in a vector, with same arity as vars. Last question can be :else,\nwhich expands to a row of wildcards.\n\nExample:\n(let [x 1\n      y 2]\n    (match [x y 3]\n           [1 2 3] :answer1\n           :else :default-answer))",
   :var-type "macro",
   :line 1740,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:file "src/main/clojure/clojure/core/match.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L249",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/pattern-compare",
   :namespace "clojure.core.match",
   :line 249,
   :var-type "var",
   :doc "Like `clojure.core/compare` but for comparing patterns",
   :name "pattern-compare"}
  {:file "src/main/clojure/clojure/core/match.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L259",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/safe-pattern-compare",
   :namespace "clojure.core.match",
   :line 259,
   :var-type "var",
   :doc "Like pattern-compare but not affected by *recur-present*",
   :name "safe-pattern-compare"}
  {:file "src/main/clojure/clojure/core/match.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L1467",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/to-source",
   :namespace "clojure.core.match",
   :line 1467,
   :var-type "var",
   :doc
   "Returns a Clojure form that, when executed, is truthy if the pattern matches\nthe occurrence. Dispatches on the `type` of the pattern. For instance, a literal pattern \nmight return `(= ~(:pattern pattern) ~ocr)`, using `=` to test for a match.",
   :name "to-source"}
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
   "https://github.com/clojure/core.match/raw/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/3bab92b6620dccdcb9e55941af4599e3adf78a6e/src/main/clojure/clojure/core/match.clj#L116",
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
  {:arglists ([regex]),
   :name "->RegexPattern",
   :namespace "clojure.core.match.regex",
   :source-url
   "https://github.com/clojure/core.match/blob/17075bb1cd0cbdab080fbc98691080a5f07f88fc/src/main/clojure/clojure/core/match/regex.clj#L11",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/17075bb1cd0cbdab080fbc98691080a5f07f88fc/src/main/clojure/clojure/core/match/regex.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.regex/->RegexPattern",
   :doc
   "Positional factory function for class clojure.core.match.regex.RegexPattern.",
   :var-type "function",
   :line 11,
   :file "src/main/clojure/clojure/core/match/regex.clj"}
  {:arglists ([m__5828__auto__]),
   :name "map->RegexPattern",
   :namespace "clojure.core.match.regex",
   :source-url
   "https://github.com/clojure/core.match/blob/17075bb1cd0cbdab080fbc98691080a5f07f88fc/src/main/clojure/clojure/core/match/regex.clj#L11",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/17075bb1cd0cbdab080fbc98691080a5f07f88fc/src/main/clojure/clojure/core/match/regex.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.regex/map->RegexPattern",
   :doc
   "Factory function for class clojure.core.match.regex.RegexPattern, taking a map of keywords to field values.",
   :var-type "function",
   :line 11,
   :file "src/main/clojure/clojure/core/match/regex.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.regex/RegexPattern",
   :namespace "clojure.core.match.regex",
   :var-type "record",
   :name "RegexPattern"})}
