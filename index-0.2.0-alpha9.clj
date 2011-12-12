{:namespaces
 ({:source-url
   "https://github.com/clojure/core.match/blob/7d29d7e2ec5cb733885fc9cbc9825b654e7dd1cc/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match-api.html",
   :name "clojure.core.match",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.match/blob/af647a77415f5a8c8cc3d1222873b55f91241dde/src/main/clojure/clojure/core/match/java.clj",
   :wiki-url
   "http://clojure.github.com/core.match/clojure.core.match.java-api.html",
   :name "clojure.core.match.java",
   :doc nil}),
 :vars
 ({:name "*backtrack-with-errors*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/7d29d7e2ec5cb733885fc9cbc9825b654e7dd1cc/src/main/clojure/clojure/core/match.clj#L58",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/7d29d7e2ec5cb733885fc9cbc9825b654e7dd1cc/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*backtrack-with-errors*",
   :doc "Enable backtracking diagnostics",
   :var-type "var",
   :line 58,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:name "*breadcrumbs*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/7d29d7e2ec5cb733885fc9cbc9825b654e7dd1cc/src/main/clojure/clojure/core/match.clj#L50",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/7d29d7e2ec5cb733885fc9cbc9825b654e7dd1cc/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*breadcrumbs*",
   :doc "Enable breadcrumb diagnostics with fail nodes",
   :var-type "var",
   :line 50,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:name "*syntax-check*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/7d29d7e2ec5cb733885fc9cbc9825b654e7dd1cc/src/main/clojure/clojure/core/match.clj#L46",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/7d29d7e2ec5cb733885fc9cbc9825b654e7dd1cc/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*syntax-check*",
   :doc "Enable syntax check of match macros",
   :var-type "var",
   :line 46,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:name "*trace*",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/7d29d7e2ec5cb733885fc9cbc9825b654e7dd1cc/src/main/clojure/clojure/core/match.clj#L54",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.match/raw/7d29d7e2ec5cb733885fc9cbc9825b654e7dd1cc/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/*trace*",
   :doc "Enable pattern compile time tracing",
   :var-type "var",
   :line 54,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:file "src/main/clojure/clojure/core/match.clj",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/7d29d7e2ec5cb733885fc9cbc9825b654e7dd1cc/src/main/clojure/clojure/core/match.clj",
   :source-url
   "https://github.com/clojure/core.match/blob/7d29d7e2ec5cb733885fc9cbc9825b654e7dd1cc/src/main/clojure/clojure/core/match.clj#L116",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/IMatchLookup",
   :namespace "clojure.core.match",
   :line 116,
   :var-type "var",
   :doc
   "Allows arbitrary objects to act like a map-like object when pattern\nmatched. Avoid extending this directly for Java Beans, see\n`match.java/bean-match`.",
   :name "IMatchLookup"}
  {:arglists ([vars & clauses]),
   :name "match",
   :namespace "clojure.core.match",
   :source-url
   "https://github.com/clojure/core.match/blob/7d29d7e2ec5cb733885fc9cbc9825b654e7dd1cc/src/main/clojure/clojure/core/match.clj#L1567",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/7d29d7e2ec5cb733885fc9cbc9825b654e7dd1cc/src/main/clojure/clojure/core/match.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match/match",
   :doc
   "Pattern match a row of occurrences. Take a vector of occurrences, vars.\nClause question-answer syntax is like `cond`. Questions must be\nwrapped in a vector, with same arity as vars. Last question can be :else,\nwhich expands to a row of wildcards.\n\nExample:\n(let [x 1\n      y 2]\n    (match [x y 3]\n           [1 2 3] :answer1\n           :else :default-answer))",
   :var-type "macro",
   :line 1567,
   :file "src/main/clojure/clojure/core/match.clj"}
  {:arglists ([class]),
   :name "bean-match",
   :namespace "clojure.core.match.java",
   :source-url
   "https://github.com/clojure/core.match/blob/af647a77415f5a8c8cc3d1222873b55f91241dde/src/main/clojure/clojure/core/match/java.clj#L20",
   :raw-source-url
   "https://github.com/clojure/core.match/raw/af647a77415f5a8c8cc3d1222873b55f91241dde/src/main/clojure/clojure/core/match/java.clj",
   :wiki-url
   "http://clojure.github.com/core.match//clojure.core.match-api.html#clojure.core.match.java/bean-match",
   :doc
   "Generate an implementation of match.core/IMatchLookup for a Java bean.\nAccessor method names are mapped to keys like this:\n\n  isVisible       -> :visible?\n  getText         -> :text\n  getAbsolutePath -> :absolute-path \n  isFUD           -> :fud?\n  getFUDFactor    -> :fud-factor\n\n",
   :var-type "macro",
   :line 20,
   :file "src/main/clojure/clojure/core/match/java.clj"})}
