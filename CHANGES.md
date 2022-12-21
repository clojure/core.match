From 1.0.0 to 1.0.1
===

* MATCH-129 - fix syntax error using string literal with a / in it

From 0.3.0 to 1.0.0
===

No changes

From 0.3.0-alpha5 to 0.3.0
===

Fixes
---
* MATCH-124 - correct namespaced keywords to work with Clojure 1.9+

From 0.3.0-alpha4 to 0.3.0-alpha5
===

Fixes
---
* Remove unused tools.analyzer dependency

From 0.3.0-alpha3 to 0.3.0-alpha4
===

Fixes
---
* Add `matchm` to ClojureScript support

From 0.3.0-alpha2 to 0.3.0-alpha3
===

Fixes
---
* Fix regression in test sharing due to unfinished literal case work

From 0.3.0-alpha1 to 0.3.0-alpha2
===

Fixes
---
* Fix fn app pattern issue when nested in maps & vectors

===

From 0.2.2 to 0.3.0-alpha1
===

Changes
---
* Depend on tools.analyzer
* MATCH-100: Support function application in match syntax
* MATCH-97: satisfies? calls for IMatchLookup perf issues, breaking

Fixes
---
* MATCH-98: RegexPattern cannot be aot-compiled

From 0.2.1 to 0.2.2
===

Changes
---
* Depend on tools.analyzer

Fixes
---
* MATCH-86: Cannot AOT OrPatterns
* sort pattern duplicates to avoid order issues now that hash values have changed in Clojure 1.6.0

From 0.2.0 to 0.2.1
===

Fixes
---
* MATCH-91: keyword with dots cause core.match to fail

From 0.2.0-rc6 to 0.2.0
===

None

From 0.2.0-rc5 to 0.2.0-rc6
===

Fixes
---
* MATCH-86: or patterns do not AOT

Changes
---
* Requires ClojureScript 0.0-1889
* Cleaned up dependencies (no more core.logic requirement)

Enhancements
---
* Use identical? when matching strings and numbers in ClojureScript

From 0.2.0-rc4 to 0.2.0-rc5
===

Fixes
---
* MATCH-82: CLJS locals matching did not work correctly
* MATCH-83: vector pattern with rest pattern bug
* MATCH-84: vector pattern with rest pattern bug

From 0.2.0-rc3 to 0.2.0-rc4
===

Fixes
---
* MATCH-81: fix code-size issue, add match*, matchv* and match-let*
  which optimize for performance over code-size

Changes - Breaking
---
* Reorganize ClojureScript support. Runtime support ns is now
  cljs.core.match and macro support is cljs.core.match.macros. For
  array specialization you must require clojure.core.match and
  clojure.core.match.array

From 0.2.0-rc2 to 0.2.0-rc3
===

Fixes
---
* MATCH-80: repeated match literal bug

From 0.2.0-rc1 to 0.2.0-rc2
===

Fixes
---
* Fix no match case, don't reevalute expressions

From 0.2.0-beta4 to 0.2.0-rc1
===

None

From 0.2.0-beta3 to 0.2.0-beta4
===

Fixes
---
* MATCH-61: emit init expressions only once
* MATCH-77: `*recur-present*` compilation inconsistent

Enhancements
---
* add `match-let` and `matchv` to ClojureScript support
* array specialization for vector patterns when type hinted

From 0.2.0-beta2 to 0.2.0-beta3
===

Fixes
---
* MATCH-73: irrelevant bindings at leaf nodes
* MATCH-71: non-optimal decision trees for map patterns

Enhancements
---
* Optimize literal matching, don't backtrack just test

====

From 0.2.0-beta1 to 0.2.0-beta2
====

Fixes
---
* Bring CLJS support closer to CLJ

From 0.2.0-alpha12 to 0.2.0-beta1
====

AOT issues should be addressed across the board.

Fixes
---
* MATCH-70 map pattern matching behavior is now more logical,
  specifying a key means it must at least be present even if a
  wildcard
* MATCH-66: cannot match whole value
* MATCH-51: fail to match empty vector after guard
* MATCH-36: no match now throws an exception if no default provided a
  la case
* MATCH-55: seq pattern with just rest pattern fails
* MATCH-56: exception when matching empty vector
* MATCH-68: variant of 55
* MATCH-35: seq pattern matching needed to test `seq`

From 0.2.0-alpha11 to 0.2.0-alpha12
====

Fixes
---
* MATCH-67: fix ClojureScript support regression

From 0.2.0-alpha10 to 0.2.0-alpha11
====

Fixes
---
* MATCH-52: bad map pattern matching behavior

From 0.2.0-alpha9 to 0.2.0-alpha10
====

Breaking Changes
---
* :when is now for predicates. Use :guard for the old behavior of :when.

Fixes
---
* MATCH-62: ClojureScript map-matching should use cljs.core/ILookup, not cljs.core.ILookup
* MATCH-60: Matching maps with :only broken in CLJS

From 0.2.0-alpha8 to 0.2.0-alpha9
====

Fixes
---
* MATCH-43: fix another subtle pattern ordering issue
* MATCH-45: group like patterns together, including vector patterns of different sizes
* MATCH-46: fix :or leakage

Breaking Changes
---
* val-at* -> val-at

Enhancements
---

From 0.2.0-alpha7 to 0.2.0-alpha8
====

Fixes
---
* map patterns with heterogenous keys work now
* MATCH-41: remove sorted-set-by use, this returned a incorrect list of column constructors
* MATCH-42: can now match symbols by quoting them


From 0.2.0-alpha6 to 0.2.0-alpha7
====

Enhancements
---
* remove match-1, passing single value to match now works


From 0.2.0-alpha5 to 0.2.0-alpha6
====

Fixes
----
* MATCH-34: no more infix or pattern syntax
* MATCH-10: support maps with keys of heterogenous types
* MATCH-30: throw if same binding name used in row
* MATCH-33: fix readme typo

Enhancements
---
* supported flattened syntax for :when and :as
* added Steve Miner's match-let
