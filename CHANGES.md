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
