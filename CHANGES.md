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
