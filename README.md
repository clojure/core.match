match
====

An optimized pattern matching library for Clojure. It supports Clojure
1.5.1 and later as well as ClojureScript.

You can find more detailed information
[here](https://github.com/clojure/core.match/wiki/Overview).

Releases and dependency information
----

This project follows the version scheme MAJOR.MINOR.PATCH where each component provides some relative indication of the size of the change, but does not follow semantic versioning. In general, all changes endeavor to be non-breaking (by moving to new names rather than by breaking existing names).

Latest release: 1.0.0

* [All released versions](http://search.maven.org/#search%7Cgav%7C1%7Cg%3A%22org.clojure%22%20AND%20a%3A%22core.match%22)

[CLI/`deps.edn`](https://clojure.org/reference/deps_and_cli) dependency information:
```clojure
org.clojure/core.match {:mvn/version "1.0.0"}
```

[Leiningen](http://github.com/technomancy/leiningen/) dependency information:

```
[org.clojure/core.match "1.0.0"]
```

[Maven](http://maven.apache.org) dependency information:

```
<dependency>
  <groupId>org.clojure</groupId>
  <artifactId>core.match</artifactId>
  <version>1.0.0</version>
</dependency>
```

Example Usage
----

From Clojure:

```clojure
(require '[clojure.core.match :refer [match]])

(doseq [n (range 1 101)]
  (println
    (match [(mod n 3) (mod n 5)]
      [0 0] "FizzBuzz"
      [0 _] "Fizz"
      [_ 0] "Buzz"
      :else n)))
```

From ClojureScript:

```clojure
(ns foo.bar
  (:require [cljs.core.match :refer-macros [match]]))

(doseq [n (range 1 101)]
  (println
    (match [(mod n 3) (mod n 5)]
      [0 0] "FizzBuzz"
      [0 _] "Fizz"
      [_ 0] "Buzz"
      :else n)))
```

For more detailed descriptions of usage please refer to the
[wiki](http://github.com/clojure/core.match/wiki).

Developer information
----

* [Bug Tracker](https://clojure.atlassian.net/projects/MATCH)
* [Continuous Integration](http://build.clojure.org/job/core.match/)
* [Compatibility Test Matrix](http://build.clojure.org/job/core.match-test-matrix/)

Copyright and license
----

Copyright © 2010-2020 David Nolen, Ambrose Bonnaire-Sergeant, Rich
Hickey & contributors.

Licensed under the EPL (see the file epl.html).
