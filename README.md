match
====

An optimized pattern matching library for Clojure. It supports Clojure
1.5.1 and later as well as ClojureScript.

You can find more detailed information
[here](https://github.com/clojure/core.match/wiki/Overview).

Releases and dependency information
----

Latest release: 0.3.0

* [All released versions](http://search.maven.org/#search%7Cgav%7C1%7Cg%3A%22org.clojure%22%20AND%20a%3A%22core.match%22)

[Leiningen](http://github.com/technomancy/leiningen/) dependency information:

```
[org.clojure/core.match "0.3.0"]
```

[Maven](http://maven.apache.org) dependency information:

```
<dependency>
  <groupId>org.clojure</groupId>
  <artifactId>core.match</artifactId>
  <version>0.3.0</version>
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

* [Bug Tracker](http://dev.clojure.org/jira/browse/MATCH)
* [Continuous Integration](http://build.clojure.org/job/core.match/)
* [Compatibility Test Matrix](http://build.clojure.org/job/core.match-test-matrix/)

Copyright and license
----

Copyright © 2010-2019 David Nolen, Ambrose Bonnaire-Sergeant, Rich
Hickey & contributors.

Licensed under the EPL (see the file epl.html).
