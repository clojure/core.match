match
====

An optimized pattern match and predicate dispatch library for Clojure. Currently the library only implements pattern matching. It supports Clojure 1.2.0 and later as well as ClojureScript.

You can find more detailed information [here](https://github.com/clojure/core.match/wiki/Overview).

Releases and dependency information
----

Latest beta: 0.2.0-beta2

* [All released versions](http://search.maven.org/#search%7Cgav%7C1%7Cg%3A%22org.clojure%22%20AND%20a%3A%22core.match%22)

[Leiningen](http://github.com/technomancy/leiningen/) dependency information:

```
[org.clojure/core.match "0.2.0-beta2"]
```

[Maven](http://maven.apache.org) dependency information:

```
<dependency>
  <groupId>org.clojure</groupId>
  <artifactId>core.match</artifactId>
  <version>0.2.0-beta2</version>
</dependency>
```

Example Usage
----

From Clojure:

```clojure
(use '[clojure.core.match :only (match)])

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
  (:use-macros [clojure.core.match.js :only [match]])
  (:require [clojure.core.match]))

(doseq [n (range 1 101)]
  (println
    (match [(mod n 3) (mod n 5)]
      [0 0] "FizzBuzz"
      [0 _] "Fizz"
      [_ 0] "Buzz"
      :else n)))
```

For more detailed description of usage please refer to the [wiki](http://github.com/clojure/core.match/wiki).

Developer information
----

* [Bug Tracker](http://dev.clojure.org/jira/browse/MATCH)
* [Continuous Integration](http://build.clojure.org/job/core.match/)
* [Compatibility Test Matrix](http://build.clojure.org/job/core.match-test-matrix/)

Copyright and license
----

Copyright © 2010-2013 David Nolen, Rich Hickey & contributors.

Licensed under the EPL (see the file epl.html).
