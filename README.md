match
====

An optimized pattern match and predicate dispatch library for Clojure. Currently the library only implements pattern matching. It supports Clojure 1.2.0 and later as well as ClojureScript.

You can find more detailed information [here](https://github.com/clojure/core.match/wiki/Overview).

Alpha Status
----
It's important to note that core.match is alpha quality software. There are many known issues. In particular if your project depends on AOT do not use core.match at this time. Patches for any and all issues welcome!

Releases and dependency information
----

Latest alpha: 0.2.0-alpha10

* [All released versions](http://search.maven.org/#search%7Cgav%7C1%7Cg%3A%22org.clojure%22%20AND%20a%3A%22core.match%22)

[Leiningen](http://github.com/technomancy/leiningen/) dependency information:

```
[org.clojure/core.match "0.2.0-alpha10"]
```

[Maven](http://maven.apache.org) dependency information:

```
<dependency>
  <groupId>org.clojure</groupId>
  <artifactId>core.match</artifactId>
  <version>0.2.0-alpha10</version>
</dependency>
```

Example Usage
----

From Clojure:

```clojure
(use '[match.core :only (match)])

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
  (:use-macros [clojure.core.match.js :only [match]]))

(doseq [n (range 1 101)]
  (println
    (match [(mod n 3) (mod n 5)]
      [0 0] "FizzBuzz"
      [0 _] "Fizz"
      [_ 0] "Buzz"
      :else n)))
```

Developer information
----

* [Bug Tracker](http://dev.clojure.org/jira/browse/MATCH)
* [Continuous Integration](http://build.clojure.org/job/core.match/)
* [Compatibility Test Matrix](http://build.clojure.org/job/core.match-test-matrix/)

Copyright and license
----

Copyright © 2010-2012 David Nolen, Rich Hickey & contributors.

Licensed under the EPL (see the file epl.html).
