.PHONY: test main jar clean

test:
	clojure -T:build test

main:
	clojure -M:run-main

jar:
	clojure -T:build ci

clean:
	clojure -T:build clean
