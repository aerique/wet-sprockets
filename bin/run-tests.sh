#!/bin/sh

echo "=== Running Unit Tests ==="
echo
echo "--- running tests ---"
sbcl --noinform --load tests/run-tests.lisp --no-linedit --no-swank;
