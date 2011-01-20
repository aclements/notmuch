#!/bin/bash
test_description="query parser"
. ./test-lib.sh

EXPECTED=../qparser.expected-output

test_begin_subtest "Quoted phrases"
output=$(../qparser-test < $EXPECTED/quoted-phrases)
expected=$(cat $EXPECTED/quoted-phrases)
test_expect_equal "$output" "$expected"

test_begin_subtest "Prefixes"
output=$(../qparser-test < $EXPECTED/prefixes)
expected=$(cat $EXPECTED/prefixes)
test_expect_equal "$output" "$expected"

test_begin_subtest "Terms"
output=$(../qparser-test < $EXPECTED/terms)
expected=$(cat $EXPECTED/terms)
test_expect_equal "$output" "$expected"

test_begin_subtest "Operators"
output=$(../qparser-test < $EXPECTED/operators)
expected=$(cat $EXPECTED/operators)
test_expect_equal "$output" "$expected"

test_begin_subtest "Probs"
output=$(../qparser-test < $EXPECTED/probs)
expected=$(cat $EXPECTED/probs)
test_expect_equal "$output" "$expected"

test_done
