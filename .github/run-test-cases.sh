#!/bin/sh

for testCase in ./test-cases/*.in; do
    current="$(echo "$testCase" | sed 's/\.\/test-cases\///')"
    output="$(echo "$testCase" | sed 's/\.in/.out/')"

    expected="$(cat $output)"

    if [ -z "$(which "stack")" ]; then
        received="$(cabal run < $testCase)"
    else
        received="$(stack run < $testCase)"
    fi

    if [ "$expected" = "$received" ]; then
        echo "ok   - $current"
    else
        printf "\nERR! - %s\n\n" "$current"
        printf "EXPECTED OUTPUT:\n%s\n\n" "$expected"
        printf "RECEIVED OUTPUT:\n%s\n\n-----------\n\n" "$received"
    fi
done
