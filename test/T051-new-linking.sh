#!/usr/bin/env bash
test_description='"notmuch new" thread linking'

. ./test-lib.sh

test_begin_subtest "All four-message threads get linked in all delivery orders"
# Generate all possible single-root four message thread structures.
# Each line in THREADS is a thread structure, where the n'th field is
# the parent of message n.
THREADS=$(python -c '
def mkTrees(free, tree={}):
    if not free:
        print(" ".join(map(str, [msg[1] for msg in sorted(tree.items())])))
        return
    # Attach each free message to each message in the tree (if there is
    # no tree, make the free message the root)
    for msg in sorted(free):
        parents = sorted(tree.keys()) if tree else ["none"]
        for parent in parents:
            ntree = tree.copy()
            ntree[msg] = parent
            mkTrees(free - set([msg]), ntree)
mkTrees(set(range(4)))')
for ((n = 0; n < 4; n++)); do
    # Deliver the n'th message of every thread
    thread=0
    while read -a parents; do
        parent=${parents[$n]}
        generate_message \
            [id]=m$n@t$thread [in-reply-to]="\<m$parent@t$thread\>" \
            [subject]=p$thread [from]=m$n
        thread=$((thread + 1))
    done <<< "$THREADS"
    notmuch new > /dev/null
done
output=$(notmuch search '*' | notmuch_search_sanitize)
nthreads=$(wc -l <<< "$THREADS")
expected=$(for ((i = 0; i < $nthreads; i++)); do
        echo "thread:XXX   2001-01-05 [4/4] m3, m2, m1, m0; p$i (inbox unread)"
    done)
test_expect_equal "$output" "$expected"

test_done
