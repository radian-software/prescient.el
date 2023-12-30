#!/usr/bin/env bash

set -e
set -o pipefail

find=(
    find .
    -name .git -prune -o
    -name "*.elc" -o
    -name "*.png" -o
    -name prescient-test.el -o
    -type f -print
)

readarray -t files < <("${find[@]}" | sed 's#./##' | sort)

# Don't print the line length if:
# - If it is the first line
# - It is the Requirements line
# - The line contains a URL
code="$(cat <<"EOF"

((NR > 1) \
 && ($0 !~ /^;; Package-Requires:/) \
 && (length($0) >= 80) \
 && ($0 !~ /https?:\/\//)) \
{ printf "%s:%d: %s\n", FILENAME, NR, $0 }

EOF
)"

for file in "${files[@]}"; do
    echo "[longlines] $file" >&2
    awk "$code" "$file"
done | (! grep .)
