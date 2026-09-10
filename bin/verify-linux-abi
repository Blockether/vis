#!/usr/bin/env bash
# Reject release ELF objects requiring a libc newer than Ubuntu 22.04 (glibc 2.35).
# Inspect every file, including workers, libpython and Python extension modules.
set -euo pipefail
export LC_ALL=C
(( $# > 0 )) || { echo "usage: $0 <file-or-directory>..." >&2; exit 2; }
command -v readelf >/dev/null || { echo 'binutils/readelf is required' >&2; exit 2; }
checked=0
for input in "$@"; do
  [[ -e "$input" ]] || { echo "missing ABI input: $input" >&2; exit 1; }
  while IFS= read -r -d '' file; do
    magic="$(od -An -tx1 -N4 "$file" | tr -d '[:space:]')"
    [[ "$magic" == 7f454c46 ]] || continue
    info="$(readelf --version-info --wide "$file")"
    checked=$((checked + 1))
    while IFS= read -r version; do
      [[ -n "$version" ]] || continue
      if [[ "$version" =~ ^GLIBC_([0-9]+)\.([0-9]+)(\.[0-9]+)?$ ]] &&
         (( BASH_REMATCH[1] < 2 || (BASH_REMATCH[1] == 2 && BASH_REMATCH[2] < 35) ||
            (BASH_REMATCH[1] == 2 && BASH_REMATCH[2] == 35 && ${#BASH_REMATCH[3]} == 0) )); then
        continue
      fi
      echo "$file requires $version; Linux releases support glibc 2.35" >&2
      exit 1
    done < <(printf '%s\n' "$info" | sed -nE 's/.*Name: (GLIBC_[^[:space:]]+).*/\1/p' | sort -u)
  done < <(find "$input" -type f -print0)
done
(( checked > 0 )) || { echo 'no ELF files found' >&2; exit 1; }
printf 'glibc 2.35 ABI check passed: %s ELF files\n' "$checked"
