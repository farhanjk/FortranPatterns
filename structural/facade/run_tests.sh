#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

gfortran_cmd=$(command -v gfortran || true)
if [[ -z "$gfortran_cmd" ]]; then
  echo "gfortran is required to run the facade test." >&2
  exit 1
fi

temp_dir=$(mktemp -d)
trap 'rm -rf "${temp_dir}"' EXIT

${gfortran_cmd} -std=f2008 -Wall -Wextra -pedantic -fimplicit-none \
  "${SCRIPT_DIR}/facade_module.f90" "${SCRIPT_DIR}/facade_main.f90" \
  -J"${temp_dir}" -o "${temp_dir}/facade_demo"

"${temp_dir}/facade_demo" > "${temp_dir}/output.txt"

diff -u "${SCRIPT_DIR}/expected_output.txt" "${temp_dir}/output.txt"

echo "Facade pattern output matches expected lines."
