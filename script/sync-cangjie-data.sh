#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SOURCE_URL="https://raw.githubusercontent.com/rime-aca/rime-cangjie6/master/cangjie6.dict.yaml"

mkdir -p "$ROOT/data"
curl -L "$SOURCE_URL" -o "$ROOT/data/cangjie6.dict.yaml"
cd "$ROOT"
clojure -M:generate-data
