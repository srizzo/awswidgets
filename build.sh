#!/usr/bin/env bash

set -Eeuo pipefail

unset GITHUB_TOKEN
R --vanilla --quiet --no-echo -e 'devtools::document()'
R --vanilla --quiet --no-echo -e 'devtools::install()'

