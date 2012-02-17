#!/bin/bash

set -e

HOST="$1"
[ -n "${HOST}" ] || { echo "Usage: $0 <hostname>"; exit 1; }

[ -d "rel/fix_id" ] || { echo 'No release found; run `make release` first'; exit 1; }

echo "Deploying to ${HOST}"

echo "  * Cleaning deploy directory"
ssh "${HOST}" 'sudo rm -rf /src/sites/fix_id'

echo "  * Copying files"
scp -r rel/fix_id/ "${HOST}:/src/sites/"
scp fix_id.init "${HOST}:/src/sites/fix_id/"

echo "  * Restarting application"
ssh "${HOST}" 'sudo /src/sites/fix_id/fix_id.init restart'

echo "  * Done"
