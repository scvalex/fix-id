#!/bin/bash

set -e

INSTALL='install -o scvalex -g staff'

echo "Installing Fix-Id..."

echo "  * Creating directories"
${INSTALL} -d /var/lib/fix_id/
${INSTALL} -d /var/log/fix_id/

echo "Done"
