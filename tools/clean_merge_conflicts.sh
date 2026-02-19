#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
sed -i -e '/<<<<<<< HEAD/,/=======/d' -e '/>>>>>>> fc3ebfd (chore: drop unused import in iterator)/d' "$1"