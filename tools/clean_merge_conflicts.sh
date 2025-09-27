#!/bin/bash
sed -i -e '/<<<<<<< HEAD/,/=======/d' -e '/>>>>>>> fc3ebfd (chore: drop unused import in iterator)/d' "$1"