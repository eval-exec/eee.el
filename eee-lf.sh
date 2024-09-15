#!/usr/bin/env bash

cat /dev/null >/tmp/ee-lf.tmp
lf -print-selection >/tmp/ee-lf.tmp
cat /tmp/ee-lf.tmp
