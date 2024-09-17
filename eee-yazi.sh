#!/usr/bin/env bash

cat /dev/null >/tmp/ee-yazi.tmp
yazi $1 --chooser-file=/tmp/ee-yazi.tmp
cat /tmp/ee-yazi.tmp
