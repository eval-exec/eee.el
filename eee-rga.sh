#!/usr/bin/env bash

CURR_DIR=$(dirname $(readlink -f $0))
. ${CURR_DIR}/eee-common.sh

QUERY_FILE="$1"

RG_PREFIX="rga --no-heading"
local file
FZF_DEFAULT_COMMAND="$RG_PREFIX '' '$QUERY_FILE'" \
	fzf --no-sort --preview="[[ ! -z {} ]] && rga --pretty --context 5 {q} \"${QUERY_FILE}\" | grep :{1}:  " \
	--delimiter : \
	--border \
	--bind "change:reload:$RG_PREFIX {q} \"${QUERY_FILE}\" " \
	--preview-window="60%:wrap"
