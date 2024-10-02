#!/usr/bin/env bash

CURR_DIR=$(dirname $(readlink -f $0))
. ${CURR_DIR}/eee-common.sh

QUERY_FILE="$1"

RG_PREFIX="rg --column --line-number --no-heading --color=always --smart-case "

# INITIAL_QUERY="${*:-}"
INITIAL_QUERY=""

fzf --ansi --disabled --query "$INITIAL_QUERY" \
	--border \
	--bind "start:reload:$RG_PREFIX {q} '${QUERY_FILE}'" \
	--bind "change:reload:sleep 0.01; $RG_PREFIX {q} '${QUERY_FILE}' || true" \
	--delimiter : \
	--preview "bat --color=always '${QUERY_FILE}' --highlight-line {1}" \
	--preview-window 'up,60%,border-bottom,+{1}+3/3,~3' \
	--layout=reverse-list | xargs -0 -I{} echo "${QUERY_FILE}":{}
