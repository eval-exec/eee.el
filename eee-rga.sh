#!/usr/bin/env bash

CURR_DIR=$(dirname $(readlink -f $0))
. ${CURR_DIR}/eee-common.sh

EE_QUERY_FILE="$1"

function prepend_file_name() {
	if [[ "$2" =~ ^Page* ]]; then
		printf "%s:%s" "${1}" "${2}"
	else
		echo "${2}"
	fi
}

export -f prepend_file_name
export EE_QUERY_FILE

RG_PREFIX="rga --no-heading"
FZF_DEFAULT_COMMAND="$RG_PREFIX '' '$EE_QUERY_FILE'" \
	fzf --no-sort \
	--border \
	--bind "change:reload:$RG_PREFIX {q} \"${EE_QUERY_FILE}\" " |
	xargs -I{} bash -c 'prepend_file_name "${EE_QUERY_FILE}" "{}"'
