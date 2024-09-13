#!/usr/bin/env bash

CURR_DIR=$(dirname $(readlink -f $0))
. ${CURR_DIR}/eee-common.sh

FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude .git --exclude target' \
	fzf \
	--border \
	--color "border:#A15ABD" \
	--header-first \
	--header "CWD:$(pwd)
[Alt-C]:Change Dir(TODO); [RET]: select;
TOOD: extended help information
${HEADER_KEYBIND_HELP}
" \
	--preview 'bat -n --color=always {}' \
	--preview-window 'right,60%,border-bottom,wrap,+{2}+3/3,~3' \
	--bind "${FZF_BINDS}" \
	--bind 'ctrl-/:change-preview-window(down|hidden|)' \
	--bind 'ctrl-d:change-prompt(Directories> )+reload(find * -type d)' \
	--bind 'ctrl-f:change-prompt(Files> )+reload(find * -type f)' \
	--bind 'ctrl-u:change-prompt(Directories> )+reload(find $(dirname $(pwd)) -type d)' |
	xargs -0 -I{} echo $(pwd)/{}
