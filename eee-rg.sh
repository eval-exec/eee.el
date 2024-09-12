#!/usr/bin/env bash
set -e

CURR_DIR=$(dirname $(readlink -f $0))
. ${CURR_DIR}/eee-common.sh

# Switch between Ripgrep mode and fzf filtering mode (CTRL-T)
rm -f /tmp/rg-fzf-{r,f}
RG_PREFIX="rg --column --line-number --no-heading --color=always --smart-case "
INITIAL_QUERY="${*:-}"
fzf --ansi --disabled --query "$INITIAL_QUERY" \
	--border \
	--bind "start:reload:$RG_PREFIX {q}" \
	--bind "change:reload:sleep 0.01; $RG_PREFIX {q} || true" \
	--bind "${FZF_BINDS}" \
	--bind 'ctrl-t:transform: [[ ! $FZF_PROMPT =~ ripgrep ]] &&
      echo "rebind(change)+change-prompt(1. ripgrep> )+disable-search+transform-query:echo \{q} > /tmp/rg-fzf-f; cat /tmp/rg-fzf-r" ||
      echo "unbind(change)+change-prompt(2. fzf> )+enable-search+transform-query:echo \{q} > /tmp/rg-fzf-r; cat /tmp/rg-fzf-f"' \
	--color "hl:-1:underline,hl+:-1:underline:reverse,border:#A15ABD" \
	--prompt '1. ripgrep> ' \
	--delimiter : \
	--header-first \
	--header " CWD:$(pwd)
[Ctrl-t]: Switch between ripgrep/fzf
${HEADER_KEYBIND_HELP}
" \
	--layout=reverse-list \
	--preview 'bat --color=always {1} --highlight-line {2}' \
	--preview-window 'up,70%,border-bottom,+{2}+3/3,~3' |
	xargs -0 -I{} echo $(pwd)/{}
#    --bind 'enter:become(vim {1} +{2})'
