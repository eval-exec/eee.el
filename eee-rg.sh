#!/usr/bin/env bash

export TEMP_RG_PAT=$(mktemp -u)
export TEMP_FLAGS=$(mktemp -u)
trap 'rm -f "$TEMP_RG_PAT"' EXIT
trap 'rm -f "$TEMP_FLAGS"' EXIT

CURR_DIR=$(dirname $(readlink -f $0))
. ${CURR_DIR}/eee-common.sh
EE_REGEX=${CURR_DIR}/eee-rich-regex.sh

check_tools fzf bat rg

# Switch between Ripgrep mode and fzf filtering mode (CTRL-T)
rm -f /tmp/rg-fzf-{r,f}

INITIAL_QUERY="$1"

export QUERY_PATH="${2:-.}"

function rg_align() {
    while IFS= read -r line; do
        prefix=$(printf '%s' "$line" | cut -d: -f1-3)
        rest=$(printf '%s' "$line" | cut -d: -f4-)
        printf '%-80s' "$prefix"
        [[ -n $rest ]] && printf ':%s\n' "$rest" || printf '\n'
    done

}


TRANSFORMER='
  rg_pat={q:1}
  fzf_pat={q:2..}
  flags=$(cat ${TEMP_FLAGS})

  # if flags contains --fixed-strings, then set rg_pat to be literal, set fzf_pat to be empty
  if grep -q -- "--fixed-strings" <<< "$flags"; then
        rg_pat={q}
        fzf_pat=""
  fi

  rg_full_pat=${rg_pat}${flags}
  
  if ! [[ -r "$TEMP_RG_PAT" ]] || [[ $rg_full_pat != $(cat "$TEMP_RG_PAT") ]]; then
    echo "$rg_full_pat" > "$TEMP_RG_PAT"
    printf "reload:sleep 0.01; rg --hidden --no-ignore-dot --column --line-number --with-filename --no-heading --color=always --smart-case %s %q %q || true" "${flags}" "${rg_pat}" "${QUERY_PATH}"
  fi
  echo "+search:$fzf_pat"
'

# Generic function to toggle a flag in TEMP_FLAGS
function toggle_flag() {
    local flag="$1"
    if grep -q -- " --${flag}" "$TEMP_FLAGS"; then
        sed -i "s/ --${flag}//g" "$TEMP_FLAGS"
    else
        echo -n " --${flag}" >>"$TEMP_FLAGS"
    fi
    touch "$TEMP_FLAGS"
    logger "Flags: $(cat $TEMP_FLAGS)"
}

export -f toggle_flag

# Wrapper functions for specific toggles
function toggle_word_rexp() {
    toggle_flag "word-regexp"
}

export -f toggle_word_rexp

function toggle_case_sensitive() {
    toggle_flag "case-sensitive"
}

export -f toggle_case_sensitive

function toggle_fixed_strings() {
    toggle_flag "fixed-strings"
}

export -f toggle_fixed_strings

function read_input_label() {
    logger "input-label:" "$(cat ${TEMP_FLAGS})"
    printf " %s " "$(cat ${TEMP_FLAGS})"
}

export -f read_input_label


$FZF --ansi --disabled --query "$INITIAL_QUERY" \
    --delimiter : --nth 3.. \
    --border \
    --input-border \
    --list-border \
    --info=inline-right \
    --reverse \
    --exact \
    --cycle \
    --with-shell 'bash -c' \
    --bind "start:transform:$TRANSFORMER" \
    --bind "change:transform:$TRANSFORMER" \
    --color "hl:-1:underline,hl+:-1:underline:reverse,border:#A15ABD" \
    --preview "$BAT"' --color=always {1} --highlight-line {2}' \
    --preview-window 'up,70%,+{2}+3/3,~3' \
    --bind "alt-w:execute-silent(toggle_word_rexp)+transform-list-label(read_input_label)+transform:${TRANSFORMER}" \
    --bind "alt-c:execute-silent(toggle_case_sensitive)+transform-list-label(read_input_label)+transform:${TRANSFORMER}" \
    --bind "alt-f:execute-silent(toggle_fixed_strings)+transform-list-label(read_input_label)+transform:${TRANSFORMER}" \
    --bind 'ctrl-f:page-down,ctrl-b:page-up' |
    xargs -0 -I{} echo $(pwd)/{}
