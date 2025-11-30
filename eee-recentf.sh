cat /tmp/ee-recentf-list.txt | devicon-lookup -i -c | \
    fzf \
        --reverse \
        --border \
        --exact \
        --ansi \
        --cycle \
        --no-sort \
        --color "border:#A15ABD" \
        --preview 'filename={}; bash -c "bat -n --color=always ${filename:2}"' \
        --preview-window 'right,60%,border-bottom,wrap,+{2}+3/3,~3' \
        --bind 'ctrl-f:page-down,ctrl-b:page-up' |
    xargs -0 -I{} bash -c 'filename_with_icon="{}"; filename="${filename_with_icon:2}"; eval echo "${filename}"'
