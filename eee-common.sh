CURR_DIR=$(dirname $(readlink -f $0))


# [M-1]:file;   [M-2]:rg;    [M-3]: imenu;
# [M-4]:symbol; [M-5]:ee-rg; [M-6]: git;
HEADER_KEYBIND_HELP="[F1]: ee-rg, [F2]: ee-find, [F3]: ee-symbols(TODO)"

EEE_RG_SCRIPT=${CURR_DIR}/eee-rg.sh
EEE_FIND_SCRIPT=${CURR_DIR}/eee-find.sh

FZF_BINDS="\
f1:become(${EEE_RG_SCRIPT}),\
f2:become(${EEE_FIND_SCRIPT} {}),\
f3:execute(${EEE_SYMBOL_SCRIPT} {})\
"
# FZF_F1_BIND="f1:become(${EEE_FIND_SCRIPT} {})"
# FZF_F2_BIND="f2:become(${EEE_RG_SCRIPT} {})"

logger -t eee.el -- EEE_F1_BIND=${FZF_F1_BIND}
