# basic
alias c='clear'

# color
# export TERM=xterm-256color
export TERM=screen-256color # for tmux


# tmux alias
function tmux_color_ls() {
    if [ -n "$TMUX" ]; then
        cur_session=$(tmux display-message -p "#S")
        tmux ls | sed "s/^\($cur_session:\)/$(printf '\e[32m')\1/" | sed "s/$/$(printf '\e[0m')/"
    else
        tmux ls
    fi
}

alias tmls='tmux_color_ls'
alias tmnew='tmux new -s'
alias tmkill='tmux kill-session -t'
alias tmat='tmux attach-session -t'
alias tmde='tmux detach'
alias tmex='exit'

# Emacs
alias em='emacs -nw'
alias emc='emacsclient -t -a ""'
alias semc='sudo emacsclient -t -a ""'
alias ec='cat ~/.emacs.d/.tmp_copy_region'

# Common Lisp
alias cl='rlwrap sbcl'

# x86 as & ld
alias as32='as --32 -g'
alias ld32='ld -m elf_i386 -dynamic-linker /lib32/ld-2.31.so -lc'

# color cat
alias bat="batcat"

# git
alias gs='git status'

# glances
alias g='glances'

# Ocaml Opam Environment
eval $(opam env)

# z foo       # 跳转到包含 foo 并且权重（Frecent）最高的路径
# z foo bar   # 跳转到同时包含 foo 和 bar 并且权重最高的路径
# z -r foo    # 跳转到包含 foo 并且访问次数最高的路径
# z -t foo    # 跳转到包含 foo 并且最近访问过的路径
# z -l foo    # 不跳转，只是列出所有匹配 foo 的路径
# z -c foo    # 跳转到包含 foo 并且是当前路径的子路径的权重最高的路径
# z -e foo    # 不跳转，只是打印出匹配 foo 并且权重最高的路径
# z -i foo    # 进入交互式选择模式，让你自己挑选去哪里（多个结果的话）
# z -I foo    # 进入交互式选择模式，但是使用 fzf 来选择
# z -b foo    # 跳转到父目录中名称以 foo 开头的那一级
# z --          # 显示当前的 dir stack
# z -2          # cd 到栈里 2 号路径
# z -           # 弹出栈顶 (cd 到上一次的老路径)，和 "z -0" 相同
eval "$(lua ~/github/z.lua/z.lua --init bash fzf)"
alias zf="z -I"
alias zb="z -b"

# guile
alias gu="guile --no-auto-compile"

# chez
alias cz="chezscheme"
alias cs="chezscheme --script"
