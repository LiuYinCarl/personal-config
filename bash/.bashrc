############################################################
## global settings
############################################################
export LESSCHARSET=utf-8 # 确保分页工具显示 UTF-8
export EDITOR='emacs -nw'
# export TERM=xterm-256color
export TERM=screen-256color # for tmux

############################################################
## tmux settings
############################################################
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

############################################################
## z.lua alais
############################################################
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

############################################################
## common basic alias
############################################################
function show_dir_info () {
    dir_cnt=$(ls -l | grep -c "^d")
    file_cnt=$(ls -l | grep -c "^-")
    dir_size=$(du -sh | cut -f 1)
    newest=$(ls -ltrF | tail -n 1)
    printf "\e[32m %-12s: $dir_cnt  \n\e[0m" "dir count"
    printf "\e[32m %-12s: $file_cnt \n\e[0m" "file count"
    printf "\e[32m %-12s: $dir_size \n\e[0m" "dir size"
    printf "\e[32m %-12s: $newest   \n\e[0m" "newest"
    printf "\e[32m\n biggest sub dirs:\n"
    du -sh */ | sort -hr | head -n 3 | awk '{printf "    %-20s: %s\n", $2, $1}'
    printf "\e[0m\n"
}
alias c='clear'
alias e='emacs -nw ~/.emacs'
alias s='source ~/.bashrc'
alias di='show_dir_info'

############################################################
## software alias
############################################################
alias bat="batcat"
alias cl='rlwrap sbcl'
alias em='emacs -nw'
alias ec='cat ~/.emacs.d/.tmp_copy_region'
alias g='glances'
alias gs='git status'
alias gb='git branch'
alias gcp='git commit -m "update" && git push'
alias gu="guile --no-auto-compile"
alias cz="chezscheme"
alias cs="chezscheme --script"
alias rgf="rg -F" # 不使用正则
alias g++v="g++ -dM -E -x c++  /dev/null | grep -F __cplusplus"
alias clang++v="clang++ -dM -E -x c++  /dev/null | grep -F __cplusplus"
alias f="fzf --preview 'bat --color always {}' --bind 'enter:execute(emacs -nw {})'"

# Rust Toolchain
alias cloc="tokei"
alias delta="delta --side-by-side --syntax-theme 1337"
alias tspin="tspin" # tool to highlight log
alias cat=batcat
alias b="basilk" # 终端看板
alias ls="lla -g"
alias hex="hex-patch" # 二进制查看
# skim fzf 平替
alias sk="sk --preview 'batcat --color always {}' --bind 'enter:execute(emacs -nw {})'"
alias y="yazi"

# OCaml Toolchain
alias ocb="ocamlc -dinstr"

############################################################
## local machine alias
############################################################
