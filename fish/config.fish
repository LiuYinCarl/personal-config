if status is-interactive
    # Commands to run in interactive sessions can go here
end

function fish_prompt
    # 显示当前目录
    set_color blue
    echo -n (prompt_pwd)
    set_color normal

    # 显示一个分隔符号
    set_color yellow
    echo -n " > "
    set_color normal
end

function em
    emacs -nw $argv
end

function c
    clear
end

function f
    fzf --preview 'bat --color always {}' --bind 'enter:execute(emacs -nw {})'
end

function y
    yazi
end