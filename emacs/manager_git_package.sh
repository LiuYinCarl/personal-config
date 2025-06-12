#!/bin/bash

############################################################
## Emacs Git Package Manage Script                        ##
##   Once you edit this script, use shellcheck to check   ##
##   the changes.                                         ##
############################################################

############################################################
## User Config                                            ##
############################################################

# where you want to save the git repos
repo_dir="${HOME}/.emacs.d/plugins"

# which repos you want to manage
repos=(
    "git@github.com:manateelazycat/awesome-tab.git"       \
    "git@github.com:tarsius/hl-todo.git"                  \
    "git@github.com:scottjad/parenface.git"               \
    "git@github.com:redguardtoo/find-file-in-project.git" \
    "git@github.com:gonewest818/dimmer.el.git"            \
    "git@github.com:emacs-vs/goto-line-preview.git"       \
    "git@github.com:joaotavora/breadcrumb.git"            \
    "git@github.com:jdtsmith/indent-bars.git"             \
    "git@github.com:manateelazycat/auto-save.git"         \
    "git@github.com:manateelazycat/delete-block.git"      \
    "git@github.com:nverno/llvm-mode.git"                 \
    "git@github.com:eval-exec/super-hint.el.git"          \
    "git@github.com:jart/disaster.git"
)

############################################################
## Function                                               ##
############################################################

function update_current_dir_repos() {
    git_repo_cnt=0

    for dir in *; do
        if [ -d "${dir}" ]; then
            cd "${dir}" || exit
            if [ -d ".git" ]; then
                ((git_repo_cnt += 1))
                cur_dir=$(pwd)
                echo "[${git_repo_cnt}] start pull ${cur_dir}..."
                time git pull
            fi
            cd ..
        fi
    done
}

function list_current_dir_repos() {
   for dir in *; do
       if [ -d "${dir}" ]; then
           cd "${dir}" || exit
           if [ -d ".git" ]; then
               git remote -v | head -n 1 | cut -f 2 | cut -d " " -f 1
           fi
       fi
       cd ..
   done
}

function run() {
    if [ "${1}" = "ls" ]; then
        dir_path=$(pwd)
        dir_size=$(du -sh | cut -f 1)
        echo "repo_dir_path: ${dir_path}"
        echo "repo_dir_size: ${dir_size}"
        echo ""
        ls
    elif [ "${1}" = "backup" ]; then
        echo "start backup..."
        name=repo_backup_$(date +%Y%m%d-%H%M%S).tar.gz
        tar -czf "${name}" ./
        printf "backup file: $%s/%s\n" "$(pwd)" "${name}"
    elif [ "${1}" = "clear" ]; then
        rm repo_backup_*
    elif [ "${1}" = "clone" ]; then
        for repo in "${repos[@]}"
        do
            git clone --depth=1 "${repo}"
        done
    elif [ "${1}" = "pull" ]; then
        update_current_dir_repos
    elif [ "${1}" = "list" ]; then
        list_current_dir_repos
    else
        echo "Script Args:"
        echo "    ls      list repo directory"
        echo "    backup  backup repo directory"
        echo "    clear   remove all backup"
        echo "    clone   clone all uninstalled repo"
        echo "    pull    update all installed repo"
        echo "    list    list all repo's remove path"
    fi
}

############################################################
## Start                                                  ##
############################################################

mkdir -p "${repo_dir}"
cd "${repo_dir}" || exit

printf "\e[32m" # set color

run "$@"

printf "\e[0m"  # reset color

