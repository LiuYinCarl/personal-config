#!/bin/bash

mkdir -p ~/.emacs.d/plugins

cd ~/.emacs.d/plugins

git clone --depth=1 git@github.com:manateelazycat/awesome-tab.git

git clone --depth=1 git@github.com:tarsius/hl-todo.git

git clone --depth=1 git@github.com:scottjad/parenface.git

git clone --depth=1 git@github.com:redguardtoo/find-file-in-project.git

git clone --depth=1 git@github.com:gonewest818/dimmer.el.git

git clone --depth=1 git@github.com:LiuYinCarl/fzf.el.git

git clone --depth=1 git@github.com:emacs-vs/goto-line-preview.git

# git clone --depth=1 git@github.com:laishulu/hl-fill-column.git

git clone --depth=1 git@github.com:joaotavora/breadcrumb.git

git clone --depth=1 git@github.com:jdtsmith/indent-bars.git
