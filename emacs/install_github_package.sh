#!/bin/bash

mkdir -p ~/.emacs.d/plugins

cd ~/.emacs.d/plugins

git clone --depth=1 git@github.com:manateelazycat/awesome-tab.git

git clone --depth=1 git@github.com:tarsius/hl-todo.git

git clone --depth=1 git@github.com:scottjad/parenface.git
