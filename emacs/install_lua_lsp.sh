#!/bin/bash

# Install lua-language-server for emacs.

dir="${HOME}/.emacs.d/plugins/lua-lsp"

mkdir -p "${dir}"

cd "${dir}"

wget https://github.com/LuaLS/lua-language-server/releases/download/3.14.0/lua-language-server-3.14.0-linux-x64.tar.gz

tar -zxf lua-language-server-3.14.0-linux-x64.tar.gz
