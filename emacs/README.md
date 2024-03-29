## 使用本 Emacs 配置的方式

1. 将本目录下的 `.emacs` 文件复制到 `~/` 目录下
2. 执行本目录下的 `install_github_package.sh` 脚本，用于安装部分不在 melpa 上的包
3. 启动 Emacs，自动安装 melpa 上的包


## 快捷键

C-x C-b 打开 *Buffer List* 缓冲区

C-x h 全选
C-w 删除




## 部分插件的介绍和用法

[column-marker](https://www.emacswiki.org/emacs/column-marker.el)


## 将 Emacs 设置为后台进程

在使用的 shell 的配置文件中添加如下配置(bash 在  ~/.bashrc, fish 在 ~/.config/fish/config.fish)

``` bash

alias em="emacsclient -t"
export ALTERNATE_EDITOR="" 

```

在之后使用 em 命令即可打开 Emacs，shell 会检查有没有启动Emacs的daemon模式，
没有启动的话会自动调用, 然后打开 Emacs 的 client 端， 大大加速了Emacs的启动速度。
[我的EMACS设置](https://zhuanlan.zhihu.com/p/114329781)

如果需要关闭 Emacs 后台进程，使用 `M-x save-buffers-kill-emacs` 命令

## elisp 资料

[Emacs Lisp 简明教程](http://smacs.github.io/elisp/)


## 安装 company

1. 先将 .emacs 复制到系统，启动 emacs, 自动替换源
2. `M-x package-list-package` 搜索 company 插件



## Emacs 内置 mode 参考

[Emacs builtin mode 功能介绍](https://emacs-china.org/t/emacs-builtin-mode/11937/68)
