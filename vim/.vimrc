syntax on "语法高亮
" set mouse=a  "鼠标可用
set number "显示行号
" 开启实时搜索功能
set incsearch
" 高亮搜索
set hlsearch
" 搜索时大小写不敏感
set ignorecase
set number

call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree'
"关闭NERDTree快捷键
map <leader>t :NERDTreeToggle<CR>
""当NERDTree为剩下的唯一窗口时自动关闭
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
"修改树的显示图标
let g:NERDTreeDirArrowExpandable = '►'
let g:NERDTreeDirArrowCollapsible = '▼'
let NERDTreeAutoCenter=1
"是否显示隐藏文件
let NERDTreeShowHidden=1
"设置宽度
let NERDTreeWinSize=25
"在终端启动vim时，共享NERDTree
let g:nerdtree_tabs_open_on_console_startup=1
"忽略一下文件的显示
let NERDTreeIgnore=['\.pyc','\~$','\.swp']
let g:NERDTreeIndicatorMapCustom = {
            \ "Modified"  : "M",
            \ "Staged"    : "S",
            \ "Untracked" : "U",
            \ "Renamed"   : "➜",
            \ "Unmerged"  : "═",
            \ "Deleted"   : "D",
            \ "Dirty"     : "✗",
            \ "Clean"     : "✔︎",
            \ 'Ignored'   : 'I',
            \ "Unknown"   : "?"
            \ }


Plug 'w0rp/ale'
let g:ale_set_highlights = 0
"保持侧边栏可见
let g:ale_sign_column_always = 1
"自定义error和warning图标
let g:ale_sign_error = '✗'
let g:ale_sign_warning = '⚡'
"在vim自带的状态栏中整合ale
let g:ale_statusline_format = ['✗ %d', '⚡ %d', '✔ OK']
"显示Linter名称,出错或警告等相关信息
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
"打开文件时不进行检查
let g:ale_lint_on_enter = 0
"普通模式下，sp前往上一个错误或警告，sn前往下一个错误或警告
nmap sp <Plug>(ale_previous_wrap)
nmap sn <Plug>(ale_next_wrap)
"<Leader>s触发/关闭语法检查
nmap <Leader>s :ALEToggle<CR>
"<Leader>d查看错误或警告的详细信息
nmap <Leader>d :ALEDetail<CR>
"使用clang对c和c++进行语法检查，对python使用pylint进行语法检查
let g:ale_linters = {
            \   'c++': ['clang'],
            \   'c': ['clang'],
            \   'python': ['pylint'],
            \ }

Plug 'jiangmiao/auto-pairs'

"补全
Plug 'maralla/completor.vim'
Plug 'maralla/completor-neosnippet'
"使用tab补全
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>\<cr>" : "\<cr>"

Plug 'majutsushi/tagbar'
let g:tagbar_ctags_bin = '/usr/bin/ctags'
nmap <F8> :TagbarToggle<CR>

Plug 'ludovicchabant/vim-gutentags'
" gutentags搜索工程目录的标志，碰到这些文件/目录名就停止向上一级目录递归 "
let g:gutentags_project_root = ['.root', '.svn', '.git', '.project']
" 所生成的数据文件的名称 "
let g:gutentags_ctags_tagfile = '.tags'

" 将自动生成的 tags 文件全部放入 ~/.cache/tags 目录中，避免污染工程目录 "
let s:vim_tags = expand('~/.cache/tags')
let g:gutentags_cache_dir = s:vim_tags
" 检测 ~/.cache/tags 不存在就新建 "
if !isdirectory(s:vim_tags)
    silent! call mkdir(s:vim_tags, 'p')
endif
" 配置 ctags 的参数 "
let g:gutentags_ctags_extra_args = ['--fields=+niazS', '--extra=+q']
let g:gutentags_ctags_extra_args += ['--c++-kinds=+pxI']
let g:gutentags_ctags_extra_args += ['--c-kinds=+px']

Plug 'junegunn/vim-easy-align'
vmap <Leader>a <Plug>(EasyAlign)
nmap <Leader>a <Plug>(EasyAlign)
if !exists('g:easy_align_delimiters')
    let g:easy_align_delimiters = {}
endif
let g:easy_align_delimiters['#'] = { 'pattern': '#', 'ignore_groups': ['String'] }
" <Leader>a 加上某个符号就根据某个符号对齐
"<Leader>a=        对齐等号表达
"<Leader>a:        对齐冒号表达式(json/map等)
"默认左对齐
"<Leader>a<space>  首个空格对齐
"<Leaderleaderace> 第二个空格对齐
"<Leader>a-<space> 倒数第一个空格对齐
"<Leader>a-2<space> 倒数第二个空格对齐
"<Leader>a*<space> 所有空格依次对齐
"右对齐
"<Leader>a<Enter>*<space>

" 格式化插件
Plug 'Chiel92/vim-autoformat'
let g:python3_host_prog = '/usr/bin/python3.8'
noremap <F3> :Autoformat<CR>
let g:autoformat_autoindent = 0
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 1

" 对齐线
Plug 'Yggdroot/indentLine'
" 设置缩进使用的字符
let g:indentLine_char = '|'

Plug 'octol/vim-cpp-enhanced-highlight'
let g:cpp_class_scope_highlight = 1
let g:cpp_member_variable_highlight = 1
let g:cpp_class_decl_highlight = 1
let g:cpp_posix_standard = 1
let g:cpp_experimental_simple_template_highlight = 1
let g:cpp_concepts_highlight = 1

call plug#end()

" Startup
filetype indent plugin on

augroup vimrcEx
    au!
    autocmd FileType text setlocal textwidth=78
augroup END

" vim 文件折叠方式为 marker
augroup ft_vim
    au!
    autocmd FileType vim setlocal foldmethod=marker
    " 打开文件总是定位到上次编辑的位置
    autocmd BufReadPost *
                \ if line("'\"") > 1 && line("'\"") <= line("$") |
                \   exe "normal! g`\"" |
                \ endif
augroup END

"在光标接近底端或顶端时，自动下滚或上滚
set scrolloff=8
" leader 键
let mapleader="\\"
" 设置主题配色 配置文件 /usr/share/vim/vim82/colors
colorscheme pablo
set encoding=utf-8
" 设置打开文件的编码格式
set fileencodings=ucs-bom,utf-8,cp936,gb18030,big5,euc-jp,euc-kr,latin1
" 高亮第 80 列
" set cc=80
" 将光标所在行移动到屏幕中间
nmap <c-l> zz
imap <c-l> <Esc>zzi
" 自动格式化代码
nnoremap fm gg=G
"自动缩进与 C 语言风格缩进
filetype indent on
set autoindent
set cindent
"不在单词中间折行
set lbr
set tabstop=4
set softtabstop=4
set shiftwidth=4
"将tab自动替换为空格
set expandtab
"按退格键时可以一次删除4个空格
set softtabstop=4
"自动折叠
set foldmethod=marker

" 修改自动补全背景颜色
highlight Pmenu    ctermbg=darkgrey ctermfg=yellow 
highlight PmenuSel ctermbg=darkgrey ctermfg=red


"VIM 快捷键{{{
" =====================================================
" |                     VIM 快捷键                    |
" =====================================================
"
" ========== 折叠设置 ==========
" set foldmethod=indent "set default foldmethod
" zi 打开关闭折叠
" zv 查看此行
" zm 关闭折叠
" zM 关闭所有
" zr 打开
" zR 打开所有
" zc 折叠当前行
" zo 打开当前折叠
" zd 删除折叠
" zD 删除所有折叠
"
" ========== 多行插入/删除 ==========
" ctrl+v 进入列选择模式，选择好需要注释的列，
" shift+i/a/s（列前插入/列后插入/删除当前选中内容后插入）插入内容
" 按两次 ESC
" 删除操作只需要将第二步改为键入 x 删除即可
"
"
" ========== 内置终端 ==========
" :term 打开内置终端
" :!command 打开外部终端, exit 退出外部终端，回到 VIM
" <c-\><c-N> 将内置终端从 insert 模式调到 normal 模式，可以进行滚屏操作
" i/A 内置终端回到 insert 模式
"
" =========== tab 跳转 ==========
"
" gt  转到下一个tab
" gT 转到上一个tab
" [n]gt 向下 转n个tab
" [n]gT 向上转n个tab
"
"========== 窗口操作 ==========
"
" 同时打开多个文件，并横向排列
" vim -o t.c t.h
"
" 同一文件分割窗口
" :split 或者 :sp
"
" 关闭当前窗口
" :close
"
" 保留当前窗口，关闭其他窗口
" :only

" 横向打开一个文件
" :sp 文件名
"
" 纵向打开一个文件
" :vsplit(vsp) 文件名

" 打开一个新窗口，注意这时是看不到的
" :new 文件名
" 纵向打开它
" :vnew 文件名
"
" 窗口跳转
" ctrl+w j 向下移动
" ctrl+w k 向上移动
" ctrl+w h 向左移动
" ctrl+w l  向右移动
" ctrl+w t 移动到顶端
" ctrl+w b 移动到底端
" 移动窗口
" Ctrl + w + r：向右或向下方交换窗口，而Ctrl + w + R则和它方向相反。
" Ctrl + w + x：交换同列或同行的窗口的位置。vim默认交换当前窗口的与它下一个窗口
" 的位置，如果下方没有窗口，则试着与上一个窗口交换位置。亦可在此命令前加上数量
" ，与制定的窗口交换位置。i
"
"
" ========== 查看当前正在编辑的文件名 ===========
" :f
"
" ========== 跳转操作 ===========
" ctrl+o/ctrl+i 跳转到上一个/下一个编辑过的位置
"
" ========== 格式化代码 ==========
" gg 跳到第一行
" shift+v 进入可视行模式
" shift+g 全选
" 按 = 全部格式化
"  方式二
"" gg=G gg是文件首，G是文件尾，所以 gg=G 是整理全部代码

" :noh 在查找后消除查找字符高亮
"
" =========== 文件内跳转操作 ===========
" hjkl 这是代替箭头键功能的
" H M L 跳到屏幕的顶上 中间 下方
" w 跳到下一个单词的开始
" e 跳到单词的结束
" b 向后跳
" gg 跳到文件的开始
" G 跳到文件的结束
" 10gg 或10G 跳到第10行
" ta 跳到下一个a 前面
" fa 跳到下一个a
" 大写的意思相反
"
"}}}
