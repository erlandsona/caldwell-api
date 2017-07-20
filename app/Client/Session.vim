let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/Code/personal/Caldwell
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +263 libs/Normalize.elm
badd +21 app/Caldwell/Nav/Styles.elm
badd +124 app/Caldwell/Main/Styles.elm
badd +1 app/Caldwell/Styles.elm
badd +1 app/Caldwell
badd +0 app/Caldwell/Helpers.elm
badd +1 webpack.coffee
badd +1 .
badd +36 package.json
badd +9 app/index.js
badd +1 app/assets/fonts.css
badd +31 public/main-e34d94191b8ed1ad2382.css
badd +1 public/main-75076067214f7e0bde72.css
badd +1 app/index.html
badd +1 app
badd +27 app/Caldwell/View.elm
badd +32 app/Caldwell/Main/View.elm
badd +1 app/Caldwell/Update.elm
badd +20 app/Caldwell/Nav/View.elm
badd +1 app/Caldwell/Header/View.elm
badd +1 app/Caldwell/Bio/View.elm
badd +0 app/Caldwell/Model.elm
argglobal
silent! argdel *
$argadd .
edit app/index.html
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
2wincmd h
wincmd w
wincmd w
wincmd t
set winminheight=1 winheight=1 winminwidth=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 59 + 89) / 178)
exe 'vert 2resize ' . ((&columns * 59 + 89) / 178)
exe 'vert 3resize ' . ((&columns * 58 + 89) / 178)
argglobal
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=10
setlocal fml=1
setlocal fdn=10
setlocal fen
3
normal! zo
102
normal! zo
let s:l = 1 - ((0 * winheight(0) + 16) / 33)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
wincmd w
argglobal
edit app/Caldwell/View.elm
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=10
setlocal fml=1
setlocal fdn=10
setlocal fen
26
normal! zo
26
normal! zo
29
normal! zo
29
normal! zo
30
normal! zo
let s:l = 27 - ((16 * winheight(0) + 16) / 33)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
27
normal! 013|
wincmd w
argglobal
edit app/Caldwell/Model.elm
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=10
setlocal fml=1
setlocal fdn=10
setlocal fen
25
normal! zo
25
normal! zo
32
normal! zo
let s:l = 35 - ((32 * winheight(0) + 16) / 33)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
35
normal! 0
wincmd w
3wincmd w
exe 'vert 1resize ' . ((&columns * 59 + 89) / 178)
exe 'vert 2resize ' . ((&columns * 59 + 89) / 178)
exe 'vert 3resize ' . ((&columns * 58 + 89) / 178)
tabnext 1
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToOc
set winminheight=1 winminwidth=1
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
