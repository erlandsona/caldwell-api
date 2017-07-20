let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/code/personal/caldwell-servant
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +8 server/Main.hs
badd +2 src/Lib.hs
badd +39 caldwell-servant.cabal
badd +1 src/Types.hs
badd +33 client/Main.hs
badd +1 client
badd +36 src/Client.hs
badd +1 src
badd +4 ~/code/personal/caldwell-servant/src/Server/Main.hs
badd +1 .
badd +36 app/Client.hs
badd +36 app/Client/Main.hs
badd +0 app/Lib.hs
badd +0 app/Server/Main.hs
badd +1 Setup.hs
argglobal
silent! argdel *
$argadd .
edit app/Lib.hs
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
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=10
setlocal fml=1
setlocal fdn=10
setlocal fen
let s:l = 1 - ((0 * winheight(0) + 16) / 33)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
wincmd w
argglobal
edit app/Server/Main.hs
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=10
setlocal fml=1
setlocal fdn=10
setlocal fen
let s:l = 1 - ((0 * winheight(0) + 16) / 33)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
wincmd w
argglobal
enew
file NERD_tree_11
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=10
setlocal fml=1
setlocal fdn=10
setlocal nofen
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
