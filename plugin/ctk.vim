" Script Nmame: code toolkit
" File Name:    ctk.vim
" Author:       StarWing
" Version:      0.2
" Last Change:  2009-03-30 18:20:00
" Note:         see :ctk for details
" ==========================================================
" load once {{{1

if v:version < 700
    echomsg "ctk.vim requires Vim 7.0 or above."
    finish
endif

let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8
if !exists('g:loaded_ctk')
    let g:loaded_ctk = 'v0.2'

    " options {{{2
    function! s:defopt(opt, val)
	if !exists(a:opt) | let {a:opt} = a:val | endif
    endfunction

    call s:defopt('g:ctk_autofname', 'strftime("%Y-%m-%d")."-".idx')
    call s:defopt('g:ctk_cinfo_file', '.compiler_info')
    call s:defopt('g:ctk_defoutput', './output')
    call s:defopt('g:ctk_ext_var', 'ft_ext')
    call s:defopt('g:ctk_tempdir', './noname')
    if has('win32')
        call s:defopt('g:ctk_execprg', executable('vimrun') ?
                    \ 'start vimrun $exec' : 'start $exec')
    elseif has('unix') && has('gui_running')
        call s:defopt('g:ctk_execprg', 'xterm -e "$exec; '.
                    \ 'read -s -n1 -p''press any key to continue...''"')
    else
        call s:defopt('g:ctk_execprg', &sh.' $exec')
    endif

    delfunc s:defopt

    " commands {{{2
    command! -bar StartCTK call s:start_ctk()
    command! -bar StopCTK call s:stop_ctk()
    command! -bar EditCompilerInfo exec 'drop '.globpath(&rtp, g:ctk_cinfo_file)

    command! -nargs=* -complete=customlist,ctk:info_name_complete -bar -count=0 
	    \ ListCompiler call ctk:list_compiler(<q-args>, <count>)
    command! -nargs=* -complete=custom,ctk:info_item_complete -bang
	    \ SetCompilerInfo call ctk:set_compiler_info(<q-args>, '<bang>')
    command! -nargs=+ -complete=custom,ctk:info_item_complete
            \ SetDefaultInfo call ctk:set_default_info(<q-args>)
    command! -nargs=+ -complete=custom,ctk:info_item_complete -count=0
	    \ AddFlags call ctk:add_flags(<q-args>, <count>)

    command! -nargs=? -bar -count=0 CC call ctk:compile(<count>, <q-args>)
    command! -nargs=? -bar -count=0 RUN call ctk:run(<count>, <q-args>)

    map <Plug>CTK_compile :<C-U>call ctk:compile(v:count)<CR>
    imap <Plug>CTK_compile <ESC><Plug>CTK_compile
    map <Plug>CTK_run :<C-U>call ctk:run(v:count)<CR>
    imap <Plug>CTK_run <ESC><Plug>CTK_run

    " menus {{{2

    amenu &Tools.&CTK.&Start :StartCTK<CR>
    amenu &Tools.&CTK.&Stop  :StopCTK<CR>
    amenu &Tools.&CTK.-Sep- :
    amenu <silent> &Tools.&CTK.&Add\ a\ modeline :exec 'AddFlags '.(has('gui_running') ? inputdialog("Please input the text in modeline:", "flags += ''") : input("Modeline:", "flags += ''"))<CR>
    amenu &Tools.&CTK.&List\ All\ Compiler :ListCompiler all<CR>
    amenu &Tools.&CTK.&Compile :CC<CR>
    amenu &Tools.&CTK.&Run :RUN<CR>

    " start and stop ctk {{{2
    let s:sfile = expand('<sfile>')

    function! s:start_ctk()
        augroup ctk_autocmds
            au!
            au BufFilePost * unlet! b:ctk_generated_name
            au FileType * unlet! b:{g:ctk_ext_var}
            au FileType * unlet! b:compiler_info
            exec 'run '.g:ctk_cinfo_file

            au FileType * if g:ctk_autofname != '' && (exists('b:compiler_info')
                        \ || exists('b:ctk_generated_name'))
                        \|     call ctk:set_filename() 
                        \| endif

            au FuncUndefined * if expand('<afile>')[:3] == 'ctk:'
                        \|     exec 'so '.s:sfile
                        \| endif
        augroup END
        map gc <Plug>CTK_compile
        map gC <Plug>CTK_run
    endfunction

    function! s:stop_ctk()
        au! ctk_autocmd
        unmap gc
        unmap gC
    endfunction

    " }}}2

    StartCTK
    finish
endif

" }}}1
" functions {{{1

" some inner variables {{{2

let s:ci = 'compiler_info'
let s:ci_name = 'b:'.s:ci
" patterns: info-variables, modeline-variables and modeline patterns
let s:pat_infvar = '\v(\w+)\s*\=\s*(\S)(.{-})\2'
let s:pat_mlvar = '\v(\w+)(\s*(\+)=\=\s*(\S)(.{-})\4)='
let s:pat_modeline = '\v<cc%(-([^:]*))=:\s*(.*)'
" some default attr
let s:def_attr = {'input': '%:.', 'output': '%:t:r'}

" tricks to get command-style arglist {{{2
command! -nargs=* CTKGetEscapedList let l:args = [<f-args>]
function! s:get_escaped_list(str)
    exec 'CTKGetEscapedList '.a:str
    return args
endfunction

function! ctk:set_filename() " {{{2
    if &ft == '' && expand('%') != '' && exists('b:ctk_generated_name')
        silent! noau 0f
        unlet b:ctk_generated_name
        call Decho('s:set_filename: delete the fname now!')
    endif
    if &ft == 'Decho' | return | endif
    if &ft == '' || &bt != '' | return | endif
    call Dfunc('s:set_filename()')

    if exists('b:'.g:ctk_ext_var) | let ext = b:{g:ctk_ext_var}
    elseif &sua != '' | let ext = &sua[1:]
    elseif &ft != '' | let ext = &ft
    else | return
    endif

    call Decho('ext = "'.ext.'"')
    let tempdir = get(b:, 'ctk_tempdir', g:ctk_tempdir)
    if !isdirectory(tempdir) && exists('*mkdir')
        call mkdir(tempdir, 'p')
    endif
    if !isdirectory(tempdir)
        let tempdir = '.'
    endif
    let tempdir = fnamemodify(tempdir, ':p')
    call Decho('tempdir = "'.tempdir.'"')

    if exists('b:ctk_tempdir')
        let b:ctk_tempdir = tempdir
    else
        let g:ctk_tempdir = tempdir
    endif

    let fname = get(b:, 'ctk_autofname', g:ctk_autofname)
    if !exists('g:ctk_idx')
        let g:ctk_idx = 1
    endif
    let idx = g:ctk_idx
    while filereadable(tempdir.'/'.eval(fname).'.'.ext)
        let idx += 1
    endwhile
    let g:ctk_idx = idx + 1
    call Decho('fname = "'.eval(fname).'.'.ext.'"')

    if getcwd() == $VIMRUNTIME
        call Decho('now we are in $VIMRUNTIME, just out of it...')
        exec 'lcd '.tempdir
        call Decho('now we are in "'.getcwd().'"')
    endif

    silent exec 'file '.simplify(fnamemodify(tempdir.glob('/').
                \ eval(fname).'.'.ext, ':.'))

    let b:ctk_generated_name = expand('%:p')
    call Decho('generated fname is "'.b:ctk_generated_name.'"')
    call Dret('s:set_filename')
endfunction

function! ctk:compile(count, ...) " {{{2
    if s:find_source() || s:save_source() ||
                \ a:count < 0 || a:count > len(b:{s:ci}.list)
        redraw | echo 'Nothing Done'
        return 1
    endif
    call Dfunc('s:compile(count = '.a:count.', '.string(a:000).')')

    let ci = b:{s:ci}
    let spec = (a:0 != 0 && a:1 != '' ? a:1.'_' : '')
    let idx = a:count == 0 ? has_key(ci, 'cur_idx') ?
                \ ci.cur_idx : 0 : (a:count - 1)
    call s:set_cur_info(ci.list[idx])
    let ci.cur_idx = idx
    let cmd = s:get_specarg(spec, 'cmd', 0)

    redraw
    if type(cmd) != type('')
        echo "can't compiling with spec=".spec.'cmd'
        call Dret("s:compile : can't compiling with spec = ".spec)
        return
    endif
    echo 'Compiling ...'

    let msg = 'Compiling... using '.get(ci.cur_info, 'title', ci.cur_info.name)
    let cmd = s:make_cmd(cmd, spec)
    let is_shell = cmd !~ '^[:*]'
    let res = s:run_cmd(cmd)

    let cfile = [msg, cmd, ''] + split(res, "\<NL>")

    redraw
    if is_shell
        let cfile += [ci.cur_info.name.' returned '.v:shell_error]
        if res != ''
            echo 'Compile' (v:shell_error ? 'Fail' : 'Successd')
            if res != ''
                call writefile(cfile, &errorfile)
                cgetfile | if v:shell_error != 0 | copen
                else | cwindow | endif
            endif
        endif

        call Dret('s:compile : '.v:shell_error)
        return v:shell_error
    else
        if res != ''
            echo join(cfile, "\n")
            return 1
        endif
    endif
endfunction

function! ctk:run(count, ...) " {{{2
    if s:find_source() | return | endif
    let bufnr = bufnr('%')
    if (&modified || !has_key(b:{s:ci}, 'cur_info')
                \ || b:{s:ci}.cur_idx != (a:count - 1)
                \ ) && call('ctk:compile', [a:count] + a:000)
        return 1
    endif
    call Dfunc('s:run(count = '.a:count.', '.string(a:000).')')

    exec bufwinnr(bufnr).'wincmd w'
    let spec = (a:0 == 0 || a:1 == '' ? '' : a:1.'_')
    let cmd = s:get_specarg(spec, 'run', 0)

    if type(cmd) != type('')
        redraw | echo "can't exec program with spec=".spec.'run'
        call Dret("s:run : can't exec program with spec = ".spec)
        return
    endif
    let cmd = s:make_cmd(cmd, spec)
    if cmd !~ '^[:*]'
        if cmd[0] == '!' | let cmd = cmd[1:] | endif
        if g:ctk_execprg != ''
            let cmd = substitute(g:ctk_execprg, '$exec\>', escape(cmd, '\'), 'g')
        endif
        let cmd = ':silent !'.cmd
    endif
    redraw
    echomsg s:run_cmd(cmd)
    redraw

    if has('win32') && cmd =~ '^:!'
        call feedkeys("\<NL>", 't')
    endif

    call Dret('s:run')
endfunction

function! ctk:add_flags(flags, count) " {{{2
    if s:find_source() | return | endif

    if a:count > 0 && a:count <= len(b:{s:ci}.list)
        let compiler = '-'.b:ctk.info[a:count - 1].name
    else
        let compiler = ''
    endif

    let com_begin = matchstr(&com, 's.\=:\zs[^,]\+\ze')
    if com_begin != ''
        let com_begin .= ' '
        let com_end = ' '.matchstr(&com, 'e.\=:\zs[^,]\+\ze')
    else
        let com_begin = matchstr(&com, ':\zs[^,]\+').' '
        let com_end = ''
    endif
    
    call append(line('$'), com_begin.'cc'.compiler.': '.a:flags.com_end)
endfunction

function! ctk:set_default_info(cmdarg) " {{{2
    call Dfunc('s:set_default_info(cmdarg = "'.a:cmdarg.'")')
    let def_info = {}
    if !exists(s:ci_name)
        let b:{s:ci} = {}
    endif
    let b:{s:ci}.default = def_info

    call substitute(a:cmdarg, s:pat_infvar, '\=s:sub_info(def_info)', 'g')

    if has_key(def_info, g:ctk_ext_var)
        let b:{g:ctk_ext_var} = def_info[g:ctk_ext_var]
        unlet def_info[g:ctk_ext_var]
    endif

    for key in keys(s:def_attr)
        if !has_key(def_info, key)
            let def_info.input = s:def_attr[key]
        endif
    endfor

    call Dret('s:set_default_info')
endfunction

function! ctk:set_compiler_info(cmdarg, bang) " {{{2
    call Dfunc('s:set_compiler_info(cmdarg = "'.a:cmdarg.'")')
    if !exists(s:ci_name)
        let b:{s:ci} = {'list':[]}
    elseif !has_key(b:{s:ci}, 'list')
        let b:{s:ci}.list = []
    endif

    " empty command
    if a:cmdarg == ''
        if a:bang == '!' " delete all
            call Decho('delete all')
            for info in b:{s:ci}.list
                silent! exec info.unmap
            endfor
            let b:{s:ci}.list = []
        endif
        return Dret('s:set_compiler_info')
    endif

    " find name and others, mlist = [all, name, infos]
    let mlist = matchlist(a:cmdarg, '\v^\s*(.{-})%(\s+(.{-})\s*)=$')

    " add or modify a info
    if mlist[2] != ''
        let info = s:find_info(mlist[1])

        " add a new info, or clean old info
        " if exists ci.default, use it for default values
        if empty(info)
            let info.name = mlist[1]
            call add(b:{s:ci}.list, info)
        else
            call filter(info, 0)
        endif

        let info.name = mlist[1]
        call substitute(mlist[2], s:pat_infvar,
                    \ '\=s:sub_info(info)', 'g')

        let info.unmap = ''
        let idx = s:get_idx(info)
        let dict = {'cmdmap': 'compile', 'runmap': 'run'}
        for key in keys(dict)
            if !has_key(info, key) | continue | endif
            call Decho('setup '.key)
            for mkey in s:get_escaped_list(info[key])
                let cpos = stridx(mkey, ':')
                for mode in split(cpos <= 0 ? 'nvi' : mkey[:cpos - 1], '\zs')
                    try | exec mode.'noremap <unique> '.mkey[cpos+1:].
                                \ ' <C-\><C-N>:call ctk:'.dict[key].'('.(idx + 1).')<CR>'
                        let info.unmap .= mode.'unmap '.mkey[cpos+1:].'|'
                        call Decho(mode.'map '.mkey[cpos+1:].' success!')
                    catch | endtry
                endfor
            endfor
        endfor

    " remove a info
    elseif a:bang == '!'
        let info = s:find_info(mlist[1])

        if !empty(info)
            silent! exec info.unmap
            call remove(b:{s:ci}, s:get_idx(info))
        endif

    " list a info
    else
        call s:list_compiler(mlist[1])
    endif

    call Decho('>> now info = '.string(info))
    call Dret('s:set_compiler_info')
endfunction

function! ctk:list_compiler(name, ...) " {{{2
    if s:find_source() | return | endif
    " offer index, just show the speciafied info
    if a:0 != 0 && a:1 != 0
        call s:show_list(b:{s:ci}.list[a:1])

    " didn't offer anything, show the must normal things
    elseif a:name == ''
        if has_key(b:{s:ci}, 'default')
            call s:show_list(b:{s:ci}.default)
        endif
        if !has_key(b:{s:ci}, 'cur_info') " all
            for info in b:{s:ci}.list
                call s:show_list(info)
            endfor
        else " current
            call s:show_list(b:{s:ci}.cur_info)
        endif
    elseif a:name ==? 'all'
        for info in b:{s:ci}.list
            call s:show_list(info)
        endfor
    elseif a:name ==? 'current' && has_key(b:{s:ci}, 'cur_info')
        call s:show_list(b:{s:ci}.cur_info)
    elseif a:name ==? 'default' && has_key(b:{s:ci}, 'default')
        call s:show_list(b:{s:ci}.default)
    else
        call s:show_list(s:find_info(a:name))
    endif
endfunction

function! s:find_source() " {{{2
    let cur_winnr = winnr()

    while 1
        if exists(s:ci_name) | return 0 | endif
        wincmd w

        if winnr() == cur_winnr
            call s:echoerr("Can't Find Source Window!")
            return 1
        endif
    endwhile
endfunction

function! s:save_source() " {{{2
    try
        silent write

    catch /E13/ " File exists
        let res = s:question("File Exists, Overwrite?(y/n)"
        if res | silent write! | endif
        return !res

    catch /E45/ " Readonly
        let res = s:question("File Readonly, Still write?(y/n)")
        if res | silent write! | endif
        return !res

    endtry
endfunction

function! s:set_cur_info(info) " {{{2
    if expand('%') == '' | return | endif
    call Dfunc('s:set_cur_info(info = '.a:info.name.')')
    let cur_info = copy(a:info)
    for var in ['cmd', 'run', 'un']
        silent! unlet! cur_info[var.'map']
    endfor
    let b:{s:ci}.cur_info = cur_info

    if exists('b:ctk_generated_name')
                \ && b:ctk_generated_name == expand('%:p')
        let cur_info.output = g:ctk_defoutput
    endif

    " analyze the modeline to modifie the info
    if &modeline
        let last = line('$')
        if last <= &mls * 2
            call s:read_modeline(1, last)
        else
            call s:read_modeline(0, &mls)
            call s:read_modeline(last - &mls, last)
        endif
    endif

    " change input/output into filelist
    call Decho('set the IO flags')
    for key in ['input', 'output']
        let val = ''
        for file in s:get_escaped_list(s:get_specarg('',
                    \ 'default', s:def_attr[key]))
            call Decho(key.'.file = '.file)
            let file = file =~ '^[#%]\%(:.\)*$'
                        \ ? fnamemodify(expand('%'), file[1:])
                        \ : fnamemodify(file, ':.')
            if file =~ '\s'
                let file = shellescape(file)
            endif
            let val .= file.' '
        endfor
        let cur_info[key] = matchstr(val, '^\s*\zs.\{-}\ze\s*$')
    endfor

    call Decho('now cur_info = '.string(cur_info))
    call Dret('s:set_cur_info')
endfunction

function! s:read_modeline(begin, end) " {{{2
    call Dfunc('s:read_modeline(begin = '.a:begin.', end = '.a:end.')')
    let pos = winsaveview()

    call cursor(a:begin, 1)
    while search(s:pat_modeline, '', a:end) != 0
        call Decho('find a modeline in line '.line('.'))
        let mlist = matchlist(getline('.'), s:pat_modeline)
        if mlist[1] == '' || b:{s:ci}.cur_info.name =~ '^\V'.escape(mlist[1], '\')
            call substitute(mlist[2], s:pat_mlvar,
                        \ '\=s:sub_modeline()', 'g')
        endif
    endwhile

    call winrestview(pos)
    call Dret('s:read_modeline')
endfunction

function! s:sub_modeline() " {{{2
    call Decho('let cur_info['.submatch(1).'] '.submatch(3).'= "'.
                \ submatch(5).'"')

    if submatch(3) != ''
        let val = get(b:{s:ci}.cur_info, submatch(1),
                    \ get(b:{s:ci}.default, submatch(1), 0))
        if type(val) != type('')
            call s:echoerr("modeline: can't find '".
                        \ submatch(1)."' in current info")
            call Dret('ctk:process_modeline')
            return
        endif

        let b:{s:ci}.cur_info[submatch(1)] = val.' '.submatch(5)
    elseif submatch(2) != ''
        let b:{s:ci}.cur_info[submatch(1)] = submatch(5)
    endif
endfunction

function! s:make_cmd(cmd, spec) " {{{2
    if !has_key(b:{s:ci}, 'cur_info') | return | endif
    call Dfunc('s:make_cmd(cmd = "'.a:cmd.'", spec = "'.a:spec.'"')

    let cmd = substitute(a:cmd, '$\l\+', '\=s:sub_repvar(a:spec)', 'g')
    if cmd !~ '^[:*]'
        call Decho('this is a executable cmd')
        let exe = matchstr(cmd, '^!\=\zs.\{-}\ze\(\s\|$\)')
        call Decho('exe = '.exe)
        if glob(exe) != '' && !executable(exe)
                    \ && executable('./'.exe)
            let cmd = './'.(cmd[0] == '!' ? cmd[1:] : cmd)
        endif
    endif
    if cmd !~ '^[!:*]' | let cmd = '!'.cmd | endif
    call Dret('s:make_cmd : '.cmd)
    return cmd
endfunction

function! s:run_cmd(cmdarg) " {{{2
    if !has_key(b:{s:ci}, 'cur_info') | return | endif
    call Dfunc('s:run_cmd(cmdarg = '.a:cmdarg.')')

    let mlist = matchlist(a:cmdarg, '\v^([!:*])=(.*)$')
    call Decho('mlist = '.string(mlist[:2]))
    if mlist[1] == '!' || mlist[1] == ''
        let output = system(mlist[2])
    endif
    if mlist[1] == ':'
        redir => output
        silent! exec mlist[2]
        redir END
        let output = matchstr(output, '\%^[\r\n]*\zs.\{-}\ze[\r\n]*\%$')
    endif
    if mlist[1] == '*'
        let output = eval(mlist[2])
    endif

    call Dret('s:run_cmd : '.output)
    return output
endfunction

function! s:question(msg) " {{{2
    redraw
    echohl Question
    echo a:msg
    echohl NONE
    return nr2char(getchar()) ==? 'y'
endfunction

function! s:echoerr(errmsg) " {{{2
    echohl ErrorMsg
    echomsg 'ctk: '.a:msg
    echohl NONE
endfunction

function! s:get_idx(info) " {{{2
    let idx = 0
    for info in b:{s:ci}.list
        if info is a:info
            return idx
        endif
        let idx += 1
    endfor
    return -1
endfunction

function! s:get_specarg(spec, key, default) " {{{2
    if has_key(b:{s:ci}, 'default')
        return get(b:{s:ci}.cur_info, a:spec.a:key,
                    \ get(b:{s:ci}.default, a:spec.a:key,
                    \ get(b:{s:ci}.cur_info, a:key,
                    \ get(b:{s:ci}.default, a:key, a:default))))
    else
        return get(b:{s:ci}.cur_info, a:spec.a:key,
                    \ get(b:{s:ci}.cur_info, a:key, a:default))
    endif
endfunction

function! s:show_list(info) " {{{2
    if empty(a:info) | return | endif

    echohl Title
    echo has_key(a:info, 'name') ?
                \ (has_key(a:info, 'title') ? 
                \ a:info.title."\n\tname         = ".a:info.name."\n"
                \ : a:info.name."\n")
                \ : "Default Values: ".&ft." files"
    echohl NONE

    for key in sort(filter(keys(a:info),
                \ "v:val !~ '".'^\%(title\|name\|unmap\)$'."'"))
        echo printf("\t%-12s = %s", key, a:info[key])
    endfor
endfunction

function! s:find_info(name) " {{{2
    for info in b:{s:ci}.list
        if info.name ==? a:name
            return info
        endif
    endfor
    return {}
endfunction

function! s:sub_info(info) " {{{2
    let name = has_key(a:info, 'name') ? a:info.name : 'noname' "Decho
    call Dfunc('s:sub_info(info = '.name.')')
    call Decho('let '.name.'.'.submatch(1).' = "'.submatch(3).'"')
    let a:info[submatch(1)] = submatch(3)
    call Dret('s:sub_info')
endfunction

function! s:sub_repvar(spec) " {{{2
    let cur_info = b:{s:ci}.cur_info
    let default = b:{s:ci}.default

    let val = s:get_specarg(a:spec, submatch(0)[1:], '')
    call Decho('val = "'.val.'"')

    let mstr = matchstr(val, '$\l\+')
    while mstr != ''
        let vval = s:get_specarg('', mstr[1:], '')
        let val = substitute(val, mstr, vval, 'g')
        let mstr = matchstr(val, '$\U\+')
    endwhile

    return val
endfunction

function! ctk:info_name_complete(A,L,P) " {{{2
    let list = []
    for dict in b:{s:ci}.list
        let list += [dict.name]
    endfor
    let pat = "'^\\v".substitute(escape(a:A, '\'), "'", "''", 'g')."'"
    return sort(filter(list + ['all', 'default', 'current'],
                \ 'v:val =~ '.pat))
endfunction

function! ctk:info_item_complete(A,L,P) " {{{2
    return "asm_\ncc\ncmd\ndebug_\nflags\n".
                \ "input\noutput\nrun\ntitle"
endfunction " }}}2

" other works {{{1

let &cpo = s:cpo_save
unlet s:cpo_save
set debug=beep " Decho

" }}}1
" vim: ff=unix ft=vim fdm=marker sw=4 ts=8 et sta nu
