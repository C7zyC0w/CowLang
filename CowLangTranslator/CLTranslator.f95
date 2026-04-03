program CLtranslator
    implicit none

    ! ================= PARAMETERS =================
    integer, parameter :: MAXV     = 200
    integer, parameter :: MAXIF    = 100
    integer, parameter :: MAXLOOP  = 100
    integer, parameter :: MAXBODY  = 4000
    integer, parameter :: MAXOUT   = 20000
    integer, parameter :: MAXFUNC  = 30
    integer, parameter :: MAXPARAM = 10
    integer, parameter :: MAXFBODY = 400
    integer, parameter :: MAXA     = 50
    integer, parameter :: MAXELEM  = 200

    logical, parameter :: DIRECT_PRINT = .false.
    logical, parameter :: TRACE        = .false.

    ! ================= SCALAR VARIABLE TABLE =================
    character(len=32)  :: vname(MAXV)
    character(len=16)  :: vtype(MAXV)
    character(len=200) :: vval(MAXV)
    integer :: vcount

    character(len=300) :: line
    logical :: in_prog
    character(len=64) :: current_prog_name
    character(len=64) :: given_name
    character(len=300) :: errmsg

    ! ---- Output buffer ----
    character(len=1000) :: outbuf(MAXOUT)
    integer :: outcount

    ! ---- If-stack ----
    logical :: if_active(MAXIF)
    logical :: if_matched(MAXIF)
    integer :: if_sp

    ! ================= LOOP STRUCT =================
    type :: loop_struct
        character(len=16) :: ltype
        character(len=32) :: var
        real              :: start, endv, step
        character(len=200):: cond
        integer           :: body_count
        character(len=300):: body(MAXBODY)
        integer           :: nest
        logical           :: do_awaiting_while
    end type

    type(loop_struct) :: loops(MAXLOOP)
    integer :: loop_sp

    ! ================= FUNCTION / PROCEDURE STRUCT =================
    type :: func_struct
        character(len=32) :: fname
        character(len=8)  :: fkind        ! 'func' or 'proc'
        integer           :: param_count
        character(len=32) :: params(MAXPARAM)
        integer           :: body_count
        character(len=300):: body(MAXFBODY)
    end type

    type(func_struct) :: funcs(MAXFUNC)
    integer :: func_count

    logical :: in_func_def
    integer :: current_func_idx
    integer :: func_def_nest

    ! ---- Return state ----
    character(len=1000) :: return_value
    logical             :: has_return

    ! ================= ARRAY / LIST STORAGE =================
    character(len=32)  :: aname(MAXA)
    character(len=8)   :: akind(MAXA)     ! 'arr' or 'list'
    integer            :: asize(MAXA)
    character(len=200) :: avals(MAXA, MAXELEM)
    integer            :: acount

    ! ---- Prompts ----
    character(len=*), parameter :: PROMPT_MAIN = '>>> '
    character(len=*), parameter :: PROMPT_CONT = '... '
    integer :: depth, i

    ! ================= INITIALISE =================
    vcount           = 0
    outcount         = 0
    if_sp            = 0
    loop_sp          = 0
    in_prog          = .false.
    current_prog_name= ''
    func_count       = 0
    in_func_def      = .false.
    current_func_idx = 0
    func_def_nest    = 0
    has_return       = .false.
    return_value     = ''
    acount           = 0

    ! ================= MAIN REPL LOOP =================
    do
        depth = loop_sp + if_sp
        if (depth > 0 .or. in_func_def) then
            write(0,'(A)',advance='no') PROMPT_CONT
            do i = 1, max(0, depth-1)
                write(0,'(A)',advance='no') '    '
            end do
        else
            write(0,'(A)',advance='no') PROMPT_MAIN
        end if

        read(*,'(A)', end=100) line

        line = normalize_html(line)
        line = strip_any_prompt(line)
        line = lstrip_ws(line)

        if (starts_with_keyword(line,'prog')) then
            current_prog_name = trim(extract_name_after_keyword(line,'prog'))
            in_prog = .true.
            cycle
        end if

        if (starts_with_keyword(line,'end prog')) then
            given_name = trim(lstrip_ws(line(len_trim('end prog')+1:)))
            if (len_trim(given_name) == 0) then
                call handle_end_prog()
                exit
            else
                if (to_lower(given_name) == to_lower(trim(current_prog_name))) then
                    call handle_end_prog(given_name)
                    exit
                else
                    errmsg = 'Error: end prog name "'//trim(given_name)//'" does not match '// &
                             'current program "'//trim(current_prog_name)//'".'
                    call push_line(outbuf, outcount, trim(errmsg)//' Use "end prog" to omit name.')
                end if
            end if
        end if

        if (.not. in_prog) cycle
        if (len_trim(line) == 0) cycle

        call parse_line(line)
    end do

100 continue
    if (in_prog) call handle_end_prog()

contains

! ================== DEBUG / IO ==================
subroutine dbg(msg)
    character(len=*), intent(in) :: msg
    if (TRACE) write(*,'(A)') '[TRACE] '//trim(msg)
end subroutine

subroutine push_line(outbuf, outcount, s)
    character(len=1000), intent(inout) :: outbuf(:)
    integer,             intent(inout) :: outcount
    character(len=*),    intent(in)    :: s
    if (DIRECT_PRINT) then
        write(*,'(A)') trim(s)
    else
        if (outcount < size(outbuf)) then
            outcount = outcount + 1
            outbuf(outcount) = trim(s)
        else
            write(*,*) 'Error: output buffer full'
        end if
    end if
end subroutine

subroutine flush_output(outbuf, outcount)
    character(len=1000), intent(inout) :: outbuf(:)
    integer,             intent(inout) :: outcount
    integer :: k
    if (DIRECT_PRINT) then
        outcount = 0; return
    end if
    do k = 1, outcount
        write(*,'(A)') trim(outbuf(k))
    end do
    outcount = 0
end subroutine

! ================= UTILITY =================
function to_lower(s) result(o)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: o
    integer :: i, c
    do i = 1, len(s)
        c = iachar(s(i:i))
        if (c >= 65 .and. c <= 90) then
            o(i:i) = achar(c+32)
        else
            o(i:i) = s(i:i)
        end if
    end do
end function

function to_upper(s) result(o)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: o
    integer :: i, c
    do i = 1, len(s)
        c = iachar(s(i:i))
        if (c >= 97 .and. c <= 122) then
            o(i:i) = achar(c-32)
        else
            o(i:i) = s(i:i)
        end if
    end do
end function

function lstrip_ws(s) result(o)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: o
    integer :: i
    i = 1
    do while (i <= len(s))
        if (s(i:i) /= ' ' .and. s(i:i) /= char(9)) exit
        i = i + 1
    end do
    if (i <= len(s)) then
        o = s(i:)
    else
        o = ''
    end if
end function

function normalize_html(s) result(o)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: o
    integer :: i, w
    o = ''
    i = 1; w = 1
    do while (i <= len(s))
        if (i+3 <= len(s) .and. s(i:i+3) == '&lt;') then
            if (w <= len(o)) o(w:w) = '<'; w = w+1; i = i+4
        else if (i+3 <= len(s) .and. s(i:i+3) == '&gt;') then
            if (w <= len(o)) o(w:w) = '>'; w = w+1; i = i+4
        else if (i+4 <= len(s) .and. s(i:i+4) == '&amp;') then
            if (w <= len(o)) o(w:w) = '&'; w = w+1; i = i+5
        else
            if (w <= len(o)) o(w:w) = s(i:i); w = w+1; i = i+1
        end if
    end do
    if (w <= len(o)) o(w:) = ' '
end function

function strip_any_prompt(s) result(o)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: o
    character(len=:), allocatable :: t
    t = lstrip_ws(s)
    if (len_trim(t) >= 4) then
        if (t(1:4) == '>>> ') then; o = lstrip_ws(t(5:)); return; end if
    end if
    if (len_trim(t) >= 4) then
        if (t(1:4) == '... ') then; o = lstrip_ws(t(5:)); return; end if
    end if
    o = t
end function

logical function starts_with_keyword(s, kw)
    character(len=*), intent(in) :: s, kw
    starts_with_keyword = index(to_lower(lstrip_ws(s)), trim(kw)) == 1
end function

function extract_name_after_keyword(s, kw) result(n)
    character(len=*), intent(in) :: s, kw
    character(len=64) :: n
    n = trim(lstrip_ws(s(len_trim(kw)+1:)))
end function

function strip_quotes(s) result(o)
    character(len=*), intent(in) :: s
    character(len=200) :: o
    if (len_trim(s) >= 2) then
        if (s(1:1) == '"' .and. s(len_trim(s):len_trim(s)) == '"') then
            o = s(2:len_trim(s)-1); return
        end if
    end if
    o = s
end function

function strip_any_quotes(s) result(o)
    character(len=*), intent(in) :: s
    character(len=200) :: o
    integer :: n
    o = trim(s)
    n = len_trim(o)
    if (n >= 2) then
        if ((o(1:1)=='"'  .and. o(n:n)=='"') .or. &
            (o(1:1)=="'"  .and. o(n:n)=="'")) then
            o = o(2:n-1)
        end if
    end if
end function

function rstrip_kw(s, kw) result(out)
    character(len=*), intent(in) :: s, kw
    character(len=200) :: out
    integer :: n, k
    character(len=200) :: low
    n = len_trim(s)
    if (n <= 0) then; out = ''; return; end if
    low = to_lower(s(1:n))
    k = len_trim(kw)
    if (n >= k) then
        if (low(n-k+1:n) == to_lower(kw)) then
            out = trim(s(1:n-k)); return
        end if
        if (n >= k+1) then
            if (low(n-k:n) == ' '//to_lower(kw)) then
                out = trim(s(1:n-k-1)); return
            end if
        end if
    end if
    out = trim(s(1:n))
end function

function strip_ctrl_tail(s) result(o)
    character(len=*), intent(in) :: s
    character(len=200) :: o
    o = rstrip_kw(trim(s),'then')
    o = rstrip_kw(o,'do')
end function

pure function bool_str(x) result(s)
    logical, intent(in) :: x
    character(len=5) :: s
    if (x) then; s = 'true '; else; s = 'false'; end if
end function

function int2s(i) result(s)
    integer, intent(in) :: i
    character(len=16) :: s
    write(s,'(I0)') i
    s = trim(s)
end function

function str_replace(src, old, new) result(res)
    character(len=*), intent(in) :: src, old, new
    character(len=1000) :: res, tmp
    integer :: p, olen
    res = trim(src)
    olen = len_trim(old)
    if (olen == 0) return
    do
        p = index(trim(res), trim(old))
        if (p <= 0) exit
        tmp = res(1:p-1)//trim(new)//trim(res(p+olen:len_trim(res)))
        res = trim(tmp)
    end do
end function

! ================= COMMA / QUOTE HELPERS =================
subroutine find_comma_outside_quotes(s, pos)
    character(len=*), intent(in)  :: s
    integer,          intent(out) :: pos
    integer :: i, n
    logical :: in_sq, in_dq
    pos = 0; in_sq = .false.; in_dq = .false.
    n = len_trim(s)
    do i = 1, n
        if (.not. in_sq .and. .not. in_dq) then
            if (s(i:i) == "'") in_sq = .true.
            if (s(i:i) == '"') in_dq = .true.
            if (s(i:i) == ',') then; pos = i; return; end if
        else
            if (in_sq .and. s(i:i) == "'") in_sq = .false.
            if (in_dq .and. s(i:i) == '"') in_dq = .false.
        end if
    end do
end subroutine

! Split argument list respecting parens and quotes
subroutine parse_arg_list(s, args, count)
    character(len=*),   intent(in)  :: s
    character(len=200), intent(out) :: args(MAXPARAM)
    integer,            intent(out) :: count
    character(len=400) :: rest
    integer :: p, depth, i, n
    logical :: in_sq, in_dq
    count = 0
    rest = trim(lstrip_ws(s))
    do while (len_trim(rest) > 0 .and. count < MAXPARAM)
        p = 0; depth = 0
        in_sq = .false.; in_dq = .false.
        n = len_trim(rest)
        do i = 1, n
            if (.not. in_sq .and. .not. in_dq) then
                if (rest(i:i) == '(') depth = depth + 1
                if (rest(i:i) == ')') depth = depth - 1
                if (rest(i:i) == '"') in_dq = .true.
                if (rest(i:i) == "'") in_sq = .true.
                if (rest(i:i) == ',' .and. depth == 0) then
                    p = i; exit
                end if
            else
                if (in_dq .and. rest(i:i) == '"') in_dq = .false.
                if (in_sq .and. rest(i:i) == "'") in_sq = .false.
            end if
        end do
        count = count + 1
        if (p > 0) then
            args(count) = trim(lstrip_ws(rest(:p-1)))
            rest = trim(lstrip_ws(rest(p+1:)))
        else
            args(count) = trim(lstrip_ws(rest))
            rest = ''
        end if
    end do
end subroutine

subroutine parse_param_list(s, params, count)
    character(len=*),  intent(in)  :: s
    character(len=32), intent(out) :: params(MAXPARAM)
    integer,           intent(out) :: count
    character(len=200) :: rest, tok
    integer :: p
    count = 0
    rest = trim(lstrip_ws(s))
    do while (len_trim(rest) > 0 .and. count < MAXPARAM)
        p = index(rest, ',')
        if (p > 0) then
            tok = trim(lstrip_ws(rest(:p-1)))
            rest = trim(lstrip_ws(rest(p+1:)))
        else
            tok = trim(lstrip_ws(rest))
            rest = ''
        end if
        if (len_trim(tok) > 0) then
            count = count + 1
            params(count) = tok
        end if
    end do
end subroutine

! ================= PARSER =================
recursive subroutine parse_line(cmd)
    character(len=*), intent(in) :: cmd
    character(len=300) :: l, rest, inner, cond
    integer :: pwhile

    l = lstrip_ws(normalize_html(cmd))

    ! ---- Function definition capture ----
    if (in_func_def) then
        if (starts_with_keyword(l,'func') .or. starts_with_keyword(l,'proc')) then
            func_def_nest = func_def_nest + 1
            call append_to_func_body(l)
            return
        end if
        if (starts_with_keyword(l,'end func') .or. starts_with_keyword(l,'end proc')) then
            if (func_def_nest > 0) then
                func_def_nest = func_def_nest - 1
                call append_to_func_body(l)
            else
                in_func_def = .false.
                if (TRACE) call dbg('END DEF: '//trim(funcs(current_func_idx)%fname))
            end if
            return
        end if
        call append_to_func_body(l)
        return
    end if

    ! ---- Inline do-while ----
    if (starts_with_keyword(l,'do')) then
        pwhile = index(to_lower(l),' while ')
        if (pwhile > 0 .and. loop_sp == 0) then
            inner = trim(l(4:pwhile-1))
            cond  = strip_ctrl_tail(l(pwhile+7:))
            if (len_trim(inner) > 0) then
                call exec_single_do_while(inner, cond)
            end if
            return
        end if
    end if

    ! ---- Loop body capture ----
    if (loop_sp > 0) then
        if (loops(loop_sp)%do_awaiting_while .and. loops(loop_sp)%nest == 0) then
            if (starts_with_keyword(l,'while')) then
                loops(loop_sp)%cond = trim(lstrip_ws(l(6:)))
                loops(loop_sp)%do_awaiting_while = .false.
                return
            end if
        end if
        if (starts_with_keyword(l,'for') .or. &
            starts_with_keyword(l,'while') .or. &
            trim(to_lower(l)) == 'do') then
            loops(loop_sp)%nest = loops(loop_sp)%nest + 1
            call append_to_current_body(l)
            return
        end if
        if (starts_with_keyword(l,'end for') .or. &
            starts_with_keyword(l,'end while') .or. &
            starts_with_keyword(l,'end do')) then
            if (loops(loop_sp)%nest > 0) then
                loops(loop_sp)%nest = loops(loop_sp)%nest - 1
                call append_to_current_body(l)
                return
            else
                call run_loop(loops(loop_sp))
                loop_sp = loop_sp - 1
                return
            end if
        end if
        call append_to_current_body(l)
        return
    end if

    ! ---- Func/proc definition start ----
    if (starts_with_keyword(l,'func') .and. .not. starts_with_keyword(l,'end func')) then
        call handle_func_start(l, 'func'); return
    end if
    if (starts_with_keyword(l,'proc') .and. .not. starts_with_keyword(l,'end proc')) then
        call handle_func_start(l, 'proc'); return
    end if

    ! ---- If/elif/else/end if ----
    if (starts_with_keyword(l,'if')) then
        call handle_if(l); return
    end if
    if (starts_with_keyword(l,'elif')) then
        call handle_elif(l); return
    end if
    if (starts_with_keyword(l,'else')) then
        call handle_else(); return
    end if
    if (starts_with_keyword(l,'end if')) then
        call handle_end_if(); return
    end if

    if (if_sp > 0) then
        if (.not. if_active(if_sp)) return
    end if

    ! ---- Loop start ----
    if (starts_with_keyword(l,'for') .or. &
        starts_with_keyword(l,'while') .or. &
        trim(to_lower(l)) == 'do') then
        call handle_loop_start(l); return
    end if
    if (starts_with_keyword(l,'end for') .or. &
        starts_with_keyword(l,'end while') .or. &
        starts_with_keyword(l,'end do')) then
        return
    end if

    ! ---- Return ----
    if (starts_with_keyword(l,'return')) then
        rest = trim(lstrip_ws(l(7:)))
        has_return = .true.
        if (len_trim(rest) > 0) then
            return_value = trim(eval_expr_to_str(rest))
        else
            return_value = ''
        end if
        return
    end if

    ! ---- Call (procedure) ----
    if (starts_with_keyword(l,'call')) then
        rest = trim(lstrip_ws(l(5:)))
        call dispatch_call(rest); return
    end if

    ! ---- Array/list declarations ----
    if (starts_with_keyword(l,'arr')) then
        call handle_array_decl(l); return
    end if
    if (starts_with_keyword(l,'list')) then
        call handle_list_decl(l); return
    end if

    ! ---- List ops ----
    if (index(l,'.append(') > 0 .or. index(l,'.remove(') > 0 .or. index(l,'.pop(') > 0) then
        call handle_list_op(l); return
    end if

    ! ---- Array element assign: name[i] = expr ----
    if (index(l,'[') > 0 .and. index(l,'=') > 0 .and. index(l,'==') == 0) then
        call handle_array_assign(l); return
    end if

    ! ---- Typed declaration: type :: name = expr ----
    if (index(l,'::') > 0 .and. index(l,'=') > 0) then
        call store_variable(l); return
    end if

    ! ---- Assignment ----
    if (index(l,'=') > 0 .and. index(l,'==') == 0) then
        call handle_assignment(l); return
    end if

    ! ---- Print ----
    if (starts_with_keyword(l,'print')) then
        rest = lstrip_ws(l(6:))
        if (len_trim(rest) > 0) then
            if (rest(1:1) == ',') rest = lstrip_ws(rest(2:))
        end if
        call do_print(rest); return
    end if

    ! ---- Read ----
    if (starts_with_keyword(l,'read')) then
        rest = lstrip_ws(l(5:))
        if (len_trim(rest) > 0) then
            if (rest(1:1) == ',') rest = lstrip_ws(rest(2:))
        end if
        call do_read(rest); return
    end if
end subroutine

subroutine execute_stmt_list(list)
    character(len=*), intent(in) :: list
    character(len=300) :: s, part
    integer :: p
    s = trim(list)
    do while (len_trim(s) > 0)
        if (has_return) exit
        p = index(s, ';')
        if (p > 0) then
            part = trim(s(:p-1))
            if (len_trim(part) > 0) call parse_line(part)
            s = lstrip_ws(s(p+1:))
        else
            if (len_trim(s) > 0) call parse_line(s)
            exit
        end if
    end do
end subroutine

subroutine exec_single_do_while(stmt, condition)
    character(len=*), intent(in) :: stmt, condition
    call execute_stmt_list(stmt)
    if (has_return) return
    do
        if (.not. eval_bool_expr(condition)) exit
        call execute_stmt_list(stmt)
        if (has_return) exit
    end do
end subroutine

subroutine append_to_current_body(s)
    character(len=*), intent(in) :: s
    if (loops(loop_sp)%body_count < MAXBODY) then
        loops(loop_sp)%body_count = loops(loop_sp)%body_count + 1
        loops(loop_sp)%body(loops(loop_sp)%body_count) = s
        if (TRACE) call dbg('CAPTURE['//trim(int2s(loop_sp))//'] '//trim(s))
    else
        call push_line(outbuf, outcount, 'Error: loop body too large')
    end if
end subroutine

! ================= IF HANDLERS =================
subroutine handle_if(line)
    character(len=*), intent(in) :: line
    logical :: r
    r = eval_bool_expr(strip_ctrl_tail(line(3:)))
    if_sp = if_sp + 1
    if_active(if_sp)  = r
    if_matched(if_sp) = r
    if (TRACE) call dbg('IF -> '//trim(bool_str(r)))
end subroutine

subroutine handle_elif(line)
    character(len=*), intent(in) :: line
    logical :: r
    character(len=200) :: cond
    if (if_sp == 0) return
    if (if_matched(if_sp)) then
        if_active(if_sp) = .false.; return
    end if
    cond = strip_ctrl_tail(line(5:))
    r = eval_bool_expr(cond)
    if_active(if_sp)  = r
    if_matched(if_sp) = r
    if (TRACE) call dbg('ELIF -> '//trim(bool_str(r)))
end subroutine

subroutine handle_else()
    if (if_sp == 0) return
    if (.not. if_matched(if_sp)) then
        if_active(if_sp)  = .true.
        if_matched(if_sp) = .true.
    else
        if_active(if_sp) = .false.
    end if
end subroutine

subroutine handle_end_if()
    if (if_sp > 0) if_sp = if_sp - 1
end subroutine

! ================= LOOP CAPTURE + EXECUTE =================
subroutine handle_loop_start(l)
    character(len=*), intent(in) :: l
    integer :: p1, p2, p3, dpdum
    character(len=32) :: varname
    character(len=200) :: tmpstr_start, tmpstr_end, tmpstr_step, rest
    real :: startv, endv, stepv
    character(len=300) :: low

    low = to_lower(l)
    loop_sp = loop_sp + 1
    if (loop_sp > MAXLOOP) then
        loop_sp = MAXLOOP
        call push_line(outbuf, outcount, 'Error: too many nested loops')
        return
    end if

    loops(loop_sp)%body_count      = 0
    loops(loop_sp)%nest            = 0
    loops(loop_sp)%var             = ''
    loops(loop_sp)%cond            = ''
    loops(loop_sp)%start           = 0.0
    loops(loop_sp)%endv            = 0.0
    loops(loop_sp)%step            = 1.0
    loops(loop_sp)%do_awaiting_while = .false.

    if (starts_with_keyword(low,'for')) then
        loops(loop_sp)%ltype = 'for'
        p1 = index(low,'range')
        p2 = index(low,' to ')
        p3 = index(low,' step ')
        if (p1 <= 0 .or. p2 <= 0) then
            call push_line(outbuf, outcount, 'Error: for loop syntax: for VAR range X to Y [step Z]')
            loop_sp = loop_sp - 1; return
        end if
        varname      = trim(lstrip_ws(l(5:p1-1)))
        tmpstr_start = trim(lstrip_ws(l(p1+5:p2-1)))
        call eval_num_expr(tmpstr_start, startv, dpdum)
        if (p3 > 0) then
            tmpstr_end  = trim(lstrip_ws(l(p2+4:p3-1)))
            tmpstr_step = trim(lstrip_ws(l(p3+6:)))
            call eval_num_expr(tmpstr_end,  endv,  dpdum)
            call eval_num_expr(tmpstr_step, stepv, dpdum)
        else
            tmpstr_end = trim(lstrip_ws(l(p2+4:)))
            call eval_num_expr(tmpstr_end, endv, dpdum)
            stepv = 1.0
        end if
        loops(loop_sp)%var   = varname
        loops(loop_sp)%start = startv
        loops(loop_sp)%endv  = endv
        loops(loop_sp)%step  = stepv

    else if (starts_with_keyword(low,'while')) then
        loops(loop_sp)%ltype = 'while'
        rest = strip_ctrl_tail(l(6:))
        loops(loop_sp)%cond = rest

    else if (trim(low) == 'do') then
        loops(loop_sp)%ltype = 'dowhile'
        loops(loop_sp)%do_awaiting_while = .true.

    else
        loops(loop_sp)%ltype = 'unknown'
        call push_line(outbuf, outcount, 'Error: unknown loop header')
    end if
end subroutine

subroutine run_loop(ls)
    type(loop_struct), intent(in) :: ls
    real :: val
    select case (trim(ls%ltype))
    case ('for')
        val = ls%start
        do while ((ls%step >= 0.0 .and. val <= ls%endv) .or. &
                  (ls%step <  0.0 .and. val >= ls%endv))
            call set_variable(ls%var, 'int', trim(num_to_str(val,0)))
            call execute_block(ls%body, ls%body_count)
            if (has_return) exit
            val = val + ls%step
        end do
    case ('while')
        do while (eval_bool_expr(ls%cond))
            call execute_block(ls%body, ls%body_count)
            if (has_return) exit
        end do
    case ('dowhile')
        do
            call execute_block(ls%body, ls%body_count)
            if (has_return) exit
            if (.not. eval_bool_expr(ls%cond)) exit
        end do
    case default
        call push_line(outbuf, outcount, 'Error: unknown loop type')
    end select
end subroutine

! Skip from a loop header to past its matching end (used when skipping dead branches)
subroutine skip_to_end_loop(body, count, j)
    character(len=300), intent(in) :: body(:)
    integer,            intent(in) :: count
    integer,         intent(inout) :: j
    integer :: nest, k
    character(len=300) :: bl
    nest = 0
    k = j + 1
    do while (k <= count)
        bl = to_lower(lstrip_ws(body(k)))
        if (starts_with_keyword(bl,'for') .or. &
            starts_with_keyword(bl,'while') .or. &
            trim(bl) == 'do') then
            nest = nest + 1
        else if (starts_with_keyword(bl,'end for') .or. &
                 starts_with_keyword(bl,'end while') .or. &
                 starts_with_keyword(bl,'end do')) then
            if (nest == 0) then
                j = k + 1; return
            else
                nest = nest - 1
            end if
        end if
        k = k + 1
    end do
    j = count + 1
end subroutine

recursive subroutine execute_block(body, count)
    character(len=300), intent(in) :: body(:)
    integer,            intent(in) :: count

    integer :: j, k, start_idx, nest, dpdum
    character(len=300) :: s, low, rest
    type(loop_struct)  :: inner
    integer :: p1, p2, p3
    character(len=200) :: a, b, c

    ! Local if-stack for if/elif/else/end if inside blocks
    logical :: blk_if_active(MAXIF), blk_if_matched(MAXIF)
    integer :: blk_if_sp
    logical :: cur_active, r2
    character(len=200) :: blk_cond

    blk_if_sp = 0

    j = 1
    do while (j <= count)
        if (has_return) exit

        s   = lstrip_ws(normalize_html(body(j)))
        low = to_lower(s)

        ! ---- Local if/elif/else/end if tracking ----
        if (starts_with_keyword(low,'end if')) then
            if (blk_if_sp > 0) blk_if_sp = blk_if_sp - 1
            j = j + 1; cycle
        end if

        if (starts_with_keyword(low,'if') .and. .not. starts_with_keyword(low,'end if')) then
            cur_active = (blk_if_sp == 0) .or. blk_if_active(blk_if_sp)
            blk_if_sp = blk_if_sp + 1
            if (blk_if_sp <= MAXIF) then
                r2 = cur_active .and. eval_bool_expr(strip_ctrl_tail(s(3:)))
                blk_if_active(blk_if_sp)  = r2
                blk_if_matched(blk_if_sp) = r2
            end if
            j = j + 1; cycle
        end if

        if (starts_with_keyword(low,'elif')) then
            if (blk_if_sp > 0) then
                if (.not. blk_if_matched(blk_if_sp)) then
                    cur_active = (blk_if_sp <= 1) .or. blk_if_active(blk_if_sp-1)
                    blk_cond = strip_ctrl_tail(s(5:))
                    r2 = cur_active .and. eval_bool_expr(blk_cond)
                    blk_if_active(blk_if_sp)  = r2
                    blk_if_matched(blk_if_sp) = r2
                else
                    blk_if_active(blk_if_sp) = .false.
                end if
            end if
            j = j + 1; cycle
        end if

        if (starts_with_keyword(low,'else')) then
            if (blk_if_sp > 0) then
                if (.not. blk_if_matched(blk_if_sp)) then
                    cur_active = (blk_if_sp <= 1) .or. blk_if_active(blk_if_sp-1)
                    blk_if_active(blk_if_sp)  = cur_active
                    blk_if_matched(blk_if_sp) = cur_active
                else
                    blk_if_active(blk_if_sp) = .false.
                end if
            end if
            j = j + 1; cycle
        end if

        ! Skip inactive branch (skip loops too, but track their ends)
        if (blk_if_sp > 0 .and. .not. blk_if_active(blk_if_sp)) then
            if (starts_with_keyword(low,'for') .or. &
                starts_with_keyword(low,'while') .or. &
                trim(low) == 'do') then
                call skip_to_end_loop(body, count, j)
                cycle
            end if
            j = j + 1; cycle
        end if

        ! ---- Nested loop inside body ----
        if (starts_with_keyword(low,'for') .or. &
            starts_with_keyword(low,'while') .or. &
            trim(low) == 'do') then

            inner%body_count         = 0
            inner%nest               = 0
            inner%var                = ''
            inner%cond               = ''
            inner%start              = 0.0
            inner%endv               = 0.0
            inner%step               = 1.0
            inner%do_awaiting_while  = .false.

            if (starts_with_keyword(low,'for')) then
                inner%ltype = 'for'
                p1 = index(low,'range'); p2 = index(low,' to '); p3 = index(low,' step ')
                inner%var = trim(lstrip_ws(s(5:p1-1)))
                a = trim(lstrip_ws(s(p1+5:p2-1))); call eval_num_expr(a, inner%start, dpdum)
                if (p3 > 0) then
                    b = trim(lstrip_ws(s(p2+4:p3-1))); call eval_num_expr(b, inner%endv, dpdum)
                    c = trim(lstrip_ws(s(p3+6:)));      call eval_num_expr(c, inner%step, dpdum)
                else
                    b = trim(lstrip_ws(s(p2+4:)));      call eval_num_expr(b, inner%endv, dpdum)
                    inner%step = 1.0
                end if
            else if (starts_with_keyword(low,'while')) then
                inner%ltype = 'while'
                inner%cond = strip_ctrl_tail(s(6:))
            else if (trim(low) == 'do') then
                inner%ltype = 'dowhile'
                inner%do_awaiting_while = .true.
            end if

            start_idx = j + 1
            nest = 0
            do k = start_idx, count
                if (starts_with_keyword(to_lower(lstrip_ws(body(k))),'for') .or. &
                    starts_with_keyword(to_lower(lstrip_ws(body(k))),'while') .or. &
                    trim(to_lower(lstrip_ws(body(k)))) == 'do') then
                    nest = nest + 1
                    if (inner%body_count < MAXBODY) then
                        inner%body_count = inner%body_count + 1
                        inner%body(inner%body_count) = body(k)
                    end if
                else if (starts_with_keyword(to_lower(lstrip_ws(body(k))),'end for') .or. &
                         starts_with_keyword(to_lower(lstrip_ws(body(k))),'end while') .or. &
                         starts_with_keyword(to_lower(lstrip_ws(body(k))),'end do')) then
                    if (nest == 0) then
                        exit
                    else
                        nest = nest - 1
                        if (inner%body_count < MAXBODY) then
                            inner%body_count = inner%body_count + 1
                            inner%body(inner%body_count) = body(k)
                        end if
                    end if
                else if (inner%do_awaiting_while .and. nest == 0 .and. &
                         starts_with_keyword(to_lower(lstrip_ws(body(k))),'while')) then
                    a = lstrip_ws(body(k))
                    inner%cond = trim(lstrip_ws(a(6:)))
                    inner%do_awaiting_while = .false.
                else
                    if (inner%body_count < MAXBODY) then
                        inner%body_count = inner%body_count + 1
                        inner%body(inner%body_count) = body(k)
                    end if
                end if
            end do

            call run_loop(inner)
            j = k + 1
            cycle
        end if

        ! ---- return ----
        if (starts_with_keyword(s,'return')) then
            rest = trim(lstrip_ws(s(7:)))
            has_return = .true.
            if (len_trim(rest) > 0) then
                return_value = trim(eval_expr_to_str(rest))
            else
                return_value = ''
            end if
            exit
        end if

        ! ---- call (procedure) ----
        if (starts_with_keyword(s,'call')) then
            rest = trim(lstrip_ws(s(5:)))
            call dispatch_call(rest)
            j = j + 1; cycle
        end if

        ! ---- arr/list declarations ----
        if (starts_with_keyword(s,'arr')) then
            call handle_array_decl(s); j = j + 1; cycle
        end if
        if (starts_with_keyword(s,'list')) then
            call handle_list_decl(s); j = j + 1; cycle
        end if

        ! ---- list ops ----
        if (index(s,'.append(') > 0 .or. index(s,'.remove(') > 0 .or. index(s,'.pop(') > 0) then
            call handle_list_op(s); j = j + 1; cycle
        end if

        ! ---- array element assign ----
        if (index(s,'[') > 0 .and. index(s,'=') > 0 .and. index(s,'==') == 0) then
            call handle_array_assign(s); j = j + 1; cycle
        end if

        ! ---- typed declaration ----
        if (index(s,'::') > 0 .and. index(s,'=') > 0) then
            call store_variable(s); j = j + 1; cycle
        end if

        ! ---- assignment ----
        if (index(s,'=') > 0 .and. index(s,'==') == 0) then
            call handle_assignment(s); j = j + 1; cycle
        end if

        ! ---- print ----
        if (starts_with_keyword(s,'print')) then
            rest = lstrip_ws(s(6:))
            if (len_trim(rest) > 0) then
                if (rest(1:1) == ',') rest = lstrip_ws(rest(2:))
            end if
            call do_print(rest); j = j + 1; cycle
        end if

        ! ---- read ----
        if (starts_with_keyword(s,'read')) then
            rest = lstrip_ws(s(5:))
            if (len_trim(rest) > 0) then
                if (rest(1:1) == ',') rest = lstrip_ws(rest(2:))
            end if
            call do_read(rest); j = j + 1; cycle
        end if

        j = j + 1
    end do
end subroutine

! ================= FUNCTION / PROCEDURE SYSTEM =================
subroutine handle_func_start(l, kind)
    character(len=*), intent(in) :: l, kind
    integer :: p1, p2
    character(len=32) :: fname
    character(len=200) :: paramstr
    character(len=32) :: params(MAXPARAM)
    integer :: pcount

    p1 = index(l,'(')
    p2 = index(l,')')
    pcount = 0

    if (p1 <= 0) then
        fname = trim(lstrip_ws(l(len_trim(kind)+1:)))
    else
        fname = trim(lstrip_ws(l(len_trim(kind)+1:p1-1)))
        if (p2 > p1+1) then
            paramstr = l(p1+1:p2-1)
            call parse_param_list(paramstr, params, pcount)
        end if
    end if

    if (func_count >= MAXFUNC) then
        call push_line(outbuf, outcount, 'Error: too many functions/procedures')
        return
    end if
    func_count = func_count + 1
    funcs(func_count)%fname       = trim(fname)
    funcs(func_count)%fkind       = trim(kind)
    funcs(func_count)%param_count = pcount
    funcs(func_count)%params(1:pcount) = params(1:pcount)
    funcs(func_count)%body_count  = 0

    in_func_def      = .true.
    current_func_idx = func_count
    func_def_nest    = 0
    if (TRACE) call dbg('DEF '//trim(kind)//': '//trim(fname))
end subroutine

subroutine append_to_func_body(s)
    character(len=*), intent(in) :: s
    if (funcs(current_func_idx)%body_count < MAXFBODY) then
        funcs(current_func_idx)%body_count = funcs(current_func_idx)%body_count + 1
        funcs(current_func_idx)%body(funcs(current_func_idx)%body_count) = s
    else
        call push_line(outbuf, outcount, 'Error: function body too large')
    end if
end subroutine

integer function get_func_index(name)
    character(len=*), intent(in) :: name
    integer :: i
    get_func_index = 0
    do i = 1, func_count
        if (to_lower(trim(funcs(i)%fname)) == to_lower(trim(name))) then
            get_func_index = i; return
        end if
    end do
end function

subroutine dispatch_call(rest)
    character(len=*), intent(in) :: rest
    character(len=32)  :: fname
    character(len=200) :: argstr, args(MAXPARAM)
    character(len=1000):: dummy
    integer :: argc, p1, p2, fidx

    p1 = index(rest,'(')
    p2 = index(rest,')')
    argc = 0

    if (p1 <= 0) then
        fname = trim(rest)
    else
        fname = trim(lstrip_ws(rest(:p1-1)))
        if (p2 > p1+1) then
            argstr = rest(p1+1:p2-1)
            call parse_arg_list(argstr, args, argc)
        end if
    end if

    fidx = get_func_index(fname)
    if (fidx <= 0) then
        call push_line(outbuf, outcount, 'Error: unknown procedure "'//trim(fname)//'"')
        return
    end if
    call invoke_func(fidx, args, argc, dummy)
end subroutine

subroutine invoke_func(fidx, args, argc, result)
    integer,            intent(in)  :: fidx, argc
    character(len=200), intent(in)  :: args(MAXPARAM)
    character(len=1000),intent(out) :: result

    integer :: saved_vcount, k
    character(len=1000) :: arg_val

    saved_vcount = vcount

    ! Bind parameters
    do k = 1, funcs(fidx)%param_count
        if (k <= argc) then
            arg_val = trim(eval_expr_to_str(args(k)))
        else
            arg_val = ''
        end if
        call set_or_create_var(trim(funcs(fidx)%params(k)), 'str', trim(arg_val))
    end do

    has_return   = .false.
    return_value = ''
    call execute_block(funcs(fidx)%body, funcs(fidx)%body_count)

    result       = trim(return_value)
    has_return   = .false.
    return_value = ''

    ! Pop local variables (restore frame)
    vcount = saved_vcount
end subroutine

! ================= ARRAY / LIST SYSTEM =================
integer function get_array_index(name)
    character(len=*), intent(in) :: name
    integer :: i
    get_array_index = 0
    do i = 1, acount
        if (to_lower(trim(aname(i))) == to_lower(trim(name))) then
            get_array_index = i; return
        end if
    end do
end function

subroutine handle_array_decl(l)
    ! arr :: name[size]
    ! arr :: name[size] = {"a","b","c"}
    character(len=*), intent(in) :: l
    character(len=200) :: rest, anam, sizestr, initstr
    integer :: p2, pb, pe, p3, sz, ai, m

    p2 = index(l,'::')
    if (p2 <= 0) then
        call push_line(outbuf, outcount, 'Error: arr syntax: arr :: name[size]'); return
    end if
    rest = trim(lstrip_ws(l(p2+2:)))
    pb = index(rest,'['); pe = index(rest,']')
    if (pb <= 0 .or. pe <= pb) then
        call push_line(outbuf, outcount, 'Error: arr syntax: arr :: name[size]'); return
    end if
    anam    = trim(lstrip_ws(rest(:pb-1)))
    sizestr = trim(rest(pb+1:pe-1))
    sz = nint(eval_num_expr_val(sizestr))
    if (sz < 1 .or. sz > MAXELEM) then
        call push_line(outbuf, outcount, 'Error: array size must be 1-'//trim(int2s(MAXELEM))); return
    end if

    ai = get_array_index(anam)
    if (ai <= 0) then
        if (acount >= MAXA) then
            call push_line(outbuf, outcount, 'Error: too many arrays/lists'); return
        end if
        acount = acount + 1; ai = acount
    end if
    aname(ai) = trim(anam)
    akind(ai) = 'arr'
    asize(ai) = sz
    do m = 1, MAXELEM
        avals(ai,m) = ''
    end do

    ! Optional initialiser
    p3 = index(l,'=')
    if (p3 > 0 .and. p3 > p2 + pe) then
        initstr = trim(lstrip_ws(l(p3+1:)))
        call init_array_from_brace(ai, sz, initstr)
    end if
    if (TRACE) call dbg('ARR '//trim(anam)//'['//trim(int2s(sz))//']')
end subroutine

subroutine handle_list_decl(l)
    ! list :: name
    ! list :: name = {"a","b","c"}
    character(len=*), intent(in) :: l
    character(len=200) :: rest, lnam, initstr
    integer :: p2, p3, ai, m

    p2 = index(l,'::')
    if (p2 <= 0) then
        call push_line(outbuf, outcount, 'Error: list syntax: list :: name'); return
    end if
    rest = trim(lstrip_ws(l(p2+2:)))
    p3 = index(rest,'=')
    if (p3 > 0) then
        lnam    = trim(lstrip_ws(rest(:p3-1)))
        initstr = trim(lstrip_ws(rest(p3+1:)))
    else
        lnam    = trim(lstrip_ws(rest))
        initstr = ''
    end if

    ai = get_array_index(lnam)
    if (ai <= 0) then
        if (acount >= MAXA) then
            call push_line(outbuf, outcount, 'Error: too many arrays/lists'); return
        end if
        acount = acount + 1; ai = acount
    end if
    aname(ai) = trim(lnam)
    akind(ai) = 'list'
    asize(ai) = 0
    do m = 1, MAXELEM
        avals(ai,m) = ''
    end do

    if (len_trim(initstr) > 0) then
        call init_array_from_brace(ai, MAXELEM, initstr)
        ! Count how many were filled
        asize(ai) = 0
        do m = 1, MAXELEM
            if (len_trim(avals(ai,m)) == 0) exit
            asize(ai) = m
        end do
    end if
    if (TRACE) call dbg('LIST '//trim(lnam))
end subroutine

subroutine init_array_from_brace(ai, maxsz, s)
    integer, intent(in) :: ai, maxsz
    character(len=*), intent(in) :: s
    character(len=400) :: rest, tok
    integer :: p, ecount, n

    rest = trim(lstrip_ws(s))
    ! Strip braces
    if (len_trim(rest) >= 1) then
        if (rest(1:1) == '{') rest = trim(lstrip_ws(rest(2:)))
    end if
    n = len_trim(rest)
    if (n >= 1) then
        if (rest(n:n) == '}') rest = trim(rest(:n-1))
    end if

    ecount = 0
    do while (len_trim(rest) > 0 .and. ecount < maxsz)
        p = index(rest, ',')
        if (p > 0) then
            tok  = trim(lstrip_ws(rest(:p-1)))
            rest = trim(lstrip_ws(rest(p+1:)))
        else
            tok  = trim(lstrip_ws(rest))
            rest = ''
        end if
        ecount = ecount + 1
        avals(ai, ecount) = trim(strip_any_quotes(tok))
    end do
end subroutine

subroutine handle_array_assign(l)
    ! name[i] = expr
    character(len=*), intent(in) :: l
    character(len=200) :: anam, idx_str, expr_str
    integer :: pb, pe, peq, aidx, elem_idx

    pb  = index(l,'[')
    pe  = index(l,']')
    peq = index(l,'=')

    if (pb <= 0 .or. pe <= pb .or. peq <= pe) then
        call handle_assignment(l); return
    end if

    anam     = trim(lstrip_ws(l(:pb-1)))
    idx_str  = trim(l(pb+1:pe-1))
    expr_str = trim(lstrip_ws(l(peq+1:)))

    elem_idx = nint(eval_num_expr_val(idx_str))
    aidx = get_array_index(anam)
    if (aidx <= 0) then
        call push_line(outbuf, outcount, 'Error: unknown array "'//trim(anam)//'"'); return
    end if
    if (elem_idx < 1 .or. elem_idx > MAXELEM) then
        call push_line(outbuf, outcount, 'Error: array index out of bounds'); return
    end if

    avals(aidx, elem_idx) = trim(eval_expr_to_str(expr_str))
    if (akind(aidx) == 'list' .and. elem_idx > asize(aidx)) then
        asize(aidx) = elem_idx
    end if
end subroutine

subroutine handle_list_op(l)
    ! name.append(val)  name.remove(i)  name.pop()
    character(len=*), intent(in) :: l
    character(len=200) :: lnam, op, argstr
    integer :: pd, pb, pe, aidx, idx, m

    pd = index(l,'.')
    pb = index(l,'(')
    pe = index(l,')')
    if (pd <= 0 .or. pb <= pd .or. pe < pb) return

    lnam   = trim(lstrip_ws(l(:pd-1)))
    op     = trim(lstrip_ws(l(pd+1:pb-1)))
    argstr = trim(lstrip_ws(l(pb+1:pe-1)))

    aidx = get_array_index(lnam)
    if (aidx <= 0) then
        call push_line(outbuf, outcount, 'Error: unknown list "'//trim(lnam)//'"'); return
    end if

    select case (to_lower(trim(op)))
    case ('append')
        if (asize(aidx) < MAXELEM) then
            asize(aidx) = asize(aidx) + 1
            avals(aidx, asize(aidx)) = trim(eval_expr_to_str(argstr))
        else
            call push_line(outbuf, outcount, 'Error: list is full')
        end if
    case ('remove')
        idx = nint(eval_num_expr_val(argstr))
        if (idx >= 1 .and. idx <= asize(aidx)) then
            do m = idx, asize(aidx)-1
                avals(aidx,m) = avals(aidx,m+1)
            end do
            avals(aidx, asize(aidx)) = ''
            asize(aidx) = asize(aidx) - 1
        else
            call push_line(outbuf, outcount, 'Error: list index out of bounds')
        end if
    case ('pop')
        if (asize(aidx) > 0) then
            avals(aidx, asize(aidx)) = ''
            asize(aidx) = asize(aidx) - 1
        end if
    end select
end subroutine

! ================= EXPRESSION EVALUATOR (top-level) =================
! Evaluates any expression to a string result (string or numeric)
function eval_expr_to_str(expr) result(res)
    character(len=*), intent(in) :: expr
    character(len=1000) :: res
    character(len=300) :: ex
    real :: numval
    integer :: dp
    character(len=200) :: rv

    ex = trim(lstrip_ws(normalize_html(expr)))
    res = ''

    ! String literal or concat
    if (len_trim(ex) >= 1 .and. ex(1:1) == '"') then
        if (expr_has_str_op(ex)) then
            res = eval_str_expr(ex)
        else
            res = strip_quotes(ex)
        end if
        return
    end if

    if (expr_has_str_op(ex)) then
        res = eval_str_expr(ex)
        return
    end if

    ! Try resolve_value (handles vars, array elems, builtins, func calls)
    rv = trim(resolve_value(ex))
    if (trim(rv) /= trim(ex)) then
        res = rv; return
    end if

    ! Try numeric
    call eval_num_expr(ex, numval, dp)
    if (dp > 0) then
        res = trim(num_to_str(numval, max(1,dp)))
    else
        res = trim(num_to_str(numval, 0))
    end if
end function

! ================= ASSIGNMENT =================
subroutine handle_assignment(line)
    character(len=*), intent(in) :: line
    integer :: p
    character(len=32)  :: name
    character(len=300) :: expr
    real :: val
    integer :: dp
    character(len=200) :: sval

    p = index(line,'=')
    if (p <= 0) return
    name = trim(lstrip_ws(line(:p-1)))
    expr = trim(lstrip_ws(line(p+1:)))

    ! String literal
    if (len_trim(expr) >= 1) then
        if (expr(1:1) == '"') then
            if (expr_has_str_op(expr)) then
                sval = eval_str_expr(expr)
                call set_or_create_var(name, 'str', trim(sval))
            else
                sval = strip_quotes(expr)
                call set_or_create_var(name, 'str', sval)
            end if
            return
        end if
    end if

    ! String concat
    if (expr_has_str_op(expr)) then
        sval = eval_str_expr(expr)
        call set_or_create_var(name, 'str', trim(sval))
        return
    end if

    ! Check if it's a function/builtin call (has parens but not a plain number)
    if (index(expr,'(') > 0) then
        sval = trim(resolve_value(expr))
        if (trim(sval) /= trim(expr)) then
            ! Determine if result looks numeric
            call eval_num_expr(sval, val, dp)
            if (dp > 0) then
                call set_or_create_var(name, 'float', trim(sval))
            else
                ! Try integer
                read(sval,*,err=50,end=50) val
                if (nint(val) == val .and. index(sval,'.') == 0) then
                    call set_or_create_var(name, 'int', trim(sval))
                else
                    call set_or_create_var(name, 'float', trim(sval))
                end if
                return
50              call set_or_create_var(name, 'str', trim(sval))
            end if
            return
        end if
    end if

    ! Array element on RHS: name[i]
    if (index(expr,'[') > 0) then
        sval = trim(resolve_value(expr))
        if (trim(sval) /= trim(expr)) then
            call set_or_create_var(name, 'str', trim(sval))
            return
        end if
    end if

    call eval_num_expr(expr, val, dp)
    if (dp > 0) then
        call set_or_create_var(name, 'float', trim(num_to_str(val, max(1,dp))))
    else
        if (nint(val) == val) then
            call set_or_create_var(name, 'int',   trim(num_to_str(val, 0)))
        else
            call set_or_create_var(name, 'float', trim(num_to_str(val, 6)))
        end if
    end if
end subroutine

subroutine set_or_create_var(name, typ, val)
    character(len=*), intent(in) :: name, typ, val
    integer :: i
    i = get_var_index(name)
    if (i > 0) then
        vtype(i) = to_lower(typ)
        vval(i)  = val
    else
        if (vcount < MAXV) then
            vcount = vcount + 1
            vname(vcount) = trim(name)
            vtype(vcount) = to_lower(typ)
            vval(vcount)  = val
        else
            call push_line(outbuf, outcount, 'Error: variable table full')
        end if
    end if
end subroutine

! ================= VARIABLE STORE =================
subroutine store_variable(cmd)
    character(len=*), intent(in) :: cmd
    character(len=32)  :: name, typ
    character(len=200) :: val, expr_str
    integer :: p2, p3

    p2 = index(cmd,'::')
    p3 = index(cmd,'=')
    read(cmd(:p2-1),*) typ
    typ      = to_lower(trim(typ))
    name     = trim(lstrip_ws(cmd(p2+2:p3-1)))
    expr_str = trim(lstrip_ws(cmd(p3+1:)))

    if (expr_has_str_op(expr_str)) then
        val = eval_str_expr(expr_str)
    else
        val = trim(strip_quotes(expr_str))
    end if

    if (vcount < MAXV) then
        vcount = vcount + 1
        vname(vcount) = trim(name)
        vtype(vcount) = typ
        vval(vcount)  = val
    end if
end subroutine

subroutine set_variable(name, typ, val)
    character(len=*), intent(in) :: name, typ, val
    integer :: idx
    idx = get_var_index(name)
    if (idx > 0) then
        vtype(idx) = typ
        vval(idx)  = val
    end if
end subroutine

integer function get_var_index(name)
    character(len=*), intent(in) :: name
    integer :: i
    get_var_index = 0
    do i = 1, vcount
        if (to_lower(trim(vname(i))) == to_lower(trim(name))) then
            get_var_index = i; return
        end if
    end do
end function

! ================= PRINT =================
subroutine next_token_and_op(s, token, op)
    character(len=*),   intent(inout) :: s
    character(len=200), intent(out)   :: token
    character,          intent(out)   :: op
    integer :: i, n
    logical :: in_squote, in_dquote
    character(len=400) :: t

    token = ''; op = char(0)
    t = lstrip_ws(normalize_html(s))
    n = len_trim(t)
    if (n == 0) then; s = ''; return; end if

    in_squote = .false.; in_dquote = .false.
    i = 1
    do while (i <= n)
        if (.not. in_squote .and. .not. in_dquote) then
            if (t(i:i) == "'") then
                in_squote = .true.
            else if (t(i:i) == '"') then
                in_dquote = .true.
            else if (t(i:i) == '&' .or. t(i:i) == '+') then
                token = trim(t(1:i-1))
                op = t(i:i)
                if (i+1 <= n) then; s = lstrip_ws(t(i+1:)); else; s = ''; end if
                return
            else if (i+4 <= n) then
                if (t(i:i+4) == '&amp;') then
                    token = trim(t(1:i-1))
                    op = '&'
                    if (i+5 <= n) then; s = lstrip_ws(t(i+5:)); else; s = ''; end if
                    return
                end if
            end if
        else
            if (in_squote .and. t(i:i) == "''") in_squote = .false.
            if (in_dquote .and. t(i:i) == '"')  in_dquote = .false.
        end if
        i = i + 1
    end do
    token = trim(t(1:n))
    op = char(0)
    s = ''
end subroutine

subroutine do_print(expr)
    character(len=*), intent(in) :: expr
    character(len=400) :: rest
    character(len=200) :: token, piece
    character :: op, last_op
    logical :: first
    character(len=1000) :: lineout

    rest = lstrip_ws(normalize_html(expr))
    first = .true.; last_op = char(0); lineout = ''

    do
        call next_token_and_op(rest, token, op)
        if (len_trim(token) == 0 .and. op == char(0)) exit
        piece = trim(resolve_value(token))
        if (.not. first) then
            if (last_op == '&') lineout = trim(lineout)//' '
        end if
        if (len_trim(piece) > 0) lineout = trim(lineout)//trim(piece)
        if (op == char(0)) exit
        last_op = op
        first = .false.
    end do
    call push_line(outbuf, outcount, trim(lineout))
end subroutine

! ================= RESOLVE VALUE (single token) =================
! Handles: string literals, variables, array[i], func(args), builtins
function resolve_value(tok) result(res)
    character(len=*), intent(in) :: tok
    character(len=200) :: res
    integer :: i, pb, pe
    real :: rtmp
    integer :: itmp
    character(len=200) :: ttrim, tok_lower, sv, fname_s, idx_str, aname_tok
    character(len=200) :: args(MAXPARAM)
    integer :: argc, fidx, aidx, elem_idx
    character(len=1000) :: func_result

    ttrim     = trim(lstrip_ws(tok))
    tok_lower = to_lower(ttrim)
    res = ''

    if (len_trim(ttrim) == 0) return

    ! String literal "..."
    if (len_trim(ttrim) >= 2) then
        if (ttrim(1:1) == '"' .and. ttrim(len_trim(ttrim):len_trim(ttrim)) == '"') then
            res = ttrim(2:len_trim(ttrim)-1); return
        end if
    end if

    ! Array element: name[i]
    pb = index(ttrim,'[')
    pe = index(ttrim,']')
    if (pb > 0 .and. pe > pb) then
        aname_tok = trim(ttrim(:pb-1))
        idx_str   = trim(ttrim(pb+1:pe-1))
        elem_idx  = nint(eval_num_expr_val(idx_str))
        aidx = get_array_index(aname_tok)
        if (aidx > 0) then
            if (elem_idx >= 1 .and. elem_idx <= asize(aidx)) then
                res = trim(avals(aidx, elem_idx))
            else
                res = 'Error: array index out of bounds'
            end if
        else
            res = 'Error: unknown array "'//trim(aname_tok)//'"'
        end if
        return
    end if

    ! Function call or builtin: name(args)
    pb = index(ttrim,'(')
    pe = len_trim(ttrim)
    if (pb > 0 .and. pe >= pb .and. ttrim(pe:pe) == ')') then
        fname_s = trim(ttrim(:pb-1))
        argc = 0
        if (pe > pb+1) then
            call parse_arg_list(ttrim(pb+1:pe-1), args, argc)
        end if

        select case (to_lower(trim(fname_s)))

        ! ---- Built-in string functions ----
        case ('len')
            if (argc >= 1) then
                res = trim(int2s(len_trim(eval_expr_to_str(args(1)))))
            else
                res = '0'
            end if
            return

        case ('upper')
            if (argc >= 1) res = to_upper(trim(eval_expr_to_str(args(1))))
            return

        case ('lower')
            if (argc >= 1) res = to_lower(trim(eval_expr_to_str(args(1))))
            return

        case ('substr')
            ! substr(str, start, end)  — 1-based inclusive
            if (argc >= 3) then
                sv = trim(eval_expr_to_str(args(1)))
                i   = nint(eval_num_expr_val(args(2)))
                pe  = nint(eval_num_expr_val(args(3)))
                if (i >= 1 .and. pe >= i .and. pe <= len_trim(sv)) then
                    res = sv(i:pe)
                else
                    res = ''
                end if
            end if
            return

        case ('indexof')
            ! indexof(str, substr)  — returns 0 if not found
            if (argc >= 2) then
                sv      = trim(eval_expr_to_str(args(1)))
                fname_s = trim(eval_expr_to_str(args(2)))
                res = trim(int2s(index(trim(sv), trim(fname_s))))
            else
                res = '0'
            end if
            return

        case ('replace')
            ! replace(str, old, new)
            if (argc >= 3) then
                res = str_replace(eval_expr_to_str(args(1)), &
                                  eval_expr_to_str(args(2)), &
                                  eval_expr_to_str(args(3)))
            end if
            return

        case ('trim')
            if (argc >= 1) res = trim(eval_expr_to_str(args(1)))
            return

        case ('arrlen','listlen')
            if (argc >= 1) then
                aidx = get_array_index(trim(eval_expr_to_str(args(1))))
                if (aidx > 0) then
                    res = trim(int2s(asize(aidx)))
                else
                    res = '0'
                end if
            else
                res = '0'
            end if
            return

        ! ---- Type casts ----
        case ('int')
            if (argc >= 1) then
                rtmp = eval_num_expr_val(eval_expr_to_str(args(1)))
                write(res,'(I0)') nint(rtmp)
            else
                res = '0'
            end if
            return

        case ('float')
            if (argc >= 1) then
                rtmp = eval_num_expr_val(eval_expr_to_str(args(1)))
                write(res,'(G0)') rtmp
            else
                res = '0.0'
            end if
            return

        case ('str')
            if (argc >= 1) then
                res = trim(eval_expr_to_str(args(1)))
            end if
            return

        case ('bool')
            if (argc >= 1) then
                sv = to_lower(trim(eval_expr_to_str(args(1))))
                select case (sv)
                case ('true','yes','1','t')
                    res = 'true'
                case ('false','no','0','f','')
                    res = 'false'
                case default
                    rtmp = 0.0
                    read(sv,*,err=801,end=801) rtmp
801                 if (rtmp /= 0.0) then; res = 'true'; else; res = 'false'; end if
                end select
            else
                res = 'false'
            end if
            return

        end select

        ! User-defined function
        fidx = get_func_index(trim(fname_s))
        if (fidx > 0) then
            call invoke_func(fidx, args, argc, func_result)
            res = trim(func_result)
            return
        end if

        ! Unknown — return token as-is
        res = ttrim
        return
    end if

    ! Lookup variable
    do i = 1, vcount
        if (to_lower(trim(vname(i))) == tok_lower) then
            select case (to_lower(trim(vtype(i))))
            case ('str','string')
                res = trim(vval(i))
            case ('int','integer')
                read(vval(i),*,err=901) itmp
                write(res,'(I0)') itmp; return
901             res = 'Error: bad int for '//trim(vname(i))
            case ('float','floatation')
                read(vval(i),*,err=902) rtmp
                write(res,'(G0)') rtmp; return
902             res = 'Error: bad float for '//trim(vname(i))
            case ('char','character')
                sv = trim(vval(i))
                if (len_trim(sv) >= 1) then; res = sv(1:1); else; res = '?'; end if
            case ('bool','booleon')
                res = trim(vval(i))
            case default
                res = trim(vval(i))
            end select
            return
        end if
    end do

    ! Fallback
    res = ttrim
end function

! ================= NUMERIC UTILITIES =================
function num_to_str(x, dp) result(s)
    real, intent(in) :: x
    integer, intent(in) :: dp
    character(len=64) :: s
    character(len=20) :: fmt
    integer :: w
    character(len=64) :: tmp
    if (dp <= 0) then
        write(s,'(I0)') nint(x)
    else
        w = dp + 3
        fmt = '(F'//trim(adjustl(int2s(w)))//'.'//trim(adjustl(int2s(dp)))//')'
        write(tmp,fmt) x
        s = trim(tmp)
    end if
end function

function is_str_token(s) result(r)
    character(len=*), intent(in) :: s
    logical :: r
    integer :: i
    character(len=200) :: t
    t = trim(lstrip_ws(s))
    r = .false.
    if (len_trim(t) >= 1 .and. t(1:1) == '"') then; r = .true.; return; end if
    do i = 1, vcount
        if (to_lower(trim(vname(i))) == to_lower(trim(t))) then
            r = (to_lower(trim(vtype(i))) == 'str' .or. to_lower(trim(vtype(i))) == 'string')
            return
        end if
    end do
end function

function eval_str(s) result(r)
    character(len=*), intent(in) :: s
    character(len=200) :: r
    integer :: i
    if (len_trim(s) >= 1) then
        if (s(1:1) == '"') then; r = strip_quotes(s); return; end if
    end if
    do i = 1, vcount
        if (to_lower(vname(i)) == to_lower(trim(s))) then
            r = vval(i); return
        end if
    end do
    r = trim(s)
end function

! ================= NUMERIC + BOOL EXPRESSION EVALUATORS =================
recursive subroutine eval_num_expr(expr, val, dp)
    character(len=*), intent(in) :: expr
    real,    intent(out) :: val
    integer, intent(out) :: dp
    character(len=200) :: ex
    integer :: p
    real :: vl, vr
    integer :: dpl, dpr
    integer :: idx
    character(len=200) :: left

    dp = 0; val = 0.0
    ex = trim(normalize_html(expr))

    if (index(ex,' + ') > 0) then
        p = index(ex,' + ')
        call eval_num_expr(trim(ex(:p-1)),vl,dpl)
        call eval_num_expr(trim(ex(p+3:)),vr,dpr)
        val = vl + vr; dp = max(dpl,dpr); return
    end if
    if (index(ex,' - ') > 0) then
        p = index(ex,' - ')
        call eval_num_expr(trim(ex(:p-1)),vl,dpl)
        call eval_num_expr(trim(ex(p+3:)),vr,dpr)
        val = vl - vr; dp = max(dpl,dpr); return
    end if
    if (index(ex,' * ') > 0) then
        p = index(ex,' * ')
        call eval_num_expr(trim(ex(:p-1)),vl,dpl)
        call eval_num_expr(trim(ex(p+3:)),vr,dpr)
        val = vl * vr; dp = max(dpl,dpr); return
    end if
    if (index(ex,' / ') > 0) then
        p = index(ex,' / ')
        call eval_num_expr(trim(ex(:p-1)),vl,dpl)
        call eval_num_expr(trim(ex(p+3:)),vr,dpr)
        if (vr == 0.0) then
            call push_line(outbuf, outcount, 'Error: division by zero')
            val = 0.0; dp = 0; return
        end if
        val = vl / vr; dp = max(dpl,dpr); return
    end if
    if (index(ex,' % ') > 0) then
        p = index(ex,' % ')
        call eval_num_expr(trim(ex(:p-1)),vl,dpl)
        call eval_num_expr(trim(ex(p+3:)),vr,dpr)
        val = real(mod(nint(vl), nint(vr))); dp = 0; return
    end if
    if (index(ex,' += ') > 0) then
        p = index(ex,' += ')
        call eval_num_expr(trim(ex(p+4:)),vr,dpr)
        left = trim(ex(:p-1))
        idx = get_var_index(left)
        if (idx <= 0) then
            call push_line(outbuf, outcount, 'Error: variable '//trim(left)//' not found')
            val = 0.0; dp = 0; return
        end if
        read(vval(idx),*) vl
        vl = vl + vr
        write(vval(idx),*) vl
        val = vl; dp = max(dp,dpr); return
    end if
    call get_val_dp(ex, val, dp)
end subroutine

recursive logical function eval_bool_expr(expr) result(res)
    character(len=*), intent(in) :: expr
    character(len=200) :: ex, l, r
    integer :: p

    ex = trim(normalize_html(expr))

    ! not operator
    if (starts_with_keyword(trim(ex),'not ')) then
        res = .not. eval_bool_expr(trim(lstrip_ws(ex(5:))))
        return
    end if
    if (index(to_lower(ex),' or ') > 0) then
        p = index(to_lower(ex),' or ')
        l = trim(ex(:p-1)); r = trim(ex(p+4:))
        res = eval_bool_expr(l) .or. eval_bool_expr(r); return
    end if
    if (index(to_lower(ex),' and ') > 0) then
        p = index(to_lower(ex),' and ')
        l = trim(ex(:p-1)); r = trim(ex(p+5:))
        res = eval_bool_expr(l) .and. eval_bool_expr(r); return
    end if
    res = eval_condition(ex)
end function

logical function eval_condition(expr) result(res)
    character(len=*), intent(in) :: expr
    character(len=200) :: ex, l, r, ls, rs
    integer :: p, oplen, vi
    character(len=2) :: opk

    ex = trim(normalize_html(expr))
    res = .false.
    p = 0; oplen = 0; opk = '  '
    ls = ''; rs = ''

    if (index(ex,'<=') > 0) then
        p = index(ex,'<='); oplen = 2; opk = 'LE'
    else if (index(ex,'>=') > 0) then
        p = index(ex,'>='); oplen = 2; opk = 'GE'
    else if (index(ex,'!=') > 0) then
        p = index(ex,'!='); oplen = 2; opk = 'NE'
    else if (index(ex,'==') > 0) then
        p = index(ex,'=='); oplen = 2; opk = 'EQ'
    else if (index(ex,'<') > 0) then
        p = index(ex,'<');  oplen = 1; opk = 'LT'
    else if (index(ex,'>') > 0) then
        p = index(ex,'>');  oplen = 1; opk = 'GT'
    else
        ! No operator — bare boolean literal or variable
        if (to_lower(trim(ex)) == 'true') then; res = .true.; return; end if
        if (to_lower(trim(ex)) == 'false') then; res = .false.; return; end if
        vi = get_var_index(trim(ex))
        if (vi > 0) then
            res = (to_lower(trim(vval(vi))) == 'true')
        end if
        return
    end if

    l = trim(ex(:p-1))
    r = trim(ex(p+oplen:))

    ! String comparison for == and !=
    if ((opk == 'EQ' .or. opk == 'NE') .and. (is_str_token(l) .or. is_str_token(r))) then
        ls = trim(resolve_value(trim(l)))
        rs = trim(resolve_value(trim(r)))
        if (opk == 'EQ') then
            res = (trim(ls) == trim(rs))
        else
            res = (trim(ls) /= trim(rs))
        end if
        return
    end if

    select case (opk)
    case ('LT'); res = (eval_num_expr_val(l) .lt. eval_num_expr_val(r))
    case ('GT'); res = (eval_num_expr_val(l) .gt. eval_num_expr_val(r))
    case ('LE'); res = (eval_num_expr_val(l) .le. eval_num_expr_val(r))
    case ('GE'); res = (eval_num_expr_val(l) .ge. eval_num_expr_val(r))
    case ('EQ'); res = (eval_num_expr_val(l) .eq. eval_num_expr_val(r))
    case ('NE'); res = (eval_num_expr_val(l) .ne. eval_num_expr_val(r))
    end select
end function

recursive function eval_num_expr_val(expr) result(val)
    character(len=*), intent(in) :: expr
    real :: val
    integer :: dp
    call eval_num_expr(expr, val, dp)
end function

subroutine get_val_dp(expr, val, dp)
    character(len=*), intent(in) :: expr
    real,    intent(out) :: val
    integer, intent(out) :: dp
    integer :: idx
    character(len=200) :: rv

    val = 0.0
    dp = 0

    idx = get_var_index(expr)

    if (idx > 0) then
        if (vtype(idx) == 'int') then
            read(vval(idx),*,err=10,end=10) val
            return
        else if (vtype(idx) == 'float') then
            read(vval(idx),*,err=10,end=10) val
            return
        else
            val = 0.0
        end if
    else
        read(expr,*,err=10,end=10) val
        return
    end if

10  continue   !Try resolve_value (func call, array elem, builtin)

    rv = trim(resolve_value(expr))

    if (trim(rv) /= trim(expr)) then
        read(rv,*,err=20,end=20) val
        return
20      continue
        val = 0.0
    end if

end subroutine

! ================= STRING EXPRESSION EVALUATOR =================
logical function expr_has_str_op(s)
    character(len=*), intent(in) :: s
    integer :: i, n
    logical :: in_sq, in_dq
    in_sq = .false.; in_dq = .false.
    n = len_trim(s)
    expr_has_str_op = .false.
    do i = 1, n
        if (.not. in_sq .and. .not. in_dq) then
            if (s(i:i) == "'") in_sq = .true.
            if (s(i:i) == '"') in_dq = .true.
            if (s(i:i) == '&' .or. s(i:i) == '+') then
                if (i+1 <= n) then
                    if (s(i+1:i+1) == '=') return
                end if
                expr_has_str_op = .true.; return
            end if
        else
            if (in_sq .and. s(i:i) == "'") in_sq = .false.
            if (in_dq .and. s(i:i) == '"') in_dq = .false.
        end if
    end do
end function

function eval_str_expr(expr) result(res)
    character(len=*),   intent(in) :: expr
    character(len=1000) :: res
    character(len=400)  :: rest, token_s
    character(len=200)  :: piece
    character           :: op, last_op
    logical             :: first

    rest    = trim(lstrip_ws(normalize_html(expr)))
    res     = ''
    first   = .true.
    last_op = char(0)

    do
        call next_token_and_op(rest, token_s, op)
        if (len_trim(token_s) == 0 .and. op == char(0)) exit
        piece = trim(resolve_value(token_s))
        if (.not. first) then
            if (last_op == '&') res = trim(res)//' '
        end if
        res   = trim(res)//trim(piece)
        first = .false.
        if (op == char(0)) exit
        last_op = op
    end do
end function

! ================= READ =================
subroutine do_read(rest)
    character(len=*), intent(in) :: rest
    character(len=300) :: r, varname, prompt_str, typ, decl_part, after_decl
    character(len=200) :: input_line
    integer :: comma_pos, dcolon_pos, idx
    logical :: has_prompt, is_decl

    r = trim(lstrip_ws(rest))
    prompt_str = ''; varname = ''; typ = ''
    has_prompt = .false.; is_decl = .false.

    dcolon_pos = index(r,'::')
    if (dcolon_pos > 0) then
        is_decl   = .true.
        decl_part = trim(lstrip_ws(r(:dcolon_pos-1)))
        after_decl= trim(lstrip_ws(r(dcolon_pos+2:)))
        read(decl_part,'(A)') typ
        typ = to_lower(trim(typ))
        select case (typ)
        case ('integer');    typ = 'int'
        case ('floatation'); typ = 'float'
        case ('string');     typ = 'str'
        case ('booleon');    typ = 'bool'
        case ('character');  typ = 'char'
        end select
        r = after_decl
    end if

    comma_pos = 0
    call find_comma_outside_quotes(r, comma_pos)
    if (comma_pos > 0) then
        varname    = trim(lstrip_ws(r(:comma_pos-1)))
        prompt_str = trim(lstrip_ws(r(comma_pos+1:)))
        has_prompt = .true.
    else
        varname = trim(lstrip_ws(r))
    end if
    if (has_prompt) prompt_str = strip_any_quotes(prompt_str)

    call flush_output(outbuf, outcount)
    if (has_prompt .and. len_trim(prompt_str) > 0) then
        write(*,'(A)') trim(prompt_str)
    end if
    if (len_trim(varname) == 0) return

    if (is_decl) then
        idx = get_var_index(varname)
        if (idx <= 0) then
            if (vcount < MAXV) then
                vcount = vcount + 1
                vname(vcount) = trim(varname)
                vtype(vcount) = trim(typ)
                vval(vcount)  = ''
                idx = vcount
            else
                call push_line(outbuf, outcount, 'Error: variable table full'); return
            end if
        else
            vtype(idx) = trim(typ)
        end if
    end if

    read(*,'(A)', end=200) input_line
    input_line = trim(lstrip_ws(input_line))
    goto 201
200 input_line = ''
201 continue

    idx = get_var_index(varname)
    if (idx <= 0) then
        if (vcount < MAXV) then
            vcount = vcount + 1
            vname(vcount) = trim(varname)
            vtype(vcount) = 'str'
            vval(vcount)  = trim(input_line)
        else
            call push_line(outbuf, outcount, 'Error: variable table full')
        end if
        return
    end if

    select case (to_lower(trim(vtype(idx))))
    case ('int','integer')
        vval(idx) = trim(input_line)
    case ('float','floatation','real')
        vval(idx) = trim(input_line)
    case ('bool','booleon')
        select case (to_lower(trim(input_line)))
        case ('true','yes','1','t'); vval(idx) = 'true'
        case default;                vval(idx) = 'false'
        end select
    case ('char','character')
        if (len_trim(input_line) >= 1) then
            vval(idx) = input_line(1:1)
        else
            vval(idx) = ''
        end if
    case default
        vval(idx) = trim(input_line)
    end select
end subroutine

! ================= END PROGRAM =================
subroutine handle_end_prog(given_name)
    character(len=*), intent(in), optional :: given_name
    call flush_output(outbuf, outcount)
    vcount           = 0
    outcount         = 0
    if_sp            = 0
    loop_sp          = 0
    in_prog          = .false.
    current_prog_name= ''
    func_count       = 0
    in_func_def      = .false.
    current_func_idx = 0
    func_def_nest    = 0
    has_return       = .false.
    return_value     = ''
    acount           = 0
end subroutine

end program CLtranslator
