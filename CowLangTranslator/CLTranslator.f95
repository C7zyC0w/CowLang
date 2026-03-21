program CLtranslator
    implicit none

    ! ================= PARAMETRES =================
    integer, parameter :: MAXV   = 200
    integer, parameter :: MAXIF  = 100
    integer, parameter :: MAXLOOP= 100
    integer, parameter :: MAXBODY= 4000
    integer, parameter :: MAXOUT = 20000

    ! ---- IO/Debug toggles ----
    logical, parameter :: DIRECT_PRINT = .false.  ! false => buffer & flush at end prog
    logical, parameter :: TRACE        = .false.  ! set true to see trace

    ! ================= VARIABLES =================
    character(len=32)  :: vname(MAXV)
    character(len=16)  :: vtype(MAXV)
    character(len=200) :: vval(MAXV)
    integer :: vcount

    character(len=300) :: line
    logical :: in_prog
    character(len=64) :: current_prog_name
    character(len=64) :: given_name
    character(len=300) :: errmsg

    ! ---- Output line buffer (used when DIRECT_PRINT=.false.) ----
    character(len=1000) :: outbuf(MAXOUT)
    integer :: outcount

    logical :: if_active(MAXIF)
    logical :: if_matched(MAXIF)
    integer :: if_sp

    ! ================= LOOP STRUCT =================
    type :: loop_struct
        character(len=16) :: ltype     ! "for", "while", "dowhile"
        character(len=32) :: var       ! for loop var
        real              :: start, endv, step
        character(len=200):: cond      ! while/dowhile condition
        integer           :: body_count
        character(len=300):: body(MAXBODY)
        integer           :: nest      ! nesting depth while capturing body
        logical           :: do_awaiting_while  ! true when inside "do...while" before "while" seen
    end type

    type(loop_struct) :: loops(MAXLOOP)
    integer :: loop_sp

    ! ---- prompt strings ----
    character(len=*), parameter :: PROMPT_MAIN   = '>>> '
    character(len=*), parameter :: PROMPT_CONT   = '... '
    integer :: depth, i

    ! init
    vcount  = 0
    outcount= 0
    if_sp   = 0
    loop_sp = 0
    in_prog = .false.
    current_prog_name = ''

    ! ================= MAIN REPL LOOP =================
    do
        ! Prompts go to stderr (unit 0) so they never mix with program output
        ! on stdout. Interactive terminal users still see them; pipe/IDE users don't.
        depth = loop_sp + if_sp
        if (depth > 0) then
            write(0,'(A)',advance='no') PROMPT_CONT
            do i=1, depth-1
                write(0,'(A)',advance='no') '    '
            end do
        else
            write(0,'(A)',advance='no') PROMPT_MAIN
        end if

        read(*,'(A)', end=100) line

        ! Normalize & strip possible pasted prompts
        line = normalize_html(line)
        line = strip_any_prompt(line)
        line = lstrip_ws(line)

        ! handle program start/end
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
    if (in_prog) then
        call handle_end_prog()
    end if

contains
! ================== DEBUG / IO HELPERS ==================
subroutine dbg(msg)
    character(len=*), intent(in) :: msg
    if (TRACE) write(*,'(A)') '[TRACE] '//trim(msg)
end subroutine

subroutine push_line(outbuf, outcount, s)
    character(len=1000), intent(inout) :: outbuf(:)
    integer,            intent(inout) :: outcount
    character(len=*),   intent(in)    :: s
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
    integer,            intent(inout) :: outcount
    integer :: k
    if (DIRECT_PRINT) then
        outcount = 0
        return
    end if
    do k = 1, outcount
        write(*,'(A)') trim(outbuf(k))
    end do
    outcount = 0
end subroutine

! ================= UTIL FUNCTIONS =================
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
    ! Replace "&lt;" -> "<", "&gt;" -> ">", "&amp;" -> "&"
    character(len=*), intent(in) :: s
    character(len=len(s)) :: o
    integer :: i, w
    o = ''
    i = 1; w = 1
    do while (i <= len(s))
        if (i+3 <= len(s) .and. s(i:i+3) == '&lt;') then
            if (w <= len(o)) o(w:w) = '<'; w = w + 1; i = i + 4
        else if (i+3 <= len(s) .and. s(i:i+3) == '&gt;') then
            if (w <= len(o)) o(w:w) = '>'; w = w + 1; i = i + 4
        else if (i+4 <= len(s) .and. s(i:i+4) == '&amp;') then
            if (w <= len(o)) o(w:w) = '&'; w = w + 1; i = i + 5
        else
            if (w <= len(o)) o(w:w) = s(i:i); w = w + 1; i = i + 1
        end if
    end do
    if (w <= len(o)) o(w:) = ' '
end function

function strip_any_prompt(s) result(o)
    ! Strip leading ">>> " or "... " and any following spaces
    character(len=*), intent(in) :: s
    character(len=len(s)) :: o
    character(len=:), allocatable :: t
    t = lstrip_ws(s)
    if (len_trim(t) >= 4) then
        if (t(1:4) == '>>> ') then
            o = lstrip_ws(t(5:)); return
        end if
    end if
    if (len_trim(t) >= 4) then
        if (t(1:4) == '... ') then
            o = lstrip_ws(t(5:)); return
        end if
    end if
    o = t
end function

logical function starts_with_keyword(s,kw)
    character(len=*), intent(in) :: s, kw
    starts_with_keyword = index(to_lower(lstrip_ws(s)), trim(kw)) == 1
end function

function extract_name_after_keyword(s,kw) result(n)
    character(len=*), intent(in) :: s, kw
    character(len=64) :: n
    n = trim(lstrip_ws(s(len_trim(kw)+1:)))
end function

function strip_quotes(s) result(o)
    character(len=*), intent(in) :: s
    character(len=200) :: o
    if (len_trim(s) >= 2) then
        if (s(1:1) == '"' .and. s(len_trim(s):len_trim(s)) == '"') then
            o = s(2:len_trim(s)-1)
            return
        end if
    end if
    o = s
end function

! Remove trailing control words ("then"/"do") if present
function rstrip_kw(s, kw) result(out)
    character(len=*), intent(in) :: s, kw
    character(len=200) :: out
    integer :: n, k
    character(len=200) :: low
    n = len_trim(s)
    if (n <= 0) then
        out = ''; return
    end if
    low = to_lower(s(1:n))
    k = len_trim(kw)
    if (n >= k) then
        if (low(n-k+1:n) == to_lower(kw)) then
            out = trim(s(1:n-k))
            return
        end if
        if (n >= k+1) then
            if (low(n-k:n) == ' '//to_lower(kw)) then
                out = trim(s(1:n-k-1))
                return
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
    if (x) then
        s = 'true '
    else
        s = 'false'
    end if
end function

function int2s(i) result(s)
    integer, intent(in) :: i
    character(len=16) :: s
    write(s,'(I0)') i
    s = trim(s)
end function

! ================= PARSER =================
recursive subroutine parse_line(cmd)
    character(len=*), intent(in) :: cmd
    character(len=300) :: l, rest, inner, cond
    integer :: pwhile

    l = lstrip_ws(normalize_html(cmd))

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

    if (loop_sp > 0) then
        ! If we are in a do-while and haven't seen "while" yet,
        ! check if this line is "while <cond>" (the closing condition line)
        if (loops(loop_sp)%do_awaiting_while .and. loops(loop_sp)%nest == 0) then
            if (starts_with_keyword(l,'while')) then
                loops(loop_sp)%cond = trim(lstrip_ws(l(6:)))
                loops(loop_sp)%do_awaiting_while = .false.
                if (TRACE) call dbg('DOWHILE got cond: ['//trim(loops(loop_sp)%cond)//']')
                ! Now wait for "end do"
                return
            end if
        end if

        if (starts_with_keyword(l,'for') .or. &
            starts_with_keyword(l,'while') .or. &
            (trim(to_lower(l)) == 'do')) then
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

    if (starts_with_keyword(l,'for') .or. &
        starts_with_keyword(l,'while') .or. &
        (trim(to_lower(l)) == 'do')) then
        call handle_loop_start(l)
        return
    end if

    if (starts_with_keyword(l,'end for') .or. &
        starts_with_keyword(l,'end while') .or. &
        starts_with_keyword(l,'end do')) then
        return
    end if

    if (index(l,'::')>0 .and. index(l,'=')>0) then
        call store_variable(l)
        return
    end if

    if (index(l,'=')>0 .and. index(l,'==')==0) then
        call handle_assignment(l)
        return
    end if

    if (starts_with_keyword(l,'print')) then
        rest = lstrip_ws(l(6:))
        if (len_trim(rest) > 0) then
            if (rest(1:1) == ',') then
                rest = lstrip_ws(rest(2:))
            end if
        end if
        call do_print(rest)
        return
    end if

    if (starts_with_keyword(l,'read')) then
        rest = lstrip_ws(l(5:))
        ! strip optional leading comma  (e.g. "read, "prompt"")
        if (len_trim(rest) > 0) then
            if (rest(1:1) == ',') rest = lstrip_ws(rest(2:))
        end if
        call do_read(rest)
        return
    end if
end subroutine

subroutine execute_stmt_list(list)
    character(len=*), intent(in) :: list
    character(len=300) :: s, part
    integer :: p
    s = trim(list)
    do while (len_trim(s) > 0)
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
    do
        if (.not. eval_bool_expr(condition)) exit
        call execute_stmt_list(stmt)
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
    if_active(if_sp) = r
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
    if_active(if_sp) = r
    if_matched(if_sp) = r
    if (TRACE) call dbg('ELIF -> '//trim(bool_str(r)))
end subroutine

subroutine handle_else()
    if (if_sp == 0) return
    if (.not. if_matched(if_sp)) then
        if_active(if_sp) = .true.
        if_matched(if_sp) = .true.
    else
        if_active(if_sp) = .false.
    end if
    if (TRACE) call dbg('ELSE active='//trim(bool_str(if_active(if_sp))))
end subroutine

subroutine handle_end_if()
    if (if_sp > 0) if_sp = if_sp - 1
    if (TRACE) call dbg('END IF')
end subroutine

! ================= LOOP CAPTURE + EXECUTE =================
subroutine handle_loop_start(l)
    character(len=*), intent(in) :: l
    integer :: p1,p2,p3,dpdum
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

    loops(loop_sp)%body_count = 0
    loops(loop_sp)%nest = 0
    loops(loop_sp)%var = ''
    loops(loop_sp)%cond = ''
    loops(loop_sp)%start = 0.0
    loops(loop_sp)%endv  = 0.0
    loops(loop_sp)%step  = 1.0
    loops(loop_sp)%do_awaiting_while = .false.

    if (starts_with_keyword(low,'for')) then
        loops(loop_sp)%ltype = 'for'
        p1 = index(low,'range')
        p2 = index(low,' to ')
        p3 = index(low,' step ')

        if (p1 <= 0 .or. p2 <= 0) then
            call push_line(outbuf, outcount, 'Error: for loop syntax: for VAR range X to Y [step Z]')
            loop_sp = loop_sp - 1
            return
        end if

        varname = trim(lstrip_ws(l(5:p1-1)))

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
        if (TRACE) then
            call dbg('FOR '//trim(varname)//' from '//trim(num_to_str(startv,0)))
            call dbg('  to '//trim(num_to_str(endv,0))//' step '//trim(num_to_str(stepv,0)))
        end if

    else if (starts_with_keyword(low,'while')) then
        loops(loop_sp)%ltype = 'while'
        rest = strip_ctrl_tail(l(6:))
        loops(loop_sp)%cond = rest
        if (TRACE) call dbg('WHILE cond: ['//trim(rest)//']')

    else if (trim(low) == 'do') then
        ! "do" alone starts a do-while body capture; "while <cond>" closes it
        loops(loop_sp)%ltype = 'dowhile'
        loops(loop_sp)%do_awaiting_while = .true.
        if (TRACE) call dbg('DO (awaiting while <cond>)')

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
        do while ( (ls%step >= 0.0 .and. val <= ls%endv) .or. (ls%step < 0.0 .and. val >= ls%endv) )
            call set_variable(ls%var,'int',trim(num_to_str(val,0)))
            call execute_block(ls%body, ls%body_count)
            val = val + ls%step
        end do
    case ('while')
        do while (eval_bool_expr(ls%cond))
            call execute_block(ls%body, ls%body_count)
        end do
    case ('dowhile')
        do
            call execute_block(ls%body, ls%body_count)
            if (.not. eval_bool_expr(ls%cond)) exit
        end do
    case default
        call push_line(outbuf, outcount, 'Error: unknown loop type')
    end select
end subroutine

subroutine execute_block(body, count)
    character(len=300), intent(in) :: body(:)
    integer,            intent(in) :: count

    integer :: j, k, start_idx, nest, dpdum
    character(len=300) :: s, low
    type(loop_struct)  :: inner
    integer :: p1, p2, p3
    character(len=200) :: a, b, c
    character(len=300) :: rest

    j = 1
    do while (j <= count)
        s = lstrip_ws(normalize_html(body(j)))
        low = to_lower(s)

        ! Nested loop inside body
        if (starts_with_keyword(low,'for') .or. &
            starts_with_keyword(low,'while') .or. &
            (trim(low) == 'do')) then

            inner%body_count = 0
            inner%nest = 0
            inner%var = ''
            inner%cond = ''
            inner%start = 0.0
            inner%endv  = 0.0
            inner%step  = 1.0
            inner%do_awaiting_while = .false.

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
                    (trim(to_lower(lstrip_ws(body(k)))) == 'do')) then
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
                    ! This is the "while <cond>" line for the do-while
                    a = lstrip_ws(body(k))
                    inner%cond = trim(lstrip_ws(a(6:)))
                    inner%do_awaiting_while = .false.
                    ! Don't add to body; next line should be "end do"
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

        ! Declarations inside body
        if (index(s,'::')>0 .and. index(s,'=')>0) then
            call store_variable(s)
            j = j + 1
            cycle
        end if

        ! Assignment inside body
        if (index(s,'=')>0 .and. index(s,'==')==0) then
            call handle_assignment(s)
            j = j + 1
            cycle
        end if

        ! print
        if (starts_with_keyword(s,'print')) then
            rest = lstrip_ws(s(6:))
            if (len_trim(rest) > 0) then
                if (rest(1:1) == ',') then
                    rest = lstrip_ws(rest(2:))
                end if
            end if
            call do_print(rest)
            j = j + 1
            cycle
        end if

        ! read
        if (starts_with_keyword(s,'read')) then
            rest = lstrip_ws(s(5:))
            if (len_trim(rest) > 0) then
                if (rest(1:1) == ',') rest = lstrip_ws(rest(2:))
            end if
            call do_read(rest)
            j = j + 1
            cycle
        end if

        j = j + 1
    end do
end subroutine

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

    ! string literal
    if (len_trim(expr) >= 1) then
        if (expr(1:1) == '"') then
            ! still check for concat: "hello" & varname
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

    ! variable or expression containing & or + for string concat
    if (expr_has_str_op(expr)) then
        sval = eval_str_expr(expr)
        ! keep the existing type if variable already declared, else str
        call set_or_create_var(name, 'str', trim(sval))
        return
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

! ================= VARIABLE + PRINT + STRINGS + CONDITIONS =================
subroutine store_variable(cmd)
    character(len=*), intent(in) :: cmd
    character(len=32)  :: name, typ
    character(len=200) :: val, expr_str
    integer :: p2, p3
    real :: numval
    integer :: dp

    p2 = index(cmd,'::')
    p3 = index(cmd,'=')

    read(cmd(:p2-1),*) typ
    typ      = to_lower(trim(typ))
    name     = trim(lstrip_ws(cmd(p2+2:p3-1)))
    expr_str = trim(lstrip_ws(cmd(p3+1:)))

    ! If the expression contains & or + (outside quotes) treat as str concat
    if (expr_has_str_op(expr_str)) then
        val = eval_str_expr(expr_str)
        if (vcount < MAXV) then
            vcount = vcount + 1
            vname(vcount) = trim(name)
            vtype(vcount) = typ
            vval(vcount)  = trim(val)
        end if
        return
    end if

    ! plain literal or numeric
    val = trim(strip_quotes(expr_str))

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
        if (to_lower(vname(i)) == to_lower(trim(name))) then
            get_var_index = i
            return
        end if
    end do
end function

! ---- tokenizer for print: supports + (tight) and & (space). Also detects literal "&amp;".
subroutine next_token_and_op(s, token, op)
    character(len=*),  intent(inout) :: s
    character(len=200),intent(out)   :: token
    character,         intent(out)   :: op
    integer :: i, n
    logical :: in_squote, in_dquote
    character(len=400) :: t

    token = ''; op = char(0)
    t = lstrip_ws(normalize_html(s))      ! Option A: normalize &amp; -> &
    n = len_trim(t)
    if (n == 0) then
        s = ''; return
    end if

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
                if (i+1 <= n) then
                    s = lstrip_ws(t(i+1:))
                else
                    s = ''
                end if
                return
            else if (i+4 <= n) then
                ! Option B: detect literal "&amp;" as '&'
                if (t(i:i+4) == '&amp;') then
                    token = trim(t(1:i-1))
                    op = '&'
                    if (i+5 <= n) then
                        s = lstrip_ws(t(i+5:))
                    else
                        s = ''
                    end if
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
            if (last_op == '&') then
                lineout = trim(lineout)//' '
            end if
        end if

        if (len_trim(piece) > 0) then
            lineout = trim(lineout)//trim(piece)
        end if

        if (op == char(0)) exit
        last_op = op
        first = .false.
    end do

    call push_line(outbuf, outcount, trim(lineout))
end subroutine

function resolve_value(tok) result(res)
    character(len=*), intent(in) :: tok
    character(len=200) :: res
    integer :: i
    real :: rtmp
    integer :: itmp
    character(len=200) :: ttrim, tok_lower, s

    ttrim = trim(lstrip_ws(tok))
    tok_lower = to_lower(ttrim)
    res = ''

    ! string literal
    if (len_trim(ttrim) >= 2) then
        if ( (ttrim(1:1) == '"' .and. ttrim(len_trim(ttrim):len_trim(ttrim)) == '"') .or. &
             (ttrim(1:1) == "''" .and. ttrim(len_trim(ttrim):len_trim(ttrim)) == "''") ) then
            res = ttrim(2:len_trim(ttrim)-1)
            return
        end if
    end if

    ! lookup variable
    do i = 1, vcount
        if (to_lower(trim(vname(i))) == tok_lower) then
            select case (to_lower(trim(vtype(i))))
            case ('str','string')
                res = trim(vval(i))
            case ('int','integer')
                read(vval(i),*,err=901) itmp
                write(res,'(I0)') itmp
                return
901             continue
                res = 'Error: bad int value for '//trim(vname(i))
            case ('float','floatation')
                read(vval(i),*,err=902) rtmp
                write(res,'(G0)') rtmp
                return
902             continue
                res = 'Error: bad float value for '//trim(vname(i))
            case ('char','character')
                s = trim(vval(i))
                if (len_trim(s) >= 1) then
                    res = s(1:1)
                else
                    res = '?'
                end if
            case ('bool','booleon')
                res = trim(vval(i))
            case default
                res = trim(vval(i))
            end select
            return
        end if
    end do

    ! fallback: return token text
    res = ttrim
end function

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

logical function is_string(s)
    character(len=*), intent(in) :: s
    integer :: i
    is_string = .false.
    if (len_trim(s) >= 1) then
        if (s(1:1) == '"') then
            is_string = .true.
            return
        end if
    end if
    do i = 1, vcount
        if (to_lower(vname(i)) == to_lower(trim(s))) then
            is_string = (vtype(i) == 'str')
            return
        end if
    end do
end function

function eval_str(s) result(r)
    character(len=*), intent(in) :: s
    character(len=200) :: r
    integer :: i
    if (len_trim(s) >= 1) then
        if (s(1:1) == '"') then
            r = strip_quotes(s)
            return
        end if
    end if
    do i = 1, vcount
        if (to_lower(vname(i)) == to_lower(trim(s))) then
            r = vval(i)
            return
        end if
    end do
    r = trim(s)
end function

! ================= NUMERIC + BOOL =================
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

    dp = 0
    val = 0.0
    ex = trim(normalize_html(expr))

    if (index(ex,' + ') > 0) then
        p = index(ex,' + ')
        call eval_num_expr(trim(ex(:p-1)),vl,dpl)
        call eval_num_expr(trim(ex(p+3:)),vr,dpr)
        val = vl + vr
        dp = max(dpl,dpr)
        return
    end if

    if (index(ex,' - ') > 0) then
        p = index(ex,' - ')
        call eval_num_expr(trim(ex(:p-1)),vl,dpl)
        call eval_num_expr(trim(ex(p+3:)),vr,dpr)
        val = vl - vr
        dp = max(dpl,dpr)
        return
    end if

    if (index(ex,' * ') > 0) then
        p = index(ex,' * ')
        call eval_num_expr(trim(ex(:p-1)),vl,dpl)
        call eval_num_expr(trim(ex(p+3:)),vr,dpr)
        val = vl * vr
        dp = max(dpl,dpr)
        return
    end if

    if (index(ex,' / ') > 0) then
        p = index(ex,' / ')
        call eval_num_expr(trim(ex(:p-1)),vl,dpl)
        call eval_num_expr(trim(ex(p+3:)),vr,dpr)
        val = vl / vr
        dp = max(dpl,dpr)
        return
    end if

<<<<<<< HEAD
    ! += is handled at the assignment level, not inside numeric expression eval
    ! so we skip it here and fall through to get_val_dp
=======
<<<<<<< HEAD
    ! += is handled at the assignment level, not inside numeric expression eval
    ! so we skip it here and fall through to get_val_dp
=======
    if (index(ex, ' += ') > 0) then
        p = index(ex, ' += ')
        call eval_num_expr(trim(ex(p+4:)),vr,dpr)
        left = trim(ex(:p-1))
        idx = get_var_index(left)
        if (idx <= 0) then
            call push_line(outbuf, outcount, 'Error: variable '//trim(left)//' not found')
            val = 0.0
            dp = 0
            return
        end if
        read(vval(idx),*) vl
        vl = vl + vr
        write(vval(idx),*) vl
        val = vl
        dp = max(dp, dpr)
        return
    end if
>>>>>>> 521d9d61c24255c412b0f9c3b0500c46e958336b
>>>>>>> 9907cdcab7ad78e3e20eb753dacb85e6dd6a5bf9

    call get_val_dp(ex,val,dp)
end subroutine

recursive logical function eval_bool_expr(expr) result(res)
    character(len=*), intent(in) :: expr
    character(len=200) :: ex, l, r
    integer :: p

    ex = trim(normalize_html(expr))

    if (index(to_lower(ex),' or ') > 0) then
        p = index(to_lower(ex),' or ')
        l = trim(ex(:p-1))
        r = trim(ex(p+4:))
        res = eval_bool_expr(l) .or. eval_bool_expr(r)
        return
    end if
    if (index(to_lower(ex),' and ') > 0) then
        p = index(to_lower(ex),' and ')
        l = trim(ex(:p-1))
        r = trim(ex(p+5:))
        res = eval_bool_expr(l) .and. eval_bool_expr(r)
        return
    end if

    res = eval_condition(ex)
end function

logical function eval_condition(expr) result(res)
    character(len=*), intent(in) :: expr
    character(len=200) :: ex, l, r
    integer :: p, oplen
    character(len=2) :: opk

    ex = trim(normalize_html(expr))
    res = .false.
    p = 0; oplen = 0; opk = '  '

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
        res = .false.
        return
    end if

    l = trim(ex(:p-1))
    r = trim(ex(p+oplen:))

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
    call eval_num_expr(expr,val,dp)
end function

subroutine get_val_dp(expr, val, dp)
    character(len=*), intent(in) :: expr
    real,    intent(out) :: val
    integer, intent(out) :: dp
    integer :: idx
    val = 0.0
    dp = 0
    idx = get_var_index(expr)
    if (idx > 0) then
        if (vtype(idx) == 'int') then
            read(vval(idx),*) val
        else if (vtype(idx) == 'float') then
            read(vval(idx),*) val
        else
            val = 0.0
        end if
    else
        read(expr,*,err=100,end=100) val
    end if
100 continue
end subroutine

! ================= STRING EXPRESSION EVALUATOR =================
! eval_str_expr: resolves a & / + concatenation expression to a string.
!   &  adds a space between values  (like Python's join-with-space)
!   +  concatenates without a space
! Any token that is a known variable is resolved to its value.
! Numeric variables are coerced to their display string.
! String literals (single or double quoted) are unquoted.
!
! expr_has_str_op: returns .true. if the expression contains a bare
! & or + that is outside quotes — meaning it is a string concat op.
! This lets handle_assignment / store_variable decide which path to use.

logical function expr_has_str_op(s)
    character(len=*), intent(in) :: s
    integer :: i, n
    logical :: in_sq, in_dq
    in_sq = .false.; in_dq = .false.
    n = len_trim(s)
    expr_has_str_op = .false.
    do i = 1, n
        if (.not. in_sq .and. .not. in_dq) then
            if (s(i:i) == "'")  in_sq = .true.
            if (s(i:i) == '"')  in_dq = .true.
            if (s(i:i) == '&' .or. s(i:i) == '+') then
                ! Make sure it's not == or +=  (those are two-char ops)
                if (i+1 <= n) then
                    if (s(i+1:i+1) == '=' ) return  ! skip += or ==
                end if
                ! Also skip numeric-only contexts: if there are NO variable
                ! names in the expression, & and + are numeric ops, not string.
                ! We rely on the caller (handle_assignment) to try numeric first
                ! when no variables are found. Here we just say "yes, op present".
                expr_has_str_op = .true.
                return
            end if
        else
            if (in_sq .and. s(i:i) == "'")  in_sq = .false.
            if (in_dq .and. s(i:i) == '"')  in_dq = .false.
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

    rest     = trim(lstrip_ws(normalize_html(expr)))
    res      = ''
    first    = .true.
    last_op  = char(0)

    do
        call next_token_and_op(rest, token_s, op)
        if (len_trim(token_s) == 0 .and. op == char(0)) exit

        ! resolve the token: literal or variable
        piece = trim(resolve_value(token_s))

        if (.not. first) then
            if (last_op == '&') then
                ! & inserts a space between values
                res = trim(res)//' '
            end if
            ! + just concatenates (no space)
        end if

        res   = trim(res)//trim(piece)
        first = .false.

        if (op == char(0)) exit
        last_op = op
    end do
end function

! ================= READ =================
! Supported syntaxes:
!   read varname
!   read varname, "prompt"
!   read, "prompt"                      (prompt only, no capture)
!   read type :: varname                (declare then read)
!   read type :: varname, "prompt"      (declare then read with prompt)
!
! The prompt (if any) is printed to stdout immediately (no newline),
! then the user's input is read on the next line.
! User input is NOT echoed back or prefixed with ">>>".
subroutine do_read(rest)
    character(len=*), intent(in) :: rest
    character(len=300) :: r, varname, prompt_str, typ, decl_part, after_decl
    character(len=200) :: input_line
    integer :: comma_pos, dcolon_pos, idx
    logical :: has_prompt, is_decl

    r          = trim(lstrip_ws(rest))
    prompt_str = ''
    varname    = ''
    typ        = ''
    has_prompt = .false.
    is_decl    = .false.

    ! ── detect "type :: varname" declaration form ─────────────────────────────
    dcolon_pos = index(r, '::')
    if (dcolon_pos > 0) then
        is_decl    = .true.
        decl_part  = trim(lstrip_ws(r(:dcolon_pos-1)))  ! e.g. "int"
        after_decl = trim(lstrip_ws(r(dcolon_pos+2:)))  ! e.g. 'varname, "prompt"'
        read(decl_part, '(A)') typ
        typ = to_lower(trim(typ))
        ! normalise type aliases to match vtype conventions
        select case (typ)
        case ('integer');    typ = 'int'
        case ('floatation'); typ = 'float'
        case ('string');     typ = 'str'
        case ('booleon');    typ = 'bool'
        case ('character');  typ = 'char'
        end select
        r = after_decl
    end if

    ! ── split off optional prompt after comma ─────────────────────────────────
    ! We look for the LAST comma that is outside quotes to be safe,
    ! but a simple first-comma-outside-quotes is fine for read syntax.
    comma_pos = 0
    call find_comma_outside_quotes(r, comma_pos)

    if (comma_pos > 0) then
        varname    = trim(lstrip_ws(r(:comma_pos-1)))
        prompt_str = trim(lstrip_ws(r(comma_pos+1:)))
        has_prompt = .true.
    else
        varname = trim(lstrip_ws(r))
    end if

    ! strip quotes from prompt if present
    if (has_prompt) then
        prompt_str = strip_any_quotes(prompt_str)
    end if

    ! ── flush buffered output BEFORE the prompt so it appears in order ────────
    call flush_output(outbuf, outcount)

    ! ── print prompt to stdout (no newline) ───────────────────────────────────
    if (has_prompt .and. len_trim(prompt_str) > 0) then
        write(*,'(A)') trim(prompt_str)
    end if

    ! ── if varname is empty (bare "read, prompt") just return after prompt ────
    if (len_trim(varname) == 0) return

    ! ── declare variable if this is a declaration form ────────────────────────
    if (is_decl) then
        idx = get_var_index(varname)
        if (idx <= 0) then
            ! create with empty value
            if (vcount < MAXV) then
                vcount = vcount + 1
                vname(vcount) = trim(varname)
                vtype(vcount) = trim(typ)
                vval(vcount)  = ''
                idx = vcount
            else
                call push_line(outbuf, outcount, 'Error: variable table full')
                return
            end if
        else
            vtype(idx) = trim(typ)
        end if
    end if

    ! ── read a line from stdin ────────────────────────────────────────────────
    read(*,'(A)', end=200) input_line
    input_line = trim(lstrip_ws(input_line))
    goto 201
200 input_line = ''
201 continue

    ! ── store into variable ───────────────────────────────────────────────────
    idx = get_var_index(varname)
    if (idx <= 0) then
        ! variable not declared — auto-create as str
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

    ! coerce to declared type
    select case (to_lower(trim(vtype(idx))))
    case ('int','integer')
        ! validate it looks numeric; store as-is for int2s later
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
    case default   ! str / string
        vval(idx) = trim(input_line)
    end select
end subroutine

! Helper: find position of first comma that is outside single or double quotes
subroutine find_comma_outside_quotes(s, pos)
    character(len=*), intent(in)  :: s
    integer,          intent(out) :: pos
    integer :: i, n
    logical :: in_sq, in_dq
    pos = 0; in_sq = .false.; in_dq = .false.
    n = len_trim(s)
    do i = 1, n
        if (.not. in_sq .and. .not. in_dq) then
            if (s(i:i) == "'")  in_sq = .true.
            if (s(i:i) == '"')  in_dq = .true.
            if (s(i:i) == ',')  then; pos = i; return; end if
        else
            if (in_sq .and. s(i:i) == "'")  in_sq = .false.
            if (in_dq .and. s(i:i) == '"')  in_dq = .false.
        end if
    end do
end subroutine

! Helper: strip surrounding single or double quotes from a string
function strip_any_quotes(s) result(o)
    character(len=*),   intent(in) :: s
    character(len=200) :: o
    integer :: n
    o = trim(s)
    n = len_trim(o)
    if (n >= 2) then
        if ((o(1:1) == '"'  .and. o(n:n) == '"') .or. &
            (o(1:1) == "'"  .and. o(n:n) == "'")) then
            o = o(2:n-1)
        end if
    end if
end function

! ================= END PROGRAM =================
subroutine handle_end_prog(given_name)
    character(len=*), intent(in), optional :: given_name
    ! Flush buffered program output to stdout — no extra noise lines
    call flush_output(outbuf, outcount)

    vcount  = 0
    outcount= 0
    if_sp   = 0
    loop_sp = 0
    in_prog = .false.
    current_prog_name = ''
end subroutine

end program CLtranslator