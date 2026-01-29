program CLtranslator
    implicit none

    integer, parameter :: MAXV   = 100
    integer, parameter :: MAXOUT = 1000

    character(len=32)   :: vname(MAXV)
    character(len=16)   :: vtype(MAXV)
    character(len=200)  :: vval(MAXV)
    integer             :: vcount = 0

    ! ---- Print buffer as lines (safer than one big string)
    character(len=1000) :: outbuf(MAXOUT)
    integer             :: outcount = 0

    character(len=300)  :: line
    logical             :: in_prog = .false.
    character(len=64)   :: current_prog_name = ""

    ! main input loop
    do
        read(*,'(A)', end=100) line

        ! Normalize "&amp;" -> "&" (for pasted HTML) and strip leading ws
        line = replace_all_amp(line)
        line = lstrip_ws(line)

        ! detect "prog <name>"
        if (starts_with_keyword(line, "prog")) then
            current_prog_name = trim(extract_name_after_keyword(line, "prog"))
            in_prog = .true.
            cycle
        end if

        ! detect "end prog <name>"
        if (starts_with_keyword(line, "end prog")) then
            call handle_end_prog(line, current_prog_name, in_prog, outbuf, outcount)
            exit
        end if

        if (.not. in_prog) cycle
        if (len_trim(line) == 0) cycle

        call parse_line(line, outbuf, outcount)
    end do

100 continue
    ! If EOF while in a program, flush once
    if (in_prog) then
        call print_program_finished(current_prog_name)
        call flush_output(outbuf, outcount)
        call clear_state(vcount, outcount)
    end if

contains
    ! --------------------------
    function to_lower(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: i, c
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= iachar('A') .and. c <= iachar('Z')) then
                out(i:i) = achar(c + 32)
            else
                out(i:i) = s(i:i)
            end if
        end do
    end function to_lower

    ! --------------------------
    function replace_all_amp(s) result(out)
        ! Replace the literal sequence "&;" with "&"
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: i, n, wpos

        out  = s  ! default (so length stays consistent)
        wpos = 1
        n    = len(s)
        i    = 1
        do while (i <= n)
            if (i+4 <= n .and. s(i:i+4) == "&;") then
                out(wpos:wpos) = "&"
                wpos = wpos + 1
                i    = i + 5
            else
                out(wpos:wpos) = s(i:i)
                wpos = wpos + 1
                i    = i + 1
            end if
        end do
        ! If we wrote fewer chars than len(out), fill the rest with blanks
        if (wpos <= len(out)) out(wpos:) = " "
    end function replace_all_amp

    ! --------------------------
    function lstrip_ws(s) result(out)
        ! Remove leading spaces, tabs, and carriage returns.
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: i, n, c
        n = len(s); i = 1
        do while (i <= n)
            c = iachar(s(i:i))
            if (c == 32 .or. c == 9 .or. c == 13) then
                i = i + 1
            else
                exit
            end if
        end do
        if (i <= n) then; out = s(i:); else; out = ""; end if
    end function lstrip_ws

    ! --------------------------
    logical function starts_with_keyword(s, kw, next_char) result(ok)
        character(len=*), intent(in) :: s, kw
        character(len=*), intent(in), optional :: next_char
        integer :: n, klen
        character(len=:), allocatable :: t
        character :: ch

        t = to_lower(lstrip_ws(s))
        klen = len_trim(kw); n = len_trim(t)

        ok = .false.
        if (n < klen) return
        if (t(1:klen) /= to_lower(kw(1:klen))) return
        if (n == klen) then; ok = .true.; return; end if

        ch = t(klen+1:klen+1)
        if (ch == ' ') then
            ok = .true.
        else if (present(next_char)) then
            if (ch == next_char(1:1)) ok = .true.
        else
            if (.not. ((ch >= 'a' .and. ch <= 'z') .or. (ch >= '0' .and. ch <= '9'))) ok = .true.
        end if
    end function starts_with_keyword

    ! --------------------------
    function extract_name_after_keyword(s, kw) result(name)
        character(len=*), intent(in) :: s, kw
        character(len=64) :: name
        character(len=:), allocatable :: t
        integer :: klen, n

        t    = lstrip_ws(s)
        klen = len_trim(kw)
        n    = len_trim(t)

        if (n <= klen) then
            name = ""
        else
            name = trim(lstrip_ws(t(klen+1:)))
        end if
    end function extract_name_after_keyword

    ! --------------------------
    subroutine handle_end_prog(line, current_name, in_prog_flag, outbuf, outcount)
        character(len=*), intent(in) :: line
        character(len=*), intent(inout) :: current_name
        logical, intent(inout) :: in_prog_flag
        character(len=1000), intent(inout) :: outbuf(:)
        integer, intent(inout) :: outcount
        character(len=64) :: end_name

        end_name = extract_name_after_keyword(line, "end prog")

        if (.not. in_prog_flag) then
            print *, "Warning: 'end prog' found without matching 'prog'"
        else
            if (len_trim(end_name) > 0 .and. len_trim(current_name) > 0) then
                if (to_lower(trim(end_name)) /= to_lower(trim(current_name))) then
                    print *, "Warning: end prog '"//trim(end_name)//"' does not match started prog '"//trim(current_name)//"'"
                end if
            end if
        end if

        call print_program_finished(current_name)
        call flush_output(outbuf, outcount)

        ! Reset state for next program
        in_prog_flag = .false.
        current_name = ""
        call clear_state(vcount, outcount)
    end subroutine handle_end_prog

    ! --------------------------
    subroutine print_program_finished(pname)
        character(len=*), intent(in) :: pname
        if (len_trim(pname) > 0) then
            print *, "Program '"//trim(pname)//"' finished. Output:"
        else
            print *, "Program finished. Output:"
        end if
    end subroutine print_program_finished

    ! --------------------------
    subroutine flush_output(outbuf, outcount)
        character(len=1000), intent(inout) :: outbuf(:)
        integer, intent(inout) :: outcount
        integer :: i
        do i = 1, outcount
            write(*,'(A)') trim(outbuf(i))
        end do
        outcount = 0
    end subroutine flush_output

    ! --------------------------
    subroutine clear_state(vcount, outcount)
        integer, intent(inout) :: vcount, outcount
        vcount   = 0
        outcount = 0
    end subroutine clear_state

    ! --------------------------
    function strip_quotes(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len_trim(s)) :: out
        integer :: n
        n = len_trim(s)
        if (n >= 2) then
            if ( (s(1:1) == '"'  .and. s(n:n) == '"')  .or. &
                 (s(1:1) == "'"  .and. s(n:n) == "'") ) then
                out = s(2:n-1)
                return
            end if
        end if
        out = s(1:n)
    end function strip_quotes

    ! --------------------------
    subroutine parse_line(cmd, outbuf, outcount)
        character(len=*), intent(in)    :: cmd
        character(len=1000), intent(inout) :: outbuf(:)
        integer, intent(inout) :: outcount
        character(len=300) :: line_norm, rest
        integer :: n

        line_norm = lstrip_ws(cmd)
        n = len_trim(line_norm)

        ! variable declaration
        if (index(to_lower(line_norm),"::") > 0 .and. index(line_norm,"=") > 0) then
            call store_variable(line_norm)
            return
        end if

        ! print command: "print expr" or "print, expr"
        if (starts_with_keyword(line_norm, "print", ",")) then
            if (n >= 6) then
                if (line_norm(6:6) == ",") then
                    rest = lstrip_ws(line_norm(7:))
                else
                    rest = lstrip_ws(line_norm(6:))
                end if
            else
                rest = ""
            end if

            if (len_trim(rest) > 0) then
                call do_print(rest, outbuf, outcount)
            else
                call push_line(outbuf, outcount, "Error: print statement has nothing to print")
            end if
            return
        end if

        call push_line(outbuf, outcount, "Unknown Command: "//trim(cmd))
    end subroutine parse_line

    ! --------------------------
    subroutine store_variable(cmd)
        character(len=*), intent(in) :: cmd
        integer :: p1, p2, p3
        character(len=32) :: name, typ
        character(len=200) :: val
        character(len=32) :: typ_lower
        character(len=200) :: sval
        character(len=300) :: line

        line = lstrip_ws(cmd)
        p2 = index(line,"::"); p3 = index(line,"="); p1 = p2 - 1

        if (p1 < 1 .or. p2 <= 0 .or. p3 <= 0 .or. p3 <= p2) then
            print *, "Error: invalid declaration ->", trim(cmd)
            return
        end if

        read(line(1:p1),*,err=10) typ
        name = trim(lstrip_ws(line(p2+2:p3-1)))
        val  = trim(lstrip_ws(line(p3+1:)))
        goto 11
10      continue
        print *, "Error: invalid type in declaration ->", trim(cmd)
        return
11      continue

        vcount = vcount + 1
        if (vcount > MAXV) then
            print *, "Error: variable table full"
            vcount = MAXV
            return
        end if

        vname(vcount) = trim(name)
        vtype(vcount) = trim(typ)
        typ_lower = to_lower(vtype(vcount))

        select case (typ_lower)
        case ("bool","booleon")
            if (to_lower(val) == "true") then
                vval(vcount) = "true"
            else if (to_lower(val) == "false") then
                vval(vcount) = "false"
            else
                print *, "Error: bool must be true/false for", trim(name)
                vval(vcount) = "false"
            end if

        case ("char","character")
            sval = strip_quotes(val)
            if (len_trim(sval) == 1) then
                vval(vcount) = sval
            else
                print *, "Error: char must be one character for", trim(name)
                if (len_trim(sval) > 0) then
                    vval(vcount) = sval(1:1)
                else
                    vval(vcount) = "?"
                end if
            end if

        case ("str","string")
            vval(vcount) = strip_quotes(val)

        case default
            vval(vcount) = val
        end select
    end subroutine store_variable

    ! --------------------------
    subroutine next_token_and_op(s, token, op)
        ! Extract next token (respecting quotes) and the following operator ('&' or '+').
        ! Returns token, op, and advances s.
        character(len=*), intent(inout) :: s
        character(len=200), intent(out) :: token
        character, intent(out) :: op
        integer :: i, n
        logical :: in_squote, in_dquote
        character(len=400) :: t

        token = ""; op = char(0)
        ! Normalize any "&amp;" left in expr (safety)
        t = replace_all_amp(lstrip_ws(s))
        n = len_trim(t)
        if (n == 0) then; s = ""; return; end if

        in_squote = .false.; in_dquote = .false.
        do i = 1, n
            if (.not. in_squote .and. .not. in_dquote) then
                select case (t(i:i))
                case ("'")
                    in_squote = .true.
                case ('"')
                    in_dquote = .true.
                case ('&','+')
                    token = trim(t(1:i-1))
                    op = t(i:i)
                    if (i+1 <= n) then; s = lstrip_ws(t(i+1:)); else; s = ""; end if
                    return
                end select
            else
                if (in_squote .and. t(i:i) == "'") in_squote = .false.
                if (in_dquote .and. t(i:i) == '"') in_dquote = .false.
            end if
        end do

        token = trim(t(1:n)); op = char(0); s = ""
    end subroutine next_token_and_op

    ! --------------------------
    subroutine do_print(expr, outbuf, outcount)
        ! '&'  -> insert a space between parts (A & B => "A B")
        ! '+'  -> normal concatenation (A + B => "AB")
        ! Build a single line locally, then push to outbuf.
        character(len=*), intent(in)       :: expr
        character(len=1000), intent(inout) :: outbuf(:)
        integer, intent(inout)             :: outcount
        character(len=400)  :: rest
        character(len=200)  :: token
        character           :: op, last_op
        logical             :: first
        character(len=1000) :: lineout
        character(len=200)  :: piece

        rest = replace_all_amp(lstrip_ws(expr))
        first = .true.; last_op = char(0); lineout = ""

        do
            call next_token_and_op(rest, token, op)
            if (len_trim(token) == 0 .and. op == char(0)) exit

            piece = trim(get_value(token))

            if (.not. first) then
                if (last_op == '&') then
                    if (len_trim(lineout) < len(lineout)) lineout = trim(lineout)//" "
                end if
            end if

            if (len_trim(piece) > 0) then
                if (len_trim(lineout) + len_trim(piece) <= len(lineout)) then
                    lineout = trim(lineout)//trim(piece)
                else
                    ! truncate safely if needed
                    lineout = trim(lineout)//trim(piece(1:max(0, len(lineout)-len_trim(lineout))))
                end if
            end if

            if (op == char(0)) exit
            last_op = op
            first = .false.
        end do

        call push_line(outbuf, outcount, trim(lineout))
    end subroutine do_print

    ! --------------------------
    subroutine push_line(outbuf, outcount, s)
        character(len=1000), intent(inout) :: outbuf(:)
        integer, intent(inout) :: outcount
        character(len=*), intent(in) :: s
        if (outcount < size(outbuf)) then
            outcount = outcount + 1
            outbuf(outcount) = s
        else
            write(*,*) "Error: output buffer full"
        end if
    end subroutine push_line

    ! --------------------------
    function get_value(tok) result(res)
        character(len=*), intent(in) :: tok
        character(len=200) :: res
        integer :: i
        real    :: rtmp
        integer :: itmp
        character(len=200) :: ttrim, tok_lower, s

        ttrim = trim(lstrip_ws(tok))
        tok_lower = to_lower(ttrim)
        res = ""

        ! reject type names
        if (tok_lower=="str" .or. tok_lower=="string" .or. tok_lower=="int" .or. tok_lower=="integer" .or. &
            tok_lower=="float" .or. tok_lower=="floatation" .or. tok_lower=="char" .or. tok_lower=="character" .or. &
            tok_lower=="bool" .or. tok_lower=="booleon") then
            res = "Error: cannot print type "//ttrim
            return
        end if

        ! string literal: accept "..." or '...'
        if (len_trim(ttrim) >= 2) then
            if ( (ttrim(1:1) == '"' .and. ttrim(len_trim(ttrim):len_trim(ttrim)) == '"') .or. &
                 (ttrim(1:1) == "'" .and. ttrim(len_trim(ttrim):len_trim(ttrim)) == "'") ) then
                res = ttrim(2:len_trim(ttrim)-1)
                return
            end if
        end if

        ! lookup variable
        do i = 1, vcount
            if (to_lower(trim(vname(i))) == tok_lower) then
                select case(to_lower(trim(vtype(i))))
                case ("str","string")
                    res = trim(vval(i))
                case ("int","integer")
                    read(vval(i),*,err=901) itmp
                    write(res,'(I0)') itmp
                    return
901                 continue
                    res = "Error: bad int value for "//trim(vname(i))
                case ("float","floatation")
                    read(vval(i),*,err=902) rtmp
                    write(res,'(G0)') rtmp
                    return
902                 continue
                    res = "Error: bad float value for "//trim(vname(i))
                case ("char","character")
                    s = trim(vval(i))
                    if (len_trim(s) >= 1) then
                        res = s(1:1)
                    else
                        res = "?"
                    end if
                case ("bool","booleon")
                    res = trim(vval(i))
                case default
                    res = trim(vval(i))
                end select
                return
            end if
        end do

        res = "Unknown identifier: "//ttrim
    end function get_value

end program CLtranslator