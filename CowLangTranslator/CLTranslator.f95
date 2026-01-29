program CLtranslator
    implicit none

    integer, parameter :: MAXV = 100
    character(len=32)  :: vname(MAXV)
    character(len=16)  :: vtype(MAXV)
    character(len=200) :: vval(MAXV)
    integer :: vcount = 0

    character(len=300) :: line
    logical :: in_prog = .false.
    character(len=64)  :: current_prog_name = ""

    character(len=10000) :: output_buffer = ""

    do
        write(*,'(A)',advance="no") ">>> "
        read(*,'(A)', end=100) line
        line = lstrip_ws(line)
        
        if (index(line, ">>>") == 1) then
            line = lstrip_ws(line(4:))
        end if


        if (starts_with_keyword(line,"prog")) then
            current_prog_name = trim(extract_name_after_keyword(line,"prog"))
            in_prog = .true.
            cycle
        end if

        if (starts_with_keyword(line,"end prog")) then
            call handle_end_prog(line)
            exit
        end if

        if (.not. in_prog) cycle
        if (len_trim(line) == 0) cycle

        call parse_line(line)
    end do
100 continue

contains

! ==================== UTIL ====================

function to_lower(s) result(o)
    character(len=*),intent(in)::s
    character(len=len(s))::o
    integer::i,c
    do i=1,len(s)
        c=iachar(s(i:i))
        if(c>=65.and.c<=90) then
            o(i:i)=achar(c+32)
        else
            o(i:i)=s(i:i)
        end if
    end do
end function

function lstrip_ws(s) result(o)
    character(len=*),intent(in)::s
    character(len=len(s))::o
    integer::i
    i=1
    do while(i<=len(s))
        if(s(i:i)/=' '.and.s(i:i)/=char(9)) exit
        i=i+1
    end do
    if(i<=len(s)) then
        o=s(i:)
    else
        o=""
    end if
end function

logical function starts_with_keyword(s,kw)
    character(len=*),intent(in)::s,kw
    character(len=:),allocatable::t
    t=to_lower(lstrip_ws(s))
    starts_with_keyword = index(t,trim(kw))==1
end function

function extract_name_after_keyword(s,kw) result(n)
    character(len=*),intent(in)::s,kw
    character(len=64)::n
    n=trim(lstrip_ws(s(len_trim(kw)+1:)))
end function

! ==================== PARSER ====================

subroutine parse_line(cmd)
    character(len=*),intent(in)::cmd
    character(len=300)::l,rest

    l=lstrip_ws(cmd)

    if(index(l,"::")>0.and.index(l,"=")>0) then
        call store_variable(l)
        return
    end if

    if(starts_with_keyword(l,"print")) then
        if(index(l,",")>0) then
            rest=lstrip_ws(l(index(l,",")+1:))
        else
            rest=lstrip_ws(l(6:))
        end if
        call do_print(rest)
        return
    end if

    print *,"Unknown Command:",trim(cmd)
end subroutine

! ==================== VARS ====================

subroutine store_variable(cmd)
    character(len=*),intent(in)::cmd
    character(len=32)::name,typ
    character(len=200)::val
    integer::p2,p3

    p2=index(cmd,"::")
    p3=index(cmd,"=")

    read(cmd(:p2-1),*) typ
    name=trim(lstrip_ws(cmd(p2+2:p3-1)))
    val=trim(lstrip_ws(cmd(p3+1:)))

    vcount=vcount+1
    vname(vcount)=name
    vtype(vcount)=to_lower(typ)

    if(vtype(vcount)=="string".or.vtype(vcount)=="str") then
        vval(vcount)=strip_quotes(val)
    else
        vval(vcount)=val
    end if
end subroutine

! ==================== PRINT ====================

subroutine do_print(expr)
    character(len=*),intent(in)::expr
    character(len=400)::rest
    character(len=200)::tok
    character::op,last_op
    logical::first

    rest=lstrip_ws(expr)
    first=.true.
    last_op=char(0)

    do
        call next_token_and_op(rest,tok,op)
        if(len_trim(tok)==0.and.op==char(0)) exit

        if(.not.first.and.last_op=='&') then
            output_buffer=trim(output_buffer)//"_"
        end if

        output_buffer=trim(output_buffer)//trim(get_value(tok))

        if(op==char(0)) exit
        last_op=op
        first=.false.
    end do

    output_buffer=trim(output_buffer)//new_line("a")
end subroutine

subroutine next_token_and_op(s,token,op)
    character(len=*),intent(inout)::s
    character(len=200),intent(out)::token
    character,intent(out)::op
    integer::i,n
    logical::dq

    s=lstrip_ws(s)
    n=len_trim(s)
    dq=.false.

    do i=1,n
        if(s(i:i)=='"') dq=.not.dq
        if(.not.dq.and.(s(i:i)=='&'.or.s(i:i)=='+')) then
            token=trim(s(:i-1))
            op=s(i:i)
            s=lstrip_ws(s(i+1:))
            return
        end if
    end do

    token=trim(s)
    op=char(0)
    s=""
end subroutine

function get_value(tok) result(r)
    character(len=*),intent(in)::tok
    character(len=200)::r
    integer::i

    if(tok(1:1)=='"') then
        r=tok(2:len_trim(tok)-1)
        return
    end if

    do i=1,vcount
        if(to_lower(vname(i))==to_lower(tok)) then
            r=vval(i)
            return
        end if
    end do

    r="Unknown identifier: "//tok
end function

function strip_quotes(s) result(o)
    character(len=*),intent(in)::s
    character(len=200)::o
    o=s
    if(s(1:1)=='"' .and. s(len_trim(s):len_trim(s))=='"') then
        o=s(2:len_trim(s)-1)
    end if
end function

! ==================== END ====================

subroutine handle_end_prog(line)
    character(len=*),intent(in)::line
    print *,"Program '"//trim(current_prog_name)//"' finished. Output:"
    print *,trim(output_buffer)
end subroutine

end program CLtranslator
