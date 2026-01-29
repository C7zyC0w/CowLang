program CLtranslator
    implicit none

    integer, parameter :: MAXV = 100, MAXIF = 50
    character(len=32)  :: vname(MAXV)
    character(len=16)  :: vtype(MAXV)
    character(len=200) :: vval(MAXV)
    integer :: vcount = 0

    character(len=300) :: line
    logical :: in_prog = .false.
    character(len=64)  :: current_prog_name = ""

    character(len=10000) :: output_buffer = ""

    logical :: if_active(MAXIF)
    logical :: if_matched(MAXIF)
    integer :: if_sp = 0

    do
        write(*,'(A)',advance="no") ">>> "
        read(*,'(A)', end=100) line
        line = lstrip_ws(line)

        if (index(line,">>>")==1) line=lstrip_ws(line(4:))

        if(starts_with_keyword(line,"prog")) then
            current_prog_name=trim(extract_name_after_keyword(line,"prog"))
            in_prog=.true.
            cycle
        end if

        if(starts_with_keyword(line,"end prog")) then
            call handle_end_prog()
            exit
        end if

        if(.not.in_prog) cycle
        if(len_trim(line)==0) cycle

        call parse_line(line)
    end do
100 continue

    print *,"Press ENTER to exit..."
    read(*,'(A)') line

contains

! ================= UTIL =================

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
    if(i<=len(s)) o=s(i:)
    if(i>len(s))  o=""
end function

logical function starts_with_keyword(s,kw)
    character(len=*),intent(in)::s,kw
    starts_with_keyword=index(to_lower(lstrip_ws(s)),trim(kw))==1
end function

function extract_name_after_keyword(s,kw) result(n)
    character(len=*),intent(in)::s,kw
    character(len=64)::n
    n=trim(lstrip_ws(s(len_trim(kw)+1:)))
end function

! ================= PARSER =================

subroutine parse_line(cmd)
    character(len=*),intent(in)::cmd
    character(len=300)::l,rest

    l=lstrip_ws(cmd)

    if(starts_with_keyword(l,"if")) then
        call handle_if(l)
        return
    end if

    if(starts_with_keyword(l,"elif")) then
        call handle_elif(l)
        return
    end if

    if(starts_with_keyword(l,"else")) then
        call handle_else()
        return
    end if

    if(starts_with_keyword(l,"end if")) then
        call handle_end_if()
        return
    end if

    if(if_sp>0) then
        if(.not.if_active(if_sp)) return
    end if

    if(index(l,"::")>0.and.index(l,"=")>0) then
        call store_variable(l)
        return
    end if

    if(starts_with_keyword(l,"print")) then
        rest=lstrip_ws(l(index(l,",")+1:))
        call do_print(rest)
        return
    end if
end subroutine

! ================= IF =================

subroutine handle_if(line)
    character(len=*),intent(in)::line
    logical::res
    res = eval_bool_expr(strip_then(line(3:)))
    if_sp=if_sp+1
    if_active(if_sp)=res
    if_matched(if_sp)=res
end subroutine

subroutine handle_elif(line)
    character(len=*),intent(in)::line
    logical::res
    character(len=200)::cond

    if(if_sp==0) return
    if(if_matched(if_sp)) then
        if_active(if_sp)=.false.
        return
    end if

    cond = strip_then(line(5:))
    res = eval_bool_expr(cond)

    if_active(if_sp)=res
    if_matched(if_sp)=res
end subroutine

subroutine handle_else()
    if(if_sp==0) return
    if(.not.if_matched(if_sp)) then
        if_active(if_sp)=.true.
        if_matched(if_sp)=.true.
    else
        if_active(if_sp)=.false.
    end if
end subroutine

subroutine handle_end_if()
    if(if_sp>0) if_sp=if_sp-1
end subroutine

! ================= BOOL =================

recursive logical function eval_bool_expr(expr) result(res)
    character(len=*),intent(in)::expr
    character(len=200)::l,r
    integer::p

    if(index(to_lower(expr)," or ")>0) then
        p=index(to_lower(expr)," or ")
        l=trim(expr(:p-1))
        r=trim(expr(p+4:))
        res = eval_bool_expr(l) .or. eval_bool_expr(r)
        return
    end if

    if(index(to_lower(expr)," and ")>0) then
        p=index(to_lower(expr)," and ")
        l=trim(expr(:p-1))
        r=trim(expr(p+5:))
        res = eval_bool_expr(l) .and. eval_bool_expr(r)
        return
    end if

    res = eval_condition(expr)
end function

! ================= NUMERIC EXPRESSIONS =================

recursive subroutine eval_num_expr(expr, val, dp)
    character(len=*),intent(in)::expr
    real,intent(out)::val
    integer,intent(out)::dp
    character(len=200)::l,r
    integer::p
    real::vl,vr
    integer::dpl,dpr

    dp = 0
    val = 0.0
    l = trim(expr)

    ! handle +, -, *, /
    if(index(l," + ")>0) then
        p=index(l," + ")
        call eval_num_expr(trim(l(:p-1)), vl, dpl)
        call eval_num_expr(trim(l(p+3:)), vr, dpr)
        val = vl + vr
        dp = max(dpl,dpr)
        return
    end if

    if(index(l," - ")>0) then
        p=index(l," - ")
        call eval_num_expr(trim(l(:p-1)), vl, dpl)
        call eval_num_expr(trim(l(p+3:)), vr, dpr)
        val = vl - vr
        dp = max(dpl,dpr)
        return
    end if

    if(index(l," * ")>0) then
        p=index(l," * ")
        call eval_num_expr(trim(l(:p-1)), vl, dpl)
        call eval_num_expr(trim(l(p+3:)), vr, dpr)
        val = vl * vr
        dp = max(dpl,dpr)
        return
    end if

    if(index(l," / ")>0) then
        p=index(l," / ")
        call eval_num_expr(trim(l(:p-1)), vl, dpl)
        call eval_num_expr(trim(l(p+3:)), vr, dpr)
        val = vl / vr
        dp = max(dpl,dpr)
        return
    end if

    ! literal number or variable
    call get_val_dp(l, val, dp)
end subroutine

subroutine get_val_dp(s, val, dp)
    character(len=*),intent(in)::s
    real,intent(out)::val
    integer,intent(out)::dp
    integer::i,ios

    val = 0.0
    dp = 0

    do i=1,vcount
        if(to_lower(vname(i))==to_lower(trim(s))) then
            read(vval(i),*,iostat=ios) val
            if(ios/=0) val=0.0
            if(vtype(i)=="float") then
                dp = get_decimal_places(vval(i))
            else
                dp = 0
            end if
            return
        end if
    end do

    read(s,*,iostat=ios) val
    if(ios/=0) val = 0.0
    dp = get_decimal_places(s)
end subroutine

integer function get_decimal_places(s)
    character(len=*),intent(in)::s
    integer::p
    p=index(s,'.')
    if(p>0) then
        get_decimal_places = len_trim(s) - p
    else
        get_decimal_places = 0
    end if
end function

! ================= PRINT =================

function num_to_str(x, dp) result(s)
    real,intent(in)::x
    integer,intent(in)::dp
    character(len=64)::s
    character(len=20)::fmt
    integer :: w
    character(len=64)::tmp

    if(dp<=0) then
        ! integer output with no leading space
        write(s,'(I0)') nint(x)
    else
        ! float output with decimal
        w = dp + 3
        fmt = '(F'//trim(adjustl(int_to_str(w)))//'.'//trim(adjustl(int_to_str(dp)))//')'
        write(tmp,fmt) x
        s = trim(tmp)
    end if
end function

function int_to_str(i) result(s)
    integer,intent(in)::i
    character(len=12)::s
    write(s,'(I0)') i
    s = trim(s)
end function

! ================= CONDITIONS =================

logical function eval_condition(expr)
    character(len=*),intent(in)::expr
    character(len=200)::l,r
    character(len=2)::op

    call split_comparison(expr,l,op,r)

    if(is_string(l).or.is_string(r)) then
        select case(op)
        case("=="); eval_condition = trim(eval_str(l)) == trim(eval_str(r))
        case("!="); eval_condition = trim(eval_str(l)) /= trim(eval_str(r))
        case default; eval_condition = .false.
        end select
        return
    end if

    select case(op)
    case("<");  eval_condition = eval_num_expr_val(l) <  eval_num_expr_val(r)
    case("<="); eval_condition = eval_num_expr_val(l) <= eval_num_expr_val(r)
    case(">");  eval_condition = eval_num_expr_val(l) >  eval_num_expr_val(r)
    case(">="); eval_condition = eval_num_expr_val(l) >= eval_num_expr_val(r)
    case("=="); eval_condition = eval_num_expr_val(l) == eval_num_expr_val(r)
    case("!="); eval_condition = eval_num_expr_val(l) /= eval_num_expr_val(r)
    case default
        eval_condition=.false.
    end select
end function

real function eval_num_expr_val(s) result(r)
    character(len=*),intent(in)::s
    integer :: dp
    call eval_num_expr(s, r, dp)
end function

! ================= DO PRINT =================

subroutine do_print(expr)
    character(len=*),intent(in)::expr
    real :: val
    integer :: dp

    if(is_string(expr)) then
        output_buffer=trim(output_buffer)//trim(eval_str(expr))//new_line("a")
    else
        call eval_num_expr(expr, val, dp)
        output_buffer=trim(output_buffer)//trim(num_to_str(val, dp))//new_line("a")
    end if
end subroutine

! ================= VARS =================

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
    vval(vcount)=strip_quotes(val)
end subroutine

subroutine handle_end_prog()
    print *,"Program '"//trim(current_prog_name)//"' finished. Output:"
    print *,trim(output_buffer)
end subroutine

function strip_then(s) result(o)
    character(len=*),intent(in)::s
    character(len=200)::o
    integer::p
    o=trim(s)
    p=index(to_lower(o),"then")
    if(p>0) o=trim(o(:p-1))
end function

function strip_quotes(s) result(o)
    character(len=*),intent(in)::s
    character(len=200)::o
    o=s
    if(s(1:1)=='"' .and. s(len_trim(s):len_trim(s))=='"') &
        o=s(2:len_trim(s)-1)
end function

subroutine split_comparison(s,l,op,r)
    character(len=*),intent(in)::s
    character(len=200),intent(out)::l,r
    character(len=2),intent(out)::op
    integer::p

    if(index(s,"==")>0) then; op="=="; p=index(s,"==")
    elseif(index(s,"!=")>0) then; op="!="; p=index(s,"!=")
    elseif(index(s,">=")>0) then; op=">="; p=index(s,">=")
    elseif(index(s,"<=")>0) then; op="<="; p=index(s,"<=")
    elseif(index(s,">")>0)  then; op=">";  p=index(s,">")
    elseif(index(s,"<")>0)  then; op="<";  p=index(s,"<")
    else; op=""; p=0
    end if

    if(p>0) then
        l=trim(s(:p-1))
        r=trim(s(p+len_trim(op):))
    end if
end subroutine

! ================= STRINGS =================

logical function is_string(s)
    character(len=*),intent(in)::s
    integer::i
    if(s(1:1)=='"') then
        is_string=.true.
        return
    end if
    do i=1,vcount
        if(to_lower(vname(i))==to_lower(trim(s))) then
            is_string = (vtype(i)=="str")
            return
        end if
    end do
    is_string=.false.
end function

function eval_str(s) result(r)
    character(len=*),intent(in)::s
    character(len=200)::r
    integer::i

    if(s(1:1)=='"') then
        r=strip_quotes(s)
        return
    end if

    do i=1,vcount
        if(to_lower(vname(i))==to_lower(trim(s))) then
            r=vval(i)
            return
        end if
    end do

    r=trim(s)
end function

end program CLtranslator
