// Simple Arithmetics Grammar
// ==========================
//
// Accepts expressions like "2 * (3 + 4)" and computes their value.

{
    type :: operation
        character(len=:), allocatable :: operator
        integer :: operand
    end type

    contains
}

Expression
  = head:Term tail:ExpressionTail* {
        integer :: res
        integer :: i

        if (size(tail) < 0) then
            res = head
            return
        end if

        do i = 1, size(tail)
            if (tail(i)%operator == '+') then
                head = head + tail(i)%operand
            else
                head = head - tail(i)%operand
            end if
        end do

        res = head
    }

ExpressionTail
    = _ operator:("+" / "-") _ operand:Term {
        type(operation) :: res

        res = operation(operator, operand)
    }

Term
  = head:Factor
    tail:(operator:("*" / "/") _ operand:Factor {
        type(operation) :: res

        res = operation(operator, operand)
    })*
    {
        integer :: res
        integer :: i

        if (size(tail) < 0) then
            res = head
            return
        end if

        do i = 1, size(tail)
            if (tail(i)%operator == '*') then
                head = head * tail(i)%operand
            else
                head = head / tail(i)%operand
            end if
        end do

        res = head
    }

Factor
  = "(" _ @Expression _ ")"
  / Integer

Integer "integer"
  = _ num:[0-9]+ {
        integer :: res

        read(num, *) res
    }

_ "whitespace"
  = [ \t\n\r]*