
module parser
    implicit none
    integer, private :: cursor
    character(len=:), allocatable, private :: entrada, esperado ! entrada es la entrada a consumir
    ! variables globales
     
    contains
    ! Funciones globales
     
    subroutine parse(cad) result(res)
        character(len=:), allocatable, intent(in) :: cad
        entrada = cad
        cursor = 1
    
        res = jola() ! esperamos el retorno
    end subroutine parse
    ! funciones útiles
    
function tolower(str) result(lower_str)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: lower_str
    integer :: i

    lower_str = str 
    do i = 1, len(str)
        if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
            lower_str(i:i) = achar(iachar(str(i:i)) + 32)
        end if
    end do
end function tolower

function aceptarPunto() result(aceptacion)
logical :: aceptacion

if (cursor > len(entrada)) then
    aceptacion = .false.
    esperado = "<ANYTHING>"
    return
end if
cursor = cursor + 1
aceptacion = .true.
end function aceptarPunto

function aceptacionEOF() result(aceptacion)
logical :: aceptacion

if(.not. cursor > len(entrada)) then
    aceptacion = .false.
    esperado = "<EOF>"
    return
end if
aceptacion = .true.
end function aceptacionEOF

function acioacioentradal_characters(input_string) result(output_string)
implicit none
character(len=:), allocatable, intent(in) :: input_string
character(len=:), allocatable :: temp_string
character(len=:), allocatable :: output_string
integer :: i, length

temp_string = ""
length = len(input_string)

do i = 1, length
    select case (ichar(input_string(i:i)))
    case (10) ! Nueva línea
        temp_string = temp_string // '\n'
    case (9)  ! Tabulación
        temp_string = temp_string // '\t'
    case (13) ! Retorno de carro
        temp_string = temp_string // '\r'
    case (32) ! Espacio
        if (input_string(i:i) == " ") then
            temp_string = temp_string // "_"
        else
            temp_string = temp_string // input_string(i:i)
        end if
    case default
        temp_string = temp_string // input_string(i:i)
    end select
end do
allocate(character(len=len(temp_string)) :: output_string)
output_string = temp_string
end function acioacioentradal_characters

function aceptarLiterales(literales, isCase) result(aceptacion)
character(len=*) :: literales
character(len=*) :: isCase
logical :: aceptacion
integer :: offset

offset = len(literales) - 1

if (isCase == "i") then ! case insentive
    if (tolower(literales) /= tolower(entrada(cursor:cursor + offset))) then
        aceptacion = .false.
        esperado = literales
        return
    end if
else
    if (literales /= entrada(cursor:cursor + offset)) then
        aceptacion = .false.
        esperado = literales
        return
    end if
end if

cursor = cursor + len(literales)
aceptacion = .true.
return
end function aceptarLiterales

function aceptarRango(inicio, final) result(accept)
character(len=1) :: inicio, final
logical :: accept

if(.not. (entrada(cursor:cursor) >= inicio .and. entrada(cursor:cursor) <= final)) then
    accept = .false.
    return
end if
!lexeme = consume(1)
accept = .true.
end function aceptarRango
    
recursive function jola() result(res)
character(len=:), allocatable :: s00

character(len=:), allocatable
integer :: no_caso
logical :: temporal  ! para el ?


    
    do no_caso = 0, 1 ! lista de concatenaciones
        select case(no_caso)
            
                    case(0)
                        
            if (.not. (aceptarLiterales("","null"))) then
                cycle
            end if
             
res = undefineds00
                        exit
                    
        case default
            return
        end select
    end do
    

    if (cursor > len(entrada)) then
        res = .true.
    end if  
return
END function jola
    
    ! Acciones
    
    ! grupos
    
    end module parser
            