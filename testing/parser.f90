
module parser
implicit none
integer, private :: cursor
character(len=:), allocatable, private :: entrada, expected ! entrada es la entrada a consumir

	! initial code from FortranPEG grammar
    	type :: node	
		integer :: value
		type(node), pointer :: next => null()
	end type node

	type(node), pointer :: head => null()

    	! global var for input
	character(len=:), allocatable :: input



contains

! codigo del inge




subroutine parse(cad)
    character(len=:), allocatable, intent(in) :: cad
    type(node), pointer :: res
    entrada = cad
    cursor = 1

    res => hola()

end subroutine parse

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

function replace_special_characters(input_string) result(output_string)
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
end function

function aceptarLiterales(literales, isCase) result(aceptacion)
    character(len=*) :: literales
    character(len=*) :: isCase
    logical :: aceptacion
    integer :: offset
    
    offset = len(literales) - 1

    if (isCase == "i") then ! case insentive
        if (tolower(literales) /= tolower(entrada(cursor:cursor + offset))) then
            aceptacion = .false.
            expected = literales
            return
        end if
    else
        if (literales /= entrada(cursor:cursor + offset)) then
            aceptacion = .false.
            expected = literales
            return
        end if
    end if

    cursor = cursor + len(literales);
    aceptacion = .true.
    return
end function aceptarLiterales

function hola() result(aceptacion)
    logical :: aceptacion
    integer :: no_caso
    logical :: temporal

    aceptacion = .false.
        
        do no_caso = 0, 1 ! lista de concatenaciones
            select case(no_caso)
                
                        case(0)
                            
                if (.not. (ever())) then
                    cycle
                end if
                
                            exit
                        
            case default
                return
            end select
        end do
        
    
    
        if (cursor > len(entrada)) then
            aceptacion = .true.
        end if
    return
END function hola
        

function ever() result(aceptacion)
    logical :: aceptacion
    integer :: no_caso
    logical :: temporal

    aceptacion = .false.
        
        do no_caso = 0, 1 ! lista de concatenaciones
            select case(no_caso)
                
                        case(0)
                            
                if (.not. (aceptarLiterales("si","null"))) then
                    cycle
                end if
                
                            exit
                        
            case default
                return
            end select
        end do
        
    
    
    return
END function ever
        
end module parser
        