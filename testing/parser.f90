
module parser
    implicit none
    integer, private :: cursor, InicioLexema, GuardarPunto
    character(len=:), allocatable, private :: entrada, esperado, verificador ! entrada es la entrada a consumir
    ! variables globales
    
        type :: operation
            character(len=:), allocatable :: operator
            integer :: operand
        end type
    
        
    contains
    ! Funciones globales
     
    function parse(cad) result(res)
        character(len=:), allocatable, intent(in) :: cad
        character(len=:), allocatable :: res
        entrada = cad
        cursor = 1
            
        res = pExpression() ! esperamos el retorno
    end function parse
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
    end function replace_special_characters
    
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
    
    function aceptarRango(inicio, final, isCase) result(accept)
        character(len=1) :: inicio, final
        character(len=*) :: isCase
        logical :: accept
        if (isCase == "i") then
            if(.not. (tolower(entrada(cursor:cursor)) >= tolower(inicio) .and. &
             tolower(entrada(cursor:cursor)) <= tolower(final))) then
                accept = .false.
                return
            end if
        else
            if(.not. (entrada(cursor:cursor) >= inicio .and. entrada(cursor:cursor) <= final)) then
                accept = .false.
                return
            end if
        end if
        !lexeme = consume(1)
        cursor = cursor +1
        accept = .true.
    end function aceptarRango
    
    function ConsumirEntrada() result(Expresion)
        character(len=:), allocatable :: Expresion
        Expresion = entrada(InicioLexema:cursor - 1) 
    end function ConsumirEntrada
    
    
    function aceptarConjunto(set, isCase) result(accept)
        character(len=1), dimension(:) :: set
        character(len=*) :: isCase
        logical :: accept
    
        if (isCase == "i") then
            if(.not. (findloc(set, tolower(entrada(cursor:cursor)), 1) > 0)) then
                accept = .false.
                return
            end if
        else 
            if(.not. (findloc(set, entrada(cursor:cursor), 1) > 0)) then
                accept = .false.
                return
            end if
        end if
        
        cursor = cursor + 1
        accept = .true.
    end function aceptarConjunto
    
    
    
    
    recursive function pExpression() result(res)
        character(len=:), allocatable :: s00
    character(len=:), allocatable :: s01
     
        character(len=:), allocatable :: res
        integer :: i
        integer :: no_caso
        logical :: temporal  ! para el ?
     
            GuardarPunto = cursor
            
            do no_caso = 0, 1 ! lista de concatenaciones
                select case(no_caso)
                    
                            case(0)
                                cursor = GuardarPunto
                                
                s00 = pTerm()
                
    
            do while (len(entrada) >= cursor)
                s01 =s01//pExpressionTail()
            end do
             
    res = s00//s01
                                exit
                            
                case default
                    return
                end select
            end do
            
    
        return
    END function pExpression
            
    
    recursive function pExpressionTail() result(res)
        character(len=:), allocatable :: s00
    character(len=:), allocatable :: s01
    character(len=:), allocatable :: s02
    character(len=:), allocatable :: s03
     
        character(len=:), allocatable :: res
        integer :: i
        integer :: no_caso
        logical :: temporal  ! para el ?
     
            GuardarPunto = cursor
            
            do no_caso = 0, 1 ! lista de concatenaciones
                select case(no_caso)
                    
                            case(0)
                                cursor = GuardarPunto
                                
                s00 = p_()
                
    
                s01 = grupo0()
                
    
                s02 = p_()
                
    
                s03 = pTerm()
                 
    res = s00//s01//s02//s03
                                exit
                            
                case default
                    return
                end select
            end do
            
    
        return
    END function pExpressionTail
            
    
    recursive function pTerm() result(res)
        character(len=:), allocatable :: s00
    character(len=:), allocatable :: s01
     
        character(len=:), allocatable :: res
        integer :: i
        integer :: no_caso
        logical :: temporal  ! para el ?
     
            GuardarPunto = cursor
            
            do no_caso = 0, 1 ! lista de concatenaciones
                select case(no_caso)
                    
                            case(0)
                                cursor = GuardarPunto
                                
                s00 = pFactor()
                
    
            do while (len(entrada) >= cursor)
                s01 =s01//grupo1()
            end do
             
    res = s00//s01
                                exit
                            
                case default
                    return
                end select
            end do
            
    
        return
    END function pTerm
            
    
    recursive function pFactor() result(res)
        character(len=:), allocatable :: s00
    character(len=:), allocatable :: s01
    character(len=:), allocatable :: s02
    character(len=:), allocatable :: s03
    character(len=:), allocatable :: s04
    character(len=:), allocatable :: s10
     
        character(len=:), allocatable :: res
        integer :: i
        integer :: no_caso
        logical :: temporal  ! para el ?
     
            GuardarPunto = cursor
            
            do no_caso = 0, 2 ! lista de concatenaciones
                select case(no_caso)
                    
                            case(0)
                                cursor = GuardarPunto
                                
                InicioLexema = cursor
                if (.not. aceptarLiterales("(","null")) then
                    cycle
                end if
                s00 = ConsumirEntrada()
                        
    
                s01 = p_()
                
    
                s02 = pExpression()
                
    
                s03 = p_()
                
    
                InicioLexema = cursor
                if (.not. aceptarLiterales(")","null")) then
                    cycle
                end if
                s04 = ConsumirEntrada()
                         
    res = s02
                                exit
                            
    
                            case(1)
                                cursor = GuardarPunto
                                
                s10 = pInteger()
                 
    res = s10
                                exit
                            
                case default
                    return
                end select
            end do
            
    
        return
    END function pFactor
            
    
    recursive function pInteger() result(res)
        character(len=:), allocatable :: s00
    character(len=:), allocatable :: s01
     
        character(len=:), allocatable :: res
        integer :: i
        integer :: no_caso
        logical :: temporal  ! para el ?
     
            GuardarPunto = cursor
            
            do no_caso = 0, 1 ! lista de concatenaciones
                select case(no_caso)
                    
                            case(0)
                                cursor = GuardarPunto
                                
                s00 = p_()
                
    
            InicioLexema = cursor
            if (.not. (aceptarRango("0" ,"9","null"))) then
                cycle
            end if
            do while (len(entrada) >= cursor)
                if (.not. (aceptarRango("0" ,"9","null"))) then
                    exit
                end if
            end do
            s01 = ConsumirEntrada()
                     
    res = s00//s01
                                exit
                            
                case default
                    return
                end select
            end do
            
    
        return
    END function pInteger
            
    
    recursive function p_() result(res)
        character(len=:), allocatable :: s00
     
        character(len=:), allocatable :: res
        integer :: i
        integer :: no_caso
        logical :: temporal  ! para el ?
     
            GuardarPunto = cursor
            
            do no_caso = 0, 1 ! lista de concatenaciones
                select case(no_caso)
                    
                            case(0)
                                cursor = GuardarPunto
                                
            InicioLexema = cursor
            do while (len(entrada) >= cursor)
                if (.not. (aceptarConjunto([char(32),char(9),char(10),char(13)],"null"))) then
                    exit
                end if
            end do
            s00 = ConsumirEntrada()
                     
    res = s00
                                exit
                            
                case default
                    return
                end select
            end do
            
    
        return
    END function p_
            
    ! Acciones
    
    ! grupos
    
    function grupo0() result(res)
        character(len=:), allocatable :: s00
    character(len=:), allocatable :: s10
     
        character(len=:), allocatable :: res
        integer :: no_caso
        logical :: temporal  ! para el ?
     
            GuardarPunto = cursor
            
            do no_caso = 0, 2 ! lista de concatenaciones
                select case(no_caso)
                    
                            case(0)
                                cursor = GuardarPunto
                                
                InicioLexema = cursor
                if (.not. aceptarLiterales("+","null")) then
                    cycle
                end if
                s00 = ConsumirEntrada()
                         
    res = s00
                                exit
                            
    
                            case(1)
                                cursor = GuardarPunto
                                
                InicioLexema = cursor
                if (.not. aceptarLiterales("-","null")) then
                    cycle
                end if
                s10 = ConsumirEntrada()
                         
    res = s10
                                exit
                            
                case default
                    return
                end select
            end do
            
    
        return
    END function grupo0
            
    
    function grupo2() result(res)
        character(len=:), allocatable :: s00
    character(len=:), allocatable :: s10
     
        character(len=:), allocatable :: res
        integer :: no_caso
        logical :: temporal  ! para el ?
     
            GuardarPunto = cursor
            
            do no_caso = 0, 2 ! lista de concatenaciones
                select case(no_caso)
                    
                            case(0)
                                cursor = GuardarPunto
                                
                InicioLexema = cursor
                if (.not. aceptarLiterales("*","null")) then
                    cycle
                end if
                s00 = ConsumirEntrada()
                         
    res = s00
                                exit
                            
    
                            case(1)
                                cursor = GuardarPunto
                                
                InicioLexema = cursor
                if (.not. aceptarLiterales("/","null")) then
                    cycle
                end if
                s10 = ConsumirEntrada()
                         
    res = s10
                                exit
                            
                case default
                    return
                end select
            end do
            
    
        return
    END function grupo2
            
    
    function grupo1() result(res)
        character(len=:), allocatable :: s00
    character(len=:), allocatable :: s01
    character(len=:), allocatable :: s02
     
        character(len=:), allocatable :: res
        integer :: no_caso
        logical :: temporal  ! para el ?
     
            GuardarPunto = cursor
            
            do no_caso = 0, 1 ! lista de concatenaciones
                select case(no_caso)
                    
                            case(0)
                                cursor = GuardarPunto
                                
                s00 = grupo2()
                
    
                s01 = p_()
                
    
                s02 = pFactor()
                 
    res = s00//s01//s02
                                exit
                            
                case default
                    return
                end select
            end do
            
    
        return
    END function grupo1
            
    end module parser
                    