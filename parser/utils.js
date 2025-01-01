// Generacion de código

let funciones =
`
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
            temp_string = temp_string // '\\n'
        case (9)  ! Tabulación
            temp_string = temp_string // '\\t'
        case (13) ! Retorno de carro
            temp_string = temp_string // '\\r'
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
end function aceptarRango`

function CrearGrupos(grupos){
    return grupos.map((funcion,index) => `
recursive function grupo${index}() result(aceptacion)
    logical :: aceptacion
    integer :: no_caso
    logical :: temporal  

    aceptacion = .false.
    `+
    funcion
    +`

    aceptacion = .true.
    return
END function
    `
).join('\n')
}

// generacion de variables concatenadas ej: s0,s1,s2,s3 => concatenarlas como return
function generarVariablesLexemas(Lista_Opciones){
    let Numero_Concatenaciones_por_Opcion = []
    Lista_Opciones.forEach((Opcion,index) => {
        Numero_Concatenaciones_por_Opcion.push(Number(Opcion.exprs.length)); // Opcion.exprs es la lista de expresiones de la union
        
    });

    Numero_Concatenaciones_por_Opcion.sort((a,b) => b-a);
    let Numero_Variables = Numero_Concatenaciones_por_Opcion[0];    
    let variables = "";
    for (let i = 0; i < Numero_Variables; i++) {
        variables +=`class(*), allocatable :: s${i}\n`
    }
    return variables; // generará variables polimórficas
}


// a = "hola" ever / jonas "que" "olga"


export {funciones,CrearGrupos,generarVariablesLexemas}