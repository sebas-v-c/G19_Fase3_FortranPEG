// Importaciones

import { String } from "./visitor/CST.js";

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

// Tipo Retorno
function Generar_Variable_Res(){

}

// generacion de variables concatenadas ej: s0,s1,s2,s3 => concatenarlas como return
function EleccionTipo(expresion) {
    if (expresion.expr instanceof String) {
        return "character(len=:), allocatable";
    } else {
        return "No es un string";
    }
}

let Tipos_Variables = []
function generarVariablesLexemas(Lista_Opciones){
    let variables = "";
    Tipos_Variables = []
    Lista_Opciones.forEach((Opcion,i) => {
        let Tipos_Expresiones = [];
        Opcion.exprs.forEach((expresion,j) => {
            Tipos_Expresiones.push(EleccionTipo(expresion));
            
            variables +=`${EleccionTipo(expresion)} :: s${i}${j}\n`
       }); 
       Tipos_Variables.push(Tipos_Expresiones);
    });
    return variables; // generará variables polimórficas
}

// Acciones semánticas f0, f1 f2 ... ó posibles retornos

function CrearAcciones(Acciones){
    let codigo = ""
    for (let i = 0; i < Acciones.length; i++) { 
        codigo +=`
        function f${i}() result(res)
        ${Acciones[i]}
        end function f${i}
        `
    }
    return codigo
}

function Casteo(Tipo_de_la_Variable){ // Todo a retornará a string
    
}

function Elegir_Retorno_res(numero_Concatenaciones, numero_Caso){
    let retorno = "res = "

    for (let i = 0; i < numero_Concatenaciones; i++) {
        retorno +=`${Casteo(Tipos_Variables[numero_Caso][i])}s${numero_Caso}${i}//`
    }
    
    retorno = retorno.slice(0, -2);

    return retorno;
}

export {funciones,CrearGrupos,generarVariablesLexemas, Generar_Variable_Res, Elegir_Retorno_res, CrearAcciones}