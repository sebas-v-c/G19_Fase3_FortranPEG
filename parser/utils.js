// Importaciones

import * as n from "./visitor/CST.js";

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


`

function CrearGrupos(grupos){
    return grupos.map((funcion) => funcion).join('\n')
}

// Tipo Retorno
function Generar_Variable_Res(){

}

// generacion de variables concatenadas ej: s0,s1,s2,s3 => concatenarlas como return
function EleccionTipo(parsing_expresion, Producciones_Retornos) {
    let expresion;
    if (parsing_expresion instanceof n.Pluck){
        expresion = parsing_expresion.Etiqueta.Anotado.expr;
    }else if (parsing_expresion instanceof n.Asersion || parsing_expresion instanceof n.NegAsersion){
        expresion = parsing_expresion.asersion;
    }else if (parsing_expresion instanceof n.Any){
        expresion = parsing_expresion;
    }
        
    if (expresion instanceof n.String || expresion instanceof n.Corchetes) {
        return "character(len=:), allocatable";
    }else if(expresion instanceof n.idRel){
        return Producciones_Retornos.get(expresion.val);
    } else if(expresion instanceof n.grupo ) {
        return expresion.expr.exprs[0].Predicado? expresion.expr.exprs[0].Predicado.Declarion_res : "character(len=:), allocatable"
    }else {
        return "Error";
    }
}

let Tipos_Variables = []
function generarVariablesLexemas(Lista_Uniones, Producciones_Retornos){
    let variables = "";
    Tipos_Variables = []
    Lista_Uniones.forEach((Union,i) => {
        let Tipos_Expresiones = [];
        Union.exprs.forEach((parsing_expresion,j) => {
            Tipos_Expresiones.push(EleccionTipo(parsing_expresion, Producciones_Retornos));
            
            variables +=`${EleccionTipo(parsing_expresion,Producciones_Retornos)} :: s${i}${j}\n`
       }); 
       Tipos_Variables.push(Tipos_Expresiones);
    });
    return variables; // generará variables
}

// Acciones semánticas f0, f1 f2 ... ó posibles retornos de las producciones

function generarVariablesEtiquetas(caso,Parametros_Func){
    return Parametros_Func.map((Declarar, i) => {
        return `${Tipos_Variables[caso][i]} :: ${Declarar}`
    }) .join("\n")
}

function CrearAcciones(Acciones){
    let codigo = Acciones.map((codigo, i) => {
        return codigo
    }).join("\n");
    return codigo
}

function Casteo(Tipo_de_la_Variable){ // Todo a retornará a string
    return ""
    
}

function Elegir_Retorno_res(Lista_Concatenaciones, numero_Caso, plucks_Union){

    let retorno = "res = "
    let i = 0;
    if (plucks_Union.every(value => value === false)) {
       
        Lista_Concatenaciones.forEach(parsing_expresion => {
            //console.log("->"+expresion.label+"<-")
            retorno +=`${Casteo(Tipos_Variables[numero_Caso][i])}s${numero_Caso}${i}//`;
            i++;
        });
        
        retorno = retorno.slice(0, -2);
        return retorno;
    } else {
        Lista_Concatenaciones.forEach(parsing_expresion => {
            if (plucks_Union[i]){
                retorno +=`${Casteo(Tipos_Variables[numero_Caso][i])}s${numero_Caso}${i}//`;  
            }
            i++;
        });
        retorno = retorno.slice(0, -2);
        return retorno;
    } 
}

// Retornos de producciones a una variable específica

function Retorno_Produccion_Mas(expresion, caso, index,visitor){ // cerradura +
    if (expresion instanceof n.String || expresion instanceof n.Corchetes || expresion instanceof n.Any){
        return `
        InicioLexema = cursor
        if (.not. ${expresion.accept(visitor)}) then
            cycle
        end if
        do while (len(entrada) >= cursor)
            if (.not. ${expresion.accept(visitor)}) then
                exit
            end if
        end do
        s${caso}${index} = ConsumirEntrada()
                `;
    }else if (expresion instanceof n.idRel || expresion instanceof n.grupo){
        return `
        s${caso}${index} = ${expresion.accept(visitor)}
        do while (len(entrada) >= cursor)
            s${caso}${index} =s${caso}${index}//${expresion.accept(visitor)}
        end do
        `
    }
}

function Retorno_Produccion_Kleene(expresion, caso, index,visitor){ // cerradura *
    if (expresion instanceof n.String || expresion instanceof n.Corchetes || expresion instanceof n.Any){
        return `
        InicioLexema = cursor
        do while (len(entrada) >= cursor)
            if (.not. ${expresion.accept(visitor)}) then
                exit
            end if
        end do
        s${caso}${index} = ConsumirEntrada()
                `;
    }else if (expresion instanceof n.idRel || expresion instanceof n.grupo){
        return `
        do while (len(entrada) >= cursor)
            s${caso}${index} =s${caso}${index}//${expresion.accept(visitor)}
        end do
        `
    }
}

function Retorno_Produccion_Condicional(expresion, caso, index,visitor){ // cerradura ?
    if (expresion instanceof n.String || expresion instanceof n.Corchetes || expresion instanceof n.Any){
        return `
        InicioLexema = cursor
        temporal = ${expresion.accept(visitor)}
        s${caso}${index} = ConsumirEntrada()
                `;
    }else if (expresion instanceof n.idRel || expresion instanceof n.grupo){
        return `
        s${caso}${index} = ${expresion.accept(visitor)}
        `
    }
}


function Delimitadores(expresion, qty, caso, index, visitor) {
    console.log(qty)
    const { val1, ranged, val2, separator } = qty;
    let lowerBound = val1 !== null ? val1 : 0; // Default to 0 if not specified
    let upperBound = ranged && val2 !== null ? val2 : null; // Null means no upper limit
    let fortranCode = '';

    // Function to add separator logic using aceptarLiterales
    const addSeparatorLogic = (isFinalIterationCheck = false) => {
        if (separator) {
            return `
            if (${isFinalIterationCheck}) then
                if (.not. (aceptarLiterales(${separator}, "null"))) then
                    exit
                end if
            end if
            `;
        }
        return '';
    };

    // Handle lower bound
    if (lowerBound > 0) {
        fortranCode += `
        ! Lower bound: ${lowerBound} repetitions
        do i = 1, ${lowerBound}
            if (.not. (${expresion.accept(visitor)})) then
                exit
            end if
            ${addSeparatorLogic('.not. (i == ' + lowerBound + ')')}  ! Add separator check if not last iteration
        end do
        `;
    }

    // Handle upper bound
    if (upperBound !== null) {
        fortranCode += `
        ! Upper bound: ${upperBound} repetitions
        do i = ${lowerBound + 1}, ${upperBound}
            if (.not. (${expresion.accept(visitor)})) then
                exit
            end if
            ${addSeparatorLogic('.not. (i == ' + upperBound + ')')}  ! Add separator check if not last iteration
        end do
        `;
    } else if (ranged) {
        // Handle |n..| case (no upper limit)
        fortranCode += `
        ! No upper limit
        do while (len(entrada) >= cursor)
            if (.not. (${expresion.accept(visitor)})) then
                exit
            end if
            ${addSeparatorLogic()}  ! Add separator check for infinite loop
        end do
        `;
    }

    return `
    \t\t! Initialize lexeme tracking
    \t\tInicioLexema = cursor
    
    ${fortranCode}

    \t\t! Consume remaining input
    \t\ts${caso}${index} = ConsumirEntrada()
    `;
}




// InicioLexema = cursor ciclos s${caso}${index} = ConsumirEntrada()
function Retorno_Produccion_Default(expresion, caso, index, visitor,qty){ // cerradura sin contador
    if (qty instanceof n.Delimitador){
        // delimitador
        let delimitador = Delimitadores(expresion, qty, caso, index,visitor);
        if (delimitador !== "") return delimitador;
    }else {
      
        if (expresion instanceof n.String || expresion instanceof n.Corchetes || expresion instanceof n.Any){
            
            return `
            InicioLexema = cursor
            if (.not. ${expresion.accept(visitor)}) then
                cycle
            end if
            s${caso}${index} = ConsumirEntrada()
                    `;
        }else if (expresion instanceof n.idRel || expresion instanceof n.grupo){
            return `
            s${caso}${index} = ${expresion.accept(visitor)}
            `
        }
    }   
}


export {funciones,CrearGrupos,generarVariablesLexemas, Generar_Variable_Res, Elegir_Retorno_res, CrearAcciones,Retorno_Produccion_Mas, Retorno_Produccion_Kleene, Retorno_Produccion_Condicional, Retorno_Produccion_Default, Delimitadores, generarVariablesEtiquetas}