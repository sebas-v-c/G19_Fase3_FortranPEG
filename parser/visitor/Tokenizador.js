import Visitor from './Visitor.js';
import * as n from './CST.js';

export default class Tokenizer extends Visitor {
    constructor(){
        super();
        this.primera = true;
    }

    Generar_Codigo(grammar) {
        return `
module parser
implicit none
integer, private :: cursor
character(len=:), allocatable, private :: entrada, expected ! entrada es la entrada a consumir
contains

subroutine parse(cad)
    character(len=:), allocatable, intent(in) :: cad
    entrada = cad
    cursor = 1
    if (${grammar[0].id}()) then
        print *, "Parseo, exitoso !!"
    else
        print *, "Parser fallo, revisa que paso !!"
    end if
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
${grammar.map((reglas) => reglas.accept(this)).join('\n')}
end module parser
        `;
    }

    visitProducciones(node) {
        

        let str = `
function ${node.id}() result(aceptacion)
    logical :: aceptacion
    integer :: no_caso
    logical :: temporal

    aceptacion = .false.
        ${node.expr.accept(this)}
    
    ${
        this.primera ? `
        if (cursor > len(entrada)) then
            aceptacion = .true.
        end if` : ""
        }
    return
END function ${node.id}
        `
    this.primera = false;
    return str;
    }

    visitOpciones(node) {
        return `
        do no_caso = 0, ${node.exprs.length} ! lista de concatenaciones
            select case(no_caso)
                ${node.exprs
                    .map(
                        (expr, i) => `
                        case(${i})
                            ${expr.accept(this)}
                            exit
                        `
                    )
                    .join('\n')}
            case default
                return
            end select
        end do
        `;
        
        // node.exprs.map((expr) => expr.accept(this)).join('\n');
    }
    
    visitUnion(node) {
        return `${node.exprs.map((expr) => expr.accept(this)).join('\n')}`
    }

    visitExpresion(node) {
        switch (node.qty) {   // cerraduras y contadores +, *, ?
            case '+':
                return `
                if (.not. (${node.expr.accept(this)})) then
                    cycle
                end if
                do while (len(entrada) > cursor)
                    if (.not. (${node.expr.accept(this)})) then
                        exit
                    end if
                end do
                `;
            case '*':
                return `
                do while (len(entrada) > cursor)
                    if (.not. (${node.expr.accept(this)})) then
                        exit
                    end if
                end do
                `;
            case '?':
                return `
                temporal = ${node.expr.accept(this)}
                `;
            default:
                return `
                if (.not. (${node.expr.accept(this)})) then
                    cycle
                end if
                `;
        }
    }

    visitString(node) {
        return `aceptarLiterales("${node.val}","${node.isCase}")`;
        //return this.renderQuantifierOption(node.qty, condition, node.val.length)
    }

    visitAny(node) { 
        return `
    ! Cualquier carácter es aceptado como lexema
    if (cursor <= len_trim(input)) then
        buffer = buffer // input(cursor:cursor + ${length - 1})
        buffer = replace_special_characters(buffer)
        cursor = cursor + ${length}
    else
        concat_failed = .true.
        buffer = ""
    end if
    `;
    }

    visitCorchetes(node) {
        node.exprs.forEach(expr => { expr.isCase = node.isCase });
        let conditions = "(" + node.exprs.map((expr) => expr.accept(this)).join(')& \n    .or. (') + ")"
        return this.renderQuantifierOption(node.qty, conditions, 1)
    }

    //Solo devuelve las condiciones a cumplirse
    visitrango(node) {
        const condition = node.isCase 
        ? `iachar(tolower(input(cursor:cursor))) >= iachar("${node.start}") .and. &
        iachar(tolower(input(cursor:cursor))) <= iachar("${node.end}")`
        : `iachar(input(cursor:cursor)) >= iachar("${node.start}") .and. &
        iachar(input(cursor:cursor)) <= iachar("${node.end}")`;

        return "(" + condition + ")";
    }

    //Solo devuelve las condiciones a cumplirse
    visitliteralRango(node) {
        const literalMap = {
            "\\t": "char(9)",  // Tabulación
            "\\n": "char(10)", // Nueva línea
            " ": "char(32)",   // Espacio
            "\\r": "char(13)",  // Retorno de carro
        };
    
        // Verifica si el literal es especial y tradúcelo, de lo contrario usa comillas
        const literalFortran = literalMap[node.val] || `"${node.val}"`;
    
        const condition = node.isCase
        ? `tolower(input(cursor:cursor)) == tolower(${literalFortran})`
        : `input(cursor:cursor) == ${literalFortran}`
        return "(" + condition + ")";
    }

    visitidRel(node) {
        return `${node.val}()`;
    }

    visitgrupo(node) {
        node.expr.qty = node.qty
        return node.expr.accept(this);
    }

    visitfinCadena(node) {
        return '';
    }

    renderQuantifierOption(qty, condition, length){
        var resultOneMore = `
        initialCursor = cursor
        do while (cursor <= len_trim(input) .and. (${condition}))
            cursor = cursor + ${length}
        end do
        if (cursor > initialCursor) then
            buffer = buffer // input(initialCursor:cursor-1) 
            buffer = replace_special_characters(buffer)
        else
            cursor = initialCursor
            concat_failed = .true.
            buffer = ""
        end if`      ;

        var resultZeroMore = `
        initialCursor = cursor
        do while (cursor <= len_trim(input) .and. (${condition}))
            cursor = cursor + ${length}
        end do
        if (cursor > initialCursor) then
            buffer = buffer // input(initialCursor:cursor-1) 
            buffer = replace_special_characters(buffer)
        end if`      ;

        var resultZeroOrOne = `
        if (cursor <= len_trim(input) .and. (${condition})) then 
            buffer = buffer // input(cursor:cursor + ${length - 1})
            buffer = replace_special_characters(buffer)
            cursor = cursor + ${length}
        end if` ;

        var one = `
        if (cursor <= len_trim(input) .and. (${condition})) then 
            buffer = buffer // input(cursor:cursor + ${length - 1})
            buffer = replace_special_characters(buffer)
            cursor = cursor + ${length}
        else
            concat_failed = .true.
            buffer = ""
        end if` ;
    
        
        switch (qty) {
            case '+': return resultOneMore;
            case '*': return resultZeroMore;
            case '?': return resultZeroOrOne;
            default: return one;
        }   
    
    }

}
