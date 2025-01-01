import Visitor from './Visitor.js';
import {funciones, CrearGrupos,generarVariablesLexemas, Elegir_Retorno_res, CrearAcciones} from '../utils.js'
import * as n from './CST.js';

/*
    if (${grammar[0].id}()) then
        print *, "Parseo, exitoso !!"
    else
        print *, "Parser fallo, revisa que paso !!"
    end if
 */

export default class Tokenizer extends Visitor {
    constructor(){
        super();
        this.primera = true; // verificar primer produccion

        // Grupos
        this.contador_grupos = -1
        this.grupos = []   // codigo de los grupos

        // Acciones semánticas
        this.Contador_Acciones = -1
        this.Acciones = []   // codigo de los grupos

        this.Tipo_Retorno_parse = ""

    }

    Generar_Codigo(grammar) {
        return grammar.accept(this);
    }

    visitGramatica(node){
        return `
module parser
implicit none
integer, private :: cursor
character(len=:), allocatable, private :: entrada, esperado ! entrada es la entrada a consumir
! variables globales
${node.CodigoGlobal ? node.CodigoGlobal[0] : ""} 
contains
! Funciones globales
${node.CodigoGlobal ? node.CodigoGlobal[1] : ""} 
function parse(cad) result(res)
    character(len=:), allocatable, intent(in) :: cad
    ${node.Reglas[0].expr.exprs[0].Predicado? node.Reglas[0].expr.exprs[0].Predicado.Declarion_res+" ::" : "character(len=:), allocatable"} res
    entrada = cad
    cursor = 1
        
    res = ${node.Reglas[0].id}() ! esperamos el retorno
end function parse
! funciones útiles
${funciones}
${node.Reglas.map((produccion)=>produccion.accept(this)).join('\n')}
! Acciones
${CrearAcciones(this.Acciones)}
! grupos
${CrearGrupos(this.grupos)}
end module parser
                `;
    }

    visitProducciones(node) { // Producciones será la encargada de retornar SIEMPRE algo
        //node.expr // lista de opciones
        let str = `
recursive function ${node.id}() result(res)
    ${generarVariablesLexemas(node.expr.exprs)} 
    ${node.expr.exprs[0].Predicado? node.expr.exprs[0].Predicado.Declarion_res+" ::" : "character(len=:), allocatable"} res
    integer :: no_caso
    logical :: temporal  ! para el ?
 
    
        ${node.expr.accept(this)}
    ${
        this.primera ? `
        if (cursor > len(entrada)) then
            res = .true.
        end if` : "aceptacion = .true."
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
                            ${expr.accept(this,i)}
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
    
    visitUnion(node,caso) {
        return `${node.exprs.map((expr) => expr.accept(this)).join('\n')} \n${node.Predicado? node.Predicado.accept(this): Elegir_Retorno_res(node.exprs.length,caso)}`  // expr.accept(this) sería la escritura de las expresiones
    }

    visitPredicado(node){ // Este asigna el retorno por medio de accion semántica
        this.Contador_Acciones++;
        this.Acciones.push(node.Declarion_res+":: res\n"+node.codigo); // Guardamos el código
        return `res = f${this.Contador_Acciones}()` 
    }

    visitExpresion(node) {
        switch (node.qty) {   // cerraduras y contadores +, *, ?, conteo
            case '+':
                return `
                if (.not. (${node.expr.accept(this)})) then
                    cycle
                end if
                do while (len(entrada) >= cursor)
                    if (.not. (${node.expr.accept(this)})) then
                        exit
                    end if
                end do
                `;
            case '*':
                return `
                do while (len(entrada) >= cursor)
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
        return `aceptarPunto()`;
    }

    visitCorchetes(node) {
        node.exprs.forEach(expr => { expr.isCase = node.isCase });
        let conditions = "(" + node.exprs.map((expr) => expr.accept(this)).join(')& \n    .or. (') + ")"
        return ""
    }

    //Solo devuelve las condiciones a cumplirse
    visitrango(node) {
        return `aceptarRango(${node.start} ,${node.end})`;

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

        this.contador_grupos++;

        this.grupos.push(node.expr.accept(this));
        return `grupo${this.contador_grupos}()`;
    }

    visitfinCadena(node) {
        return 'aceptacionEOF()';
    }

}
