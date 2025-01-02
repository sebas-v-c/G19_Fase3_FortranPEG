import Visitor from './Visitor.js';
import {funciones, CrearGrupos,generarVariablesLexemas, Elegir_Retorno_res, CrearAcciones,Retorno_Produccion_Mas, Retorno_Produccion_Kleene, Retorno_Produccion_Condicional, Retorno_Produccion_Default} from '../utils.js'
import * as n from './CST.js';

/*
    if (${grammar[0].id}()) then
        print *, "Parseo, exitoso !!"
    else
        print *, "Parser fallo, revisa que paso !!"
    end if    
    ${
        this.primera ? `
        if (cursor > len(entrada)) then
            res = .true.
        end if` : "aceptacion = .true."
    }  
    */



export default class Tokenizer extends Visitor {
    constructor(){
        super();
        // Producciones y retornos
        this.producciones = new Map();
        // Grupos
        this.contador_grupos = -1
        this.grupos = []   // codigo de los grupos

        // Acciones semánticas
        this.Contador_Acciones = -1
        this.Acciones = []   // codigo de los grupos
    }

    Generar_Codigo(grammar) {
        return grammar.accept(this);
    }

    visitGramatica(node){
        node.Reglas.forEach(Produccion => { // Guardamos nombre de produccion como clave, y el retorno de la produccion
            this.producciones.set(Produccion.id,Produccion.expr.exprs[0].Predicado? Produccion.expr.exprs[0].Predicado.Declarion_res: "character(len=:), allocatable");
        });
        return `
module parser
implicit none
integer, private :: cursor, InicioLexema, GuardarPunto
character(len=:), allocatable, private :: entrada, esperado ! entrada es la entrada a consumir
! variables globales
${node.CodigoGlobal ? node.CodigoGlobal[0] : ""} 
contains
! Funciones globales
${node.CodigoGlobal ? node.CodigoGlobal[1] : ""} 
function parse(cad) result(res)
    character(len=:), allocatable, intent(in) :: cad
    ${node.Reglas[0].expr.exprs[0].Predicado? node.Reglas[0].expr.exprs[0].Predicado.Declarion_res+" ::" : "character(len=:), allocatable ::"} res
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
    ${generarVariablesLexemas(node.expr.exprs,this.producciones)} 
    ${node.expr.exprs[0].Predicado? node.expr.exprs[0].Predicado.Declarion_res+" ::" : "character(len=:), allocatable ::"} res
    integer :: no_caso
    logical :: temporal  ! para el ?
 
        GuardarPunto = cursor
        ${node.expr.accept(this)}

    return
END function ${node.id}
        `
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
                            cursor = GuardarPunto
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
        return `${node.exprs.map((expr,index) => expr.accept(this,caso,index)).join('\n')} \n${node.Predicado? node.Predicado.accept(this): Elegir_Retorno_res(node.exprs.length,caso)}`  // expr.accept(this) sería la escritura de las expresiones
    }

    visitPredicado(node){ // Este asigna el retorno por medio de accion semántica
        this.Contador_Acciones++;
        this.Acciones.push(node.Declarion_res+":: res\n"+node.codigo); // Guardamos el código
        return `res = f${this.Contador_Acciones}()` 
    }

    visitExpresion(node,caso,index) { // caso signifca el numero de caso que esta en el or e index es el numero de la expresion
        switch (node.qty) {   // cerraduras y contadores +, *, ?, conteo
            case '+':
                return Retorno_Produccion_Mas(node.expr,caso,index,this);
            case '*':
                return Retorno_Produccion_Kleene(node.expr,caso,index,this);
            case '?':
                return Retorno_Produccion_Condicional(node.expr,caso,index,this);
            default:
                return Retorno_Produccion_Default(node.expr,caso,index,this);
        }
    }

    visitString(node) {
        return `aceptarLiterales("${node.val}","${node.isCase}")`; 
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
