import Visitor from './Visitor.js';
import {funciones, CrearGrupos,generarVariablesLexemas, Elegir_Retorno_res, CrearAcciones,Retorno_Produccion_Mas, Retorno_Produccion_Kleene, Retorno_Produccion_Condicional, Retorno_Produccion_Default, Delimitadores, generarVariablesEtiquetas} from '../utils.js'
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
        //----- Retornos -----
        // Producciones y retornos de grupos
        this.producciones = new Map();
        this.grupos_retorno = new Map();

        // ----- Generación de funciones -----
        // Grupos
        this.contador_grupos = -1
        this.grupos = []   // codigo de los grupos

        // Acciones semánticas
        this.Contador_Acciones = -1
        this.Acciones = []   // codigo de los grupos

        // Lista de plucks activos por union
        this.plucks_Union = []
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
character(len=:), allocatable, private :: entrada, esperado, verificador ! entrada es la entrada a consumir
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
        
    res = p${node.Reglas[0].id}() ! esperamos el retorno
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
recursive function p${node.id}() result(res)
    ${generarVariablesLexemas(node.expr.exprs,this.producciones)} 
    ${node.expr.exprs[0].Predicado? node.expr.exprs[0].Predicado.Declarion_res+" ::" : "character(len=:), allocatable ::"} res
    integer :: i
    integer :: no_caso
    logical :: temporal  ! para el ?
 
        GuardarPunto = cursor
        ${node.expr.accept(this)}

    return
END function p${node.id}
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
        this.plucks_Union = [];
        
        return `${node.exprs.map((expr,index) => expr.accept(this,caso,index)).join('\n')} \n${node.Predicado? node.Predicado.accept(this,caso): Elegir_Retorno_res(node.exprs,caso, this.plucks_Union)}`  // expr.accept(this) sería la escritura de las expresiones
    }

    visitPredicado(node,caso){ // Este asigna el retorno por medio de accion semántica
        // Parametros tiene todos los s
        
        let Parametros_In = "";
        
        for (let i = 0; i < node.parametros.length; i++) {
            //console.log(node.parametros[i])
            if(node.parametros[i] != null){
                Parametros_In += `s${caso}${i},`
            }
        }
        Parametros_In = Parametros_In.slice(0, -1); 

        node.parametros =node.parametros.filter(Etiqueta => Etiqueta !== null)

        let Parametros_Func = node.parametros
        .map((label) => {return `${label}`}).join(", ");

        this.Contador_Acciones++;
        this.Acciones.push(
        `
function f${this.Contador_Acciones}(${Parametros_Func}) result(res)
    ${generarVariablesEtiquetas(caso, node.parametros)}
    ${node.Declarion_res+":: res\n"+node.codigo}
end function f${this.Contador_Acciones}
        `    
        ); // Guardamos el código
        return `res = f${this.Contador_Acciones}(${Parametros_In})` 
    }

    visitPluck(node,caso,index){
        this.plucks_Union.push(node.pluck);
        return node.Etiqueta.accept(this,caso,index);
    }

    visitEtiqueta(node,caso,index) { // caso signifca el numero de caso que esta en el or e index es el numero de la expresion
        switch (node.Anotado.qty) {   // cerraduras y contadores +, *, ?, conteo
            case '+':
                return Retorno_Produccion_Mas(node.Anotado.expr,caso,index,this);
            case '*':
                return Retorno_Produccion_Kleene(node.Anotado.expr,caso,index,this);
            case '?':
                return Retorno_Produccion_Condicional(node.Anotado.expr,caso,index,this);
            default:
                return Retorno_Produccion_Default(node.Anotado.expr,caso,index,this, node.Anotado.qty);
        }
    }

    visitAnotado(node){ // aqui seria el $
    }
    // s = &"hoa" "hoaf"  hoaf
    visitAsersion(node,caso,index){ // &
        if (node.asersion instanceof n.String || node.asersion instanceof n.Corchetes || node.asersion instanceof n.Any){    
            
            return `
            InicioLexema = cursor
            if (.not. ${node.asersion.accept(this)}) then
                cycle
            end if
            verificador = ConsumirEntrada()
            if (len(verificador) > 0) then
                cursor = InicioLexema
            end if
                    `;
        }else if (node.asersion instanceof n.idRel || node.asersion instanceof n.grupo){
            return `s${caso}${index} = ${node.asersion.accept(this)}`
        }
    }

    visitNegAsersion(node,caso,index){
        if (node.asersion instanceof n.String || node.asersion instanceof n.Corchetes || node.asersion instanceof n.Any){    
            return `
            InicioLexema = cursor
            if (.not. ${node.asersion.accept(this)}) then
                cycle
            end if
            verificador = ConsumirEntrada()
            if (len(verificador) < 0) then
                cursor = InicioLexema
            end if
                    `;
        }else if (node.asersion instanceof n.idRel || node.asersion instanceof n.grupo){
            return `s${caso}${index} = ${node.asersion.accept(this)}`
        }
    }
    visitString(node) {
        return `aceptarLiterales("${node.val}","${node.isCase}")`; 
    }

    visitAny(node) { 
        return `aceptarPunto()`;
    }

    visitCorchetes(node) {
        // [abc0-9A-Z]
        let characterClass = [];
        const set = node.exprs
            .filter((char) => typeof char === "string")
            .map((char) => node.isCase ? `tolower(${this.convertLiteralRango(char)})` : `${this.convertLiteralRango(char)}`);
        const ranges = node.exprs
            .filter((char) => char instanceof n.rango)
            .map((range) => range.accept(this, node.isCase)); 

        if (set.length !== 0){
            characterClass = [`aceptarConjunto([${set.join(',')}],"${node.isCase}")`];
        }
        if (ranges.length !== 0){
            characterClass = [...characterClass, ...ranges];
        }

        return `(${characterClass.join(' .or. &\n')})`;
    }

    //Solo devuelve las condiciones a cumplirse
    visitrango(node, isCase) {
        return `aceptarRango("${node.start}" ,"${node.end}","${isCase}")`;
    }

    //Solo devuelve las condiciones a cumplirse
    convertLiteralRango(char) {
        const literalMap = {
            "\\t": "char(9)",  // Tabulación
            "\\n": "char(10)", // Nueva línea
            " ": "char(32)",   // Espacio
            "\\r": "char(13)",  // Retorno de carro
        };
    
        // Verifica si el literal es especial y tradúcelo, de lo contrario usa comillas
        const literalFortran = literalMap[char] || `"${char}"`;
    
        /*
        const condition = node.isCase
        ? `tolower(input(cursor:cursor)) == tolower(${literalFortran})`
        : `input(cursor:cursor) == ${literalFortran}`
        */
        return literalFortran;
    }

    visitidRel(node) {
        return `p${node.val}()`;
    }

    visitgrupo(node) {
        node.expr.qty = node.qty

        this.contador_grupos++;

        let grupoAct = this.contador_grupos;

        this.grupos.push( // Guardamos la funcion
            `
function grupo${this.contador_grupos}() result(res)
    ${generarVariablesLexemas(node.expr.exprs,this.producciones)} 
    ${node.expr.exprs[0].Predicado? node.expr.exprs[0].Predicado.Declarion_res+" ::" : "character(len=:), allocatable ::"} res
    integer :: no_caso
    logical :: temporal  ! para el ?
 
        GuardarPunto = cursor
        ${node.expr.accept(this)}

    return
END function grupo${grupoAct}
        `        
        ); // Guardamos opciones
        return `grupo${grupoAct}()`;
    }

    visitfinCadena(node,caso,index) {
        return 'aceptacionEOF()';
    }

}
