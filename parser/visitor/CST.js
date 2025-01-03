
import Node from './Node.js';

export class Gramatica {

    constructor(CodigoGlobal,Reglas) {
        this.Reglas = Reglas;  // Lista de producciones
		this.CodigoGlobal = CodigoGlobal; // Arreglo dividido por contains
    }

    accept(visitor) {
        return visitor.visitGramatica(this);
    }
}

export class Producciones extends Node {

    constructor(id, expr, alias) {
        super();
        this.id = id;
		this.expr = expr; // dentro tiene opciones en un objeto
		this.alias = alias;
    }

    accept(visitor) {
        return visitor.visitProducciones(this);
    }
}

export class Opciones extends Node {

    constructor(exprs, qty) {
        super();
        this.exprs = exprs; //lista uniones
		this.qty = qty;
    }

    accept(visitor) {
        return visitor.visitOpciones(this);
    }
}
    
export class Union extends Node {

    constructor(exprs,Predicado) {
        super();
        this.exprs = exprs; // lista de parsing_expresiones
        this.Predicado = Predicado;
    }

    accept(visitor, caso=undefined) {
        return visitor.visitUnion(this,caso);
    }
}

export class Predicado extends Node{

    constructor(Declarion_res, codigo, parametros) {
        super();
        this.Declarion_res = Declarion_res;
		this.codigo = codigo;
		this.parametros = parametros;
    }

    accept(visitor, caso = undefined) {
        return visitor.visitPredicado(this, caso);
    }
}

// ---- Tipos de expresiones ----
export class Pluck extends Node{
    constructor(Etiqueta, pluck) {
        super();
        this.Etiqueta = Etiqueta; // aqui tiene el objeto etiqueta
		this.pluck = pluck;
    }

    accept(visitor,caso = undefined,index= undefined) { // estos parámetros tienen que llevarlos las parsing expressions
        return visitor.visitPluck(this,caso,index);
    }
}

export class Etiqueta extends Node{
    constructor(Anotado, Etiqueta) {
        super();
        this.Anotado = Anotado; // aqui tiene el objeto Anotado
		this.Etiqueta = Etiqueta;
    }

    accept(visitor,caso = undefined,index= undefined) {
        return visitor.visitEtiqueta(this,caso,index);
    }
}

export class Anotado extends Node{
    constructor(expr, qty, text) {
        super();
        this.expr = expr; // Esta guarda las expresiones
		this.qty = qty;
		this.text = text;
    }
    accept(visitor) {
        return visitor.visitAnotado(this);
    }
}

export class Asersion extends Node{ // &
    constructor(asersion) {
        super();
        this.asersion = asersion; // expresión ó predicado
    }
    accept(visitor,caso = undefined,index= undefined) {
        return visitor.visitAsersion(this,caso,index);
    }
}

export class NegAsersion extends Node{ // !

    constructor(asersion) {
        super();
        this.asersion = asersion; // expresión ó predicado
    }
    accept(visitor,caso = undefined,index= undefined) {
        return visitor.visitNegAsersion(this,caso,index);
    }
}

// ---- Tipos de expresiones ----
    
export class String extends Node {

    constructor(val, isCase, qty) {
        super();
        this.val = val;
		this.isCase = isCase;
		this.qty = qty;
    }

    accept(visitor) {
        return visitor.visitString(this);
    }
}
    
export class Any extends Node {

    constructor(isAny) {
        super();
        this.isAny = isAny;
    }

    accept(visitor) {
        return visitor.visitAny(this);
    }
}
    
export class Corchetes extends Node {

    constructor(exprs, isCase, qty) {
        super();
        this.exprs = exprs; // rango o contenido
		this.isCase = isCase;
		this.qty = qty;
    }

    accept(visitor) {
        return visitor.visitCorchetes(this);
    }
}
    
export class rango extends Node {

    constructor(start, end, isCase) {
        super();
        this.start = start;
		this.end = end;
		this.isCase = isCase;
    }

    accept(visitor,isCase= undefined) {
        return visitor.visitrango(this,isCase);
    }
}
    
export class literalRango extends Node {

    constructor(val, isCase) {
        super();
        this.val = val;
		this.isCase = isCase;
    }

    accept(visitor) {
        return visitor.visitliteralRango(this);
    }
}
    
export class idRel extends Node {

    constructor(val) {
        super();
        this.val = val;
    }

    accept(visitor) {
        return visitor.visitidRel(this);
    }
}
    
export class grupo extends Node {

    constructor(expr, qty) {
        super();
        this.expr = expr; // Objeto Opciones
		this.qty = qty;
    }

    accept(visitor) {
        return visitor.visitgrupo(this);
    }
}
    
export class finCadena extends Node {

    constructor() {
        super();
        
    }

    accept(visitor,caso = undefined,index= undefined) {
        return visitor.visitfinCadena(this,caso,index);
    }
}
    