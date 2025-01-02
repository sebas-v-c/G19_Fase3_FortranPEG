
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
        this.exprs = exprs; // lista de expresiones
        this.Predicado = Predicado;
    }

    accept(visitor, caso=undefined) {
        return visitor.visitUnion(this,caso);
    }
}

export class Predicado {

    constructor(Declarion_res, codigo, parametros) {
        this.Declarion_res = Declarion_res;
		this.codigo = codigo;
		this.parametros = parametros;
    }

    accept(visitor) {
        return visitor.visitPredicado(this);
    }
}
    
export class Expresion extends Node {

    constructor(expr, label, qty) {
        super();
        this.expr = expr;
		this.label = label;
		this.qty = qty;
    }

    accept(visitor, caso=undefined, Corr_Expr=undefined) {
        return visitor.visitExpresion(this,caso,Corr_Expr);
    }
}
    
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
        this.exprs = exprs;
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

    accept(visitor) {
        return visitor.visitrango(this);
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

    accept(visitor) {
        return visitor.visitfinCadena(this);
    }
}
    