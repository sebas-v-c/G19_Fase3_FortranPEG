{{
    import { ids, usos} from '../index.js'
    import { ErrorReglas } from './error.js';
    import { errores } from '../index.js'
    import * as n from '../parser/visitor/CST.js';
}}

gramatica
  =_ codigo:CodigoGlobal? _ prods:producciones+ _ {
    let duplicados = ids.filter((item, index) => ids.indexOf(item) !== index);
    if (duplicados.length > 0) {
        errores.push(new ErrorReglas("Regla duplicada: " + duplicados[0]));
    }

    // Validar que todos los usos están en ids
    let noEncontrados = usos.filter(item => !ids.includes(item));
    if (noEncontrados.length > 0) {
        errores.push(new ErrorReglas("Regla no encontrada: " + noEncontrados[0]));
    }


    return new n.Gramatica(codigo, prods);
  }

// ------- Acciones semánticas -------
CodigoGlobal
  = "{" antes:$(. !"contains")* [ \t\n\r]* "contains" [ \t\n\r]* despues:$[^}]* "}" {
    return [antes,despues]
}

Declaracion_res
  = t:$(. !"::")+ [ \t\n\r]* "::" [ \t\n\r]* "res" {
  return t.trim();
}

predicado
  = "{" [ \t\n\r]* Declarion_res:Declaracion_res codigo:$[^}]* "}"{
    return new n.Predicado(Declarion_res, codigo, [])
}
// ------- Acciones semánticas -------


// ------- Producciones -------
producciones
  = _ id:identificador _ alias:$(literales)? _ "=" _ expr:opciones (_";")? {
    ids.push(id);
    return new n.Producciones(id, expr, alias);
  }

opciones
  = expr:union rest:(_ "/" _ @union)* {
    return new n.Opciones([expr, ...rest]);
  }

union
  = expr:parsing_Expression rest:(_ @parsing_Expression !(_ literales? _ "=") )* accion:(_ @predicado)? {
     
    let parsing = [expr, ...rest]
    
    let plucks = parsing.filter((pexp) => pexp instanceof n.Pluck); // Todos los plucks

    let Parametros = plucks
  .map((parsing_Expresions,index) => { return parsing_Expresions.Etiqueta.Etiqueta });

  if(accion !== null){  accion.parametros = Parametros}
  return new n.Union(parsing, accion);

  }

parsing_Expression = @Pluck 
                  /  "!" assersion:(expresiones/ predicado) { return new n.NegAsersion(assersion); }
                  /  "&" assersion:(expresiones/ predicado) { return new n.Asersion(assersion); }
                  / ".!"                                    { return new n.finCadena(); }

Pluck = pluck:"@"? _ expr:etiqueta {
  return new n.Pluck(expr, pluck ? true : false);
}

etiqueta = eti:(@identificador _ ":")? _ expr:anotado {
  return new n.Etiqueta(expr, eti);
}

anotado = text:"$"? _ expr:expresiones _ qty:$([?+*]/conteo)? {
  return new n.Anotado(expr,qty,text ? true : false);
}

expresiones
  = id:identificador {
    usos.push(id);
    return new n.idRel(id);
  }
  / val:$literales isCase:"i"? {
    return new n.String(val.replace(/['"]/g, ''), isCase);
  }
  / "(" _ opciones:opciones _ ")"{
    return new n.grupo(opciones);
  }

  / exprs:corchetes isCase:"i"?{
    //console.log("Corchetes", exprs);
    return new n.Corchetes(exprs, isCase);

  }
  / "." {
    return new n.Any(true);
  }

// conteo = "|" parteconteo _ (_ delimitador )? _ "|"

conteo = "|" _ val1:(numero / id:identificador) _ "|" { return new n.Delimitador(val1=val1) }
        / "|" _ val1:(numero / id:identificador)? _ ranged:".." _ val2:(numero / id2:identificador)? _ "|" { return new n.Delimitador(val1=val1, ranged=true, val2=val2) }
        / "|" _ val1:(numero / id:identificador)? _ "," _ separator:opciones _ "|" { return new n.Delimitador(val1=val1, separator=separator) }
        / "|" _ val1:(numero / id:identificador)? _ ".." _ val2:(numero / id2:identificador)? _ "," _ separator:opciones _ "|" { return new n.Delimitador(val1=val1, ranged=true, val2=val2, separator=separator) }

// parteconteo = identificador
//             / [0-9]? _ ".." _ [0-9]?
// 			/ [0-9]

// delimitador =  "," _ expresion

// Regla principal que analiza corchetes con contenido

corchetes 
    = "[" @contenidoCorchete+ "]"

contenidoCorchete
    = inicio:$texto "-" final:$texto {
      return new n.rango(inicio, final)
    }
    / $texto


// Regla para caracteres individuales
caracter
    = [a-zA-Z0-9_ ] 

texto
    = "\\" escape
    /[^\[\]]

literales
  = '"' @stringDobleComilla* '"'
  / "'" @stringSimpleComilla* "'"

stringDobleComilla = !('"' / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

stringSimpleComilla = !("'" / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

continuacionLinea = "\\" secuenciaFinLinea

finLinea = [\n\r\u2028\u2029]

escape = "'"
        / '"'
        / "\\"
        / "b"
        / "f"
        / "n"
        / "r"
        / "t"
        / "v"
        / "u"

secuenciaFinLinea = "\r\n" / "\n" / "\r" / "\u2028" / "\u2029"

// literales = 
//     "\"" [^"]* "\""
//     / "'" [^']* "'"
    

numero = [0-9]+

identificador = [_a-z]i[_a-z0-9]i* { return text() }


_ = (Comentarios /[ \t\n\r])*


Comentarios = 
    "//" [^\n]* 
    / "/*" (!"*/" .)* "*/"
