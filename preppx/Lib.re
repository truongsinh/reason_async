
/***
 * https://ocsigen.org/lwt/dev/api/Ppx_lwt
 * https://github.com/zepalmer/ocaml-monadic
 */
let fail = (loc, txt) => raise(Location.Error(Location.error(~loc, txt)));

let rec process_bindings = (bindings) =>
  Parsetree.(
    switch bindings {
    | [] => assert false
    | [binding] => (binding.pvb_pat, binding.pvb_expr)
    | [binding, ...rest] =>
      let (pattern, expr) = process_bindings(rest);
      (
        Ast_helper.Pat.tuple([binding.pvb_pat, pattern]),
        [%expr Let_syntax.join2([%e binding.pvb_expr], [%e expr])]
      )
    }
  );

let process_let = (contents, loc) => {
  open Parsetree;
  let bindings =
    switch contents {
    | PStr([{pstr_desc: Pstr_value(Nonrecursive, bindings), pstr_loc}]) => bindings
    | _ => fail(loc, "extension must contain a nonrecursive let binding")
    };
  /* switch bindings {
     | [] => assert false
     | [binding] => (binding.pvb_pat, binding.pvb_expr)
     | _ => {

     }
     /* | _ => fail loc "only one binding supported at the moment" */
     } */
  process_bindings(bindings)
};

let getExpr = (contents, loc) =>
  Parsetree.(
    switch contents {
    | PStr([{pstr_desc: Pstr_eval(expr, _)}]) => expr
    | _ => fail(loc, "@else must contain an expression")
    }
  );

let mapper = _argv =>
  Parsetree.{
    ...Ast_mapper.default_mapper,
    expr: (mapper, expr) =>
      switch expr.pexp_desc {
      | Pexp_sequence(
          {pexp_desc: Pexp_extension(({txt: "map" | "await"}, contents)), pexp_loc},
          next
        )
      | Pexp_extension(({txt: "map" | "await"}, contents)) =>
        let (pat, expr) = process_let(contents, expr.pexp_loc);
        [%expr Let_syntax.map([%e expr], ~f=([%p pat]) => ())]
      | _ => Ast_mapper.default_mapper.expr(mapper, expr)
      }
  };
