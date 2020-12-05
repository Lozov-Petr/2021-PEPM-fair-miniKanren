module Program.Scheme where

import Syntax

scheme =
  Let (Def "eq_var" ["a", "b", "q65"] (
    ((V "a" === C "first" []) &&&
    (((V "b" === C "first" []) &&&
    (V "q65" === C "true" [])) |||
    (fresh ["q68"] (
       (V "b" === C "next" [V "q68"]) &&&
       (V "q65" === C "false" []))))) |||
    (fresh ["x"] (
       (V "a" === C "next" [V "x"]) &&&
       (((V "b" === C "first" []) &&&
       (V "q65" === C "false" [])) |||
       (fresh ["y"] (
          (V "b" === C "next" [V "y"]) &&&
          (call "eq_var" [V "x", V "y", V "q65"]))))))
  )) (
  Let (Def "eq_id" ["a", "b", "q42"] (
    ((V "a" === C "lambda" []) &&&
    (((V "b" === C "lambda" []) &&&
    (V "q42" === C "true" [])) |||
    ((V "b" === C "quote" []) &&&
    (V "q42" === C "false" [])) |||
    ((V "b" === C "list" []) &&&
    (V "q42" === C "false" [])) |||
    (fresh ["q47"] (
       (V "b" === C "var_" [V "q47"]) &&&
       (V "q42" === C "false" []))))) |||
    ((V "a" === C "quote" []) &&&
    (((V "b" === C "lambda" []) &&&
    (V "q42" === C "false" [])) |||
    ((V "b" === C "quote" []) &&&
    (V "q42" === C "true" [])) |||
    ((V "b" === C "list" []) &&&
    (V "q42" === C "false" [])) |||
    (fresh ["q53"] (
       (V "b" === C "var_" [V "q53"]) &&&
       (V "q42" === C "false" []))))) |||
    ((V "a" === C "list" []) &&&
    (((V "b" === C "lambda" []) &&&
    (V "q42" === C "false" [])) |||
    ((V "b" === C "quote" []) &&&
    (V "q42" === C "false" [])) |||
    ((V "b" === C "list" []) &&&
    (V "q42" === C "true" [])) |||
    (fresh ["q59"] (
       (V "b" === C "var_" [V "q59"]) &&&
       (V "q42" === C "false" []))))) |||
    (fresh ["x"] (
       (V "a" === C "var_" [V "x"]) &&&
       (((V "b" === C "lambda" []) &&&
       (V "q42" === C "false" [])) |||
       ((V "b" === C "quote" []) &&&
       (V "q42" === C "false" [])) |||
       ((V "b" === C "list" []) &&&
       (V "q42" === C "false" [])) |||
       (fresh ["y"] (
          (V "b" === C "var_" [V "y"]) &&&
          (call "eq_var" [V "x", V "y", V "q42"]))))))
  )) (
  Let (Def "lookup" ["x", "env", "q38"] (
    fresh ["y", "res", "env'", "q40"] (
      (V "env" === C "%" [C "pair" [V "y", V "res"], V "env'"]) &&&
      (call "eq_id" [V "x", V "y", V "q40"]) &&&
      (((V "q40" === C "true" []) &&&
      (V "res" === V "q38")) |||
      ((V "q40" === C "false" []) &&&
      (call "lookup" [V "x", V "env'", V "q38"]))))
  )) (
  Let (Def "not_in_env" ["x", "env", "q33"] (
    ((V "env" === C "nil" []) &&&
    (V "q33" === C "true" [])) |||
    (fresh ["y", "res", "env'", "q36"] (
       (V "env" === C "%" [C "pair" [V "y", V "res"], V "env'"]) &&&
       (call "eq_id" [V "x", V "y", V "q36"]) &&&
       (((V "q36" === C "true" []) &&&
       (V "q33" === C "false" [])) |||
       ((V "q36" === C "false" []) &&&
       (call "not_in_env" [V "x", V "env'", V "q33"])))))
  )) (
  Let (Def "eval" ["term", "env", "q32"] (
    Let (Def "lambda_handler" ["ts", "env", "q0"] (
      fresh ["q1", "i", "body"] (
        (V "q1" === C "true" []) &&&
        (V "ts" === C "%" [C "seq" [C "%" [C "ident" [V "i"], C "nil" []]], C "%" [V "body", C "nil" []]]) &&&
        (V "q0" === C "closure" [V "i", V "body", V "env"]) &&&
        (call "not_in_env" [C "lambda" [], V "env", V "q1"]))
    )) (
    Let (Def "quote_handler" ["ts", "env", "q4"] (
      fresh ["q5", "t"] (
        (V "q5" === C "true" []) &&&
        (V "ts" === C "%" [V "t", C "nil" []]) &&&
        (V "q4" === C "val" [V "t"]) &&&
        (call "not_in_env" [C "quote" [], V "env", V "q5"]))
    )) (
    Let (Def "list_handler" ["ts", "env", "q8"] (
      fresh ["q9"] (
        (V "q9" === C "true" []) &&&
        (call "not_in_env" [C "list" [], V "env", V "q9"]) &&&
        (Let (Def "eval_val" ["t", "env", "q10"] (
           fresh ["q11", "v"] (
             (V "q11" === C "val" [V "v"]) &&&
             (V "v" === V "q10") &&&
             (call "eval" [V "t", V "env", V "q11"]))
        )) (
        Let (Def "map_eval_val" ["l", "env", "q13"] (
          (fresh ["x", "xs", "q14", "q15"] (
             (V "l" === C "%" [V "x", V "xs"]) &&&
             (V "q13" === C "%" [V "q14", V "q15"]) &&&
             (call "eval_val" [V "x", V "env", V "q14"]) &&&
             (call "map_eval_val" [V "xs", V "env", V "q15"]))) |||
          ((V "l" === C "nil" []) &&&
          (V "q13" === C "nil" []))
        )) (
        fresh ["q18"] (
          (V "q8" === C "val" [C "seq" [V "q18"]]) &&&
          (call "map_eval_val" [V "ts", V "env", V "q18"]))))))
    )) (
    (fresh ["x"] (
       (V "term" === C "ident" [V "x"]) &&&
       (call "lookup" [V "x", V "env", V "q32"]))) |||
    (fresh ["t", "ts"] (
       (V "term" === C "seq" [C "%" [V "t", V "ts"]]) &&&
       ((fresh ["id"] (
           (V "t" === C "ident" [V "id"]) &&&
           (((V "id" === C "lambda" []) &&&
           (call "lambda_handler" [V "ts", V "env", V "q32"])) |||
           ((V "id" === C "quote" []) &&&
           (call "quote_handler" [V "ts", V "env", V "q32"])) |||
           ((V "id" === C "list" []) &&&
           (call "list_handler" [V "ts", V "env", V "q32"]))))) |||
       (fresh ["s", "arg", "q27", "x", "body", "env'", "q28"] (
          (V "t" === C "seq" [V "s"]) &&&
          (V "ts" === C "%" [V "arg", C "nil" []]) &&&
          (V "q27" === C "closure" [V "x", V "body", V "env'"]) &&&
          (call "eval" [V "t", V "env", V "q27"]) &&&
          (call "eval" [V "arg", V "env", V "q28"]) &&&
          (call "eval" [V "body", C "%" [C "pair" [V "x", V "q28"], V "env'"], V "q32"])))))))))
  )) (
  C "1" [] === C "1" [])))))

env = "open GT\nopen OCanren\nopen OCanren.Std\ntype 'a0 gvariable =\n  | First \n  | Next of 'a0 \nmodule For_gvariable =\n  (Fmap)(struct\n           let rec fmap fa0 =\n             function | First -> First | Next a0 -> Next (fa0 a0)\n           type 'a0 t = 'a0 gvariable\n         end)\nlet rec first () = inj (For_gvariable.distrib First)\nand next x__0 = inj (For_gvariable.distrib (Next x__0))\ntype 'a0 gidentifier =\n  | Lambda \n  | Quote \n  | List \n  | Var of 'a0 \nmodule For_gidentifier =\n  (Fmap)(struct\n           let rec fmap fa0 =\n             function\n             | Lambda -> Lambda\n             | Quote -> Quote\n             | List -> List\n             | Var a0 -> Var (fa0 a0)\n           type 'a0 t = 'a0 gidentifier\n         end)\nlet rec lambda () = inj (For_gidentifier.distrib Lambda)\nand quote () = inj (For_gidentifier.distrib Quote)\nand list () = inj (For_gidentifier.distrib List)\nand var_ x__0 = inj (For_gidentifier.distrib (Var x__0))\ntype ('a1, 'a0) gterm =\n  | Ident of 'a1 \n  | Seq of 'a0 \nmodule For_gterm =\n  (Fmap2)(struct\n            let rec fmap fa1 fa0 =\n              function | Ident a1 -> Ident (fa1 a1) | Seq a0 -> Seq (fa0 a0)\n            type ('a1, 'a0) t = ('a1, 'a0) gterm\n          end)\nlet rec ident x__0 = inj (For_gterm.distrib (Ident x__0))\nand seq x__0 = inj (For_gterm.distrib (Seq x__0))\ntype ('a2, 'a1, 'a0) gresult =\n  | Val of 'a1 \n  | Closure of 'a2 * 'a1 * 'a0 \nmodule For_gresult =\n  (Fmap3)(struct\n            let rec fmap fa2 fa1 fa0 =\n              function\n              | Val a1 -> Val (fa1 a1)\n              | Closure (a2_0, a1_1, a0_2) ->\n                  Closure ((fa2 a2_0), (fa1 a1_1), (fa0 a0_2))\n            type ('a2, 'a1, 'a0) t = ('a2, 'a1, 'a0) gresult\n          end)\nlet rec val_ x__0 = inj (For_gresult.distrib (Val x__0))\nand closure x__0 x__1 x__2 =\n  inj (For_gresult.distrib (Closure (x__0, x__1, x__2)))"
