module Program.GCW where

import Syntax

gcw =
  Let (Def "eqBool" ["x", "y", "q70"] (
    ((V "x" === C "true" []) &&&
    (V "y" === V "q70")) |||
    ((V "x" === C "false" []) &&&
    (((V "y" === C "true" []) &&&
    (V "q70" === C "false" [])) |||
    ((V "y" === C "false" []) &&&
    (V "q70" === C "true" []))))
  )) (
  Let (Def "eqState" ["x", "y", "q50"] (
    fresh ["a1", "a2", "a3", "a4", "b1", "b2", "b3", "b4", "q52", "q53", "q58", "q59", "q64", "q65"] (
      (V "x" === C "st" [V "a1", V "a2", V "a3", V "a4"]) &&&
      (V "y" === C "st" [V "b1", V "b2", V "b3", V "b4"]) &&&
      (call "eqBool" [V "a1", V "b2", V "q52"]) &&&
      (call "eqBool" [V "a2", V "b2", V "q58"]) &&&
      (call "eqBool" [V "a3", V "b3", V "q64"]) &&&
      (call "eqBool" [V "a4", V "b4", V "q65"]) &&&
      (((V "q64" === C "false" []) &&&
      (V "q59" === C "false" [])) |||
      ((V "q64" === C "true" []) &&&
      (V "q59" === V "q65"))) &&&
      (((V "q58" === C "false" []) &&&
      (V "q53" === C "false" [])) |||
      ((V "q58" === C "true" []) &&&
      (V "q53" === V "q59"))) &&&
      (((V "q52" === C "false" []) &&&
      (V "q50" === C "false" [])) |||
      ((V "q52" === C "true" []) &&&
      (V "q50" === V "q53"))))
  )) (
  Let (Def "checkState" ["s", "q43"] (
    fresh ["i0", "g0", "c0", "w0", "q45"] (
      (V "s" === C "st" [V "i0", V "g0", V "c0", V "w0"]) &&&
      (call "eqBool" [V "i0", V "g0", V "q45"]) &&&
      (((V "q45" === C "true" []) &&&
      (V "q43" === C "true" [])) |||
      (fresh ["q48"] (
         (V "q45" === C "false" []) &&&
         (call "eqBool" [V "i0", V "c0", V "q48"]) &&&
         (((V "q48" === C "true" []) &&&
         (call "eqBool" [V "i0", V "w0", V "q43"])) |||
         ((V "q48" === C "false" []) &&&
         (V "q43" === C "false" [])))))))
  )) (
  Let (Def "checkStep" ["state", "step", "q40"] (
    fresh ["i0", "g0", "c0", "w0"] (
      (V "state" === C "st" [V "i0", V "g0", V "c0", V "w0"]) &&&
      (((V "step" === C "n" []) &&&
      (V "q40" === C "true" [])) |||
      ((V "step" === C "g" []) &&&
      (call "eqBool" [V "i0", V "g0", V "q40"])) |||
      ((V "step" === C "c" []) &&&
      (call "eqBool" [V "i0", V "c0", V "q40"])) |||
      ((V "step" === C "w" []) &&&
      (call "eqBool" [V "i0", V "w0", V "q40"]))))
  )) (
  Let (Def "step" ["s", "p", "q13"] (
    fresh ["i0", "g0", "c0", "w0"] (
      (V "s" === C "st" [V "i0", V "g0", V "c0", V "w0"]) &&&
      ((fresh ["q15", "q16"] (
          (V "p" === C "g" []) &&&
          (V "q13" === C "st" [V "q15", V "q16", V "c0", V "w0"]) &&&
          (((V "i0" === C "true" []) &&&
          (V "q15" === C "false" [])) |||
          ((V "i0" === C "false" []) &&&
          (V "q15" === C "true" []))) &&&
          (((V "g0" === C "true" []) &&&
          (V "q16" === C "false" [])) |||
          ((V "g0" === C "false" []) &&&
          (V "q16" === C "true" []))))) |||
      (fresh ["q22", "q23"] (
         (V "p" === C "c" []) &&&
         (V "q13" === C "st" [V "q22", V "g0", V "q23", V "w0"]) &&&
         (((V "i0" === C "true" []) &&&
         (V "q22" === C "false" [])) |||
         ((V "i0" === C "false" []) &&&
         (V "q22" === C "true" []))) &&&
         (((V "c0" === C "true" []) &&&
         (V "q23" === C "false" [])) |||
         ((V "c0" === C "false" []) &&&
         (V "q23" === C "true" []))))) |||
      (fresh ["q29", "q30"] (
         (V "p" === C "w" []) &&&
         (V "q13" === C "st" [V "q29", V "g0", V "c0", V "q30"]) &&&
         (((V "i0" === C "true" []) &&&
         (V "q29" === C "false" [])) |||
         ((V "i0" === C "false" []) &&&
         (V "q29" === C "true" []))) &&&
         (((V "w0" === C "true" []) &&&
         (V "q30" === C "false" [])) |||
         ((V "w0" === C "false" []) &&&
         (V "q30" === C "true" []))))) |||
      (fresh ["q36"] (
         (V "p" === C "n" []) &&&
         (V "q13" === C "st" [V "q36", V "g0", V "c0", V "w0"]) &&&
         (((V "i0" === C "true" []) &&&
         (V "q36" === C "false" [])) |||
         ((V "i0" === C "false" []) &&&
         (V "q36" === C "true" [])))))))
  )) (
  Let (Def "checkAnswer" ["a", "q1"] (
    fresh ["startState", "finishState"] (
      (V "startState" === C "st" [C "true" [], C "true" [], C "true" [], C "true" []]) &&&
      (V "finishState" === C "st" [C "false" [], C "false" [], C "false" [], C "false" []]) &&&
      (Let (Def "checkAnswer'" ["a", "state", "finishState", "q4"] (
         ((V "a" === C "nil" []) &&&
         (call "eqState" [V "state", V "finishState", V "q4"])) |||
         (fresh ["x", "xs", "q6"] (
            (V "a" === C "%" [V "x", V "xs"]) &&&
            (call "checkStep" [V "state", V "x", V "q6"]) &&&
            ((fresh ["newState", "q9"] (
                (V "q6" === C "true" []) &&&
                (call "step" [V "state", V "x", V "newState"]) &&&
                (call "checkState" [V "newState", V "q9"]) &&&
                (((V "q9" === C "true" []) &&&
                (call "checkAnswer'" [V "xs", V "newState", V "finishState", V "q4"])) |||
                ((V "q9" === C "false" []) &&&
                (V "q4" === C "false" []))))) |||
            ((V "q6" === C "false" []) &&&
            (V "q4" === C "false" [])))))
      )) (
      call "checkAnswer'" [V "a", V "startState", V "finishState", V "q1"])))
  )) (
  C "1" [] === C "1" []))))))

env = "open GT\nopen OCanren\nopen OCanren.Std\ntype person =\n  | G \n  | C \n  | W \n  | N \nlet g () = !! G\nlet c () = !! C\nlet w () = !! W\nlet n () = !! N\ntype 'a0 gstate =\n  | St of 'a0 * 'a0 * 'a0 * 'a0 \nmodule For_gstate =\n  (Fmap)(struct\n           let rec fmap fa0 =\n             function\n             | St (a0_0, a0_1, a0_2, a0_3) ->\n                 St ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2), (fa0 a0_3))\n           type 'a0 t = 'a0 gstate\n         end)\nlet rec st x__0 x__1 x__2 x__3 =\n  inj (For_gstate.distrib (St (x__0, x__1, x__2, x__3)))"
