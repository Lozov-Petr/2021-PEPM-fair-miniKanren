module Program.Hanoi where

import Syntax

hanoi =
  Let (Def "notEqStick" ["x", "y", "q44"] (
    ((V "x" === C "one" []) &&&
    (((V "y" === C "one" []) &&&
    (V "q44" === C "false" [])) |||
    ((V "y" === C "two" []) &&&
    (V "q44" === C "true" [])) |||
    ((V "y" === C "thr" []) &&&
    (V "q44" === C "true" [])))) |||
    ((V "x" === C "two" []) &&&
    (((V "y" === C "one" []) &&&
    (V "q44" === C "true" [])) |||
    ((V "y" === C "two" []) &&&
    (V "q44" === C "false" [])) |||
    ((V "y" === C "thr" []) &&&
    (V "q44" === C "true" [])))) |||
    ((V "x" === C "thr" []) &&&
    (((V "y" === C "one" []) &&&
    (V "q44" === C "true" [])) |||
    ((V "y" === C "two" []) &&&
    (V "q44" === C "true" [])) |||
    ((V "y" === C "thr" []) &&&
    (V "q44" === C "false" []))))
  )) (
  Let (Def "isNil" ["l", "q39"] (
    ((V "l" === C "nil" []) &&&
    (V "q39" === C "true" [])) |||
    (fresh ["q41", "q42"] (
       (V "l" === C "%" [V "q41", V "q42"]) &&&
       (V "q39" === C "false" [])))
  )) (
  Let (Def "less" ["a", "b", "q36"] (
    fresh ["b'"] (
      (V "b" === C "s" [V "b'"]) &&&
      (((V "a" === C "o" []) &&&
      (V "q36" === C "true" [])) |||
      (fresh ["a'"] (
         (V "a" === C "s" [V "a'"]) &&&
         (call "less" [V "a'", V "b'", V "q36"])))))
  )) (
  Let (Def "get" ["name", "state", "q31"] (
    fresh ["s1", "s2", "s3"] (
      (V "state" === C "triple" [V "s1", V "s2", V "s3"]) &&&
      (((V "name" === C "one" []) &&&
      (V "s1" === V "q31")) |||
      ((V "name" === C "two" []) &&&
      (V "s2" === V "q31")) |||
      ((V "name" === C "thr" []) &&&
      (V "s3" === V "q31"))))
  )) (
  Let (Def "set" ["name", "stack", "state", "q26"] (
    fresh ["s1", "s2", "s3"] (
      (V "state" === C "triple" [V "s1", V "s2", V "s3"]) &&&
      (((V "name" === C "one" []) &&&
      (V "q26" === C "triple" [V "stack", V "s2", V "s3"])) |||
      ((V "name" === C "two" []) &&&
      (V "q26" === C "triple" [V "s1", V "stack", V "s3"])) |||
      ((V "name" === C "thr" []) &&&
      (V "q26" === C "triple" [V "s1", V "s2", V "stack"]))))
  )) (
  Let (Def "one_step" ["step", "state", "q13"] (
    fresh ["fromN", "toN", "q15", "q17", "x", "xs", "q19"] (
      (V "step" === C "pair" [V "fromN", V "toN"]) &&&
      (V "q15" === C "true" []) &&&
      (V "q17" === C "%" [V "x", V "xs"]) &&&
      (call "notEqStick" [V "fromN", V "toN", V "q15"]) &&&
      (call "get" [V "fromN", V "state", V "q17"]) &&&
      (call "get" [V "toN", V "state", V "q19"]) &&&
      ((fresh ["q20"] (
          (V "q19" === C "nil" []) &&&
          (call "set" [V "fromN", V "xs", V "state", V "q20"]) &&&
          (call "set" [V "toN", C "%" [V "x", C "nil" []], V "q20", V "q13"]))) |||
      (fresh ["y", "ys", "q23", "q24"] (
         (V "q19" === C "%" [V "y", V "ys"]) &&&
         (V "q23" === C "true" []) &&&
         (call "less" [V "x", V "y", V "q23"]) &&&
         (call "set" [V "fromN", V "xs", V "state", V "q24"]) &&&
         (call "set" [V "toN", C "%" [V "x", C "%" [V "y", V "ys"]], V "q24", V "q13"])))))
  )) (
  Let (Def "check" ["state", "steps", "q0"] (
    (fresh ["q1", "q2", "q7", "q9"] (
       (V "steps" === C "nil" []) &&&
       (call "get" [C "one" [], V "state", V "q7"]) &&&
       (call "isNil" [V "q7", V "q1"]) &&&
       (call "get" [C "two" [], V "state", V "q9"]) &&&
       (call "isNil" [V "q9", V "q2"]) &&&
       (((V "q1" === C "false" []) &&&
       (V "q0" === C "false" [])) |||
       ((V "q1" === C "true" []) &&&
       (V "q0" === V "q2"))))) |||
    (fresh ["x", "xs", "q11"] (
       (V "steps" === C "%" [V "x", V "xs"]) &&&
       (call "one_step" [V "x", V "state", V "q11"]) &&&
       (call "check" [V "q11", V "xs", V "q0"])))
  )) (
  C "1" [] === C "1" [])))))))

env = "open GT\nopen OCanren\nopen OCanren.Std\ntype 'a0 gnat =\n  | O \n  | S of 'a0 \nmodule For_gnat =\n  (Fmap)(struct\n           let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\n           type 'a0 t = 'a0 gnat\n         end)\nlet rec o () = inj (For_gnat.distrib O)\nand s x__0 = inj (For_gnat.distrib (S x__0))\ntype stick =\n  | One \n  | Two \n  | Thr \nlet one () = !! One\nlet two () = !! Two\nlet thr () = !! Thr\ntype 'a triple =\n  | Triple of 'a * 'a * 'a \nmodule For_triple =\n  (Fmap)(struct\n           let rec fmap fa =\n             function\n             | Triple (a_0, a_1, a_2) ->\n                 Triple ((fa a_0), (fa a_1), (fa a_2))\n           type 'a t = 'a triple\n         end)\nlet rec triple x__0 x__1 x__2 =\n  inj (For_triple.distrib (Triple (x__0, x__1, x__2)))"
