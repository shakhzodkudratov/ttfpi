-- Definition 1.3.2
inductive L where
  | var (name : String)
  | app (left : L) (right: L)
  | abs (name: String) (body: L)

open L

#check var "a"
#check app (var "a") (var "b")

-- identity 
#check abs "x" (var "x")


-- Examples 1.3.3
-- -- with Variable as construction principle:
#check var "x"
#check var "y"
#check var "z"

-- -- with Application as final construction step:
#check app (var "x") (var "x") -- (xx)
#check app (var "y") (var "x") -- (yx)
#check app (var "x") (app (var "x") (var "z")) -- (x(xz))

-- -- with Abstraction as final step:
#check abs "x" (app (var "x") (var "z")) -- (Lx.(xz))
#check abs "y" (abs "z" (var "x")) -- (Ly.(Lz.x))
#check abs "x" (abs "x" (app (var "x") (var "x"))) -- (Lx.(Lx.(xx)))

-- -- and again, with Application as final step:
#check app (abs "x" (app (var "x") (var "z"))) (var "y") -- ((Lx.(xz))y)
#check app (var "y") (abs "x" (app (var "x") (var "z"))) -- (y(Lx.(xz)))
#check app (abs "x" (var "x")) (abs "x" (var "x")) -- ((Lx.x)(Lx.x))


-- Notation 1.3.4
def syntacticEquality (l1 l2: L) : Bool := match l1 with
| var name => (
  match l2 with 
  | var name2 => name == name2
  | _ => false
)
| app left right => (
  match l2 with
  | app left2 right2 => (syntacticEquality left left2) && (syntacticEquality right right2)
  | _ => false
)
| abs name body => (
  match l2 with
  | abs name2 body2 => name == name2 && (syntacticEquality body body2)
  | _ => false
)

notation l1 " === " l2 => syntacticEquality l1 l2

#eval (var "x") === (var "x") -- x === x
#eval (var "x") === (var "y") -- x !== y
#eval (app (var "x") (var "x")) === (app (var "x") (var "x")) -- (xx) === (xx)
#eval (app (var "x") (var "x")) === (app (var "x") (var "y")) -- (xx) !== (xy)
#eval (abs "x" (var "x")) === (abs "x" (var "x")) -- (Lx.x) === (Lx.x)
#eval (abs "x" (var "x")) === (abs "x" (var "y")) -- (Lx.x) !== (Lx.y)

