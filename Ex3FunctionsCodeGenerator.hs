module Ex3FunctionsCodeGenerator where

import Ex3FunctionsTypes
import Data.List

-----------------------------------------------------------
--Solution for Compilers exercise 3

--Paul Kelly  Imperial College London  2009
-----------------------------------------------------------

--Part (1): translate function declaration

transFunction :: Function -> [Instr] 
transFunction (Defun fname paramname body)
  = [Define fname]
    ++ transExp body (allRegs \\ [paramReg]) ++ [Ret]

--Part (2): saving registers

saveRegs :: [Register] -> [Instr]
saveRegs []
  = []
saveRegs (reg:rest)
  = (Mov (Reg reg) Push) : saveRegs rest 

restoreRegs :: [Register] -> [Instr]
restoreRegs []
  = []
restoreRegs (reg:rest)
  = (Mov Pop (Reg reg)) : saveRegs rest 

--Part (3): translate expression (ie function body, perhaps including
--function calls)

transExp :: Exp -> [Register] -> [Instr]
transExp (Const i) (dst:rest) 
  = [Mov (ImmNum i) (Reg dst)]
transExp (Var str) (dst:rest)
  = [Mov (Reg paramReg) (Reg dst)]
transExp (Minus expr1 expr2) (dst:nxt:rest)
  = if (weight expr1 > weight expr2) 
    then
      (transExp expr1 (dst:nxt:rest)) 
      ++ (transExp expr2 (nxt:rest)) ++ sub
    else
      (transExp expr2 (nxt:dst:rest)) 
      ++ (transExp expr1 (dst:rest)) ++ sub
    where 
      sub = [Sub (Reg nxt) (Reg dst)]
transExp (Apply str expr) regs
  = (saveRegs usedRegs) ++ (transExp expr regs) 
  ++ [Mov (Reg r) (Reg paramReg)]
  ++ [Jsr str] ++ [Mov (Reg resultReg) (Reg r)]
  ++ (restoreRegs usedRegs)
    where
      usedRegs = (allRegs \\ regs)
      (r:rest) = regs


weight :: Exp -> Int
weight (Const x) 
  = 1
weight (Var str)
  = 1
weight (Minus expr1 expr2)
  = min w_expr1 w_expr2
  where
    w_expr1 = max (weight expr1) ((weight expr2) + 1)
    w_expr2 = max ((weight expr1) + 1) (weight expr2)
weight (Apply str expr)
  = weight expr
