data Prog = On Instr
data Instr = Off | Expr :> Instr
data Expr = Mem | V Int | Expr :+ Expr

type Env = Int
type DomProg = [Int]
type DomInstr = Env -> [Int]
type DomExpr = Env -> Int


prog :: Prog -> DomProg
prog (On instr) = stmt instr 0

stmt :: Instr -> DomInstr
stmt Off env = []
stmt (exp :> instr) env = (expr exp env) : (stmt instr (env + (expr exp env)))


expr :: Expr -> DomExpr
expr Mem env = env
expr (V a) env = a
expr (a :+ b) env = expr a env + expr b env



--2
type Name = String

data Hask = HTrue
            | HFalse
            | HLit Int
            | HIf Hask Hask Hask
            | Hask :==: Hask
            | Hask :+: Hask
            | HVar Name
            | HLam Name Hask
            | Hask :$: Hask
    deriving (Read, Show)

infix 4 :==:
infixl 6 :+:
infixl 9 :$:

data Value = VBool Bool
            | VInt Int
            | VFun (Value -> Value)
            | VError

type HEnv = [(Name, Value)]
type DomHask = HEnv -> Value

instance Show Value where
    show (VBool bool) = show bool
    show (VInt int) = show int
    show (VFun fun) = show "[Fun]"
    show (VError) = show "[Error]"

instance Eq Value where
    VBool a == VBool b = a == b
    VInt a == VInt b = a == b
    VFun a == VFun b = error "Comp func"
    VError == VError = error "Comp error"

hEval :: Hask -> DomHask
hEval HTrue _ = VBool True
hEval HFalse _ = VBool False
hEval (HLit int) _ = VInt int
hEval (HIf a b c) x
    | hEval a x == VBool True = hEval b x
    | otherwise = hEval c x
