message :: String
message = "Welcome to lecture 12!"

-- Our roadmap for the rest of the quarter:
-- - Change focus from *using* a functional PL
--   to *implementing* one!
-- - We've actually already been implementing PLs.

-- agenda:
-- - Unfortunately not the exam (that will be Thursday)
-- - Features that Nano (the little language we'll be implementing)
--   will have
-- - let-expressions (also a feature of Haskell)
-- - recap the notion of environment-passing

-- What will Nano have?
-- - Arithmetic expressions (we've seen this)
-- - Variables (we've seen this)
-- - let-expressions (today)
-- - functions (a new thing!)
-- - not just functions, but *recursive* functions (also new!)

-- Back to our little arithmetic interpreter from lecture 7:

-- Our AST (Abstract Syntax Tree) type for our little language (now with let-expressions!)
data Expr = Plus Expr Expr 
          | Minus Expr Expr
          | Times Expr Expr
          | ELet String Expr Expr
          | ENum Int                -- The base case
          | EVar String
  deriving Show

-- let x = 5 in x + 4
tinyLetExpression :: Expr
tinyLetExpression = ELet "x" (ENum 5) (Plus (EVar "x") (ENum 4))

-- let x = 5 in (let y = 4 in x + y)
biggerLetExpression :: Expr
biggerLetExpression = ELet "x" (ENum 5) 
                        (ELet "y" (ENum 4) (Plus (EVar "x") (EVar "y")))

-- let x = 5 in (let y = 4 in x + z)
problematicLetExpression :: Expr
problematicLetExpression = ELet "x" (ENum 5) 
                             (ELet "y" (ENum 4) (Plus (EVar "x") (EVar "z")))

-- let x = 5 in 3
sillyLetExpression :: Expr
sillyLetExpression = ELet "x" (ENum 5) (ENum 3)

-- 3 + (4 - 2)
bigExpression :: Expr
bigExpression = Plus (ENum 3) (Minus (ENum 4) (ENum 2))

-- ((2 + 8) - (10 * 6)) - 1
biggerExpression :: Expr
biggerExpression = Minus (Minus (Plus (ENum 2) (ENum 8)) 
                                (Times (ENum 10) (ENum 6))) (ENum 1)

-- What's an "environment"?
-- It's a way to associate variables with...something.
-- (e.g., values or types)
-- We're going to be associating variables with values.
type Env = [(String, Int)]

exampleEnv :: [(String, Int)]
exampleEnv = [("x", 3), ("y", 4)]

-- And our interpreter from lecture 8 (but now with support for let-expressions!)
eval :: Expr -> Env -> Int
eval (ENum n) env = n
eval (EVar s) env = lookupInEnv env s
eval (Plus n m) env = eval n env + eval m env
eval (Minus n m) env = eval n env - eval m env
eval (Times n m) env = eval n env * eval m env
-- We want the value of a let-expression to be
-- whatever the value of its body is,
-- but taking into account the new binding that the let-expression created.
-- Note: don't forget that the expression bound in the binding
-- part of the let-expression has to be evaluated too!
eval (ELet s expr bodyExpr) env = eval bodyExpr (extendEnv s (eval expr env) env)

lookupInEnv :: Env -> String -> Int 
lookupInEnv [] s            = error ("Sorry, the variable " ++ s ++ " is unbound!")
lookupInEnv ((id,val):xs) s = if s == id then val else lookupInEnv xs s

extendEnv :: String -> Int -> Env -> Env
extendEnv s n env = (s,n):env

-- So far, this has all been quite boring.

-- But let's think about something new.
-- What about programs that actually bind new variables within the program?

-- This leads us to let-expressions.

exampleLetExpr :: Int
exampleLetExpr = let x = 5   -- "x = 5" is a *binding*
                   in x + 3  -- "x + 3" is the *body* of this let-expression

-- The value of a let-expression is
-- whatever the value of its body is
-- when the binding has been included in the environment.

-- And the type of a let-expression is
-- whatever the type of its body is
-- with the binding taken into account.

-- The quiz expression:
-- quiz = let x = 5 in
--         (let y = x + z in
--           (let z = 10 in y))

quizExpr :: Expr
quizExpr = ELet "x" (ENum 5) (ELet "y" (Plus (EVar "x") (EVar "z")) (ELet "z" (ENum 10) (EVar "y")))
-- This expression has no value in Haskell (or in Nano);
-- you should get a run-time error saying that z is unbound.

-- BUT in a language with dynamic scope (example: Emacs Lisp),
-- this would evaluate to 15!

-- OK, that was actually a little bit of a lie.  This particular program would be OK in Emacs Lisp.
-- But if it were written using a function, like this:
--     
-- let x = 5 in
--   (let y = \_ -> x + z in
--     (let z = 10 in
--       y ()))
-- 
-- Then it would evaluate to 15 in a dynamically-scoped language, like Emacs Lisp.

-- Step by step:
-- Evaluate the body (let y = x + z in (let z = 10 in y)) in the environment [("x", 5)]
-- Evaluate the body (let z = 10 in y) in the environment [("y", ??? value of x + z ???),("x", 5)]

-- Uh-oh!  We can't extend the environment with a value bound to y,
-- because we don't know what the value of x + z is supposed to be.
-- Why don't we know what the value of x + z is supposed to be?
-- Because we don't know what the value of z is supposed to be at this point. 
