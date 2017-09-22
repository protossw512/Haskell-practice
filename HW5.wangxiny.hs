
-- Approach 1: Implement directly


module HW5 where


-- int    ::=    (any integer)    
             
-- reg    ::=    A   |   B    register names
             
-- expr    ::=    int    integer literal
-- |    reg    load from register
-- |    expr + expr    integer addition
-- |    expr <= expr    less than or equal to
-- |    not expr    boolean negation
-- |    and expr expr    boolean conjunction
             
-- stmt    ::=    reg := expr    store to register
-- |    if expr stmt stmt    conditional statement
             
-- prog    ::=    e  |  stmt ; prog    sequence of statements

data Reg = A
          |B
    deriving(Eq, Show)

type State = (Int, Int)

data Val = I Int
        | Bo Bool
        | Error
    deriving(Eq, Show)

data Expr = LitI Int
        | Case Reg
        | Add Expr Expr
        | LsEq Expr Expr
        | Not Expr
        | And Expr Expr
    deriving(Eq, Show)

data Stmt = Store Reg Expr
        | If Expr Stmt Stmt
    deriving(Eq, Show)

type Prog = [Stmt]


expr :: Expr -> State -> Val
expr (LitI i) (ll, rr)  = I i
expr (Case i) (ll, rr)  = case i of
                            A -> I ll
                            B -> I rr
expr (Add l r) (ll, rr) = case (expr l (ll, rr), expr r (ll, rr)) of
                            (I i, I j) -> I (i + j)
                            _           -> Error
expr (LsEq l r) (ll, rr) = case (expr l (ll, rr), expr r (ll, rr)) of
                            (I i, I j) -> Bo (i == j || i < j)
                            _            -> Error
expr (Not n) (ll, rr) = case (expr n (ll, rr)) of
                            Bo i -> Bo (not i)
                            _    -> Error
expr (And l r) (ll, rr) = case (expr l (ll, rr), expr r (ll, rr)) of
                            (Bo i, Bo j) -> Bo (i && j)
                            _    -> Error


                            
stmt :: Stmt -> State -> Maybe State
stmt (Store A k) (ll, rr) = case (expr k (ll, rr)) of
                              I i -> Just (i, rr)
                              _   -> Nothing
stmt (Store B k) (ll, rr) = case (expr k (ll, rr)) of
                              I i -> Just (ll, i)
                              _   -> Nothing                            
stmt (If n l r) (ll, rr)    = case expr n (ll, rr) of
                              Bo True  -> stmt l (ll, rr)
                              Bo False -> stmt r (ll, rr)
                              _        -> Nothing

prog :: [Stmt] -> State -> Maybe State
prog [] (ll, rr) = Just (ll, rr)
prog (x:xs) (ll, rr) = case (stmt x (ll, rr)) of
                         Just (i, j) -> prog xs (i, j)
                         _           -> Nothing                         
-- Test code                         
test :: Prog
test = [(Store A (LitI 3)),
        (Store B (Add (Case A) (LitI 2))),
		(If (LsEq (Case A) (Case B)) (Store A (Add (Case A) (Case A))) (Store B (Add (Case B) (Case B)))), 
		(Store B (Add (Case A) (Case B)))]

-- | Test sample program
--
--   >>> prog test (0,0)
--   Just (6,11)
--   >>> prog test (1,5)
--   Just (6,11)