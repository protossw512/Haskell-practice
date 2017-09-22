-- I completed the extra credit, which is Q5. Please check it.


module KarelLang where

import Text.PrettyPrint

-- nat	::=	(any natural number)	
-- macro	::=	(any macro name)	
 			
-- prog	::=	defs  define main as stmt	a Karel program
 			
-- defs	::=	ε ???  |  def  defs	sequence of definitions
-- stmts	::=	ε  |  stmt ; stmts	sequence of statements
 			
-- card	::=	north | south | east | west	cardinal directions
-- dir	::=	front | back | right | left	relative directions
 			
-- def	::=	define macro as stmt	macro definition
 			
-- test	::=	not test	boolean negation
-- |	facing card ?	am I facing … ?
-- |	clear dir ?	can I move … ?
-- |	beeper?	is there a beeper here?
-- |	empty?	is my bag empty?
 			
-- stmt	::=	shutdown	end the program
-- |	move	move forward
-- |	pick beeper	take a beeper
-- |	put beeper	leave a beeper
-- |	turn dir	rotate in place
-- |	call macro	invoke a macro
-- |	iterate nat times stmt	fixed repetition loop
-- |	if test then stmt else stmt	conditional branch
-- |	while test do stmt	conditional loop
-- |	begin stmts end	statement block


-- Q1
import Prelude hiding (Num, Right)

type Nat = Int

type Macro = String

data Prog = DefineMainAs Defs Stmt
	deriving(Eq, Show)

type Defs = [Def]

type Stmts = [Stmt]

data Card = North
			| South
			| East
			| West
	deriving(Eq, Show)

data Dir = Front
			| Back
			| Right
			| Left
	deriving(Eq, Show)

data Def = Define Macro Stmt
	deriving(Eq, Show)

data Test = Not Test
			| Facing Card
			| Clear Dir
			| Beeper
			| Empty
	deriving (Eq, Show)

data Stmt = Shutdown
			| Move
			| PickBeeper
			| PutBeeper
			| Turn Dir
			| Call Macro
			| Iterate Nat Stmt
			| If Test Stmt Stmt
			| While Test Stmt
			| Begin Stmts
	deriving (Eq, Show)

-- Q2

fetch :: Def
fetch = Define "fetch" (Begin [While (Not Beeper) (Begin [If (Clear Front) Move Shutdown, PickBeeper])])

fetcher :: Prog
fetcher = DefineMainAs [fetch] (Begin [PutBeeper, Call "fetch", Turn Back, Call "fetch", PutBeeper, PutBeeper, Shutdown])


--Q3
findmacros :: Def -> Macro
findmacros (Define a _) = a 

macros :: Prog -> [Macro]
macros (DefineMainAs c _) = map findmacros c

--Q4

rectangle :: Nat -> Nat -> Stmt
rectangle 0 _ = Shutdown
rectangle _ 0 = Shutdown
rectangle x y = While (Facing North) (Begin [Iterate x Move, Turn Right, Iterate y Move, Turn Right, Iterate x Move, Turn Right, Iterate y Move])

--Q5

-- pretty :: Prog -> String
-- pretty (DefineMainAs Defs Stmt) = 

-- printprog :: Prog -> String
-- printprog (DefineMainAs [a] b) = 
							-- do
								-- printdefs [a]
								-- printstm

-- Q5 Bonus
								
printtest :: Test -> String
printtest a = show a ++ "?"

printstmts :: Stmts -> String
printstmts (x:xs)
				| xs == [] = printstmt x
				| otherwise = printstmt x ++ "\n" ++ printstmts xs
								
printstmt :: Stmt -> String
printstmt (Iterate a b) = "iterate " ++ show a ++ " times " ++ show b ++ "\n" ++ ";"
printstmt (If a b c) = "if " ++ printtest a ++ "\n" ++ "then " ++ printstmt b ++ "\n" ++ "else " ++ printstmt c
printstmt (While a b) = "While " ++ printtest a ++ " do " ++ printstmt b
printstmt (Begin a) = "\n" ++ printstmts a
printstmt a = show a ++ ";"

printdef :: Def -> String
printdef (Define a b) = "Define " ++ show a ++ " as " ++ "\n" ++"Begin" ++ printstmt b ++ "\n" ++ "End" ++ "\n"

printdefs :: Defs -> String
printdefs [] = " "
printdefs (x:xs) = printdef x ++ "\n" ++ printdefs xs

pretty :: Prog -> String
pretty (DefineMainAs a b) = printdefs a ++ "Define main as" ++ "\n" ++ "Begin" ++ printstmt b ++ "\n" ++ "End"

