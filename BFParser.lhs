
> module BFParser 
>	( BFReturnCode(BFOK, BFError, BFReloop, BFRequireInput)
>	, BFErrorCode(BFUnmatchedLoop, BFPointerOutOfRange, 
>		BFUnknownInstruction)
>	, updatemem
>	, bfparse ) where

> import Data.Char
> import Data.Maybe

> type Program   = String
> type DataStore = (Int, String, String)

> data BFErrorCode 
>	= BFUnmatchedLoop
>	| BFPointerOutOfRange Int
>	| BFUnknownInstruction Char
>	deriving (Eq, Show, Read)

> data BFReturnCode 
>	= BFOK String
>	| BFError BFErrorCode
>	| BFReloop DataStore
>	| BFRequireInput Program DataStore
>	deriving (Eq, Show, Read)

> bfparse :: Program -> DataStore -> BFReturnCode
> bfparse []        (_,      _,   output) = BFOK $ reverse output
> bfparse xx@(x:xs) yy@(ptr, mem, output) 
>	| x == '['  = if ord (mem!!ptr) == 0
>		then
>			let j = eat xs 1 in 
>				if isJust $ j 
>					then bfparse (fromJust j) yy
>					else BFError BFUnmatchedLoop
>		else loop (bfparse xs yy) xs
>	| x == ']'  = if ord (mem!!ptr) == 0
>		then bfparse xs yy
>		else BFReloop yy
>	| x == ','  = BFRequireInput xs yy
>   | x == '.'  = bfparse xs (ptr, mem, (mem!!ptr):output)
>	| x == '+'  = bfparse xs (ptr, 
>		updatemem mem ptr 0 (chr . (+1) . ord), output)
>	| x == '-'  = bfparse xs (ptr, 
>		updatemem mem ptr 0 (chr . (subtract 1) . ord), output)
>	| x == '>'  = if ptr + 1 >= length mem
>		then BFError $ BFPointerOutOfRange (ptr+1)
>		else bfparse xs (ptr+1, mem, output)
>	| x == '<'  = if ptr - 1 < 0 
>		then BFError $ BFPointerOutOfRange (-1)
>		else bfparse xs (ptr-1, mem, output)
>	| x == '\n' = bfparse xs yy
>	| x == '\r' = bfparse xs yy
>	| x == '\t' = bfparse xs yy
>	| x == ' '  = bfparse xs yy
>	| otherwise = BFError $ BFUnknownInstruction x
>	where
>		eat :: Program -> Int -> Maybe Program
>		eat x      0 = Just x
>		eat []     _ = Nothing
>		eat (x:xs) c
>			| x == '['  = eat xs (c+1)
>			| x == ']'  = eat xs (c-1)
>			| otherwise = eat xs c

>		loop :: BFReturnCode -> Program -> BFReturnCode
>		loop (BFReloop x) prog = loop (bfparse prog x) prog
>		loop x            _    = x

> updatemem :: String -> Int -> Int -> (Char -> Char) -> String
> updatemem []     _ _ _ = []
> updatemem (x:xs) p c f 
>	| p == c    = (f x):xs
>	| otherwise = x:(updatemem xs p (c+1) f)
