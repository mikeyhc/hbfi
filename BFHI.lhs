> import BFParser
> import System.Environment
> import System.Directory
> import System.IO
> import Data.Char

> main = do 
>	args <- getArgs
>	contents <- if length args >= 1 
>		then readFile $ head args
>		else getLine
>	hSetBuffering stdin NoBuffering
>	mainloop $ bfparse contents (0, take 30000 $ [(chr 0)..], [])

> mainloop :: BFReturnCode -> IO ()
> mainloop (BFOK str)                     = putStrLn $ "\n" ++ str
> mainloop (BFError  e)                   = do
>	putStr "\nerror: "
>	parseError e
> mainloop (BFReloop _)                   = putStrLn "unmatched ]"
> mainloop (BFRequireInput p (ptr,mem,o)) = do
>		c <- getChar
>		mainloop $ bfparse p (ptr, (updatemem mem ptr 0 (\x -> c)), o)

> parseError :: BFErrorCode -> IO ()
> parseError BFUnmatchedLoop          = putStrLn "unmatched ["
> parseError (BFPointerOutOfRange i)  = putStrLn $ (show i) ++ 
>	" is out of pointer range"
> parseError (BFUnknownInstruction c) = putStrLn $ "unknown instruction: "
>	++ [c]
