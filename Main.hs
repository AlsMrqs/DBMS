import qualified DBMS as DBMS

import System.IO.Error (tryIOError,catchIOError)

import Data.Map        (fromList)
import Data.Bool       (bool)

main :: IO  ()
main = do
    putStr "Main: "
    bool (return ()) main . (==) () =<< menu =<< getLine
    
menu :: [Char] -> IO ()
menu "database" = DBMS.main
menu _          = putStrLn "Invalid input!"

