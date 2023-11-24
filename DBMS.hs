module DBMS where

import System.Directory (createDirectory,removeDirectory,doesFileExist,removeFile)
import System.IO        (writeFile,readFile)
import System.IO.Error  (catchIOError,tryIOError)

import Data.Bool        (bool)
import Data.Map         (fromList)

main :: IO ()
main = do
    putStr "DBMS: "
    bool (return ()) main =<< command =<< getLine

command :: [Char] -> IO (Bool)
command "exit" = return False
command _      = putStrLn "Not a command!" >>= (\_ -> return True)


-------------------- CREATE AND REMOVE DIR ----------------
createBase :: FilePath -> IO ()
createBase = (`catchIOError` err) . createDirectory
    where
    err e = putStrLn $ "DBMS.Error:\n" ++ (show e)

removeBase :: FilePath -> IO ()
removeBase = (`catchIOError` err) . removeDirectory 
    where
    err e = putStrLn $ "DBMS.Error:\n" ++ (show e)

------------------- CREATE AND REMOVE FILE ------------------
createData :: FilePath -> IO ()
createData = (`catchIOError` err) . (`writeFile` "") . (++) ".data"
    where
    err e = putStrLn $ "DBMS.Error:\n" ++ (show e)

removeData :: FilePath -> IO ()
removeData = (`catchIOError` err) . removeFile . (++) ".data"
    where
    err e = putStrLn $ "DBMS.Error:\n" ++ (show e)

