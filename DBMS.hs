module DBMS.DBMS where

import System.Directory (createDirectory,removeDirectory,doesFileExist,removeFile)
import System.IO.Error  (catchIOError,tryIOError,userError)
import System.IO        (writeFile,readFile)

import Data.Char        (isAlpha,isNumber,isUpper)
import Data.Bool        (bool)
import Data.Map         (fromList)

import qualified DBMS.Parser as Parse         

----------------------NODES------------------------------

decode :: [[Char]] -> IO ()
decode ("data":xs) = _data xs 
decode x           = invalid "decode" x

--------------------DECODER FUNCTIONS---------------------

_data :: [[Char]] -> IO ()
_data (x:xs) = either (\_ -> assign xs) putStrLn (Parse.constructor x)
_data x      = invalid "data " x

invalid :: [Char] -> [[Char]] -> IO ()
invalid str [] = return =<< putStrLn $ "Empty "  ++str++"!"
invalid str _  = return =<< putStrLn $ "Invalid "++str++"!"

----------------------DEALING WITH DATA--------------------

assign :: [[Char]] -> IO ()
assign ("=":xs) = putStrLn "Database was started but not written!"
assign x        = invalid "connect" x

-------------------- CREATE AND REMOVE DIR ----------------

createBase :: FilePath -> IO FilePath
createBase x = do
    catchIOError (createDirectory x) err
    return x 

removeBase :: FilePath -> IO ()
removeBase = (`catchIOError` err) . removeDirectory 

------------------- CREATE AND REMOVE FILE ------------------
createData :: FilePath -> FilePath -> IO FilePath
createData bas dat = do
    let dataFile = (bas ++ "/" ++ dat ++ ".data")
    (`catchIOError` err) $ writeFile dataFile ""
    return dataFile

removeData :: FilePath -> IO ()
removeData = (`catchIOError` err) . removeFile . (++ ".data")

------------------- DBMS.IO.Error ----------------
err :: IOError -> IO ()
err e = putStrLn $ "DBMS.Error:\n" ++ (show e)

