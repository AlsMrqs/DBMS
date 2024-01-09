module DBMS.DBMS where

import System.Directory (createDirectory,removeDirectory,doesFileExist,removeFile)
import System.IO        (writeFile,readFile)
import System.IO.Error  (catchIOError,tryIOError,userError)

import Data.Bool        (bool)
import Data.Map         (fromList)

decode :: [[Char]] -> IO ()
decode ("data":xs) = database xs 
decode ("drop":xs) = dropbase xs
decode x           = can't "decode" x

--------------------DECODER FUNCTIONS---------------------

database :: [[Char]] -> IO ()
database [str] = putStrLn $ "Database "++str++" created!"
database x     = can't "database" x

parser :: [[Char]] -> IO ()
parser ["="] = putStrLn "Database is updated!"
parser _     = putStrLn "Invalid Input to a Data!!"

dropbase :: [[Char]] -> IO ()
dropbase [str] = putStrLn $ "Database "++str++" dropped!"
dropbase x     = can't "dropbase" x

can't :: [Char] -> [[Char]] -> IO ()
can't str [] = return =<< putStrLn $ "Empty "   ++ str ++ "!"
can't str _  = return =<< putStrLn $ "Invalid " ++ str ++ "!"

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

