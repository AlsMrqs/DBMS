module DBMS where

import System.Directory (createDirectory,removeDirectory,doesFileExist,removeFile)
import System.IO        (writeFile,readFile)
import System.IO.Error  (catchIOError,tryIOError,userError)

import Data.Bool        (bool)
import Data.Map         (fromList)

main :: IO ()
main = return ()

--function :: [[Char]] -> IO ()
--function    []    = return ()
--function (x:xs)
--    | x == "data" = do
--        createBase $ head xs
--        putStrLn $ "Base.Created!\ntail xs:" ++ show (tail xs)
--        function (tail xs)
--    | x == "="    = do
--        createData $ x ++ "/" ++ head xs
--        putStrLn $ "Data.Created!\ntail xs:" ++ show (tail xs)
--        function (tail xs)
--   | otherwise    = return ()

decoder :: [[Char]] -> IO ()
decoder x | x == [] = return ()
decoder ("data":xs) = database xs

database :: [[Char]] -> IO ()
database (x:xs) = do
    xbase  <- createBase x
    tailxs <- bool (error "Invalid sintax!") (return $ tail xs) (head xs == "=")
    xdata <- createData xbase (head tailxs)
    writeFile xdata (unwords $ tail tailxs)

-------------------- CREATE AND REMOVE DIR ----------------
createBase :: FilePath -> IO FilePath
createBase l = do
    catchIOError (createDirectory l) err
    return l 

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

