module DBMS where

import System.Directory (createDirectory,removeDirectory,doesFileExist,removeFile)
import System.IO        (writeFile,readFile)
import System.IO.Error  (catchIOError,tryIOError,userError)

import Data.Bool        (bool)
import Data.Map         (fromList)

main :: IO ()
main = return ()

function :: [[Char]] -> IO ()
function    []    = return ()
function (x:xs)
    | x == "data" = do
        createBase $ head xs
        putStrLn $ "Base.Created!\ntail xs:" ++ show (tail xs)
        function (tail xs)
    | x == "="    = do
        createData $ x ++ "/" ++ head xs
        putStrLn $ "Data.Created!\ntail xs:" ++ show (tail xs)
        function (tail xs)
   | otherwise    = return ()

decoder :: [[Char]] -> [IO ()]
decoder x | x == [] = [return ()]
decoder ("data":xs) = createBase (head xs) : decoder (tail xs)
decoder ("alpha":xs) = putStrLn "alpha processed!" : decoder (xs)
decoder _           = [putStrLn =<< readFile "Alpha.hs"]


f ("alpha":xs) = putStrLn "alpha"
f ("beta" :xs) = putStrLn "beta"
f (x:xs)       = return ()

-------------------- CREATE AND REMOVE DIR ----------------
createBase :: FilePath -> IO ()
createBase = (`catchIOError` err) . createDirectory

removeBase :: FilePath -> IO ()
removeBase = (`catchIOError` err) . removeDirectory 

------------------- CREATE AND REMOVE FILE ------------------
createData :: FilePath -> IO ()
createData = (`catchIOError` err) . (`writeFile` "") . (++ ".data")

removeData :: FilePath -> IO ()
removeData = (`catchIOError` err) . removeFile . (++ ".data")

------------------- DBMS.IO.Error ----------------
err :: IOError -> IO ()
err e = putStrLn $ "DBMS.Error:\n" ++ (show e)

