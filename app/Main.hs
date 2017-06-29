module Main
    ( main
    , File
    , editFile
    ) where

main :: IO ()
main = putStrLn "Hello, World!"

type File = [String]

editFile :: File -> File
editFile = id
