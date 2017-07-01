module Main where

main :: IO ()
main = putStrLn "Hello, World!"

type Args = [String]

hi :: Args -> IO ()
hi args = do
  es1 <- mkInitEState args
  print es1
  cmd <- readCmd
  let es2 = edit cmd es1
  print es2

type File = [String]
type Cursor = (Int, Int)

data EState = EState
            { getFilePath :: FilePath
            , getFileContents :: File
            , getCursor :: Cursor
            } deriving (Show)

data Cmd = Insert String
         | UpCursor
         | DownCursor
         | RightCursor
         | LeftCursor
         | Delete
         deriving (Show)

mkInitEState :: Args -> IO EState
mkInitEState args = do
  file <- lines <$> readFile filepath
  return $ EState filepath file (0, 0)
  where
    filepath = argsToFilePath args

argsToFilePath :: Args -> FilePath
argsToFilePath [] = error "few arguments"
argsToFilePath (x:_) = x

readCmd :: IO Cmd
readCmd = undefined

edit :: Cmd -> EState -> EState
edit = undefined
