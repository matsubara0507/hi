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
readCmd = toCmd <$> getLine

toCmd :: String -> Cmd
toCmd "h" = LeftCursor
toCmd "j" = DownCursor
toCmd "k" = UpCursor
toCmd "l" = RightCursor
toCmd "x" = Delete
toCmd ('a' : ' ' : txt) = Insert txt
toCmd s = error $ mconcat ["undefined command \"", s, "\""]

edit :: Cmd -> EState -> EState
edit cmd = updateFile cmd . updateCursor cmd

updateFile :: Cmd -> EState -> EState
updateFile (Insert txt) es@(EState _ fc (r,c)) = es { getFileContents = insertTextOnFile txt c r fc }
updateFile Delete       es@(EState _ fc (r,c)) = es { getFileContents = deleteCharOnFile c r fc }
updateFile _ es = es

insertTextOnFile :: String -> Int -> Int -> File -> File
insertTextOnFile txt c 0 (x:xs) = insertTextOnLine txt c x : xs
insertTextOnFile txt c n (x:xs) = x : insertTextOnFile txt c (n - 1) xs

insertTextOnLine :: String -> Int -> String -> String
insertTextOnLine txt 0 line = txt `mappend` line
insertTextOnLine txt n (x:xs) = x : insertTextOnLine txt (n-1) xs

deleteCharOnFile :: Int -> Int -> File -> File
deleteCharOnFile c 0 (x:xs) = deleteCharOnLine c x : xs
deleteCharOnFile c n (x:xs) = x : deleteCharOnFile c (n-1) xs

deleteCharOnLine :: Int -> String -> String
deleteCharOnLine 0 (x:xs) = xs
deleteCharOnLine n (x:xs) = x : deleteCharOnLine (n-1) xs

updateCursor :: Cmd -> EState -> EState
updateCursor cmd es@(EState _ fc csr) = es { getCursor = (r', c') }
  where
    (r, c) = moveCursor cmd csr
    r' = max 0 $ min (length fc) r
    c' = max 0 $ min (length $ fc !! r') c

moveCursor :: Cmd -> Cursor -> Cursor
moveCursor UpCursor    (r, c) = (r - 1, c)
moveCursor DownCursor  (r, c) = (r + 1, c)
moveCursor LeftCursor  (r, c) = (r, c - 1)
moveCursor RightCursor (r, c) = (r, c + 1)
moveCursor _ csr = csr
