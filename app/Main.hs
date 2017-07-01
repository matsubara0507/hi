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
edit cmd = updateFile cmd . moveCursor cmd

updateFile :: Cmd -> EState -> EState
updateFile (Insert txt) es@(EState _ fc (r,c)) = es { getFileContents = insertTextOnFile r c fc txt }
updateFile Delete       es@(EState _ fc (r,c)) = es { getFileContents = deleteCharOnFile r c fc }
updateFile _ es = es

insertTextOnFile :: Int -> Int -> File -> String -> File
insertTextOnFile 0 c (x:xs) txt = insertTextOnLine c x txt : xs
insertTextOnFile n c (x:xs) txt = x : insertTextOnFile (n-1) c xs txt

insertTextOnLine :: Int -> String -> String -> String
insertTextOnLine 0 line txt = txt `mappend` line
insertTextOnLine n (x:xs) txt = x : insertTextOnLine (n-1) xs txt

deleteCharOnFile :: Int -> Int -> File -> File
deleteCharOnFile 0 c (x:xs) = deleteCharOnLine c x : xs
deleteCharOnFile n c (x:xs) = x : deleteCharOnFile (n-1) c xs

deleteCharOnLine :: Int -> String -> String
deleteCharOnLine 0 (x:xs) = xs
deleteCharOnLine n (x:xs) = x : deleteCharOnLine (n-1) xs

moveCursor :: Cmd -> EState -> EState
moveCursor cmd es@(EState _ fc csr) = es { getCursor = moveCursor' cmd csr}
  where
    moveCursor' UpCursor    (r,c) = (r - (if r > 0 then 1 else 0), c)
    moveCursor' DownCursor  (r,c) = (r + (if r < length fc - 1 then 1 else 0), c)
    moveCursor' LeftCursor  (r,c) = (r, c - (if c > 0 then 1 else 0))
    moveCursor' RightCursor (r,c) = (r, c + (if c < length (fc !! r) - 1 then 1 else 0))
    moveCursor' _ csr = csr
