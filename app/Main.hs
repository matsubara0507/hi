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
toCmd ('i' : ' ' : txt) = Insert txt
toCmd s = error $ mconcat ["undefined command \"", s, "\""]

edit :: Cmd -> EState -> EState
edit cmd = updateFile cmd . updateCursor cmd

updateFile :: Cmd -> EState -> EState
updateFile (Insert txt) es@(EState _ fc (r,c)) = es { getFileContents = insertTextOnFile txt c r fc }
updateFile Delete       es@(EState _ fc (r,c)) = es { getFileContents = deleteCharOnFile c r fc }
updateFile _ es = es

insertTextOnFile :: String -> Int -> Int -> File -> File
insertTextOnFile txt c = updateOn (\(x:xs) -> insertTextOnLine txt c x : xs)

insertTextOnLine :: String -> Int -> String -> String
insertTextOnLine txt = updateOn (mappend txt)

deleteCharOnFile :: Int -> Int -> File -> File
deleteCharOnFile c = updateOn (\(x:xs) -> deleteCharOnLine c x : xs)

deleteCharOnLine :: Int -> String -> String
deleteCharOnLine = updateOn tail

updateOn :: ([a] -> [a]) -> Int -> [a] -> [a]
updateOn f 0 xs = f xs
updateOn f n (x:xs') = x : updateOn f (n - 1) xs'
updateOn _ n [] = error $ "List is empty on " `mappend` (show n)

updateCursor :: Cmd -> EState -> EState
updateCursor cmd es@(EState _ fc csr) = es { getCursor = (r', c') }
  where
    (r, c) = moveCursor cmd csr
    r' = max 0 $ min (length fc - 1) r
    c' = max 0 $ min (length (fc !! r') - 1) c

moveCursor :: Cmd -> Cursor -> Cursor
moveCursor UpCursor    (r, c) = (r - 1, c)
moveCursor DownCursor  (r, c) = (r + 1, c)
moveCursor LeftCursor  (r, c) = (r, c - 1)
moveCursor RightCursor (r, c) = (r, c + 1)
moveCursor _ csr = csr
