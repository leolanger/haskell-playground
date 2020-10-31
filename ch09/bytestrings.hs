import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import System.Environment

-- ghci> B.pack [99,97,110]
-- Chunk "can" Empty
-- ghci> B.pack [98..120]
-- Chunk "bcdefghijklmnopqrstuvwx" Empty

-- ghci> B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]
-- Chunk "()*" (Chunk "+,-" (Chunk "./0" Empty))

-- ghci> B.cons 85 $ B.pack [80,81,82,84]
-- Chunk "U" (Chunk "PQRT" Empty)
-- ghci> B.cons' 85 $ B.pack [80,81,82,84]
-- Chunk "UPQRT" Empty
-- ghci> foldr B.cons B.empty [50..60]
-- Chunk "2" (Chunk "3" (Chunk "4" (Chunk "5" (Chunk "6" (Chunk "7" (Chunk "8" (Chunk "9" (Chunk ":" (Chunk ";" (Chunk "<"
-- Empty))))))))))
-- ghci> foldr B.cons' B.empty [50..60]
-- Chunk "23456789:;<" Empty

main = do
  (fileName1 : fileName2 : _) <- getArgs
  copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
  contents <- B.readFile source
  B.writeFile dest contents