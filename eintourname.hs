module Main where


import qualified Data.ByteString.Lazy as Bs
import Data.List
import Data.Word (Word8)
import Data.Bits
import Data.Ord (comparing)
import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Monad.Error 

data RleCode =   EqRle  {len::Int, byte::Word8} 
               | IncRle {len::Int, start::Word8} 
               | Raw    {bytes::[Word8]}
                  deriving (Show)


----------------------------------------Decode----------------------------------
--decodes one RleCode to corresponding byte array
decode :: RleCode -> [Word8]
decode (EqRle l b) = replicate l b 
decode (IncRle l s) = take l [s..]
decode (Raw xs) = xs

--reads rleCodes from input bytestream
readRleCode :: [Word8] -> [RleCode]
readRleCode [] = []
readRleCode (count:datas@(dataByte:ds))
  | count == 0xFF         = [] --FF is endOfStream
  | count == 0x7F         = error "0x7F:New PPU address command is not implemented"
  | not (testBit count 7) = EqRle (fromIntegral count) dataByte : readRleCode ds
  | testBit count 6       = IncRle count' dataByte : readRleCode ds
  | otherwise             = Raw  (take count' datas) : readRleCode (drop count' datas)
    where count' = fromIntegral $ count .&. 0x3F--clear two hi bits

----------------------------------------Encode----------------------------------
-- takes only ascending (+1) init part of list (e.g. [5,6,7,2,1]->[5,6,7])
takeAsc :: (Eq a, Num a) => [a] -> [a]
takeAsc [] = []
takeAsc xss@(x:xs) = (x:) $ map fst $ takeWhile (uncurry (==)) $ zip xs $ map (+1) xss

encodeEqRle:: [Word8] -> RleCode
encodeEqRle xs = EqRle (length eqGroup) (head eqGroup)
  where eqGroup = head $ group xs

encodeIncRle :: [Word8] -> RleCode
encodeIncRle xs = IncRle (length asc) (head asc)
                  where asc = takeAsc xs


mergeRaws :: [RleCode] -> [RleCode]
mergeRaws [] = []
mergeRaws (Raw x: Raw y :xs) = mergeRaws $ Raw (x++y): xs
mergeRaws (x:xs) = x:mergeRaws xs

--encode inits of given list to the best RleCode comparing length
encode :: [Word8] -> [RleCode]--first we get single RleCodes and then merge all raw values 
encode = mergeRaws.encode'
  where
  encode' [] = []
  encode' xs@(x:xss) 
    | len maxCode <= 2 = Raw [x] : encode' xss --optimization: don't break raw chains with 2-bytes Rle
    | otherwise        = maxCode : encode' (drop (len maxCode) xs)
      where maxCode = maximumBy (comparing len) [encodeIncRle xs, encodeEqRle xs]


--serialize list of RleCodes to game's format
writeRleCode:: [RleCode] -> [Word8]
writeRleCode [] = [0xFF] --End of stream sign
writeRleCode (EqRle l b:xs)
  | l > 0x7E  = 0x7E: b : writeRleCode (EqRle (l-0x7E) b: xs) --0x7E is max EqRle length
  | otherwise = fromIntegral l: b: writeRleCode xs
writeRleCode (IncRle l s:xs)
  | l > 0x3F = 0xFF: s : writeRleCode (IncRle (l-0x3F) (s+0x3F): xs)
  | otherwise = (fromIntegral l .|. 0xC0) : s : writeRleCode xs --two high bits are set at incremental Rle
writeRleCode (Raw xs: xss)
  | length xs > 0x3F = 0xBF: xs ++ writeRleCode (Raw (drop 0x3F xs) : xss)
  | otherwise        = (fromIntegral (length xs) .|. 0x80): xs ++ writeRleCode xss -- high bit is set for raw

----------------------------------------Command line parse part----------------------------------
data Action = Decode | Encode | NoAction deriving (Show, Eq)
data Options = Options
              {optHelp :: Bool
              ,optVersion :: Bool
              ,optAction :: Action
              }
              deriving (Show)
defaultOptions :: Options
defaultOptions = Options
                  {optHelp = False
                  ,optVersion = False
                  ,optAction = NoAction
                  }

usage :: String
usage = usageInfo "Usage: eintourname [-d | -e] file_name [offset]" options

options :: [OptDescr (Options -> Options)]
options =
	[ Option "d"     ["decode"]  (NoArg (\opts -> opts {optAction = Decode}))  "decode from ROM. -d <file_name offset>"
	, Option "e"     ["encode"]  (NoArg (\opts -> opts {optAction = Encode}))  "encode from raw binary. -e <file_name>"
        , Option "h?"    ["help"]    (NoArg (\ opts -> opts { optHelp = True }))   "show help."
        , Option "v"     ["version"] (NoArg (\ opts -> opts { optVersion = True })) "show version number."
	]


toolOpts :: [String] -> IO (Options, [String])
toolOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usage))

----------------------------------------------Main------------------------------------------------------

main :: IO()
main = do

  argv <- getArgs
  (opts, nonOpts) <- toolOpts argv
  when (optVersion opts) $ do
    putStrLn "Eintourname. NES Teenage Mutant Ninja Turtles - Tournament Fighters RLE utility. Version 0.1"
    exitSuccess
  when (optHelp opts) $ do
    putStrLn usage
    exitSuccess
  let action = optAction opts
  when (action == NoAction) $ do
    putStrLn "Supply action flag"
    putStrLn usage
    exitFailure 
  if action == Decode
    then do
      when (length nonOpts /= 2) $ do
        putStrLn "Supply exactly one file name and one offset for decoding"
        putStrLn usage
        exitFailure
      let [fileName, sOffset] = nonOpts
      input <- Bs.readFile fileName
      let inputU8 =  drop (read sOffset) $ Bs.unpack input
          decoded = concatMap decode . readRleCode $ inputU8
      Bs.writeFile "decoded.bin" (Bs.pack decoded)
    else do --encoding
      when (length nonOpts /= 1) $ do
        putStrLn "Supply exactly one file name for encoding"
        putStrLn usage
        exitFailure
      let [fileName] = nonOpts
      input <- Bs.readFile fileName
      let inputU8 = Bs.unpack input
          encoded = writeRleCode . encode $ inputU8
      Bs.writeFile "encoded.bin" (Bs.pack encoded)
