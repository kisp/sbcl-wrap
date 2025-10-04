{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Main (main)
where

import           Control.Monad
import           Data.List
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Posix.Process (executeFile)
import           System.Process

import           Crypto.Hash.MD5      (hash)

import           Data.ByteString      (pack, unpack)
import           Data.Char            (ord, toLower)
import           Data.Word            (Word8)

import           Text.Printf

-- EitherT
newtype EitherT a m b = EitherT { runEitherT :: m (Either a b) }

#if __GLASGOW_HASKELL__ >= 710
instance Monad m => Functor (EitherT a m) where
    fmap  = liftM

instance Monad m => Applicative (EitherT a m) where
    pure  = return
    (<*>) = ap  -- defined in Control.Monad
#endif

instance Monad m => Monad (EitherT a m) where
        return   = EitherT . return . return
        m >>= k  = EitherT $ do
                a <- runEitherT m
                case a of
                        Left  l -> return (Left l)
                        Right r -> runEitherT (k r)

liftEitherT :: Monad m => (x -> Either a b) -> x -> EitherT a m b
liftEitherT f = EitherT . return . f

-- utils
word8hex :: Word8 -> String
word8hex = printf "%02x"

md5 :: String -> String
md5 = concatMap word8hex . unpack . hash . pack . map (fromIntegral . ord)

escape :: Char -> String -> String
escape _ []                 = []
escape e (x:xs) | e == x    = '\\':x:(escape e xs)
                | otherwise = x:(escape e xs)

-- system names to hash
systemsHash :: [String] -> String
systemsHash names = md5 $ concat $ sort (map l names)
    where l = map toLower

-- sbcl
sbcl :: String
sbcl = "sbcl"

sbclScript :: String -> [String] -> String
sbclScript imagePath systems = intercalate "\n" lines
    where lines = [ "(setq *debugger-hook* (lambda (c h) (declare (ignore h)) (format *error-output* \"ERROR of type ~S:~%~A~%\" (type-of c) c) (sb-ext:exit :code 1)))"
                  , "(setq sb-ext:*invoke-debugger-hook* *debugger-hook*)" ] ++
                  map loadSystem systems ++
                  [ "(setq *evaluator-mode* :interpret)"
                  , "(ensure-directories-exist \"" ++ imagePath ++ "\")"
                  , "(sb-ext:save-lisp-and-die \"" ++ imagePath ++ "\")" ]
          loadSystem name =  "(asdf:load-system \"" ++ name ++ "\")"

-- IO
handleToDevNull :: IO Handle
handleToDevNull = openFile "/dev/null" WriteMode

makeImage :: String -> [String] -> IO (Either (String, Int) String)
makeImage imagePath systems = do
  devNull <- handleToDevNull
  (Just hIn, _, _, p) <- createProcess
                         (proc sbcl [])
                         { std_out = UseHandle devNull
                         , std_err = UseHandle devNull
                         , std_in  = CreatePipe
                         , close_fds = False }
  let script = sbclScript imagePath systems
  hPutStrLn hIn script
  hClose hIn
  code <- waitForProcess p
  hClose devNull
  case code of
    ExitSuccess -> return $ Right imagePath
    ExitFailure c -> return $ Left ("sbcl image builder for `" ++
                                    intercalate ", " systems ++
                                    "' returned " ++ show c,
                                    77)

ensureImage :: [String] -> IOErr String
ensureImage systems = EitherT $ do
  let hash = systemsHash systems
  cacheDirectory <- getCacheDirectory
  let imagePath = cacheDirectory ++ "/" ++ hash ++ ".core"
  imageExists <- doesFileExist imagePath
  if imageExists then
     return $ Right imagePath
  else
     makeImage imagePath systems

getCacheDirectory :: IO FilePath
getCacheDirectory =  liftM (++ "/.cache/sbcl-wrap") getHomeDirectory

ensureAtLeastOne :: [String] -> Either (String, Int) ()
ensureAtLeastOne (_:_) = Right ()
ensureAtLeastOne _     = Left ("no arguments given", 88)

ensureDoubleHyphen :: [String] -> Either (String, Int) ()
ensureDoubleHyphen s | "--" `elem` s = Right ()
                     | otherwise     = Left ("missing separator `--'", 88)

ensureSbclScript :: [String] -> Either (String, Int) ()
ensureSbclScript (_:_) = Right ()
ensureSbclScript _     = Left ("sbclScript argument missing", 88)

splitFirst :: [String] -> [String]
splitFirst (h:t) = words h ++ t

parseArgs :: [String] -> Either (String, Int) SystemsAndSbclCall
parseArgs args = do
  ensureAtLeastOne args
  let args' = splitFirst args
  ensureDoubleHyphen args'
  let (systemNames, "--":tail) =
          break (== "--") args'
  ensureSbclScript tail
  let (sbclScript:sbclArgs) = tail
  return (systemNames, sbclScript, sbclArgs)

parseArgsAndEnsureImage :: [String] -> IOErr ImagePathAndSbclCall
parseArgsAndEnsureImage args = do
  (systemNames, sbclScript, sbclArgs) <- liftEitherT parseArgs args
  imagePath <- ensureImage systemNames
  return (imagePath, sbclScript, sbclArgs)

putSbclWrapMessage :: String -> String -> IO ()
putSbclWrapMessage tag message =
  putStrLn $ "[sbcl-wrap] " ++ tag ++ ": " ++ message

main :: IO ()
main = do
  args <- getArgs
  res <- runEitherT $ parseArgsAndEnsureImage args
  case res of
    Left (message, code) -> putSbclWrapMessage "INFO" "version 0.0.10" >>
                            putSbclWrapMessage "INFO" ("called with args " ++ show args) >>
                            putSbclWrapMessage "ERROR" message >>
                            exitWith (ExitFailure code)
    Right imagePathAndSbclCall -> execSbcl imagePathAndSbclCall

execSbcl :: ImagePathAndSbclCall -> IO ()
execSbcl (imagePath, sbclScript, sbclArgs) =
    executeFile sbcl
              True
              ([ "--core", imagePath
               , "--dynamic-space-size", "2048"
               , "--noinform", "--disable-ldb", "--lose-on-corruption", "--end-runtime-options"
               , "--no-sysinit", "--no-userinit"
               , "--eval", "(sb-impl::process-script \"" ++ (escape '"' sbclScript) ++ "\")"
               , "--quit"
               , "--end-toplevel-options"
               ] ++ sbclArgs)
              Nothing

type IOErr a = EitherT (String, Int) IO a
type ImagePathAndSbclCall = (String, String, [String])
type SystemsAndSbclCall = ([String], String, [String])
