module Main where

import           Data.Text                  (Text)                                            -- text-2.0.2
import qualified Data.Text                  as T                                              -- text-2.0.2
import qualified Data.Text.IO               as T                                              -- text-2.0.2
import           Data.Functor                                                                 -- base-4.18.2.1
import           Data.Char                                                                    -- base-4.18.2.1
import           System.Environment         (getArgs)                                         -- base-4.18.2.1
import           Control.Applicative        (some, many, (<|>))                               -- base-4.18.2.1
import           Data.List                  (sortOn, nub)                                     -- base-4.18.2.1
import           Data.Either                (fromRight)                                       -- base-4.18.2.1
import           Control.Monad              (void)                                            -- base-4.18.2.1
import           Control.Exception          (try, SomeException)                              -- base-4.18.2.1
import           System.IO                  ( hSetBuffering
                                            , stdin
                                            , BufferMode(LineBuffering)
                                            )                                                 -- base-4.18.2.1
import           System.Process             (readProcess)                                     -- process-1.6.19.0
import           Data.Attoparsec.Text       as AT hiding (try)                                -- attoparsec-0.14.4
import           Data.Attoparsec.Combinator hiding (try)                                      -- attoparsec-0.14.4
import           Data.String.Conversions    (cs)                                              -- string-conversions-0.4.0.1
import Control.Concurrent.Async(mapConcurrently)
import Debug.Trace(traceShow)

data Bracket = Name Text | Bracket [Bracket] deriving Show
-- data M = M1 String [Bracket] | M2 String Text | M3 String Text deriving Show
data M = M1 String [Bracket] |  M3 String Text deriving Show

newline = endOfLine


commonParser = do
    void $ string "--"
    skipSpace
    r <- AT.takeWhile $ notInClass "\n"
    skipSpace
    pure r
importParser = do
    im <- string "import"
    space *> skipSpace
    qualified <- option "         " $ string "qualified"  <* space  
    skipSpace
    -- m <- module1 <|> module2 <|> module3 
    m <- module1 <|> module3 
    void $ many commonParser
    pure (im,qualified,m)
    where bracketsParser :: Parser [Bracket]
          bracketsParser = do 
            skipSpace
            void $ char '('
            xs <- sepBy (Bracket <$> bracketsParser <|> Name <$>  nameP)  $ skipSpace >> char ',' >> skipSpace
            skipSpace
            void $ char ')'
            skipSpace
            pure xs
          nameP = do
            skipSpace
            txt <- AT.takeWhile (notInClass ",() \t\n")
            void $ many $ satisfy isHorizontalSpace
            sub <- option "" cusP
            pure $ txt <> sub
          cusP = do
            void $ char '('
            xs <- AT.takeWhile $ notInClass ")" 
            void $ AT.take 1
            -- manyTill anyChar $ char ')'
            pure $ "(" <> xs <> ")"
          asParser = do
            a <- string "as" <|> string "hiding"
            void $ many $ satisfy isHorizontalSpace
            -- token <- lookAhead $ AT.take 10
            -- traceShow token $ pure ()
            i <- AT.takeWhile $ not . isSpace
            pure $ a <> " " <> i
          module1 :: Parser M
          module1 = do 
            m <- manyTill (satisfy $ not . isSpace) $ lookAhead bracketsParser
            b <- bracketsParser
            skipSpace
            pure $ M1 m b
        --   module2 :: Parser M
        --   module2 = do
        --     m <- manyTill (satisfy $ not . isSpace) $ lookAhead asParser
        --     a <- asParser
        --     skipSpace
        --     b <- option "" $ (" "<>) <$> asParser
        --     skipSpace
        --     pure $ M2 m (a <> b)
          module3 :: Parser M
          module3 = do
            m <- some (satisfy $ not . isSpace) 
            skipSpace
            subfix <- option "" asParser
            skipSpace
            b <- option "" $ (" "<>) <$> asParser
            skipSpace
            pure $ M3 m (subfix <> b)


myparser = do
    prefix <- manyTill ((<>) <$> (AT.takeWhile $ notInClass "\n\r") <*> (option "" $ AT.take 1)) $ void (lookAhead importParser) <|> endOfInput
    im <- many importParser
    subfix <- option "" AT.takeText
    pure (T.concat prefix,im,subfix)

paddingEnd :: Int -> String -> Text
paddingEnd  n txt = cs r where
    countSpace = n - length txt 
    r = txt <> replicate countSpace ' '

paddingTextEnd :: Int -> Text -> Text
paddingTextEnd  n txt = r where
    countSpace = n - T.length txt 
    r = txt <> T.replicate countSpace " "

findModule :: String -> IO Text
findModule moduleTxt = do
    iout <- try $ readProcess "stack" ["exec","--","ghc-pkg", "find-module", moduleTxt] ""
    case iout of 
        Left (_::SomeException) -> pure "stack is required"
        Right out -> do
            let eitherR = parseOnly (many moduleP) $ cs out
            case eitherR of
                Left _ -> do
                    pure "error"
                Right a -> do 
                    pure $ T.concat a
    where moduleP = do
            void $ manyTill anyChar $ lookAhead packageP
            packageP
          packageP = do
            void $ count 5 space 
            r <- string "(no packages)" $> "" <|> (AT.takeWhile $ not . isSpace) 
            skipSpace
            pure r

main = do
    hSetBuffering stdin LineBuffering
    txt <- getTxt
    let (Right (pre,b,sub)) = parseOnly myparser txt
    let maxModuleLengh = maximum $ map moduleLength b 
    let maxI = maximum $ map maxIL b
    xs <- mapConcurrently findPackage b
    -- xs <- sequence b'
    let packages = sortOn T.length $  nub $ map fst xs
    let mid = T.intercalate "\n" $ map (transPackage maxI maxModuleLengh xs) packages
    T.putStr $ pre <> mid <> "\n\n" <> sub
    where moduleLength (_,_,M1 m _) = length m
        --   moduleLength (_,_,M2 m _) = length m
          moduleLength (_,_,M3 m _) = length m

          getTxt = do
            args <- getArgs
            if null args then T.getContents
            else T.readFile $ head args
          
          maxIL (_,_,M1 _ brackets) = T.length $ bracketToText brackets
        --   maxIL (_,_,M2 _ as') = T.length as'
          maxIL (_,_,M3 _ as') = T.length as'

          transPackage :: Int -> Int -> [(Text,(Text, Text, M))] -> Text -> Text
          transPackage il n xs package = T.intercalate "\n" $ map (<> " -- " <> package) r where
            r = map (toText il n) imports
            imports = map snd $ filter ((== package) . fst) xs

            -- appendFirst :: Text -> [Text] -> [Text]
            -- appendFirst _ [] =  []
            -- appendFirst token (x:xs) =  x <> token : xs

          toText il maxM (import',qualified',M1 m brackets) = import' <> " " <> qualified' <> " " <> paddingEnd maxM m <> " " <> paddingTextEnd il (bracketToText brackets)
        --   toText il maxM (import',qualified',M2 m as') = import' <> " " <> qualified' <> " " <> paddingEnd maxM m <> " " <> paddingTextEnd il as'
          toText il maxM (import',qualified',M3 m as') = import' <> " " <> qualified' <> " " <> paddingEnd maxM m <> " " <> paddingTextEnd il as'
          bracketToText brackets = "(" <> r <> ")" where
            r = T.intercalate ", " $ map go brackets
            go (Name txt) = txt
            go (Bracket xs) = bracketToText xs
        
          findPackage a@(_,_,M1 m _) = do
            p <- findModule m
            pure (p,a)
        --   findPackage a@(_,_,M2 m _) = do
        --     p <- findModule m
        --     pure (p,a)
          findPackage a@(_,_,M3 m _) = do
            p <- findModule m
            pure (p,a)