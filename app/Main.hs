module Main where

import Data.Text(Text) -- text
import Data.Functor                   
import Data.Char                     
-- Example
import System.Environment         (getArgs)
import Control.Applicative( some
                          , many
                          , (<|>)
                          ) 
import Data.List(sortOn, nub)    
import Data.Either(fromRight)        
import qualified Data.Text as T  
import qualified Data.Text.IO as T 
import Debug.Trace(traceShow)       
import System.Process             (readProcess)   
import Control.Monad(void)
import Data.Attoparsec.Text       as AT hiding(try)
import Data.Attoparsec.Combinator  hiding (try) 
import Data.String.Conversions    (cs)         
import Control.Exception(try,SomeException)

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
            -- traceShow xs $ pure ()
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
            xs <- manyTill anyChar $ char ')'
            pure $ cs $ "(" <> xs <> ")"
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
    prefix <- manyTill (AT.take 1) $ lookAhead importParser
    im <- many importParser
    subfix <- AT.takeText
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
    [path] <- getArgs
    txt <- T.readFile path
    let (Right (pre,b,sub)) = parseOnly myparser txt
    let maxModuleLengh = maximum $ map moduleLength b 
    let maxI = maximum $ map maxIL b
    let b' = map findPackage b
    xs <- sequence b'
    let packages = sortOn T.length $  nub $ map fst xs
    let mid = T.intercalate "\n" $ map (transPackage maxI maxModuleLengh xs) packages
    T.putStr $ pre <> mid <> "\n\n" <> sub
    where moduleLength (_,_,M1 m _) = length m
        --   moduleLength (_,_,M2 m _) = length m
          moduleLength (_,_,M3 m _) = length m
          
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
