# myimport

Format the imports in the Haskell file as a table style

## Features
1. Haskell style imports(Table style)
2. Import Group by package
3. Append package comment of every module imported

## Run
1. Specify file path

```bash
myimport file/path
```

2. Through pipe 
```bash
cat file/path | myimport 
```

## A Haskell file
```Haskell
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

...Some code
```

Will output when run a command `myimport app/Main.hs`
```Haskell
module Main where

import           Data.Text                  (Text)               -- text-2.0.2
import qualified Data.Text                  as T                 -- text-2.0.2
import qualified Data.Text.IO               as T                 -- text-2.0.2
import           Data.Functor                                    -- base-4.18.2.1
import           Data.Char                                       -- base-4.18.2.1
import           System.Environment         (getArgs)            -- base-4.18.2.1
import           Control.Applicative        (some, many, (<|>))  -- base-4.18.2.1
import           Data.List                  (sortOn, nub)        -- base-4.18.2.1
import           Data.Either                (fromRight)          -- base-4.18.2.1
import           Debug.Trace                (traceShow)          -- base-4.18.2.1
import           Control.Monad              (void)               -- base-4.18.2.1
import           Control.Exception          (try, SomeException) -- base-4.18.2.1
import           System.Process             (readProcess)        -- process-1.6.19.0
import           Data.Attoparsec.Text       as AT hiding (try)   -- attoparsec-0.14.4
import           Data.Attoparsec.Combinator hiding (try)         -- attoparsec-0.14.4
import           Data.String.Conversions    (cs)                 -- string-conversions-0.4.0.1

data Bracket = Name Text | Bracket [Bracket] deriving Show
-- data M = M1 String [Bracket] | M2 String Text | M3 String Text deriving Show
data M = M1 String [Bracket] |  M3 String Text deriving Show

newline = endOfLine

...Some code

```
