module CustomInterpolation.Parser where

import Control.Monad.State (evalStateT, get, lift, modify, unless)
import CustomInterpolation.Config (Brackets (..), Interpolator (..))
import Data.Char (isDigit, isLetter)
import Data.Foldable (asum)
import Data.Functor (($>))
import Language.Haskell.Meta (parseExp)
import Language.Haskell.TH (Exp, Q)
import Text.Parsec (
  ParseError,
  State (statePos),
  anyChar,
  between,
  char,
  eof,
  getInput,
  incSourceColumn,
  lookAhead,
  manyTill,
  noneOf,
  parse,
  setInput,
  string,
  try,
  updateParserState,
  (<|>),
 )
import Text.Parsec.String (Parser)

-- | The raw segments the parser will cut the quasi-quoted string into
data StringPart a = Lit String | Esc Char | Anti (Interpolator a) (Q Exp)

data HsChompState = HsChompState
  { quoteState :: QuoteState,
    braceCt :: Int,
    consumed :: String,
    prevCharWasIdentChar :: Bool
  }
  deriving (Show)

data QuoteState = None | Single EscapeState | Double EscapeState deriving (Eq, Ord, Show)

data EscapeState = Escaped | Unescaped deriving (Bounded, Enum, Eq, Ord, Show)

parseInterpolations :: [Interpolator a] -> String -> Either ParseError [StringPart a]
parseInterpolations interps = parse (pInterp interps) ""

pInterp :: [Interpolator a] -> Parser [StringPart a]
pInterp interps = manyTill (pStringPart interps) eof

pStringPart :: [Interpolator a] -> Parser (StringPart a)
pStringPart interps = asum (map pAnti interps) <|> pEsc <|> pLit interps

pAnti :: Interpolator a -> Parser (StringPart a)
pAnti interp =
  Anti interp . parseExpQ
    <$> between
      (try (pAntiOpen interp))
      (pAntiClose interp)
      (pUntilUnbalancedCloseBracket interp)

-- | 'parseExp' but in the 'Q' Monad ('fail's on parsing errors).
parseExpQ :: String -> Q Exp
parseExpQ = either (fail . ("Error while parsing Haskell expression:\n" <>)) pure . parseExp

pAntiOpen :: Interpolator a -> Parser String
pAntiOpen Interpolator {prefix, brackets} = string (prefix ++ [opening brackets])

pAntiClose :: Interpolator a -> Parser String
pAntiClose Interpolator {brackets} = string [closing brackets]

pUntilUnbalancedCloseBracket :: Interpolator a -> Parser String
pUntilUnbalancedCloseBracket Interpolator {brackets = Brackets {opening, closing}} =
  evalStateT go $ HsChompState None 0 "" False
  where
    go = do
      c <- lift anyChar
      modify $ \st@HsChompState {consumed} -> st {consumed = c : consumed}
      HsChompState {..} <- get
      let next = setIdentifierCharState c *> go
      case quoteState of
        None ->
          if
              | c == opening -> incBraceCt 1 *> next
              | c == closing ->
                if braceCt > 0
                  then incBraceCt (-1) *> next
                  else stepBack $> reverse (tail consumed)
              | c == '\'' ->
                unless prevCharWasIdentChar (setQuoteState $ Single Unescaped)
                  *> next
              | c == '"' -> setQuoteState (Double Unescaped) *> next
              | otherwise -> next
        Single Unescaped -> do
          case c of
            '\\' -> setQuoteState (Single Escaped)
            '\'' -> setQuoteState None
            _ -> pure ()
          next
        Single Escaped -> setQuoteState (Single Unescaped) *> next
        Double Unescaped -> do
          case c of
            '\\' -> setQuoteState (Double Escaped)
            '"' -> setQuoteState None
            _ -> pure ()
          next
        Double Escaped -> setQuoteState (Double Unescaped) *> next
    stepBack =
      lift $
        updateParserState
          (\s -> s {statePos = incSourceColumn (statePos s) (-1)})
          *> getInput
          >>= setInput . (closing :)
    incBraceCt n = modify $ \st@HsChompState {braceCt} ->
      st {braceCt = braceCt + n}
    setQuoteState qs = modify $ \st -> st {quoteState = qs}
    setIdentifierCharState c = modify $ \st ->
      st
        { prevCharWasIdentChar = or [isLetter c, isDigit c, c == '_', c == '\'']
        }

pEsc :: Parser (StringPart a)
pEsc = Esc <$> (char '\\' *> anyChar)

pLit :: [Interpolator a] -> Parser (StringPart a)
pLit prefixes =
  Lit
    <$> ( try (litCharTill $ try $ asum (map (lookAhead . pAntiOpen) prefixes) <|> lookAhead (string "\\"))
            <|> litCharTill eof
        )
  where
    litCharTill = manyTill $ noneOf ['\\']
