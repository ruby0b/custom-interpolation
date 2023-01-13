module CustomInterpolation.Config where

import Data.Default.Class (Default (..))
import Language.Haskell.TH (Exp, Q, appE)

{- $setup
=== __ __
>>> import CustomInterpolation -- doctest setup, ignore this
-}

-- | Rules for interpolating a string.
data InterpolationConfig a = InterpolationConfig
  { -- | The 'Interpolator's that handle interpolated expressions.
    handlers :: [Interpolator a],
    -- | Used for complex 'Interpolator's that return additional values. Reduces these accumulated values to a single @'Q' 'Exp'@.
    finalize :: ([a], Q Exp) -> Q Exp,
    -- | Handle backslash-escaped characters (can be used to add escape sequences like @\\n@).
    escape :: Char -> Char
  }

{- | Type-restricted simple version of 'defaultConfig'.
Use this if you just want to substitute interpolated segments with a string expression.
-}
simpleConfig :: InterpolationConfig ()
simpleConfig = defaultConfig

{- | Default 'InterpolationConfig'.
Has no 'handlers', 'finalize' ignores any extra values returned when interpolating and 'escape' does nothing.
-}
defaultConfig :: InterpolationConfig a
defaultConfig = InterpolationConfig {handlers = [], finalize = snd, escape = id}

-- | @'def' = 'defaultConfig'@
instance Default (InterpolationConfig a) where
  def = defaultConfig

data Interpolator a = Interpolator
  { -- | InterpolationConfig prefix, a prefix of e.g. @"$"@ will lead to anything inside @${expr}@ being interpolated (assuming 'curlyBrackets').
    prefix :: Prefix,
    -- | Transforms the interpolated string segment into a string expression and some value of type @a@ to accumulate.
    handler :: Q Exp -> (a, Q Exp),
    -- | The brackets to use for the interpolation syntax.
    brackets :: Brackets
  }

type Prefix = String

data Brackets = Brackets {opening :: Char, closing :: Char} deriving (Show)

-- | @{}@
curlyBrackets :: Brackets
curlyBrackets = Brackets '{' '}'

-- | @()@
roundBrackets :: Brackets
roundBrackets = Brackets '(' ')'

-- | @[]@
squareBrackets :: Brackets
squareBrackets = Brackets '[' ']'

-- | @<>@
angleBrackets :: Brackets
angleBrackets = Brackets '<' '>'

{- | Default 'Interpolator'.
Inserts the interpolated expression as is and uses 'curlyBrackets' with no 'prefix'.
-}
simpleInterpolator :: Interpolator ()
simpleInterpolator = Interpolator {prefix = "", handler = pure, brackets = curlyBrackets}

{- | Create an 'Interpolator' that applies a quoted function to the interpolated expression. Uses 'curlyBrackets' and no 'prefix'.

==== __Example__
>>> $(interpolate (simpleConfig {handlers = [applyInterpolator [|show . (^ 2)|]]}) "two squared equals {2}")
"two squared equals 4"
-}
applyInterpolator :: Monoid a => Q Exp -> Interpolator a
applyInterpolator funExp = simpleInterpolator {handler = pure . appE funExp}
