module CustomInterpolation.TH where

import CustomInterpolation.Config (InterpolationConfig (..), Interpolator (..))
import CustomInterpolation.Parser (StringPart (..), parseInterpolations)
import Language.Haskell.TH (Exp, Q, appE, listE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Text.Parsec (ParseError)

-- | Create a new 'QuasiQuoter' that interpolates strings as specified by the given 'InterpolationConfig'.
interpolateQQ :: InterpolationConfig a -> QuasiQuoter
interpolateQQ interpolation =
  QuasiQuoter
    { quoteExp = interpolate interpolation,
      quotePat = error "not used",
      quoteType = error "not used",
      quoteDec = error "not used"
    }

-- | Interpolate a string as specified by the given 'InterpolationConfig'.
interpolate :: InterpolationConfig a -> String -> Q Exp
interpolate defaultConfig@InterpolationConfig {handlers, finalize} str = do
  res <- either (parsingError str) (pure . concatParts defaultConfig) (parseInterpolations handlers str)
  finalize res

{- | Concatenate the literals and interpolated parts of a list of 'StringPart's.
The interpolations may also each return some value which gets accumulated as a list in the first output.
-}
concatParts :: InterpolationConfig a -> [StringPart a] -> ([a], Q Exp)
concatParts InterpolationConfig {escape} ps = (otherData, concatenatedE)
  where
    concatenatedE = appE [|concat|] $ listE stringListE
    (otherData, stringListE) = foldr step ([], []) ps
    step subExpr (ds, qs) = case subExpr of
      Lit str -> (ds, [|str|] : qs)
      Esc c -> let c' = escape c in (ds, [|[c']|] : qs)
      Anti Interpolator {handler} expr -> (\(d, q) -> (d : ds, q : qs)) $ handler expr

parsingError :: String -> ParseError -> Q a
parsingError expStr parseError =
  error $
    "Failed to parse interpolated expression in string: "
      <> expStr
      <> "\n"
      <> show parseError
