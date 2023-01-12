{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module QQ where

import CustomInterpolation
import Data.Char
import Language.Haskell.TH (appE, listE, Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

itest :: QuasiQuoter
itest =
  interpolateQQ
    simpleConfig
      { handlers =
          [ simpleInterpolator {prefix = "$"},
            (applyInterpolator [|show . (+ 1)|]) {prefix = "++"}
          ]
      }

iManyBrackets :: QuasiQuoter
iManyBrackets =
  interpolateQQ
    simpleConfig
      { handlers =
          [ (applyInterpolator [|show|]) {prefix = "$", brackets = roundBrackets},
            simpleInterpolator {prefix = "$", brackets = angleBrackets},
            (applyInterpolator [|show . take 4|]) {brackets = squareBrackets},
            (applyInterpolator [|map toUpper|]) {brackets = curlyBrackets}
          ]
      }

-- readme stuff
i =
  interpolateQQ
    simpleConfig
      { handlers =
          [ simpleInterpolator {prefix = ""},
            (applyInterpolator [|show . take 10|]) {prefix = "@"}
          ]
      }

-- Need an existential type to wrap the differently typed interpolated expression
data SQLData = forall a. Show a => SQLData a

instance Show SQLData where show (SQLData x) = show x

-- Dummy function that would normally run the query
runSQL sql' ds = (sql', ds)

-- The quasiquoter itself
consumeInterpolated :: ([Q Exp], Q Exp) -> Q Exp
consumeInterpolated (exprs, strExpr) = appE (appE [|runSQL|] strExpr) (listE (map (appE [|SQLData|]) exprs))

sql =
  interpolateQQ
    defaultConfig
      { finalize = consumeInterpolated,
        handlers = [simpleInterpolator {handler = (\q -> (q, [|"?"|]))}]
      }
