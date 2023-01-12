{- |
This module reexports all the relevant tools of the @custom-interpolation@ package.
-}
module CustomInterpolation (
  -- * Quickstart
  -- $quickstart

  -- * Template Haskell
  --
  -- | The main entry points of the library. Use these to build your template haskell functions and quasiquoters.
  interpolateQQ,
  interpolate,

  -- * Configuration
  InterpolationConfig (..),
  simpleConfig,
  defaultConfig,

  -- * Interpolators
  Interpolator (..),
  Prefix,

  -- ** Interpolator Helpers
  --
  -- | Constructors for common 'Interpolator's.
  simpleInterpolator,
  applyInterpolator,

  -- ** Brackets
  Brackets (..),
  curlyBrackets,
  roundBrackets,
  squareBrackets,
  angleBrackets,
) where

import CustomInterpolation.Config
import CustomInterpolation.TH
