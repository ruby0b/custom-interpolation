<h1 align="center">custom-interpolation</h1>

<p align="center">
  <a href="https://hackage.haskell.org/package/custom-interpolation"><img src="https://img.shields.io/hackage/v/custom-interpolation" alt="Hackage"></a>
  <a href="https://github.com/ruby0b/custom-interpolation/actions/workflows/haskell-ci.yml"><img src="https://github.com/ruby0b/custom-interpolation/actions/workflows/haskell-ci.yml/badge.svg" alt="Build Status"></a>
  <a href="https://github.com/simmsb/calamity/blob/master/LICENSE"><img src="https://img.shields.io/github/license/ruby0b/custom-interpolation" alt="License"></a>
  <a href="https://hackage.haskell.org/package/custom-interpolation"><img src="https://img.shields.io/hackage-deps/v/custom-interpolation" alt="Hackage-Deps"></a>
</p>

This library provides tools for easily generating string interpolation quasiquoters.
The interpolation behavior is customizable and multiple different interpolation methods may be used in a single quasiquoter.

## Usage Examples

### Example 1: Multiple interpolators

Let's define a basic string interpolation quasiquoter that

- interpolates any Haskell expressions using `{}` and
- shows the first 10 elements of a list using `@{}`:

```hs
i = interpolateQQ simpleConfig
  { handlers = [ simpleInterpolator {prefix = ""}
               , (applyInterpolator [|show . take 10|]) {prefix = "@"} ] }
```

```hs
>>> [i|2^10 = {show (2 ^ 10)}. Some Fibonacci numbers: @{let fibs = 1 : 1 : zipWith (+) fibs (tail fibs) in fibs}.|]
"2^10 = 1024. Some Fibonacci numbers: [1,1,2,3,5,8,13,21,34,55]."
```

### Example 2: SQL substitution

Now for a more complicated example; defining an SQL query quasiquoter that prevents SQL injection.

We can achieve this by replacing expressions between `{}` with `?` and accumulating the actual expression in the first output of the [`Interpolator`](https://hackage.haskell.org/package/custom-interpolation/docs/CustomInterpolation.html#t:Interpolator) handler.
This allows us to then apply some SQL library function to the string and the accumulated expressions which takes care of the actual substitution.

```hs
import Language.Haskell.TH (appE, listE, Exp, Q)

-- Need an existential type to wrap the differently typed interpolated expressions
data SQLData = forall a. Show a => SQLData a
instance Show SQLData where show (SQLData x) = show x

-- Dummy function that would normally run the query
runSQL sql ds = (sql, ds)

-- The quasiquoter itself
consumeInterpolated :: ([Q Exp], Q Exp) -> Q Exp
consumeInterpolated (exprs, strExpr) = appE (appE [|runSQL|] strExpr) (listE (map (appE [|SQLData|]) exprs))

sql = interpolateQQ defaultConfig
    { finalize = consumeInterpolated,
      handlers = [simpleInterpolator { handler = (\q -> (q, [|"?"|])) }]
    }
```

```hs
>>> [sql|SELECT * FROM user WHERE id = {(11 ^ 5)} AND lastName = {"Smith"}|]
("SELECT * FROM user WHERE id = ? AND lastName = ?",[161051,"Smith"])
```

## Acknowledgements

The [`CustomInterpolation.Parser`](https://github.com/ruby0b/custom-interpolation/blob/main/src/CustomInterpolation/Parser.hs) module was derived from the [`here` package](https://github.com/tmhedberg/here).
