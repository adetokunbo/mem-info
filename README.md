# hs-core-mem

[![GitHub CI](https://github.com/adetokunbo/hs-core-mem/actions/workflows/ci.yml/badge.svg)](https://github.com/adetokunbo/hs-core-mem/actions)
[![Stackage Nightly](http://stackage.org/package/hs-core-mem/badge/nightly)](http://stackage.org/nightly/package/hs-core-mem)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/hs-core-mem/blob/master/LICENSE)

This is a terse statement that describes the `hs-core-mem` library

This is a slightly longer statement that expands on the previous one.

[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/hs-core-mem.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=hs-core-mem>
[hackage-badge]:      <https://img.shields.io/hackage/v/hs-core-mem.svg>
[hackage]:            <https://hackage.haskell.org/package/hs-core-mem>

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import System.Environment
  ( lookupEnv,
    setEnv,
    unsetEnv
   )
import Text.Read   (readEither)
import Test.Hspec
import Top.Sample.Mod

spec :: Spec
spec = describe "Checking the functions in Top.Sample.Mod" $ before_ clearIt $ do
  context "endsThen" $ do
    it "should implement the behaviour of the other functions easily" $ do
      setIt
      getIt `endsThen` (== (Just "1"))
      clearIt
      getIt `endsThen` (== Nothing)
      getAsInt `endsThen` (== (Left "not set!"))
      setIt
      getAsInt `endsThen` (== (Right 1))

getIt :: IO (Maybe String)
getIt = lookupEnv envName

getAsInt :: IO (Either String Int)
getAsInt = maybe (Left "not set!") readEither <$> getIt

setIt :: IO ()
setIt = setEnv envName "1"

setNotInt :: IO ()
setNotInt = setEnv envName "foo"

clearIt :: IO ()
clearIt = unsetEnv envName

envName :: String
envName = "AN_ENV_VAR"

main :: IO ()
main = hspec spec

```
