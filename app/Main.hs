{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-name-shadowing -Wno-unused-imports -Wno-compat-unqualified-imports -Wno-dodgy-imports -Wno-orphans -Wno-unused-local-binds  #-}
module Main (main) where
import SocialNetworkSimulation (simulate)


-- | Main
main :: IO ()
main = do
    simulate
   