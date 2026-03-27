import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    withPackageDb $ do
      -- Install framework WITHOUT instantiation (simulates nix callCabal2nix)
      recordMode DoNotRecord $
        withDirectory "repo/framework-0.1.0.0" $ setup_install []
      -- Install the App implementation package separately
      recordMode DoNotRecord $
        withDirectory "repo/app-impl-0.1.0.0" $ setup_install []
      -- Configure consumer — should fail because the instantiated
      -- framework (with App=app-impl:App) was never built
      withDirectory "repo/consumer-0.1.0.0" $ do
        r <- recordMode DoNotRecord . fails $ setup' "configure" []
        -- The error should explain the full picture:
        --
        -- 1. Which package is broken
        assertOutputContains "consumer" r
        -- 2. Which signature needs filling
        assertOutputContains "unfilled" r
        assertOutputContains "App" r
        -- 3. That the package is installed as indefinite (signatures
        --    not filled) — the user installed framework separately,
        --    so only the indefinite version exists in the package db.
        assertOutputContains "indefinite" r
        -- 4. Actionable guidance: rebuild in the same cabal project
        --    so cabal can fill the signatures.
        assertOutputContains "rebuild" r
        return ()
