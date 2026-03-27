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
        assertOutputContains "App" r
        -- 3. That the indefinite package exists but the required
        --    instantiation was not built — the user installed
        --    framework separately, so only the indefinite version
        --    (App=<App>) is in the package db.  The instantiated
        --    variant (App=app-impl:App) doesn't exist.
        assertOutputContains "instantiat" r
        -- 4. Actionable guidance: rebuild framework together with
        --    the consumer so cabal can create the instantiation.
        assertOutputContains "rebuild" r
        return ()
