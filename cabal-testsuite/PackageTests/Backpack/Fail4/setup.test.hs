import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    r <- fails $ setup' "configure" []
    -- Only the unfilled signature should appear in the error
    assertOutputContains "UnfilledSig" r
    assertOutputContains "signature declarations" r
    assertOutputContains "data Database" r
    -- The filled signature should NOT appear as a requirement
    assertOutputDoesNotContain "data Connection" r
    assertOutputDoesNotContain "connectionName" r
    return ()
