import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    r <- fails $ setup' "configure" []
    assertOutputContains "PartialSig" r
    assertOutputContains "missing declarations" r
    assertOutputContains "fun :: A -> A" r
    assertOutputDoesNotContain "data A" r
    return ()
