module Spec where

import Main
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property (expectFailure)

blankPuzzle :: Puzzle
blankPuzzle = freshPuzzle ""

prop_addToBlank :: Char -> Bool
prop_addToBlank char = fillInCharacter blankPuzzle char == Puzzle "" [] [char]

spec :: IO ()
spec = hspec $ do
    describe "fillInCharacter" $ do
        context "on a blank puzzle" $
            it "adds a guessed letter" $
                property prop_addToBlank
        context "on a puzzle with matching letter" $
            it "fills in the correct slots" $
                fillInCharacter (freshPuzzle "abc") 'c' `shouldBe`
                Puzzle "abc" [Nothing, Nothing, Just 'c'] ""
        context "on a puzzle with no matching letter" $
            it "fills in the correct slots" $
                fillInCharacter (freshPuzzle "abc") 'd' `shouldBe`
                Puzzle "abc" (replicate 3 Nothing) "d"
        -- etc.
    describe "handleGuess" $
        context "on a blank puzzle" $
            it "returns a puzzle with a guess" $ do
                p' <- handleGuess blankPuzzle 'a'
                p' `shouldBe` Puzzle "" [] "a"

{-----------------
The last spec above uses an IO action, which causes an unwanted side effect.
We could improve things by refactoring the code to do the data processing
in a pure function, but I also asked around as to ways of mitigating this:

> `WriterT [String] IO a` is basically just a wrapper for `IO (a, [String])`,
> so it lets you collect a `[String]` along the way during your `IO` computation
> by "logging". (—masaeedu)

> it's fairly common to use `mtl`-style to mock things like that. `MonadLogger`
> is one example; you can do `runNoLoggingT` and it won't log anything, or
> `runStdoutLoggingT` and it'll run logs to standard out." (—Matt Parsons)
-----------------}
