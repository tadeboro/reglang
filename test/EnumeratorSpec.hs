module EnumeratorSpec where

import Regex.Enumerator
import Regex.Enumerator.Internal
import Regex.Parser
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((.&.))
import Data.List (sort, union, nub)

-- Lists with increasing elements
newtype UpList a = UpList [a] deriving (Show)
instance (Arbitrary a, Ord a) => Arbitrary (UpList a) where
  arbitrary = do x <- orderedList
                 return $ UpList (nub x)

-- Union property
unionProp :: UpList Int -> UpList Int -> Bool
unionProp (UpList xs) (UpList ys) =
  xs \/ ys == sort (xs `union` ys)

-- Dummy NFA that serves as list of destination states in test
ds = [State 0 Accept [], State 1 (Symbol 'a') [State 0 Accept []]]
-- Regex that produces upper NFA
re = Alt Eps (Sym 'a')
-- String representation of re
rs = "a?"
-- Dummy contents of 3 groups
dg = [Just "a", Nothing, Just "c"]
da = [True, False, True]
dm = (dg, da)

-- Tests
spec :: Spec
spec = do
    describe "Enumerator.Internal.\\/" $ do
        prop "results have no duplicates and proper order" $ unionProp

    describe "Enumerator.Internal.bp" $ do
        it "returns destinations states when bypassable" $
            bp True [State 0 Accept []] `shouldBe` [State 0 Accept []]
        it "returns empty NFA when not bypassable" $
            bp False [State 0 Accept []] `shouldBe` []

    describe "Enumerator.Internal.rexp2nfa'" $ do
        it "returns empty, non-bypassable NFA from empty language" $
            rexp2nfa' Nil 3 ds `shouldBe`
              ([], 3, False)
        it "returns empty, bypassable NFA from empty symbol" $
            rexp2nfa' Eps 3 ds `shouldBe`
              ([], 3, True)
        it "returns single new state from symbol" $
            rexp2nfa' (Sym 'a') 3 ds `shouldBe`
              ([State 3 (Symbol 'a') ds], 4, False)
        it "returns single new state from backreference" $
            rexp2nfa' (GroupRef 5) 1 ds `shouldBe`
              ([State 1 (Ref 5) ds], 2, False)
        it "returns non-bypassable catenation" $
            rexp2nfa' (Cat (Sym 'a') (Sym 'b')) 5 ds `shouldBe`
              ([State 6 (Symbol 'a') [State 5 (Symbol 'b') ds]], 7, False)
        it "returns semi-bypassable catenation 1" $
            rexp2nfa' (Cat (Sym 'a') Eps) 5 ds `shouldBe`
              ([State 5 (Symbol 'a') ds], 6, False)
        it "returns semi-bypassable catenation 2" $
            rexp2nfa' (Cat Eps (Sym 'a')) 5 ds `shouldBe`
              ([State 5 (Symbol 'a') ds], 6, False)
        it "returns bypassable alternation" $
            rexp2nfa' (Alt Eps (Sym 'a')) 5 ds `shouldBe`
              ([State 5 (Symbol 'a') ds], 6, True)
        it "returns non-bypassable alternation" $
            rexp2nfa' (Alt (Sym 'b') (Sym 'a')) 3 ds `shouldBe`
              ([State 3 (Symbol 'a') ds, State 4 (Symbol 'b') ds], 5, False)
        it "returns non-bypassable group" $
            rexp2nfa' (Group 0 (Sym 'b')) 2 ds `shouldBe`
              ([State 4 (Open 0) [State 3 (Symbol 'b') [State 2 (Close 0) ds]]],
               5, False)
        it "returns bypassable group" $
            rexp2nfa' (Group 0 Eps) 2 ds `shouldBe`
              ([State 3 (Open 0) [State 2 (Close 0) ds]] \/ ds, 4, True)

    describe "Enumerator.Internal.rexp2nfa" $ do
        it "returns final NFA for passed in regular expression" $
            rexp2nfa re `shouldBe` ds

    describe "Enumerator.Internal.grp" $ do
        it "groups together transitions with equal actions" $
            grp [State 1 (Symbol 'a') [], State 3 (Symbol 'a') []] `shouldBe`
              [State (-1) (Symbol 'a') []]
        it "leaves different transitions separate" $
            grp [State 1 (Symbol 'a') [], State 3 (Symbol 'b') []] `shouldBe`
              [State 1 (Symbol 'a') [], State 3 (Symbol 'b') []]

    describe "Enumerator.Internal.accept" $ do
        it "accepts words that have Accept transition available" $
            accept [State 1 (Symbol 'a') [], State 0 Accept []] `shouldBe` True
        it "rejects words that have no Accept transition available" $
            accept [State 1 (Symbol 'a') []] `shouldBe` False

    describe "Enumerator.Internal.initMemory" $ do
        it "creates new memory and resets it" $
            initMemory 2 `shouldBe` ([Nothing, Nothing], [False, False])
        it "should not fail on creating empty memory" $
            initMemory 0 `shouldBe` ([], [])

    describe "Enumerator.Internal.resetGroup" $ do
        it "does first time activation of group" $
            resetGroup 1 ([Nothing, Nothing], [False, False]) `shouldBe`
              ([Nothing, Just ""], [False, True])
        it "resets already populated group and reactivates it" $
            resetGroup 1 ([Nothing, Just "abc"], [False, False]) `shouldBe`
              ([Nothing, Just ""], [False, True])

    describe "Enumerator.Internal.closeGroup" $ do
        it "closes selected group and makes it inactive" $
            closeGroup 2 dm `shouldBe` (dg, [True, False, False])

    describe "Enumerator.Internal.insert" $ do
        it "inserts element into all active groups" $
            insert "xy" dm `shouldBe` ([Just "axy", Nothing, Just "cxy"], da)
        it "leaves non-active groups untouched" $
            insert "x" ([Just "a"], [False]) `shouldBe` ([Just "a"], [False])

    describe "Enumerator.Internal.getGroup" $ do
        it "obtains existing group content 1" $
            getGroup 1 dm `shouldBe` Nothing
        it "obtains existing group content 2" $
            getGroup 2 dm `shouldBe` Just "c"
        it "reports non-existing group 1" $
            getGroup (-1) dm `shouldBe` Nothing
        it "reports non-existing group 2" $
            getGroup 6 dm `shouldBe` Nothing

    describe "Enumerator.Internal.genNextState" $ do
        it "appends simbol to existing word" $
            genNextState "a" (State 1 (Symbol 'b') ds) dm `shouldBe`
              Just ("ab", ds, ([Just "ab", Nothing, Just "cb"], da))
        it "opens new group" $
            genNextState "a" (State 1 (Open 0) ds) dm `shouldBe`
              Just ("a", ds, ([Just "", Nothing, Just "c"], da))
        it "closes selected group and deactivates it" $
            genNextState "a" (State 1 (Close 2) ds) dm `shouldBe`
              Just ("a", ds, (dg, [True, False, False]))
        it "does nothing" $
            genNextState "a" (State 1 Accept ds) dm `shouldBe`
              Just ("a", ds, dm)
        it "appends group contents to word" $
            genNextState "a" (State 1 (Ref 0) ds) dm `shouldBe`
              Just ("aa", ds, ([Just "aa", Nothing, Just "ca"], da))
        it "reports missing group" $
            genNextState "a" (State 1 (Ref 5) ds) dm `shouldBe`
              Nothing

    describe "Enumerator.enumerate" $ do
        it "lists all words of regular language 1" $
            enumerate rs `shouldBe` Right ["", "a"]
        it "lists all words of semi-regular language 1" $
            enumerate "(a?)b\\1" `shouldBe` Right ["b", "aba"]
        it "lists all words of semi-regular language 1" $
            enumerate "(ae)c(\\1b\\1)c\\2" `shouldBe` Right ["aecaebaecaebae"]

    describe "Enumerator.enumerate1" $ do
        it "lists all words of regular language 1" $
            enumerate1 0 re `shouldBe` ["", "a"]
