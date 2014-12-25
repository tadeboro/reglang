module EnumeratorSpec where

import Enumerator
import Enumerator.Internal
import Parser
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
ds = [State 0 None [], State 1 (Symbol 'a') [State 0 None []]]
-- Regex that produces upper NFA
re = Alt Eps (Sym 'a')
-- String representation of re
rs = "a?"

-- Tests
spec :: Spec
spec = do
    describe "Enumerator.Internal.\\/" $ do
        prop "results have no duplicates and proper order" $ unionProp

    describe "Enumerator.Internal.bp" $ do
        it "returns destinations states when bypassable" $
            bp True [State 0 None []] `shouldBe` [State 0 None []]
        it "returns empty NFA when not bypassable" $
            bp False [State 0 None []] `shouldBe` []

    describe "Enumerator.Internal.rexp2nfa'" $ do
        it "returns empty, non-bypassable NFA from empty language" $
            rexp2nfa' Nil 3 ds `shouldBe` ([], 3, False)
        it "returns empty, bypassable NFA from empty symbol" $
            rexp2nfa' Eps 3 ds `shouldBe` ([], 3, True)
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
            rexp2nfa' (Group (Sym 'b')) 2 ds `shouldBe`
              ([State 4 Open [State 3 (Symbol 'b') [State 2 Close ds]]], 5, False)
        it "returns bypassable group" $
            rexp2nfa' (Group Eps) 2 ds `shouldBe`
              ([State 3 Open [State 2 Close ds]] \/ ds, 4, True)

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
        it "accepts words that have None transition available" $
            accept [State 1 (Symbol 'a') [], State 0 None []] `shouldBe` True
        it "rejects words that have no None transition available" $
            accept [State 1 (Symbol 'a') []] `shouldBe` False

    describe "Enumerator.Internal.addGroup" $ do
        it "creates new active group" $
            addGroup (0, [], []) `shouldBe` (1, [""], [0])
        it "creates new active group and leaves others in peace" $
            addGroup (3, ["a", "b", "c"], [2, 0]) `shouldBe`
              (4, ["", "a", "b", "c"], [3, 2, 0])

    describe "Enumerator.Internal.closeGroup" $ do
        it "closes most recently opened group" $
            closeGroup (3, ["a", "b", "c"], [2, 0]) `shouldBe`
              (3, ["a", "b", "c"], [0])

    describe "Enumerator.Internal.insert" $ do
        it "inserts element into all active groups" $
            insert (3, ["a", "b", "c"], [1, 0]) 'x' `shouldBe`
              (3, ["a", "bx", "cx"], [1, 0])
        it "leaves non-active groups untouched" $
            insert (3, ["a", "b", "c"], []) 'x' `shouldBe`
              (3, ["a", "b", "c"], [])

    describe "Enumerator.Internal.getGroup" $ do
        it "obtains existing group content 1" $
            getGroup (3, ["a", "b", "c"], []) 2 `shouldBe` Just "b"
        it "obtains existing group content 2" $
            getGroup (3, ["a", "b", "c"], []) 3 `shouldBe` Just "a"
        it "reports non-existing group 1" $
            getGroup (3, ["a", "b", "c"], []) 0 `shouldBe` Nothing
        it "reports non-existing group 2" $
            getGroup (3, ["a", "b", "c"], []) 6 `shouldBe` Nothing

    describe "Enumerator.Internal.genNextState" $ do
        it "appends simbol to existing word" $
            genNextState "a" (State 1 (Symbol 'b') ds)
                         (3, ["a", "bc", "def"], [2])
              `shouldBe` Just ("ab", ds, (3, ["ab", "bc", "def"], [2]))
        it "opens new group" $
            genNextState "a" (State 1 Open ds)
                         (3, ["a", "bc", "def"], [2])
              `shouldBe` Just ("a", ds, (4, ["", "a", "bc", "def"], [3, 2]))
        it "closes most recently activated group" $
            genNextState "a" (State 1 Close ds)
                         (3, ["a", "bc", "def"], [2])
              `shouldBe` Just ("a", ds, (3, ["a", "bc", "def"], []))
        it "does nothing" $
            genNextState "a" (State 1 None ds)
                         (3, ["a", "bc", "def"], [2])
              `shouldBe` Just ("a", ds, (3, ["a", "bc", "def"], [2]))
        it "appends group contents to word" $
            genNextState "a" (State 1 (Ref 1) ds)
                         (3, ["a", "bc", "def"], [2])
              `shouldBe` Just ("adef", ds, (3, ["a", "bcdef", "def"], [2]))
        it "reports missing group" $
            genNextState "a" (State 1 (Ref 5) ds)
                         (3, ["a", "bc", "def"], [2])
              `shouldBe` Nothing

    describe "Enumerator.enumerate" $ do
        it "lists all words of regular language 1" $
            enumerate rs `shouldBe` Right ["", "a"]
        it "lists all words of semi-regular language 1" $
            enumerate "(a?)b\\1" `shouldBe` Right ["b", "aba"]
        it "lists all words of semi-regular language 1" $
            enumerate "(ae)c(\\1b\\1)c\\2" `shouldBe` Right ["aecaebaecaebae"]

    describe "Enumerator.enumerate1" $ do
        it "lists all words of regular language 1" $
            enumerate1 re `shouldBe` ["", "a"]
