module ParserSpec where

import Regex.Parser
import Regex.Parser.Internal
import Test.Hspec
import Control.Applicative ((<*))
import Text.ParserCombinators.Parsec

-- Test runner that returns Left Int or Right Rexp
run p s = case runParser (p <* eof) 0 "" s of
               Left e -> Left $ sourceColumn . errorPos $ e
               Right r -> Right r

-- Tests
spec :: Spec
spec = do
    describe "Parser.Internal.symP" $ do
        it "parses single normal character" $
            run symP "a" `shouldBe` Right (Sym 'a')
        it "reports error on more than one character" $
            run symP "ab" `shouldBe` Left 2
        it "reports error on invalid character" $
            run symP "?" `shouldBe` Left 1

    describe "Parser.Internal.anyP" $ do
        it "parses '.' as alternation of all ASCII chars" $
            run anyP "." `shouldBe` Right anyExpr
        it "reports error on more than one character" $
            run anyP ".b" `shouldBe` Left 2
        it "reports error on invalid character" $
            run symP "?" `shouldBe` Left 1

    describe "Parser.Internal.escP" $ do
        it "parses escaped special chars into regular ones" $
            run escP "\\." `shouldBe` Right (Sym '.')
        it "parses escaped digits into backreferences" $
            run escP "\\4" `shouldBe` Right (GroupRef 3)
        it "reports error if doesnt get exactly 2 chars" $
            run escP "\\" `shouldBe` Left 2
        it "reports error on invalid first char" $
            run escP "?a" `shouldBe` Left 1
        it "reports error on invalid second char" $
            run escP "\\a" `shouldBe` Left 2

    describe "Parser.Internal.groupP" $ do
        it "parses groups, eclosed in '(' and ')'" $
            run groupP "(a)" `shouldBe` Right (Group 0 (Sym 'a'))
        it "reports error id first char is not '('" $
            run groupP "b" `shouldBe` Left 1
        it "reports error on missing ')'" $
            run groupP "(abs" `shouldBe` Left 5
        it "reports error if expression doesn't end with ')'" $
            run groupP "(abs)h" `shouldBe` Left 6

    describe "Parser.Internal.baseP" $ do
        it "parses group" $
            run baseP "(a)" `shouldBe` Right (Group 0 (Sym 'a'))
        it "parses single normal character" $
            run baseP "b" `shouldBe` Right (Sym 'b')
        it "parses '.' as alternation of all ASCII chars" $
            run baseP "." `shouldBe` Right anyExpr
        it "parses escaped special chars into regular ones" $
            run baseP "\\(" `shouldBe` Right (Sym '(')

    describe "Parser.Internal.repP" $ do
        it "parses zero or more repetitions" $
            run repP "a*" `shouldBe` Right (Clo (Sym 'a'))
        it "parses one or more repetitions" $
            run repP "b+" `shouldBe` Right (Cat (Sym 'b') (Clo (Sym 'b')))
        it "parses zero or one repetition" $
            run repP "c?" `shouldBe` Right (Alt Eps (Sym 'c'))
        it "returns error on trailing chars" $
            run repP "c??" `shouldBe` Left 3
        it "returns error if repeated element cannot be parsed by baseP" $
            run repP "c|a" `shouldBe` Left 2

    describe "Parser.Internal.catP" $ do
        it "parses zero or more catenations of repetitions" $
            run catP "ab" `shouldBe` Right (Cat (Sym 'a') (Sym 'b'))
        it "is left associative" $
            run catP "abc" `shouldBe`
              Right (Cat (Cat (Sym 'a') (Sym 'b')) (Sym 'c'))
        it "handles real repetition" $
            run catP "a*b*" `shouldBe`
              Right (Cat (Clo (Sym 'a')) (Clo (Sym 'b')))
        it "returns error on alternation" $
            run catP "c|a" `shouldBe` Left 2

    describe "Parser.Internal.altP" $ do
        it "parses zero or more alternations of catenations" $
            run altP "a|b" `shouldBe` Right (Alt (Sym 'a') (Sym 'b'))
        it "is left associative" $
            run altP "a|b|c" `shouldBe`
              Right (Alt (Alt (Sym 'a') (Sym 'b')) (Sym 'c'))
        it "returns error if trailing | is found" $
            run altP "c|a|" `shouldBe` Left 5

    describe "Parser.Internal.exprP" $ do
        it "parses any valid regular expression" $
            run exprP "(a|b)\\1?" `shouldBe`
              Right (Cat (Group 0 (Alt (Sym 'a') (Sym 'b')))
                         (Alt Eps (GroupRef 0)), 1)
        it "reports error on malformed expressions" $
            run exprP "(a|b)\\1?*def" `shouldBe` Left 9

    describe "Parser.parseRexp" $ do
        it "parses any valid regular expression" $
            parseRexp "(a|b)\\1?" `shouldBe`
              Right (Cat (Group 0 (Alt (Sym 'a') (Sym 'b')))
                         (Alt Eps (GroupRef 0)), 1)
