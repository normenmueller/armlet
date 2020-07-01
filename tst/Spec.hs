{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Data.SemVer

import Parsers
import Transformers
import Renderer

main :: IO ()
main = hspec $ do
    describe "Transformers.anno" $ do
        it "adds property (eg. author)" $ do
            act <- render' . anno ("author", "Normen Müller") <$> parse "./tst/in.archimate"
            exp <- render'                                    <$> parse "./tst/exp0.archimate"
            act `shouldBe` exp
        it "is idempotent" $ do
            act <- render' . anno ("author", "Normen Müller") <$> parse "./tst/exp0.archimate"
            exp <- render'                                    <$> parse "./tst/exp0.archimate"
            act `shouldBe` exp
        it "adds property (eg. version)" $ do
            act <- render' . anno ("version", "0.1.0") <$> parse "./tst/in.archimate"
            exp <- render'                             <$> parse "./tst/exp2.archimate"
            act `shouldBe` exp
        it "is idempotent" $ do
            act <- render' . anno ("version", "0.1.0") <$> parse "./tst/exp2.archimate"
            exp <- render'                             <$> parse "./tst/exp2.archimate"
            act `shouldBe` exp
    describe "Transformers.mark" $ do
        it "adds attribute" $ do
            act <- render' . mark "ITonICE" "#8df900" <$> parse "./tst/in.archimate"
            exp <- render'                            <$> parse "./tst/exp1.archimate"
            act `shouldBe` exp
        it "is idempotent" $ do
            act <- render' . mark "ITonICE" "#8df900" <$> parse "./tst/exp1.archimate"
            exp <- render'                            <$> parse "./tst/exp1.archimate"
            act `shouldBe` exp
    describe "Transformers.unmark" $ do
        it "removes attribute" $ do
            act <- render' . unmark <$> parse "./tst/exp1.archimate"
            exp <- render'          <$> parse "./tst/in.archimate"
            act `shouldBe` exp
        it "is idempotent" $ do
            act <- render' . unmark <$> parse "./tst/in.archimate"
            exp <- render'          <$> parse "./tst/in.archimate"
            act `shouldBe` exp
    describe "Transformers.bump" $ do
        it "adds no version (major)" $ do
            act <- render' . bump incrementMajor <$> parse "./tst/in.archimate"
            exp <- render'                       <$> parse "./tst/in.archimate"
            act `shouldBe` exp
        it "increments major" $ do
            act <- render' . bump incrementMajor <$> parse "./tst/exp2.archimate"
            exp <- render'                       <$> parse "./tst/exp3.archimate"
            act `shouldBe` exp
        it "adds no version (minor)" $ do
            act <- render' . bump incrementMinor <$> parse "./tst/in.archimate"
            exp <- render'                       <$> parse "./tst/in.archimate"
            act `shouldBe` exp
        it "increments minor" $ do
            act <- render' . bump incrementMinor <$> parse "./tst/exp2.archimate"
            exp <- render'                       <$> parse "./tst/exp4.archimate"
            act `shouldBe` exp
        it "adds no version (patch)" $ do
            act <- render' . bump incrementPatch <$> parse "./tst/in.archimate"
            exp <- render'                       <$> parse "./tst/in.archimate"
            act `shouldBe` exp
        it "increments patch" $ do
            act <- render' . bump incrementPatch <$> parse "./tst/exp2.archimate"
            exp <- render'                       <$> parse "./tst/exp5.archimate"
            act `shouldBe` exp
    describe "Transformers.normalize" $ do
        it "puts representation into a normal form" $ do
            act <- render' . normalize <$> parse "./tst/in.archimate"
            exp <- render'             <$> parse "./tst/exp6.archimate"
            act `shouldBe` exp
        it "is idempotent" $ do
            act <- render' . normalize <$> parse "./tst/exp6.archimate"
            exp <- render'             <$> parse "./tst/exp6.archimate"
            act `shouldBe` exp
