{-# OPTIONS_GHC -fno-warn-orphans #-}

module Github.Review.Arbitrary () where

import Control.Applicative
import Data.Time
import Github.Data
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

instance Arbitrary Commit where
    arbitrary = Commit <$> arbitrary 
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary

instance Arbitrary GitCommit where
    arbitrary = GitCommit <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary

instance Arbitrary GitUser where
    arbitrary = GitUser <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary

instance Arbitrary GithubDate where
    arbitrary = GithubDate <$> arbitrary

instance Arbitrary Tree where
    arbitrary = Tree <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance Arbitrary GitTree where
    arbitrary = GitTree <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary

instance Arbitrary GithubOwner where
    arbitrary = oneof [ GithubUser <$> arbitrary <*> arbitrary <*> arbitrary
                                   <*> arbitrary <*> arbitrary
                      , GithubOrganization <$> arbitrary <*> arbitrary
                                           <*> arbitrary <*> arbitrary
                      ]

instance Arbitrary File where
    arbitrary = File <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance Arbitrary Stats where
    arbitrary = Stats <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary

instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary DiffTime where
    arbitrary =   secondsToDiffTime <$> choose (0, 86400)

