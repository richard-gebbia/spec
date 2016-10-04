{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson as Aeson
import Spec
import Test.HUnit

specScopeBVT :: Test
specScopeBVT = 
    let
        feature1 = Feature 1 "feature1 name" "feature1 description"
        feature2 = Feature 2 "feature2 name" "feature2 description"
        feature3 = Feature 3 "feature3 name" "feature3 description"
        feature5 = Feature 5 "feature5 name" "feature5 description"
        
        resource1 = FeatureLink 1 (Aeson.String "featurelink1")
        resource2 = FeatureLink 1 (Aeson.String "featurelink2")
        resource3 = FeatureLink 2 (Aeson.String "featurelink3")
        resource4 = FeatureLink 3 (Aeson.String "featurelink4")
        resource5 = FeatureLink 4 (Aeson.String "featurelink5")

        toTestSpec = [feature1, feature2, feature3, feature5]
        toTestResources = [resource1, resource2, resource3, resource4, resource5]

        expected = SpecScope [feature5] [resource5]
    in
    TestCase (assertEqual "General use case for specScope" expected
        (specScope toTestSpec toTestResources))


featureDiffBVT :: Test
featureDiffBVT =
    let
        feature1 = Feature 1 "feature1 name" "feature1 description"
        feature2 = Feature 2 "feature2 name" "feature2 description"
        feature3 = Feature 3 "feature3 name" "feature3 description"
        feature4 = Feature 4 "feature4 name" "feature4 description"
        feature4' = Feature 4 "feature4 name" "feature4' description"

        spec1 = [feature1, feature2, feature4]
        spec2 = [feature2, feature3, feature4']

        expected = FeatureDiff
            { changedFeatures = [feature4']
            , newFeatures = [feature3]
            , removedFeatures = [feature1]
            }
    in
    TestCase (assertEqual "General use case for featureDiff" expected
        (featureDiff spec1 spec2))


specChangeScopeBVT :: Test
specChangeScopeBVT = 
    let
        feature1 = Feature 1 "feature1 name" "feature1 description"
        feature3 = Feature 3 "feature3 name" "feature3 description"
        feature4' = Feature 4 "feature4 name" "feature4' description"

        diff = FeatureDiff
            { changedFeatures = [feature4']
            , newFeatures = [feature3]
            , removedFeatures = [feature1]
            }

        resource1 = FeatureLink 1 (Aeson.String "featurelink1")
        resource2 = FeatureLink 1 (Aeson.String "featurelink2")
        resource3 = FeatureLink 2 (Aeson.String "featurelink3")
        resource4 = FeatureLink 4 (Aeson.String "featurelink4")

        resources = [resource1, resource2, resource3, resource4]

        expected = SpecChangeScope
            { resourcesToUpdate = [resource4]
            , featuresToAddress = [feature3]
            , deprecatedResources = [resource1, resource2]
            }
    in
    TestCase (assertEqual "General use case for specChangeScope" expected 
        (specChangeScope diff resources))


main :: IO Counts
main = runTestTT $ TestList [specScopeBVT, featureDiffBVT, specChangeScopeBVT]