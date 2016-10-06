{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson as Aeson
import Spec
import Test.HUnit

specScopeBVT :: Test
specScopeBVT = 
    let
        requirement1 = Requirement 1 "requirement1 name" "requirement1 description"
        requirement2 = Requirement 2 "requirement2 name" "requirement2 description"
        requirement3 = Requirement 3 "requirement3 name" "requirement3 description"
        requirement5 = Requirement 5 "requirement5 name" "requirement5 description"
        
        resource1 = Resource 1 (Aeson.String "resource1")
        resource2 = Resource 1 (Aeson.String "resource2")
        resource3 = Resource 2 (Aeson.String "resource3")
        resource4 = Resource 3 (Aeson.String "resource4")
        resource5 = Resource 4 (Aeson.String "resource5")

        toTestSpec = [requirement1, requirement2, requirement3, requirement5]
        toTestResources = [resource1, resource2, resource3, resource4, resource5]

        expected = SpecScope [requirement5] [resource5]
    in
    TestCase (assertEqual "General use case for specScope" expected
        (specScope toTestSpec toTestResources))


requirementDiffBVT :: Test
requirementDiffBVT =
    let
        requirement1 = Requirement 1 "requirement1 name" "requirement1 description"
        requirement2 = Requirement 2 "requirement2 name" "requirement2 description"
        requirement3 = Requirement 3 "requirement3 name" "requirement3 description"
        requirement4 = Requirement 4 "requirement4 name" "requirement4 description"
        requirement4' = Requirement 4 "requirement4 name" "requirement4' description"

        spec1 = [requirement1, requirement2, requirement4]
        spec2 = [requirement2, requirement3, requirement4']

        expected = SpecDiff
            { changedRequirements = [requirement4']
            , newRequirements = [requirement3]
            , removedRequirements = [requirement1]
            }
    in
    TestCase (assertEqual "General use case for specDiff" expected
        (specDiff spec1 spec2))


specChangeScopeBVT :: Test
specChangeScopeBVT = 
    let
        requirement1 = Requirement 1 "requirement1 name" "requirement1 description"
        requirement3 = Requirement 3 "requirement3 name" "requirement3 description"
        requirement4' = Requirement 4 "requirement4 name" "requirement4' description"

        diff = SpecDiff
            { changedRequirements = [requirement4']
            , newRequirements = [requirement3]
            , removedRequirements = [requirement1]
            }

        resource1 = Resource 1 (Aeson.String "resource1")
        resource2 = Resource 1 (Aeson.String "resource2")
        resource3 = Resource 2 (Aeson.String "resource3")
        resource4 = Resource 4 (Aeson.String "resource4")

        resources = [resource1, resource2, resource3, resource4]

        expected = SpecChangeScope
            { resourcesToUpdate = [resource4]
            , requirementsToAddress = [requirement3]
            , deprecatedResources = [resource1, resource2]
            }
    in
    TestCase (assertEqual "General use case for specChangeScope" expected 
        (specChangeScope diff resources))


main :: IO Counts
main = runTestTT $ TestList [specScopeBVT, requirementDiffBVT, specChangeScopeBVT]