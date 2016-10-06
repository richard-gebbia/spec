{-# LANGUAGE DeriveGeneric #-}

module Spec where

import Data.Aeson as Aeson
import Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text as Text
import GHC.Generics

-- Data Types

data Requirement = Requirement
    { uid :: Int
    , name :: !Text.Text
    , description :: !Text.Text
    } deriving(Show, Generic, Eq)

instance FromJSON Requirement
instance ToJSON Requirement

type Spec = [Requirement]


data Resource = Resource
    { requirementUid :: Int
    , uri :: Aeson.Value
    } deriving (Show, Generic, Eq)

instance FromJSON Resource
instance ToJSON Resource


data SpecDiff = SpecDiff
    { changedRequirements :: [Requirement]
    , newRequirements :: [Requirement]
    , removedRequirements :: [Requirement]
    } deriving (Show, Generic, Eq)

instance FromJSON SpecDiff
instance ToJSON SpecDiff


data SpecChangeScope = SpecChangeScope
    { resourcesToUpdate :: [Resource]
    , requirementsToAddress :: [Requirement]
    , deprecatedResources :: [Resource]
    } deriving (Show, Generic, Eq)

instance FromJSON SpecChangeScope
instance ToJSON SpecChangeScope

data SpecScope = SpecScope
    { requirementsNotAddressed :: [Requirement]
    , resourcesNotLinked :: [Resource]
    } deriving (Show, Generic, Eq)

instance FromJSON SpecScope
instance ToJSON SpecScope


-- Functions

-- | Returns all the requirements that have no associated resources
unaddressedRequirements 
    :: Spec 
    -> [Resource] 
    -> [Requirement]
unaddressedRequirements spec linkedResources =
    let 
        requirementUids :: [Int]
        requirementUids =
            map uid spec

        addressedRequirementUids :: [Int]
        addressedRequirementUids = 
            List.intersect requirementUids
            $ List.nub 
            $ map requirementUid linkedResources

        unaddressedRequirementUids :: [Int]
        unaddressedRequirementUids =
            requirementUids \\ addressedRequirementUids
    in
    filter ((`elem` unaddressedRequirementUids) . uid) spec


-- | Returns all resources that don't have associated requirements 
-- (those requirements might be deprecated).
unassociatedResources 
    :: Spec 
    -> [Resource] 
    -> [Resource]
unassociatedResources spec linkedResources =
    let 
        unassociatedResourceUids :: [Int]
        unassociatedResourceUids =
            List.nub (map requirementUid linkedResources) \\ map uid spec
    in
    filter ((`elem` unassociatedResourceUids) . requirementUid) linkedResources


-- | Returns a data structure with two lists:
--      - all requirements that aren't addressed by the current set of resources
--      - all resources that aren't addressing a requirement in the current spec
specScope :: Spec -> [Resource] -> SpecScope
specScope spec resources =
    SpecScope 
        { requirementsNotAddressed = unaddressedRequirements spec resources
        , resourcesNotLinked = unassociatedResources spec resources
        }


-- | All requirements that are different between the two specs
specDiff :: Spec -> Spec -> SpecDiff
specDiff spec1 spec2 =
    SpecDiff
        { changedRequirements =
            let 
                requirementWasChanged :: Requirement -> Requirement -> Bool
                requirementWasChanged newFeature oldFeature =
                    uid newFeature == uid oldFeature &&
                    (
                        name newFeature /= name oldFeature ||
                        description newFeature /= description oldFeature
                    )
            in
            List.intersectBy 
                -- the more recent requirements is supplied before the older one because
                -- intersectBy grabs the element from the first list if the
                -- equality test passes
                requirementWasChanged
                spec2 
                spec1

        , newRequirements =
            filter 
                (\newRequirement -> 
                    all 
                        (\oldFeature -> 
                            uid oldFeature /= uid newRequirement) 
                        spec1) 
                spec2

        , removedRequirements =
            filter
                (\oldFeature ->
                    all
                        (\newRequirement ->
                            uid newRequirement /= uid oldFeature)
                        spec2)
                spec1
        }


-- | Given a set of requirements that have been changed or added and a set of the
-- currently addressed requirements, returns a summary of all resources that might
-- need changing and requirements that haven't yet been addressed.
specChangeScope 
    :: SpecDiff 
    -> [Resource] 
    -> SpecChangeScope
specChangeScope (SpecDiff changed new removed) linkedResources =
    SpecChangeScope
        { resourcesToUpdate = 
            filter 
                ((`elem` (map uid (changed <> new))) . requirementUid) 
                linkedResources

        , requirementsToAddress = new

        , deprecatedResources =
            filter ((`elem` (map uid removed)) . requirementUid) linkedResources
        }
