{-# LANGUAGE DeriveGeneric #-}

module Spec where

import Data.Aeson as Aeson
import Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text as Text
import GHC.Generics

-- Data Types

data Feature = Feature
    { uid :: Int
    , name :: !Text.Text
    , description :: !Text.Text
    } deriving(Show, Generic, Eq)

instance FromJSON Feature
instance ToJSON Feature

type Spec = [Feature]


data FeatureLink = FeatureLink
    { featureUid :: Int
    , uri :: Aeson.Value
    } deriving (Show, Generic, Eq)

instance FromJSON FeatureLink
instance ToJSON FeatureLink


data FeatureDiff = FeatureDiff
    { changedFeatures :: [Feature]
    , newFeatures :: [Feature]
    , removedFeatures :: [Feature]
    } deriving (Show, Generic, Eq)

instance FromJSON FeatureDiff
instance ToJSON FeatureDiff


data SpecChangeScope = SpecChangeScope
    { resourcesToUpdate :: [FeatureLink]
    , featuresToAddress :: [Feature]
    , deprecatedResources :: [FeatureLink]
    } deriving (Show, Generic, Eq)

instance FromJSON SpecChangeScope
instance ToJSON SpecChangeScope

data SpecScope = SpecScope
    { featuresNotAddressed :: [Feature]
    , resourcesNotLinked :: [FeatureLink]
    } deriving (Show, Generic, Eq)

instance FromJSON SpecScope
instance ToJSON SpecScope


-- Functions

-- | Returns all the features that have no associated resources
unaddressedFeatures 
    :: Spec 
    -> [FeatureLink] 
    -> [Feature]
unaddressedFeatures spec linkedResources =
    let 
        featureUids :: [Int]
        featureUids =
            map uid spec

        addressedFeatureUids :: [Int]
        addressedFeatureUids = 
            List.intersect featureUids
            $ List.nub 
            $ map featureUid linkedResources

        unaddressedFeatureUids :: [Int]
        unaddressedFeatureUids =
            featureUids \\ addressedFeatureUids
    in
    filter ((`elem` unaddressedFeatureUids) . uid) spec


-- | Returns all resources that don't have associated features 
-- (those features might be deprecated).
unassociatedResources 
    :: Spec 
    -> [FeatureLink] 
    -> [FeatureLink]
unassociatedResources spec linkedResources =
    let 
        unassociatedResourceUids :: [Int]
        unassociatedResourceUids =
            List.nub (map featureUid linkedResources) \\ map uid spec
    in
    filter ((`elem` unassociatedResourceUids) . featureUid) linkedResources


-- | Returns a data structure with two lists:
--      - all features that aren't addressed by the current set of resources
--      - all resources that aren't addressing a feature in the current spec
specScope :: Spec -> [FeatureLink] -> SpecScope
specScope spec resources =
    SpecScope 
        { featuresNotAddressed = unaddressedFeatures spec resources
        , resourcesNotLinked = unassociatedResources spec resources
        }


-- | All features that are different between the two specs
featureDiff :: Spec -> Spec -> FeatureDiff
featureDiff spec1 spec2 =
    FeatureDiff
        { changedFeatures =
            let 
                featureWasChanged :: Feature -> Feature -> Bool
                featureWasChanged newFeature oldFeature =
                    uid newFeature == uid oldFeature &&
                    (
                        name newFeature /= name oldFeature ||
                        description newFeature /= description oldFeature
                    )
            in
            List.intersectBy 
                -- the more recent features is supplied before the older one because
                -- intersectBy grabs the element from the first list if the
                -- equality test passes
                featureWasChanged
                spec2 
                spec1

        , newFeatures =
            filter 
                (\newFeature -> 
                    all 
                        (\oldFeature -> 
                            uid oldFeature /= uid newFeature) 
                        spec1) 
                spec2

        , removedFeatures =
            filter
                (\oldFeature ->
                    all
                        (\newFeature ->
                            uid newFeature /= uid oldFeature)
                        spec2)
                spec1
        }


-- | Given a set of features that have been changed or added and a set of the
-- currently addressed features, returns a summary of all resources that might
-- need changing and features that haven't yet been addressed.
specChangeScope 
    :: FeatureDiff 
    -> [FeatureLink] 
    -> SpecChangeScope
specChangeScope (FeatureDiff changed new removed) linkedResources =
    SpecChangeScope
        { resourcesToUpdate = 
            filter 
                ((`elem` (map uid (changed <> new))) . featureUid) 
                linkedResources

        , featuresToAddress = new

        , deprecatedResources =
            filter ((`elem` (map uid removed)) . featureUid) linkedResources
        }
