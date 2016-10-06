{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy as DBSL
import Data.ByteString.Lazy.Char8 as DBSL8
import Data.Text as Text
import GHC.Generics
import Options.Applicative
import Spec as Spec
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))

data ElmSpec = ElmSpec
    { requirements :: Spec.Spec
    , nextUid :: Int
    , title :: Text.Text
    } deriving(Show, Generic)

instance FromJSON ElmSpec
instance ToJSON ElmSpec

performDecodeEncode :: (FromJSON a, FromJSON b, ToJSON c) => String -> String -> (a -> b -> c) -> IO ()
performDecodeEncode file1 file2 f = do
    file1Contents <- DBSL.readFile file1
    file2Contents <- DBSL.readFile file2
    result1 <- return $ eitherDecode file1Contents
    result2 <- return $ eitherDecode file2Contents
    toPrint <- return $ case (result1, result2) of
        (Left errorMsg, _) -> DBSL8.pack errorMsg
        (_, Left errorMsg) -> DBSL8.pack errorMsg
        (Right result1', Right result2') ->
            encode $ f result1' result2'
    DBSL8.putStrLn toPrint


data ScopeOptions = ScopeOptions
    { scopeSpecFile :: String
    , scopeResourcesFile :: String
    } deriving (Show)

performScope :: ScopeOptions -> IO ()
performScope (ScopeOptions specFile resourcesFile) = 
    let
        specScope :: ElmSpec -> [Spec.Resource] -> Spec.SpecScope
        specScope elmSpec resources =
            Spec.specScope (requirements elmSpec) resources
    in
    performDecodeEncode specFile resourcesFile specScope
    

scopeParser :: Parser ScopeOptions
scopeParser = ScopeOptions
    <$> strOption
        (   long "spec"
        <>  metavar "SPECFILE"
        <>  help "JSON specification file")
    <*> strOption
        (   long "resources"
        <>  metavar "RESOURCESFILE"
        <>  help "JSON listing of all resources associated with the spec")


data DiffScopeOptions = DiffScopeOptions
    { diffScopeDiffFile :: String
    , diffScopeResourcesFile :: String
    } deriving (Show)

performDiffScope :: DiffScopeOptions -> IO ()
performDiffScope (DiffScopeOptions diffFile resourcesFile) =
    performDecodeEncode diffFile resourcesFile Spec.specChangeScope

diffScopeParser :: Parser DiffScopeOptions
diffScopeParser = DiffScopeOptions
    <$> strOption
        (   long "diff"
        <>  metavar "DIFFFILE"
        <>  help "diff file generated from 'spec diff'")
    <*> strOption
        (   long "resources"
        <>  metavar "RESOURCESFILE"
        <>  help "JSON listing of all resources associated with the changing spec")


data DiffOptions = DiffOptions
    { diffSpec1File :: String
    , diffSpec2File :: String
    } deriving (Show)

performDiff :: DiffOptions -> IO ()
performDiff (DiffOptions spec1File spec2File) = 
    let 
        specDiff :: ElmSpec -> ElmSpec -> Spec.SpecDiff
        specDiff spec1 spec2 =
            Spec.specDiff (requirements spec1) (requirements spec2)
    in
    performDecodeEncode spec1File spec2File specDiff

diffParser :: Parser DiffOptions
diffParser = DiffOptions
    <$> strOption
        (   long "spec1"
        <>  metavar "SPECFILE1"
        <>  help "JSON specification file before change")
    <*> strOption
        (   long "spec2"
        <>  metavar "SPECFILE2"
        <>  help "JSON specification file after change")


data ChangeScopeOptions = ChangeScopeOptions
    { changeScopeSpec1File :: String
    , changeScopeSpec2File :: String
    , changeScopeResourcesFile :: String
    } deriving (Show)

performChangeScope :: ChangeScopeOptions -> IO ()
performChangeScope (ChangeScopeOptions spec1 spec2 resources) = do
    spec1Contents <- DBSL.readFile spec1
    spec2Contents <- DBSL.readFile spec2
    resourcesContents <- DBSL.readFile resources
    spec1 <- return $ eitherDecode spec1Contents
    spec2 <- return $ eitherDecode spec2Contents
    resources <- return $ eitherDecode resourcesContents
    toPrint <- return $ case (spec1, spec2, resources) of
        (Left errorMsg, _, _) -> DBSL8.pack errorMsg
        (_, Left errorMsg, _) -> DBSL8.pack errorMsg
        (_, _, Left errorMsg) -> DBSL8.pack errorMsg
        (Right spec1', Right spec2', Right resources') ->
            encode 
            $ flip Spec.specChangeScope resources' 
            $ Spec.specDiff (requirements spec1') (requirements spec2')
    DBSL8.putStrLn toPrint

changeScopeParser :: Parser ChangeScopeOptions
changeScopeParser = ChangeScopeOptions
    <$> strOption
        (   long "spec1"
        <>  metavar "SPECFILE1"
        <>  help "JSON specification file before change")
    <*> strOption
        (   long "spec2"
        <>  metavar "SPECFILE2"
        <>  help "JSON specification file after change")
    <*> strOption
        (   long "resources"
        <>  metavar "RESOURCESFILE"
        <>  help "JSON listing of all resources associated with the changing spec")


data SpecCmd 
    = Scope ScopeOptions
    | DiffScope DiffScopeOptions
    | Diff DiffOptions
    | ChangeScope ChangeScopeOptions
    deriving (Show)

scopeCmdParser :: Parser SpecCmd
scopeCmdParser = Scope <$> scopeParser

diffScopeCmdParser :: Parser SpecCmd
diffScopeCmdParser = DiffScope <$> diffScopeParser

diffCmdParser :: Parser SpecCmd
diffCmdParser = Diff <$> diffParser

changeScopeCmdParser :: Parser SpecCmd
changeScopeCmdParser = ChangeScope <$> changeScopeParser


specCmdParser :: Parser SpecCmd
specCmdParser = hsubparser
    (   command "scope" 
            (info (helper <*> scopeCmdParser)
                (   fullDesc
                <>  progDesc "Given a spec and some resources, generate a list of all unaddressed requirements and unassociated resources"))
    <>  command "diff-scope" 
            (info (helper <*> diffScopeCmdParser)
                (   fullDesc
                <>  progDesc "Given a diff of two specs and some resources, generate a list of all the resources that are linked to requirements that are different between the two specs as well as unaddressed requirements in the new spec and resources unassociated with the new spec."))
    <>  command "diff"
            (info (helper <*> diffCmdParser)
                (   fullDesc
                <>  progDesc "Given two specs, generate a diff object."))
    <>  command "change-scope"
            (info (helper <*> changeScopeCmdParser)
                (   fullDesc
                <>  progDesc "Given two specs and some resources, generate a diff of the two specs and pipe that into 'spec diff-scope'."))
    )


specCmdParserInfo :: ParserInfo SpecCmd
specCmdParserInfo =
    info specCmdParser
        (   fullDesc
        <>  progDescDoc  
            (Just
                (   text "Keep track of your project by seeing what you need to change when your requirements change:\r\n" 
                <>  text "      spec scope        - given a spec and some resources, generate a list of all unaddressed requirements and unassociated resources\r\n"
                <>  text "      spec diff-scope   - given a diff of two specs and some resources, gets the items that will need to be addressed by the change\r\n"
                <>  text "      spec diff         - given two specs, generate a diff object\r\n"
                <>  text "      spec change-scope - give two specs and some resources, generate a diff of the two specs and pipe that into 'spec diff-scope'"
                ))
        <>  header "spec - Associate artifacts with specification requirements")


performSpecCmd :: SpecCmd -> IO ()
performSpecCmd (Scope scopeOptions) = performScope scopeOptions
performSpecCmd (Diff diffOptions) = performDiff diffOptions
performSpecCmd (DiffScope diffScopeOptions) = performDiffScope diffScopeOptions
performSpecCmd (ChangeScope changeScopeOptions) = performChangeScope changeScopeOptions


main :: IO ()
main = 
    execParser specCmdParserInfo >>= performSpecCmd
