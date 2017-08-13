{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Projects where

import           Data.Aeson            (FromJSON (parseJSON), ToJSON, object,
                                        toJSON, withObject, (.!=), (.:), (.:?),
                                        (.=))
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe            (fromJust, fromMaybe, isJust)
import qualified Data.Yaml             as Y
import           GHC.Generics          (Generic)
import           Hakyll
import           System.IO.Unsafe      (unsafePerformIO)

data Project = Project
    { name        :: String
    , shortName   :: Maybe String
    , logoUrl     :: Maybe String
    , description :: String
    , featured    :: Bool
    , link        :: Maybe String
    , github      :: String
    }

instance FromJSON Project where
    parseJSON = withObject "Project" $ \o -> do
        name        <- o .:  "name"
        shortName   <- o .:? "short_name" .!= Nothing
        logoUrl     <- o .:? "logo_url" .!= Nothing
        description <- o .:  "description"
        featured    <- o .:? "featured" .!= False
        link        <- o .:? "link" .!= Nothing
        github      <- o .:  "github"
        return Project{..}

instance ToJSON Project where
  toJSON Project{..} = object [
    "name"        .= name,
    "short_name"  .= shortName,
    "description" .= description,
    "link"        .= link,
    "github"      .= github ]

loadProjectsData :: FilePath -> [Project]
loadProjectsData dataFilePath = unsafePerformIO $ do
    ymlData <- BS.readFile dataFilePath
    let projects = fromMaybe [] (Y.decode ymlData :: Maybe [Project])
    return projects

projectNames :: Project -> String -> String
projectNames a b = Projects.name a ++ " " ++ b ++ " "

featuredProjects :: [Project] -> [Project]
featuredProjects = filter featured

projectName :: Project -> String
projectName project = fromMaybe
    (Projects.name project) (shortName project)

projectsToItems :: [Project] -> [Item Project]
projectsToItems = map
    (\project -> Item
        (fromFilePath $ Projects.github project)
        project
    )

projectCtx :: Context Project
projectCtx =
    field     "name"           (return . projectName . itemBody) `mappend`
    boolField "has_logo" (isJust . logoUrl . itemBody) `mappend`
    field     "logo_url" (return . fromMaybe "" . logoUrl . itemBody) `mappend`
    boolField "has_link" (isJust . link . itemBody) `mappend`
    field     "link" (return . fromMaybe "" . link . itemBody) `mappend`
    field     "description" (return . description . itemBody) `mappend`
    field     "github"      (return . github . itemBody)

-- projectCtx :: Context Project
-- projectCtx project = do
--     maybeShortName <- Projects.shortName (itemBody project)
--     return $ field "name" (return . Projects.name . itemBody) `mappend`
--         case maybeShortName of
--             Just shortName ->
--                 field "short_name"  (return . (fromJust . Projects.shortName) . itemBody)  `mappend`
--                 field "description" (return . Projects.description . itemBody) `mappend`
--                 field "github"      (return . Projects.github . itemBody)
--             Nothing        ->
--                 field "description" (return . Projects.description . itemBody) `mappend`
--                 field "github"      (return . Projects.github . itemBody)
