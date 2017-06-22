{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson                 (FromJSON)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.List                  (intercalate, isPrefixOf,
                                             isSuffixOf, sortBy)
import           Data.List.Split            (splitOn)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (mappend)
import           Data.Time.Calendar         (toGregorian)
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.LocalTime        (getCurrentTimeZone, localDay,
                                             utcToLocalTime)
import           GHC.Generics               (Generic)
import           Hakyll
import           Hakyll.Web.Sass            (sassCompiler)
import           System.FilePath            (takeFileName)
import           System.FilePath.Posix      (takeBaseName, takeDirectory, (</>))
import           System.IO.Unsafe           (unsafePerformIO)
import           System.Process             (system)
import           Test.RandomStrings         (onlyAlphaNum, randomASCII,
                                             randomString)
import           Text.Jasmine               as JS

import qualified Projects

conf :: Configuration
conf = defaultConfiguration
    { destinationDirectory = "../site"
    , tmpDirectory         = "_cache/tmp"
    -- , providerDirectory = "src"
    , deployCommand        = "echo 'No deploy command specified.' && " ++
                             "echo '' && echo 'Use `make deploy`.' && "
    , deploySite           = system . deployCommand
    , ignoreFile           = filesToIgnore
    , previewHost          = "localhost"
    }

filesToIgnore path
    | "." `isPrefixOf` fileName = True
    | otherwise                 = False
    where
        fileName = takeFileName path

-- TODO: Modularize this main stuff?
main :: IO ()
main = hakyllWith conf $ do
    ---------------
    -- Static files
    ---------------

    let staticFiles =
            [ "assets/fonts/*"
            , "assets/icons/*"
            , "assets/images/*"
            , "assets/images/**/*"
            , "assets/stylesheets/*.css"
            , "assets/*.*"
            , "humans.txt"
            , "robots.txt"
            ]
    -- Simply copy static files
    match (foldl (.||.) "" $ map fromGlob staticFiles) $ do
        route   idRoute
        compile copyFileCompiler

    ---------------------
    -- Compile JS and CSS
    ---------------------

    -- TODO: Parse build rules in html?

    -- Minify JavaScript
    match "assets/javascript/*.js" $ do
        -- route   jsMinIdRoute
        route   idRoute
        -- compile compressJsCompiler
        compile copyFileCompiler

    -- Compile Sass stylesheets
    match "assets/scss/*.scss" $ do
        route   sassToCssRoute
        let compressCssItem = fmap compressCss
        compile (compressCssItem <$> sassCompiler)

    ------------------
    -- Posts and Pages
    ------------------

    match "_posts/*.md" $ do
        route cleanPostRoute
        compile $ do
            let withRelatedPostsCtx = listField
                    "related_posts" postCtx
                    (recentFirst =<< loadAll "posts/*") `mappend`
                    postCtx

            pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "_layouts/post.html"    withRelatedPostsCtx
                >>= loadAndApplyTemplate "_layouts/default.html" withRelatedPostsCtx
                >>= rewriteUrls

    -- Compile markdown pages
    match (fromRegex "([a-z]*/)*index.md$") $ do
        route   htmlIdRoute
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "_layouts/page.html"    pageCtx
                >>= loadAndApplyTemplate "_layouts/default.html" pageCtx
                >>= rewriteUrls

    -- Compile HTML pages
    let notProjectsIndex = complement "projects/index.html"
        htmlPages = (.&&.)
            (fromRegex "^(404|([a-z]*/?)+/(.*)).html$")
            notProjectsIndex

    match htmlPages $ do
        route   idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate siteCtx
                >>= loadAndApplyTemplate "_layouts/default.html" siteCtx
                >>= rewriteUrls

    -- TODO: Implement loading from projects.yml data file
    match "projects/index.html" $ do
        route   idRoute
        compile $ do
            let projectsData = Projects.loadProjectsData "_data/projects.yml"
                projectItems = Projects.projectsToItems projectsData
                projectsCtx =
                    listField "projects" Projects.projectCtx (return projectItems) `mappend`
                    siteCtx

            getResourceBody
                >>= applyAsTemplate projectsCtx
                >>= loadAndApplyTemplate "_layouts/default.html" projectsCtx
                >>= rewriteUrls

    ------------------
    -- Generated files
    ------------------

    -- Post archive
    create ["blog/archive/index.html"] $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "_posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "_layouts/archive.html" archiveCtx
                >>= loadAndApplyTemplate "_layouts/default.html" archiveCtx
                >>= rewriteUrls

    -- Root index
    match "index.html" $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "_posts/*"
            let indexCtx =
                    constField "is_index" "True" `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    siteCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "_layouts/default.html" indexCtx
                >>= rewriteUrls
                >>= cleanIndexUrls

    match "_layouts/*" $ compile templateCompiler
    match "_includes/*" $ compile templateCompiler

--------------------------------------------------------------------------------
-- Contexts

siteCtx :: Context String
siteCtx =
    constField "site_title"   "Chance Snow"                         `mappend`
    constField "site_tagline" "Redhead, web developer, philosopher" `mappend`
    constField "email"        "hello@chancesnow.me"                 `mappend`
    constField "facebook"     "chancexsnow"                         `mappend`
    constField "twitter"      "ChancesOfSnow"                       `mappend`
    constField "google_plus"  "113414184061295490862"               `mappend`
    constField "lastfm"       "EnigmaticEffigy"                     `mappend`
    constField "linked_in"    "chancesnow"                          `mappend`
    constField "tumblr"       "chancesnow"                          `mappend`
    constField "github"       "chances"                             `mappend`
    constField "npm"          "chancesnow"                          `mappend`
    constField "dribbble"     "chance"                              `mappend`
    constField "year"         currentYear                           `mappend`
    defaultContext

currentYear :: String
currentYear = unsafePerformIO $ do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    let (year, _, _) = toGregorian $ localDay zoneNow
    return $ show year

postCtx :: Context String
postCtx =
    dateField   "date"         "%B %e, %Y"      `mappend`
    dateField   "date_verbose" "%A, %B %-d, %Y" `mappend`
    teaserField "teaser"       "content"        `mappend`
    siteCtx

pageCtx :: Context String
pageCtx = siteCtx

--------------------------------------------------------------------------------
-- Routing

htmlIdRoute :: Routes
htmlIdRoute = customRoute createIndexRoute where
    createIndexRoute ident = takeDirectory p
            </> takeBaseName p ++ ".html"
        where p = toFilePath ident

sassToCssRoute :: Routes
sassToCssRoute = composeRoutes
    (gsubRoute "scss/" $ const "stylesheets/")
    (setExtension (cacheBuster ++ ".min.css"))

jsMinIdRoute :: Routes
jsMinIdRoute = setExtension "min.js"

cleanRoute :: (FilePath -> FilePath) -> Routes
cleanRoute newRoute = metadataRoute $ \metadata ->
    customRoute $ \ident ->
        let
            maybePermalink = M.lookup "permalink" metadata
            p = toFilePath ident
            in case maybePermalink of
                Just permalink -> "." </> permalink </> "index.html"
                Nothing        -> newRoute p

cleanPostRoute :: Routes
cleanPostRoute = cleanRoute $ \p ->
    (takeDirectory $ takeDirectory p)
        </> "blog"
        </> takeBaseName p </> "index.html"

cleanPageRoute :: Routes
cleanPageRoute = cleanRoute $ \p ->
    takeDirectory p </> takeBaseName p </> "index.html"

--------------------------------------------------------------------------------
-- Compilers

cacheBuster :: String
cacheBuster = unsafePerformIO $ do
    randomHash <- randomString (onlyAlphaNum randomASCII) 8
    putStrLn ("Generated cache buster: " ++ randomHash)
    return randomHash

rewriteUrls :: Item String -> Compiler (Item String)
rewriteUrls item = rewriteCssUrls item

-- | Rewire local, relative CSS URLs in compiles sources to point to their
--   minified and cache busted counterparts
rewriteCssUrls :: Item String -> Compiler (Item String)
rewriteCssUrls = return . fmap (withUrls rewrite) where
  rewrite url
      | ".css" `isSuffixOf` url &&
        ("/"   `isPrefixOf` url || "./"   `isPrefixOf` url || "../"   `isPrefixOf` url) &&
        not (".min.css" `isSuffixOf` url) =
            intercalate ("." ++ cacheBuster ++ ".min.css") . splitOn ".css" $ url
      | otherwise = url

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minifyJS = C.unpack . JS.minify . C.pack . itemBody
  s <- getResourceString
  return $ itemSetBody (minifyJS s) s

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean) where
  idx = "index.html"
  clean url
      | idx `isSuffixOf` url = take (length url - length idx) url
      | otherwise            = url
