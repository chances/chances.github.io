{-# LANGUAGE OverloadedStrings #-}

import           Data.List             (isSuffixOf, sortBy)
import           Data.Monoid           (mappend)
import           Hakyll
import           System.FilePath.Posix (takeBaseName, takeDirectory, (</>))

-- TODO: https://jaspervdj.be/hakyll/reference/Hakyll-Core-Configuration.html#t:Configuration

main :: IO ()
main = hakyllWith defaultConfiguration
    { providerDirectory = toFilePath "src"
    } $ do
    -- Static assets
    match "assets/fonts/*" $ do
        route   idRoute
        compile copyFileCompiler
    match "assets/icons/*" $ do
        route   idRoute
        compile copyFileCompiler
    match "assets/images/*" $ do
        route   idRoute
        compile copyFileCompiler
    match "assets/stylesheets/*" $ do
        route   idRoute
        compile copyFileCompiler
    match "assets/*.*" $ do
        route   idRoute
        compile copyFileCompiler

    match "javascript/*.js" $ do
        -- TODO: Minify JS...
        route   idRoute
        compile compressCssCompiler

    -- Sass styles
    match "scss/*" $ do
        -- TODO: Compile Sass...
        route   idRoute
        compile compressCssCompiler

    -- match (fromList ["about.rst", "contact.markdown"]) $ do
    --     route   $ setExtension "html"
    --     compile $ pandocCompiler
    --         >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --         >>= relativizeUrls

    match "posts/*" $ do
        route $ cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------
-- No routes ending in .html

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute where
    createIndexRoute ident = takeDirectory p
            </> takeBaseName p
            </> "index.html"
        where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean) where
    idx = "index.html"
    clean url
        | idx `isSuffixOf` url = take (length url - length idx) url
        | otherwise            = url
