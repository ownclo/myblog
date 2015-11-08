--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath.Posix  (takeBaseName,takeDirectory ,(</>),splitFileName)
import           Data.List              (sortBy, isInfixOf)

--------------------------------------------------------------------------------
-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
niceRoute :: Routes
niceRoute = customRoute createIndexRoute where
      createIndexRoute ident =
              takeDirectory p </> takeBaseName p </> "index.html"
                  where p=toFilePath ident

-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr :: String -> String
    removeIndexStr url = case splitFileName url of
            (dir, "index.html") | isLocal dir -> dir
            _                                 -> url
            where isLocal uri = not (isInfixOf "://" uri)

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>=
                  withItemBody (unixFilter "sass" ["--trace"]) >>=
                  return . fmap compressCss

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ niceRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= removeIndexHtml

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archive"             `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= removeIndexHtml


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
                >>= removeIndexHtml

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%b %d '%y" `mappend`
    defaultContext
