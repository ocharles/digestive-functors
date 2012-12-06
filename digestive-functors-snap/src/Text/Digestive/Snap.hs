{-# LANGUAGE OverloadedStrings #-}
-- | Module providing a Snap backend for the digestive-functors library
module Text.Digestive.Snap
    ( SnapPartPolicy
    , SnapFormConfig (..)
    , defaultSnapFormConfig
    , runForm
    , runFormWith
    ) where

--------------------------------------------------------------------------------
import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Data.Maybe (catMaybes, fromMaybe)
import System.Directory (copyFile, getTemporaryDirectory)
import System.FilePath (takeFileName, (</>))
import qualified Data.Map as M


--------------------------------------------------------------------------------
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Snap.Core as Snap
import qualified Snap.Util.FileUploads as Snap


--------------------------------------------------------------------------------
import Text.Digestive.Form
import Text.Digestive.Form.Encoding
import Text.Digestive.Types
import Text.Digestive.View


--------------------------------------------------------------------------------
type SnapPartPolicy = Snap.PartInfo -> Snap.PartUploadPolicy


--------------------------------------------------------------------------------
data SnapFormConfig = SnapFormConfig
    { -- | Can be used to override the method detected by Snap, in case you e.g.
      -- want to perform a 'postForm' even in case of a GET request.
      method             :: Maybe Method
    , temporaryDirectory :: Maybe FilePath
    , uploadPolicy       :: Snap.UploadPolicy
    , partPolicy         :: SnapPartPolicy
    }


--------------------------------------------------------------------------------
defaultSnapFormConfig :: SnapFormConfig
defaultSnapFormConfig = SnapFormConfig
    { method             = Nothing
    , temporaryDirectory = Nothing
    , uploadPolicy       = Snap.defaultUploadPolicy
    , partPolicy         = const $ Snap.allowWithMaximumSize (128 * 1024)
    }


--------------------------------------------------------------------------------
snapEnv :: Snap.MonadSnap m => [(Text, FilePath)] -> Env m

-- This attempts to look up exact values from the environment
snapEnv allFiles path@(ActualPath _) = do
    inputs <- map (TextInput . T.decodeUtf8) . findParams <$> Snap.getParams
    let files = map (FileInput . snd) $ filter ((== name) . fst) allFiles
    return $ inputs ++ files
  where
    findParams = fromMaybe [] . M.lookup (T.encodeUtf8 name)
    name       = fromPath path

-- This attempts to lookup a 'MetaPath', which indicates a container
snapEnv allFiles path@(MetaPath _) = do
    let filePaths = map fst allFiles
    fieldPaths <- map T.decodeUtf8 . M.keys <$> Snap.getParams
    return $ return . Container . pred . length . catMaybes . map mapIndex $
      fieldPaths ++ filePaths
  where
    formattedPath = T.append (fromPath path) "."
    mapIndex f = case T.stripPrefix formattedPath f of
      Just t | not (T.null t) -> Just $ fst (T.breakOn "." t)
      _                       -> Nothing


--------------------------------------------------------------------------------
-- | Deals with uploaded files, by placing each file in the temporary directory
-- specified in the configuration. It returns a mapping of names to the
-- temporary files.
snapFiles :: Snap.MonadSnap m => SnapFormConfig -> m [(Text, FilePath)]
snapFiles config = do
    -- Get the temporary dir or use the one provided by the OS
    tmpDir <- liftIO $ maybe getTemporaryDirectory return $
        temporaryDirectory config

    -- Actually do the work...
    Snap.handleFileUploads tmpDir (uploadPolicy config) (partPolicy config) $
        fmap catMaybes . mapM (storeFile tmpDir)
  where
    storeFile _   (_,        Left _)     = return Nothing
    storeFile tmp (partinfo, Right path) = do
        let newPath = tmp </> "_" ++ takeFileName path ++
                maybe "" B.unpack (Snap.partFileName partinfo)
        liftIO $ copyFile path newPath
        return $ Just (T.decodeUtf8 $ Snap.partFieldName partinfo, newPath)


--------------------------------------------------------------------------------
-- | Runs a form with the HTTP input provided by Snap.
--
-- Automatically picks between 'getForm' and 'postForm' based on the request
-- method. Set 'method' in the 'SnapFormConfig' to override this behaviour.
runForm :: Snap.MonadSnap m
        => Text                 -- ^ Name for the form
        -> Form v m a           -- ^ Form to run
        -> m (View v, Maybe a)  -- ^ Result
runForm = runFormWith defaultSnapFormConfig


--------------------------------------------------------------------------------
-- | Runs a form with a custom upload policy, and HTTP input from snap.
--
-- Automatically picks between 'getForm' and 'postForm' based on request
-- method. Set 'method' in the 'SnapFormConfig' to override this behaviour.
runFormWith :: Snap.MonadSnap m
            => SnapFormConfig       -- ^ Tempdir and upload policies
            -> Text                 -- ^ Name for the form
            -> Form v m a           -- ^ Form to run
            -> m (View v, Maybe a)  -- ^ Result
runFormWith config name form = do
    m <- maybe snapMethod return (method config)
    case m of
        Get  -> do
            view <- getForm name form
            return (view, Nothing)
        Post -> do
            encType <- formEncType form
            files   <- case encType of
                UrlEncoded -> return []
                MultiPart  -> snapFiles config
            postForm name form (snapEnv files)
  where
    snapMethod        = toMethod . Snap.rqMethod <$> Snap.getRequest
    toMethod Snap.GET = Get
    toMethod _        = Post
