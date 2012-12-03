--------------------------------------------------------------------------------
-- | This module contains utilities for creating text fragments to identify
-- forms.
{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Ref
    ( makeRef
    , makeRefs
    ) where


--------------------------------------------------------------------------------
import qualified Data.ByteString    as B
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Text.Digestive.Types (PathElement(..))
import           Text.Printf        (printf)


--------------------------------------------------------------------------------
-- | Convert an arbitrary text value (possibly containing spaces, dots etc. to
-- a text value that safely be used as an identifier in forms.
makeRef :: Text -> PathElement
makeRef =
    -- We simply UTF-8 encode and then hex encode, so all characters are valid.
    Path . T.append "df-" . T.pack . (printf "%02x" =<<) . B.unpack . T.encodeUtf8


--------------------------------------------------------------------------------
-- | Create an infinite list of refs.
makeRefs :: [PathElement]
makeRefs = map (Path . T.pack . show) [0 :: Int ..]
