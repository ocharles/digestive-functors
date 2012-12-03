{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Types
    ( Result (..)
    , resultMapError
    , Path(..)
    , PathElement(..)
    , unPathElement
    , pathComponents
    , toPath
    , fromPath
    , Method (..)
    , FormInput (..)
    , Env
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative (Applicative(..))
import           Data.Char           (isDigit)
import           Data.Monoid         (Monoid(..), mappend)
import           Data.String

--------------------------------------------------------------------------------
import           Data.Text           (Text)
import qualified Data.Text           as T


--------------------------------------------------------------------------------
-- | A mostly internally used type for representing Success/Error, with a
-- special applicative instance
data Result v a
    = Success a
    | Error v
    deriving (Show)


--------------------------------------------------------------------------------
instance Functor (Result v) where
    fmap f (Success x) = Success (f x)
    fmap _ (Error x)   = Error x


--------------------------------------------------------------------------------
instance Monoid v => Applicative (Result v) where
    pure x                  = Success x
    Error x   <*> Error y   = Error $ mappend x y
    Error x   <*> Success _ = Error x
    Success _ <*> Error y   = Error y
    Success x <*> Success y = Success (x y)


--------------------------------------------------------------------------------
instance Monad (Result v) where
    return x          = Success x
    (Error x)   >>= _ = Error x
    (Success x) >>= f = f x


--------------------------------------------------------------------------------
-- | Map over the error type of a 'Result'
resultMapError :: (v -> w) -> Result v a -> Result w a
resultMapError f (Error x)   = Error (f x)
resultMapError _ (Success x) = Success x


--------------------------------------------------------------------------------
-- | Describes a path to a subform
data Path = ActualPath [PathElement] | MetaPath [PathElement]
  deriving (Eq, Show)

instance Monoid Path where
  mempty = ActualPath []
  mappend (ActualPath a) (ActualPath b) = ActualPath (a ++ b)
  mappend (MetaPath a) (MetaPath b) = MetaPath (a ++ b)
  mappend (ActualPath a) (MetaPath b) = MetaPath (a ++ b)
  mappend (MetaPath a) (ActualPath b) = ActualPath (a ++ b)

pathComponents :: Path -> [PathElement]
pathComponents (ActualPath p) = p
pathComponents (MetaPath p) = p


--------------------------------------------------------------------------------
-- | Describes a single element of a 'Path'.
data PathElement = Path Text | Index Int
  deriving (Eq, Show)

instance IsString PathElement where
  fromString = Path . fromString


unPathElement :: PathElement -> Text
unPathElement (Path t) = t
unPathElement (Index i) = T.pack $ show i

--------------------------------------------------------------------------------
-- | Create a 'Path' from some text
toPath :: Text -> Path
toPath = ActualPath . map toPathElement . filter (not . T.null) . T.split (== '.')
  where
    toPathElement p = let s = T.unpack p
                      in if and (map isDigit s) then Index (read s)
                         else Path p


--------------------------------------------------------------------------------
-- | Serialize a 'Path' to 'Text'
fromPath :: Path -> Text
fromPath = T.intercalate "." . map pathComponent . pathComponents
  where
    pathComponent (Path p) = p
    pathComponent (Index i) = T.pack $ show i


--------------------------------------------------------------------------------
-- | The HTTP methods
data Method = Get | Post
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
-- | The different input types sent by the browser
data FormInput
    = TextInput Text
    | FileInput FilePath
    | Container Int
    deriving (Show)


--------------------------------------------------------------------------------
-- | An environment (e.g. a server) from which we can read input parameters. A
-- single key might be associated with multiple text values (multi-select).
type Env m = Path -> m [FormInput]
