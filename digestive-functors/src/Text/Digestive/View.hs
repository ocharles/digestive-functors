--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Text.Digestive.View
    ( View (..)

      -- * Obtaining a view
    , getForm
    , postForm

      -- * Operations on views
    , subView
    , subViews

      -- * Querying a view
      -- ** Low-level
    , absolutePath
    , absoluteRef

      -- ** Form encoding
    , viewEncType

      -- ** Input
    , fieldInputText
    , fieldInputChoice
    , fieldInputBool
    , fieldInputFile
    , fieldInputElements

      -- ** Errors
    , errors
    , childErrors
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                (second)
import           Control.Monad.Identity       (Identity)
import           Data.List                    (isPrefixOf)
import           Data.Monoid                  (mappend, mempty)
import           Data.Text                    (Text)
import qualified Data.Text                    as T


--------------------------------------------------------------------------------
import           Text.Digestive.Field
import           Text.Digestive.Form.Encoding
import           Text.Digestive.Form.Internal
import           Text.Digestive.Types


--------------------------------------------------------------------------------
data View v = forall a m. Monad m => View
    { viewName    :: Text
    , viewContext :: Path
    , viewForm    :: FormTree Identity v m a
    , viewInput   :: [(Path, FormInput)]
    , viewErrors  :: [(Path, v)]
    , viewMethod  :: Method
    }


--------------------------------------------------------------------------------
instance Functor View where
    fmap f (View name ctx form input errs method) = View
        name ctx (formMapView f form) input (map (second f) errs) method


--------------------------------------------------------------------------------
instance Show v => Show (View v) where
    show (View name ctx form input errs method) =
        "View " ++ show name ++ " " ++ show ctx ++ " " ++ show form ++ " " ++
        show input ++ " " ++ show errs ++ " " ++ show method


--------------------------------------------------------------------------------
getForm :: Monad m => Text -> Form v m a -> m (View v)
getForm name form = do
    form' <- toFormTree form
    return $ View name mempty form' [] [] Get


--------------------------------------------------------------------------------
postForm :: Monad m => Text -> Form v m a -> Env m -> m (View v, Maybe a)
postForm name form env = do
    form' <- toFormTree form
    eval Post env' form' >>= \(r, inp) -> return $ case r of
        Error errs -> (View name mempty form' inp errs Post, Nothing)
        Success x  -> (View name mempty form' inp [] Post, Just x)
  where
    env' = env . mappend (ActualPath [Path name])


--------------------------------------------------------------------------------
subView :: Text -> View v -> View v
subView ref (View name ctx form input errs method) =
    View name (ctx `mappend` path) form input errs method
  where
    path = toPath ref


--------------------------------------------------------------------------------
-- | Returns all immediate subviews of a view
subViews :: View v -> [View v]
subViews view@(View _ ctx form _ _ _) =
    [subView (fromPath $ ActualPath [r]) view | f <- lookupForm ctx form, r <- go f]
  where
    go (SomeForm f) = case getRef f of
        Nothing -> [r | c <- children f, r <- go c]
        Just r  -> [r]


--------------------------------------------------------------------------------
-- | Determine an absolute 'Path' for a field in the form
absolutePath :: Text -> View v -> Path
absolutePath ref view@(View name _ _ _ _ _) =
  ActualPath [Path name] `mappend` viewPath ref view


--------------------------------------------------------------------------------
-- | Determine an absolute path and call 'fromPath' on it. Useful if you're
-- writing a view library...
absoluteRef :: Text -> View v -> Text
absoluteRef ref view = fromPath $ absolutePath ref view


--------------------------------------------------------------------------------
-- | Internal version of 'absolutePath' which does not take the form name into
-- account
viewPath :: Text -> View v -> Path
viewPath ref (View _ ctx _ _ _ _) = ctx `mappend` toPath ref


--------------------------------------------------------------------------------
viewEncType :: View v -> FormEncType
viewEncType (View _ _ form _ _ _) = formTreeEncType form


--------------------------------------------------------------------------------
lookupInput :: Path -> [(Path, FormInput)] -> [FormInput]
lookupInput path = map snd . filter ((== path) . fst)


--------------------------------------------------------------------------------
fieldInputText :: forall v. Text -> View v -> Text
fieldInputText ref view@(View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = viewPath ref view
    givenInput = lookupInput path input

    eval' :: Field v b -> Text
    eval' field = case field of
        Text t -> evalField method givenInput (Text t)
        f      -> error $ T.unpack ref ++ ": expected (Text _), " ++
            "but got: (" ++ show f ++ ")"


--------------------------------------------------------------------------------
-- | Returns a list of (identifier, view, selected?)
fieldInputChoice :: Text -> View v -> [(PathElement, v, Bool)]
fieldInputChoice ref view@(View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = viewPath ref view
    givenInput = lookupInput path input

    eval' :: Field v b -> [(PathElement, v, Bool)]
    eval' field = case field of
        Choice xs didx ->
            let idx = snd $ evalField method givenInput (Choice xs didx)
            in map (\(i, (k, (_, v))) -> (k, v, i == idx)) $ zip [0 ..] xs
        f           -> error $ T.unpack ref ++ ": expected (Choice _ _), " ++
            "but got: (" ++ show f ++ ")"


--------------------------------------------------------------------------------
fieldInputBool :: forall v. Text -> View v -> Bool
fieldInputBool ref view@(View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = viewPath ref view
    givenInput = lookupInput path input

    eval' :: Field v b -> Bool
    eval' field = case field of
        Bool x -> evalField method givenInput (Bool x)
        f      -> error $ T.unpack ref ++ ": expected (Bool _), " ++
            "but got: (" ++ show f ++ ")"


--------------------------------------------------------------------------------
fieldInputFile :: forall v. Text -> View v -> Maybe FilePath
fieldInputFile ref view@(View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = viewPath ref view
    givenInput = lookupInput path input

    eval' :: Field v b -> Maybe FilePath
    eval' field = case field of
        File -> evalField method givenInput File
        f    -> error $ T.unpack ref ++ ": expected (File), " ++
            "but got: (" ++ show f ++ ")"


--------------------------------------------------------------------------------
fieldInputElements :: Text -> View v -> Maybe Int
fieldInputElements ref view@(View _ _ _ input _ _) =
    case givenInput of
      (Container n : _) -> Just n
      _ -> Nothing
  where
    path       = viewPath ref view
    givenInput = lookupInput path input


--------------------------------------------------------------------------------
errors :: Text -> View v -> [v]
errors ref view = map snd $ filter ((== viewPath ref view) . fst) $
    viewErrors view


--------------------------------------------------------------------------------
childErrors :: Text -> View v -> [v]
childErrors ref view = map snd $
    filter ((pathComponents (viewPath ref view) `isPrefixOf`) . pathComponents . fst) $ viewErrors view
