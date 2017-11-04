module Immortelle.CMS.Frontend.Auth(
    authWidget
  , SimpleToken
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson.WithField
import Data.Either
import Data.Functor
import Data.JSString (JSString)
import Data.Maybe
import Data.Text (pack, unpack)
import JavaScript.Web.Storage (localStorage, getItem, setItem, removeItem)
import Reflex.Dom
import Servant.API.Auth.Token
import Servant.Reflex

import qualified Data.JSString as JS

import Web.Reflex.Bootstrap.Focus
import Web.Reflex.Bootstrap.Form
import Web.Reflex.Bootstrap.Modal
import Immortelle.CMS.Frontend.Client
import Immortelle.CMS.Frontend.Utils

authWidget :: forall t m . MonadWidget t m
  => Seconds -- ^ Touch token every n seconds
  -> Event t () -- ^ Manual signout
  -> m (Dynamic t (Maybe SimpleToken))
authWidget touchSecs manualSignoutE = mdo
  -- configure modal form for signing
  ev0' <- delay (realToFrac (0.5 :: Double)) =<< getPostBuild
  let ev0 = fforMaybe ev0' $ const $ case mt0 of -- dont fire if found in local storage
        Nothing -> Just ()
        Just _ -> Nothing
      showE = leftmost [ev0, invalidEvent, manualSignoutE]
      cfg = (def :: ModalConfig t)
        & modalCfgDismiss .~ False
        & modalCfgShow .~ showE
        & modalCfgTitle .~ "Авторизация"
  md <- modal cfg (authForm showE) authQuery

  -- signout request on user desire
  _ <- authSignout $ fmapMaybe id $ current tokenD `tag` manualSignoutE

  -- repetitive touching
  let haveTokenEvent = leftmost [
          fmapMaybe id $ updated tokenD
        , fmapMaybe (const mt0) ev0'
        ]
  dynInvalidEvent <- widgetHold (pure never) $ authTouchWidget touchSecs <$> haveTokenEvent
  let invalidEvent = switchPromptlyDyn dynInvalidEvent

  -- local storage handling and final return
  mt0 <- liftIO readStoredToken
  performEvent_ $ fforMaybe (updated tokenD) $ fmap $ liftIO . writeStoredToken
  tokenD <- holdDyn mt0 $ modalValue md
  pure tokenD
  where
  authQuery i (dyna, enterE) = do
    -- Setup controls
    acceptEv <- acceptModalBtn "Войти"
    let acceptEv' = leftmost [enterE, dyna `tagPromptlyDyn` acceptEv]

    -- Perform query
    mresp <- authSignin $ ffor acceptEv' $ \(login, pass) -> (login, pass, prolongedExpire touchSecs)
    resp <- dangerResult mresp
    let tokenEv = Just <$> resp

    -- Hide only when the auth is successful
    modalHideOn i $ ffilter isJust tokenEv
    pure tokenEv

  toMaybe (Left _) = Nothing
  toMaybe (Right x) = Just x

-- | Converts seconds into expiration duration with special time gap for client to
-- perform touch request
prolongedExpire :: Seconds -> Seconds
prolongedExpire n = fromIntegral $ n + 120

-- | Make authorisation form that returns login and password, return event is when user press enter
authForm :: forall t m . MonadWidget t m
  => Event t () -- ^ Show event
  -> ModalId -- ^ Modal id
  -- | Form data and event when user explicitly press login and password
  -> m (Dynamic t (Login, Password), Event t (Login, Password))
authForm showE _ = horizontalForm $ do
  -- layout form
  loginInput <- formGroupText "Пользователь" def
  passInput <- formGroupText "Пароль" def { _textInputConfig_inputType = "password" }
  -- Form output data
  let login = value loginInput
  let pass = value passInput
  let formDataD = zipDynWith (,) login pass
  -- handle focus
  focusE <- delay (realToFrac (0.5 :: Double)) $ const loginInput <$> showE
  focusElement focusE
  -- handle enter
  let enterCode = 13
  let enterE' = ffilter (== enterCode) $ domEvent Keydown passInput :: Event t Word
  let enterE = current formDataD `tag` enterE'
  -- final result
  pure (formDataD, enterE)

-- | Try to sustain existing token every n seconds, if server invalidates the
-- the token, emits event
authTouchWidget :: forall t m . MonadWidget t m
  => Seconds -- ^ Touch token every n seconds
  -> SimpleToken -- ^ Current token
  -> m (Event t ()) -- ^ Our token was invalidated
authTouchWidget n tok = do
  -- check token at start
  buildE <- getPostBuild
  respE <- authTouch $ ffor buildE $ const (prolongedExpire n, tok)
  let startInvalidE = fmap (const ()) $ fmapMaybe reqFailure respE
  -- recursive switch. Switch to same delayedTouch when we are done with current one
  rec dynRes <- widgetHold delayedTouch nextEv'
      let (dynInvalidEv, dynNextEv) = splitDynPure dynRes -- split events from delayedTouch
      let invalidEv = switchPromptlyDyn dynInvalidEv :: Event t () -- flatten
      let nextEv = switchPromptlyDyn dynNextEv :: Event t () -- flatten
      -- define next step, delay a touch again
      let nextEv' = const delayedTouch <$> nextEv :: Event t (m (Event t (), Event t ()))
  return $ leftmost [invalidEv, startInvalidE]
  where
  -- takes event, uses it to delay touch request by n seconds and returns pair of events
  -- whether the request is invalid and whether the request successed.
  delayedTouch :: m (Event t (), Event t ())
  delayedTouch = do
    -- Fires when we need to perform next touch
    e <- getPostBuild
    touchEvent <- delay (fromIntegral n) e

    -- Event with ajax touch request
    respE <- authTouch $ ffor touchEvent $ const (prolongedExpire n, tok)

    -- Fires when remote server invalidated our token
    let invalidatedEv = fmap (const ()) $ fmapMaybe reqFailure respE
    let nextEv = fmap (const ()) $ fmapMaybe reqSuccess respE
    return (invalidatedEv, nextEv)

-- | Hardcoded name of JS storage key for authorisation token
storageTokenKey :: JSString
storageTokenKey = "auth-token"

-- | Read token from local storage
readStoredToken :: IO (Maybe SimpleToken)
readStoredToken = do
  ms <- getItem storageTokenKey localStorage
  pure $ pack . JS.unpack <$> ms

-- | Write token to local storage
writeStoredToken :: SimpleToken -> IO ()
writeStoredToken t = setItem storageTokenKey (JS.pack . unpack $ t) localStorage

-- | Clear token in local storage
removeStoredToken :: IO ()
removeStoredToken = removeItem storageTokenKey localStorage
