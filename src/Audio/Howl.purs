module Audio.Howl
 ( Howlable
 , Howl
 , SpriteHowl
 , class IsHowlable
 , toHowl
 , Sprite(..)
 , AudioSource(..)
 , Options
 , defaultOptions
 , new
 , newSprite
 , play
 , playSprite
 , pause
 , stop
 , mute
 , unmute
 , volume
 , rate
 , seek
 , loop
 , playing
 ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty)
import Effect (Effect)
import Foreign.Object (Object)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Howlable :: Type
foreign import data Howl :: Type
foreign import data SpriteHowl :: Type

class IsHowlable a where
  toHowl :: a -> Howlable

instance isHowlableHowl :: IsHowlable Howl where
  toHowl = unsafeCoerce

instance isHowlableSpriteHowl :: IsHowlable SpriteHowl where
  toHowl = unsafeCoerce

newtype Sprite = Sprite (Object {start :: Int, end :: Int})
derive instance newtypeSprite :: Newtype Sprite _

newtype AudioSource = AudioSource (NonEmpty Array String)
derive instance newtypeAudioSource :: Newtype AudioSource _

type Options =
  { volume :: Number
  , html5 :: Boolean
  , loop :: Boolean
  , autoplay :: Boolean
  , rate :: Number
  , pool :: Int
  }

defaultOptions :: Options
defaultOptions =
  { volume: 1.0
  , html5: false
  , loop: false
  , autoplay: false
  , rate: 1.0
  , pool: 5
  }

foreign import newImpl :: Array String -> Options -> Effect Howl
new :: AudioSource -> Options -> Effect Howl
new = newImpl <<< fromFoldable <<< unwrap

foreign import newSpriteImpl :: Array String -> Object (Array Int) -> Options -> Effect SpriteHowl
newSprite :: AudioSource -> Sprite -> Options -> Effect SpriteHowl
newSprite as sprite = newSpriteImpl ((fromFoldable <<< unwrap) as) (map (\x -> [x.start, x.end]) (unwrap sprite))

foreign import play :: Howl -> Effect Unit
foreign import playSprite :: SpriteHowl -> String -> Effect Unit

foreign import pauseImpl :: Howlable -> Effect Unit
pause :: forall a. IsHowlable a => a -> Effect Unit
pause = pauseImpl <<< toHowl

foreign import stopImpl :: Howlable -> Effect Unit
stop :: forall a. IsHowlable a => a -> Effect Unit
stop = stopImpl <<< toHowl

foreign import muteImpl :: Howlable -> Effect Unit
mute :: forall a. IsHowlable a => a -> Effect Unit
mute = muteImpl <<< toHowl

foreign import unmuteImpl :: Howlable -> Effect Unit
unmute :: forall a. IsHowlable a => a -> Effect  Unit
unmute = unmuteImpl <<< toHowl

foreign import volumeImpl :: Howlable -> Number -> Effect Unit
volume :: forall a. IsHowlable a => a -> Number -> Effect Unit
volume = volumeImpl <<< toHowl

foreign import rateImpl :: Howlable -> Number -> Effect Unit
rate :: forall a. IsHowlable a => a -> Number -> Effect Unit
rate = rateImpl <<< toHowl

foreign import seekImpl :: Howlable -> Number -> Effect Unit
seek :: forall a. IsHowlable a => a -> Number -> Effect Unit
seek = seekImpl <<< toHowl

foreign import loopImpl :: Howlable -> Boolean -> Effect Unit
loop :: forall a. IsHowlable a => a -> Boolean -> Effect Unit
loop = loopImpl <<< toHowl

foreign import playingImpl :: Howlable -> Effect Boolean
playing :: forall a. IsHowlable a => a -> Effect Boolean
playing = playingImpl <<< toHowl
