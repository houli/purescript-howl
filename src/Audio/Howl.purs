module Audio.Howl
 ( HOWL
 , Howlable
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

import Control.Monad.Eff (Eff, kind Effect)
import Data.Array (fromFoldable)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty)
import Data.StrMap (StrMap)
import Unsafe.Coerce (unsafeCoerce)

foreign import data HOWL :: Effect

foreign import data Howlable :: Type
foreign import data Howl :: Type
foreign import data SpriteHowl :: Type

class IsHowlable a where
  toHowl :: a -> Howlable

instance isHowlableHowl :: IsHowlable Howl where
  toHowl = unsafeCoerce

instance isHowlableSpriteHowl :: IsHowlable SpriteHowl where
  toHowl = unsafeCoerce

newtype Sprite = Sprite (StrMap {start :: Int, end :: Int})
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

foreign import newImpl :: forall eff. Array String -> Options -> Eff (howl :: HOWL | eff) Howl
new :: forall eff. AudioSource -> Options -> Eff (howl :: HOWL | eff) Howl
new = newImpl <<< fromFoldable <<< unwrap

foreign import newSpriteImpl :: forall eff. Array String -> StrMap (Array Int) -> Options -> Eff (howl :: HOWL | eff) SpriteHowl
newSprite :: forall eff. AudioSource -> Sprite -> Options -> Eff (howl :: HOWL | eff) SpriteHowl
newSprite as sprite = newSpriteImpl ((fromFoldable <<< unwrap) as) (map (\x -> [x.start, x.end]) (unwrap sprite))

foreign import play :: forall eff. Howl -> Eff (howl :: HOWL | eff) Unit
foreign import playSprite :: forall eff. SpriteHowl -> String -> Eff (howl :: HOWL | eff) Unit

foreign import pauseImpl :: forall eff. Howlable -> Eff (howl :: HOWL | eff) Unit
pause :: forall a eff. IsHowlable a => a -> Eff (howl :: HOWL | eff) Unit
pause = pauseImpl <<< toHowl

foreign import stopImpl :: forall eff. Howlable -> Eff (howl :: HOWL | eff) Unit
stop :: forall a eff. IsHowlable a => a -> Eff (howl :: HOWL | eff) Unit
stop = stopImpl <<< toHowl

foreign import muteImpl :: forall eff. Howlable -> Eff (howl :: HOWL | eff) Unit
mute :: forall a eff. IsHowlable a => a -> Eff (howl :: HOWL | eff) Unit
mute = muteImpl <<< toHowl

foreign import unmuteImpl :: forall eff. Howlable -> Eff (howl :: HOWL | eff) Unit
unmute :: forall a eff. IsHowlable a => a -> Eff (howl :: HOWL | eff) Unit
unmute = unmuteImpl <<< toHowl

foreign import volumeImpl :: forall eff. Howlable -> Number -> Eff (howl :: HOWL | eff) Unit
volume :: forall a eff. IsHowlable a => a -> Number -> Eff (howl :: HOWL | eff) Unit
volume = volumeImpl <<< toHowl

foreign import rateImpl :: forall eff. Howlable -> Number -> Eff (howl :: HOWL | eff) Unit
rate :: forall a eff. IsHowlable a => a -> Number -> Eff (howl :: HOWL | eff) Unit
rate = rateImpl <<< toHowl

foreign import seekImpl :: forall eff. Howlable -> Number -> Eff (howl :: HOWL | eff) Unit
seek :: forall a eff. IsHowlable a => a -> Number -> Eff (howl :: HOWL | eff) Unit
seek = seekImpl <<< toHowl

foreign import loopImpl :: forall eff. Howlable -> Boolean -> Eff (howl :: HOWL | eff) Unit
loop :: forall a eff. IsHowlable a => a -> Boolean -> Eff (howl :: HOWL | eff) Unit
loop = loopImpl <<< toHowl

foreign import playingImpl :: forall eff. Howlable -> Eff (howl :: HOWL | eff) Boolean
playing :: forall a eff. IsHowlable a => a -> Eff (howl :: HOWL | eff) Boolean
playing = playingImpl <<< toHowl
