{-# LANGUAGE OverloadedStrings #-}
module Futurice.Time.Month (
    MonthName (..),
    Month (..),
    -- * Functions
    dayToMonth,
    firstDayOfMonth,
    lastDayOfMonth,
    ) where

import Prelude ()
import Prelude.Compat
import Control.DeepSeq  (NFData (..))
import Control.Lens     ((&), (.~), (?~))
import Data.Aeson
       (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), withText)
import Data.Aeson.Types (FromJSONKeyFunction (..), ToJSONKeyFunction (..))
import Data.Bifunctor   (first)
import Data.Hashable    (Hashable (..))
import Data.String      (fromString)
import Data.Swagger     (ToParamSchema (..), ToSchema (..))
import Data.Time        (Day, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Typeable    (Typeable)
import GHC.Generics     (Generic)
import Test.QuickCheck  (Arbitrary (..), arbitraryBoundedEnum)
import Web.HttpApiData  (FromHttpApiData (..), ToHttpApiData (..))

import qualified Data.Aeson.Encoding  as Aeson.Encoding
import qualified Data.Attoparsec.Text as AT
import qualified Data.Swagger         as Swagger
import qualified Data.Text            as T
import qualified Data.Time.Parsers    as Parsers

data MonthName
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Bounded)

instance Hashable MonthName
instance NFData MonthName

instance Enum MonthName where
    fromEnum January   = 1
    fromEnum February  = 2
    fromEnum March     = 3
    fromEnum April     = 4
    fromEnum May       = 5
    fromEnum June      = 6
    fromEnum July      = 7
    fromEnum August    = 8
    fromEnum September = 9
    fromEnum October   = 10
    fromEnum November  = 11
    fromEnum December  = 12

    toEnum 1  = January
    toEnum 2  = February
    toEnum 3  = March
    toEnum 4  = April
    toEnum 5  = May
    toEnum 6  = June
    toEnum 7  = July
    toEnum 8  = August
    toEnum 9  = September
    toEnum 10 = October
    toEnum 11 = November
    toEnum 12 = December
    toEnum _  = error "toEnum @Month: out-of-range"

instance Arbitrary MonthName where
    arbitrary = arbitraryBoundedEnum
    shrink January = []
    shrink m       = [January .. pred m]

data Month = Month { monthYear :: !Integer, monthName :: !MonthName }
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Hashable Month
instance NFData Month where rnf (Month _ _) = ()

instance Enum Month where
    succ (Month y December) = Month (y + 1) January
    succ (Month y m)        = Month y (succ m)

    pred (Month y January) = Month (y - 1) December
    pred (Month y m)       = Month y (pred m)

    fromEnum (Month y m) = fromIntegral y * 12 + fromEnum m - 1
    toEnum i =
        let (y, m) = divMod i 12
        in Month (fromIntegral y) (toEnum $ m + 1)

-- | TODO: use builder if we really want speed
instance ToJSON Month where
    toJSON = fromString . monthToString
    toEncoding = Aeson.Encoding.string . monthToString

instance FromJSON Month where
    parseJSON = withText "Month" $
        either fail pure . AT.parseOnly (mkMonth <$> Parsers.month)

instance ToJSONKey Month where
    toJSONKey = ToJSONKeyText
        (fromString . monthToString)
        (Aeson.Encoding.string . monthToString)

instance FromJSONKey Month where
    fromJSONKey = FromJSONKeyTextParser $
        either fail pure . AT.parseOnly (mkMonth <$> Parsers.month)

instance ToSchema Month where
    declareNamedSchema _ = pure $ Swagger.NamedSchema (Just "Month") $ mempty
        & Swagger.type_ .~ Swagger.SwaggerString
        & Swagger.format ?~ "month"

-- | Format @"month"@ corresponds to @yyyy-mm@ format.
instance ToParamSchema Month where
  toParamSchema _ = mempty
      & Swagger.type_  .~ Swagger.SwaggerString
      & Swagger.format ?~ "month"

instance ToHttpApiData Month where
    toUrlPiece = fromString . monthToString

instance FromHttpApiData Month where
    parseUrlPiece = first T.pack . AT.parseOnly (mkMonth <$> Parsers.month)

instance Arbitrary Month where
    arbitrary = mk <$> arbitrary <*> arbitrary
      where
        mk y m = Month (y + 2016) m

    shrink (Month y m) =
        [ Month (y' + 2016) m | y' <- shrink (y - 2016) ] ++
        [ Month y m' | m' <- shrink m ]

-------------------------------------------------------------------------------
-- functions
-------------------------------------------------------------------------------

dayToMonth :: Day -> Month
dayToMonth d =
    let (y, m, _) = toGregorian d
    in mkMonth (y, m)

firstDayOfMonth :: Month -> Day
firstDayOfMonth (Month y m) = fromGregorian y (fromEnum m) 1

lastDayOfMonth :: Month -> Day
lastDayOfMonth (Month y m) = fromGregorian y m' (gregorianMonthLength y m')
  where
    m' = fromEnum m

mkMonth :: (Integer, Int) -> Month
mkMonth (y, m) = Month y (toEnum m)

monthToString :: Month -> String
monthToString (Month y October)  = show y ++ "-10"
monthToString (Month y November) = show y ++ "-11"
monthToString (Month y December) = show y ++ "-12"
monthToString (Month y m)        = show y ++ "-0" ++ show (fromEnum m)
