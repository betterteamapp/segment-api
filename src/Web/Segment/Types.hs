{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Segment.Types where
import           Control.Applicative
import           Control.Monad             (guard, mzero)
import           Currency                  (ISO4217Currency)
import           Data.Aeson                (FromJSON, ToJSON, Value (..), Object,
                                            object, parseJSON, toJSON, (.:),
                                            (.=))
import           Data.Aeson.QQ             (aesonQQ)
import           Data.Aeson.TH
import           Data.Aeson.Types          (typeMismatch)
import qualified Data.ByteString.Char8     as BS
import           Data.Decimal              (Decimal)
import qualified Data.HashMap.Lazy         as HM
import           Data.IP
import qualified Data.ISO3166_CountryCodes as CC
import           Data.Maybe                (catMaybes)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Data.Time                 (UTCTime)
import           Data.Time.Calendar        (Day)
import           Data.Typeable
import           Data.UUID                 (UUID)
import qualified Data.UUID                 as UUID
import           Data.Version              (showVersion)
import           GHC.Generics              (Generic)
import           Network.URI               (URI, parseURI)
import           Paths_segment_api         (version)
import           Text.Email.Validate       (EmailAddress, toByteString, validate)
import           Text.Read                 (readMaybe)
import           Web.Segment.TH

newtype Currency = Currency { iso4217 :: ISO4217Currency }
  deriving (Show, Eq)

instance ToJSON Currency where
  toJSON = toJSON . show . iso4217

instance FromJSON Currency where
  parseJSON (String c) = case readMaybe (T.unpack c) of
    Nothing -> fail "Invalid currency"
    Just curr -> return $ Currency curr
  parseJSON invalid = typeMismatch "Currency" invalid

newtype URL = URL { uri :: URI }
  deriving (Show, Eq)

instance ToJSON URL where
  toJSON (URL u) = toJSON $ show u

instance FromJSON URL where
  parseJSON (String t) = let str = T.unpack t in case parseURI str of
    Nothing -> fail "Malformed URI"
    Just u -> return $ URL u
  parseJSON invalid = typeMismatch "URI" invalid

-- Anonymous should really be semi-anonymous - it's for when you have some info about the User, like a session ID,
-- but don't have a strong guarantee like a database Id or UUID.
--
-- these can be used just about anywhere - is legit to alias an
-- Anonymous ID to another Anonymous ID, for instance.
data SegmentId
  = Anonymous Text
  | IdentifiedUser Text
  deriving (Generic, Eq, Show, Typeable)

injectSegmentId :: SegmentId -> Object -> Object
injectSegmentId s o = HM.insert k (String v) o
  where
    (k, v) = case s of
      Anonymous t -> ("anonymousId", t)
      IdentifiedUser t -> ("userId", t)

idBase :: SegmentId -> Object
idBase = flip injectSegmentId HM.empty

instance ToJSON SegmentId where
  toJSON = Object . idBase

instance FromJSON SegmentId where
  parseJSON (Object o) = (IdentifiedUser <$> o .: "userId") <|> (Anonymous <$> o .: "anonymousId")
  parseJSON invalid = typeMismatch "SegmentId" invalid

data App = App
  { apName    :: Text
  , apVersion :: Text
  , apBuild   :: Text
  }  deriving (Generic, Eq, Show, Typeable)
-- FIX need standard field-munging machinery

deriveJSON (jsonOpts 2) ''App

data Campaign = Campaign
  { caName    :: Maybe Text
  , caSource  :: Maybe Text
  , caMedium  :: Maybe Text
  , caTerm    :: Maybe Text
  , caContent :: Maybe Text
  }  deriving (Generic, Eq, Show, Typeable)

deriveJSON (jsonOpts 2) ''Campaign

data Device = Device
  { deId           :: Maybe Text
  , deManufacturer :: Maybe Text
  , deModel        :: Maybe Text
  , deName         :: Maybe Text
  , deType         :: Maybe Text
  , deVersion      :: Maybe Text
  }  deriving (Generic, Eq, Show, Typeable)

deriveJSON (jsonOpts 2) ''Device

data Library = Library
  { liName    :: Text
  , liVersion :: Text
  } deriving (Generic, Eq, Show, Typeable)

deriveJSON (jsonOpts 2) ''Library

-- newtype to avoid needing to define Aeson instances for a datatype
-- we don't own
newtype OurCountryCode = OurCountryCode { unOurCountryCode :: CC.CountryCode }
  deriving (Eq,Show,Ord)

-- | to json: as a simple string
instance ToJSON OurCountryCode where
  toJSON = toJSON . CC.countryNameFromCode . unOurCountryCode

-- | from json: as a simple string
instance FromJSON OurCountryCode where
  parseJSON (String s)
    | Just a <- readMaybe (T.unpack s) = pure (OurCountryCode a)
  parseJSON _ = fail "CountryCode"

-- Unclear which of these are actually mandatory.
-- region is not in the example, so assuming it's optional.
data Location = Location
  { loCity      :: Maybe Text
  , loCountry   :: Maybe OurCountryCode
  , loLatitude  :: Maybe Double
  , loLongitude :: Maybe Double
  , loRegion    :: Maybe Text
  , loSpeed     :: Maybe Double
  } deriving (Generic, Eq, Show, Typeable)

deriveJSON (jsonOpts 2) ''Location

data Network = Network
  { neBluetooth :: Maybe Bool
  , neCellular  :: Maybe Bool
  , neWifi      :: Maybe Bool
  -- can this be null?
  , neCarrier   :: Maybe Text
  }  deriving (Generic, Eq, Show, Typeable)

deriveJSON (jsonOpts 2) ''Network

data OS = OS
  { osName    :: Maybe Text
  , osVersion :: Maybe Text
  } deriving (Generic, Eq, Show, Typeable)

deriveJSON (jsonOpts 2) ''OS

newtype City = City Text
 deriving (Generic, Eq, Show, Typeable, ToJSON, FromJSON)
newtype StateLoc = StateLoc Text
 deriving (Generic, Eq, Show, Typeable, ToJSON, FromJSON)
newtype Street = Street Text
 deriving (Generic, Eq, Show, Typeable, ToJSON, FromJSON)
newtype Postcode = Postcode Text
 deriving (Generic, Eq, Show, Typeable, ToJSON, FromJSON)

data Address = Address
  { adCity     :: Maybe City
  , adCountry  :: Maybe OurCountryCode
  , adPostcode :: Maybe Postcode
  , adState    :: Maybe StateLoc
  , adStreet   :: Maybe Street
  } deriving (Generic, Eq, Show, Typeable)

deriveJSON (jsonOpts 2) ''Address

data CommonMsg =
  CommonMsg
  { -- time when queued
    cmTimestamp :: UTCTime

  -- TODO lots more fields https://segment.com/docs/spec/common/
  } deriving (Generic, Eq, Show, Typeable)

{-
  -- optional fields
  , cmActive    :: Maybe Bool
  , cmApp       :: Maybe App
  , cmCampaign  :: Maybe Campaign
  , cmDevice    :: Maybe Device
  , cmIP        :: Maybe IP
  , cmLibrary   :: Maybe Library
  , cmLocation  :: Maybe Location
  , cmNetwork   :: Maybe Network
  , cmOS        :: Maybe OS
  , cmUserAgent :: Maybe Text
-}

type Context = Freeform

type Freeform = HM.HashMap Text Value
emptyFreeform = HM.empty

data SegmentResponse = SegmentResponse
  deriving (Eq,Show)

instance FromJSON SegmentResponse where
  parseJSON (Object o) = do
    success <- o .: "success"
    guard (success == True)
    return $ SegmentResponse
  parseJSON _ = mzero

-- | The sent-at field should be applied as close to send-time as
--   possible - the supplied recordEvent function will add the time.
data BatchedMsg
  = BatchedMsg UUID [Message Object] UTCTime
  deriving (Generic, Eq, Show, Typeable)


data Message a = Message UTCTime Context {- TODO add integration object -} a
  deriving (Generic, Eq, Show, Typeable)

class SegmentMsg m where
  segmentType :: m -> Text
  segmentObject :: m -> Object

newtype Email = Email { emailAddress :: EmailAddress }
  deriving (Show, Eq)

instance ToJSON Email where
  toJSON (Email e) = String $ T.decodeUtf8 $ toByteString e

instance FromJSON Email where
  parseJSON (String et) = case validate $ T.encodeUtf8 et of
    Left err -> fail err
    Right e -> return $ Email e
  parseJSON invalid = typeMismatch "Email" invalid

data PageProperties = PageProperties
  { ppName     :: Maybe Text
  , ppPath     :: Maybe Text
  , ppReferrer :: Maybe URL
  , ppSearch   :: Maybe Text
  , ppTitle    :: Maybe Text
  , ppUrl      :: Maybe URL
  } deriving (Generic, Eq, Show, Typeable)

deriveJSON (jsonOpts 2) ''PageProperties

data Identify = Identify
  { idUserId :: SegmentId
  , idTraits :: Object
  }
  deriving (Generic, Eq, Show, Typeable)

instance SegmentMsg Identify where
  segmentType = const "identify"
  segmentObject ident = injectSegmentId (idUserId ident) $ HM.fromList
    [ "traits" .= idTraits ident
    ]

data Track = Track
  { trUserId     :: SegmentId
  , trEvent      :: Text
  , trProperties :: Object
  }
  deriving (Generic, Eq, Show, Typeable)

instance SegmentMsg Track where
  segmentType = const "track"
  segmentObject msg = injectSegmentId (trUserId msg) $ HM.fromList
    [ "event" .= trEvent msg
    , "properties" .= trProperties msg
    ]

instance ToJSON Track where
  toJSON = Object . segmentObject

data Page = Page
  { paUserId     :: Maybe SegmentId
  , paName       :: Maybe Text
  , paProperties :: Object -- PageProperties
  }
  deriving (Generic, Eq, Show, Typeable)

instance SegmentMsg Page where
  segmentType = const "page"
  segmentObject msg = (maybe id injectSegmentId $ paUserId msg) $ HM.fromList $ catMaybes
    [ ("name" .=) <$> paName msg
    , pure ("properties" .= paProperties msg)
    ]

instance ToJSON Page where
  toJSON = Object . segmentObject

data Screen = Screen
  { scUserId     :: Maybe SegmentId
  , scName       :: Maybe Text
  , scProperties :: Object
  }
  deriving (Generic, Eq, Show, Typeable)

instance SegmentMsg Screen where
  segmentType = const "screen"
  segmentObject s = (maybe id injectSegmentId $ scUserId s) $ HM.fromList $ catMaybes
    [ ("name" .=) <$> scName s
    , pure ("properties" .= scProperties s)
    ]

instance ToJSON Screen where
  toJSON = Object . segmentObject

data Group = Group
  { grUserId  :: SegmentId
  , grGroupId :: Text
  , grTraits  :: Object
  }
  deriving (Generic, Eq, Show, Typeable)

instance SegmentMsg Group where
  segmentType = const "group"
  segmentObject msg = injectSegmentId (grUserId msg) $ HM.fromList
    [ "groupId" .= grGroupId msg
    , "traits" .= grTraits msg
    ]

instance ToJSON Group where
  toJSON = Object . segmentObject

data Alias = Alias
  { alUserId     :: SegmentId
  , alPreviousId :: SegmentId
  }
  deriving (Generic, Eq, Show, Typeable)

instance SegmentMsg Alias where
  segmentType = const "alias"
  segmentObject msg = HM.fromList
    [ "userId" .= case alUserId msg of
        Anonymous t -> t
        IdentifiedUser t -> t
    , "previousId" .= case alPreviousId msg of
        Anonymous t -> t
        IdentifiedUser t -> t
    ]

instance ToJSON Alias where
  toJSON = Object . segmentObject
