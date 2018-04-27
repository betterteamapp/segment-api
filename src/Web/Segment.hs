{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DefaultSignatures          #-}
module Web.Segment
  ( module Web.Segment
  , module Web.Segment.Types
  )
  where

import           Cases                      (snakify)
import           Control.Monad              (guard, forM_)
import           Control.Monad.Trans        (MonadIO(..))
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Reaper
import           Currency                   (Currency)
import           Data.Aeson                 (FromJSON, ToJSON, Value (..), Object,
                                             object, toJSON, (.=), encode)
import           Data.Aeson.Types           (Pair)
import           Data.Aeson.TH

import           Data.Aeson.QQ              (aesonQQ)
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Decimal               (Decimal)
import qualified Data.DList                 as DL
import qualified Data.HashMap.Lazy          as HM
import           Data.IP
import           Data.ISO3166_CountryCodes  (CountryCode)
import           Data.Hashable              (Hashable(..))
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Monoid                ((<>))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (UTCTime, getCurrentTime)
import           Data.Time.Calendar         (Day)
import           Data.Typeable
import           Data.Vector                (Vector)
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import           Data.Version               (showVersion)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Network.HTTP.Client.TLS
import           Network.URI                (URI)
import           Network.URI                (uriToString)
import           Paths_segment_api          (version)
import           Servant.API
import           Servant.Client
import           System.IO.Unsafe
import           Text.Email.Validate        (EmailAddress)
import qualified Text.Email.Validate        as TEV
import           Unsafe.Coerce
import           Web.Segment.TH
import           Web.Segment.Types

data SegmentClient = SegmentClient
  { segmentBatchHandler :: Reaper (DL.DList (Message Object)) [Message Object]
  }

newtype Trait a = Trait Text
  deriving (Show, Eq, Ord, Hashable, IsString)

address :: Trait Address
address = Trait "address"

age :: (ToJSON a, Num a) => Trait a
age = Trait "age"

avatar :: Trait URI
avatar = Trait "avatar"

birthday :: Trait UTCTime
birthday = Trait "birthday"

createdAt :: Trait UTCTime
createdAt = Trait "createdAt"

description :: Trait Text
description = Trait "description"

email :: Trait EmailAddress
email = Trait "email"

firstName :: Trait Text
firstName = Trait "firstName"

gender :: Trait Text
gender = Trait "gender"

id_ :: Trait Text
id_ = Trait "id"

lastName :: Trait Text
lastName = Trait "lastName"

name :: Trait Text
name = Trait "name"

phone :: Trait Text
phone = Trait "phone"

title :: Trait Text
title = Trait "title"

username :: Trait Text
username = Trait "username"

website :: Trait URI
website = Trait "website"

type Traits = HM.HashMap (Trait Value) Value

emptyTraits :: Traits
emptyTraits = HM.empty

trait :: (ToJSON a) => Trait a -> a -> Traits -> Traits
trait (Trait k) v = HM.insert (Trait k) $ toJSON v

class EventValue a where
  type EventValueBase a :: *
  type EventValueBase a = a
  getBase :: a -> EventValueBase a
  default getBase :: (EventValueBase a ~ a) => a -> EventValueBase a
  getBase = id

class EmptyEvent a where
  emptyEvent :: a

data ExtraData a = ExtraData a Object
  deriving (Read, Show, Eq)

instance EventValue a => EventValue (ExtraData a) where
  type EventValueBase (ExtraData a) = a
  getBase (ExtraData a _) = a

instance ToJSON a => ToJSON (ExtraData a) where
  toJSON (ExtraData v o) = case toJSON v of
    Object primary -> Object (primary <> o)
    _ -> error "ExtraData json instances requires 'a' to produce object"

-- data Revenue a = Revenue Scientific a
-- data Currency a = Currenct ISO4217Currency a
-- data EventValue a = EventValue Scientific a

newtype Event a = Event Text
  deriving (Read, Show, Eq, IsString)

-- $ Semantic Events
-- ** Mobile Events

data ApplicationInstalled = ApplicationInstalled
  { applicationInstalledVersion :: Maybe Text
  , applicationInstalledBuild :: Maybe Int
  }

deriveJSON (jsonOpts 20) ''ApplicationInstalled

instance EventValue ApplicationInstalled
instance EmptyEvent ApplicationInstalled where
  emptyEvent = ApplicationInstalled Nothing Nothing

applicationInstalled :: Event ApplicationInstalled
applicationInstalled  = "Application Installed"

data ApplicationOpened = ApplicationOpened
  { applicationOpenedFromBackground :: Maybe Bool
  , applicationOpenedUrl :: Maybe URL
  , applicationOpenedSourceApplication :: Maybe Text
  }

deriveJSON (jsonOpts 17) ''ApplicationOpened
instance EventValue ApplicationOpened
instance EmptyEvent ApplicationOpened where
  emptyEvent = ApplicationOpened Nothing Nothing Nothing

applicationOpened :: Event ApplicationOpened
applicationOpened = "Application Opened"

data ApplicationBackgrounded = ApplicationBackgrounded

deriveJSON (jsonOpts 23) ''ApplicationBackgrounded
instance EventValue ApplicationBackgrounded
instance EmptyEvent ApplicationBackgrounded where
  emptyEvent = ApplicationBackgrounded

applicationBackgrounded :: Event ApplicationBackgrounded
applicationBackgrounded = "Application Backgrounded"

data ApplicationUpdated = ApplicationUpdated
  { applicationUpdatedPreviousVersion :: Maybe Text
  , applicationUpdatedPreviousBuild :: Maybe Int
  , applicationUpdatedVersion :: Maybe Text
  , applicationUpdatedBuild :: Maybe Int
  }

deriveJSON (jsonOpts 18) ''ApplicationUpdated
instance EventValue ApplicationUpdated
instance EmptyEvent ApplicationUpdated where
  emptyEvent = ApplicationUpdated Nothing Nothing Nothing Nothing

applicationUpdated :: Event ApplicationUpdated
applicationUpdated = "Application Updated"

data ApplicationCrashed = ApplicationCrashed

deriveJSON (jsonOpts 18) ''ApplicationCrashed
instance EventValue ApplicationCrashed
instance EmptyEvent ApplicationCrashed where
  emptyEvent = ApplicationCrashed

applicationCrashed :: Event ApplicationCrashed
applicationCrashed = "Application Crashed"

data ApplicationUninstalled = ApplicationUninstalled

deriveJSON (jsonOpts 22) ''ApplicationUninstalled
instance EventValue ApplicationUninstalled
instance EmptyEvent ApplicationUninstalled where
  emptyEvent = ApplicationUninstalled

applicationUninstalled :: Event ApplicationUninstalled
applicationUninstalled = "Application Uninstalled"

data PushNotificationCampaign = PushNotificationCampaign
  { pushNotificationCampaignName :: Maybe Text
  , pushNotificationCampaignMedium :: Maybe Text
  , pushNotificationCampaignContent :: Maybe Text
  , pushNotificationCampaignSource :: Maybe Text
  }

deriveJSON (jsonOpts 24) ''PushNotificationCampaign

data PushNotificationReceived = PushNotificationReceived
  { pushNotificationReceivedCampaign :: Maybe PushNotificationCampaign
  }

deriveJSON (jsonOpts 24) ''PushNotificationReceived
instance EventValue PushNotificationReceived
instance EmptyEvent PushNotificationReceived where
  emptyEvent = PushNotificationReceived Nothing

pushNotificationReceived :: Event PushNotificationReceived
pushNotificationReceived = "Push Notification Received"

data PushNotificationTapped = PushNotificationTapped
  { pushNotificationTappedAction :: Maybe Text
  , pushNotificationTappedCampaign :: Maybe PushNotificationCampaign
  }

deriveJSON (jsonOpts 22) ''PushNotificationTapped
instance EventValue PushNotificationTapped
instance EmptyEvent PushNotificationTapped where
  emptyEvent = PushNotificationTapped Nothing Nothing

pushNotificationTapped :: Event PushNotificationTapped
pushNotificationTapped = "Push Notification Tapped"

data PushNotificationBounced = PushNotificationBounced
  { pushNotificationBouncedAction :: Maybe Text
  , pushNotificationBouncedCampaign :: Maybe PushNotificationCampaign
  }

deriveJSON (jsonOpts 23) ''PushNotificationBounced
instance EventValue PushNotificationBounced
instance EmptyEvent PushNotificationBounced where
  emptyEvent = PushNotificationBounced Nothing Nothing

pushNotificationBounced :: Event PushNotificationBounced
pushNotificationBounced = "Push Notification Bounced"

data InstallationCampaign = InstallationCampaign
  { installationCampaignSource :: Maybe Text
  , installationCampaignName :: Maybe Text
  , installationCampaignContent :: Maybe Text
  , installationCampaignAdCreative :: Maybe Text
  , installationCampaignAdGroup :: Maybe Text
  }

deriveJSON (jsonOpts 20) ''InstallationCampaign

data InstallAttributed  = InstallAttributed
  { installAttributedProvider :: Maybe Text
  , installAttributedCampaign :: Maybe InstallationCampaign
  }

deriveJSON (jsonOpts 17) ''InstallAttributed
instance EventValue InstallAttributed
instance EmptyEvent InstallAttributed where
  emptyEvent = InstallAttributed Nothing Nothing

installAttributed :: Event InstallAttributed
installAttributed = "Install Attributed"

data DeepLinkClicked = DeepLinkClicked
  { deepLinkClickedProvider :: Maybe Text
  , deepLinkClickedUrl :: Maybe URL
  }

deriveJSON (jsonOpts 15) ''DeepLinkClicked
instance EventValue DeepLinkClicked
instance EmptyEvent DeepLinkClicked where
  emptyEvent = DeepLinkClicked Nothing Nothing

deepLinkClicked :: Event DeepLinkClicked
deepLinkClicked = "Deep Link Clicked"

data DeepLinkOpened = DeepLinkOpened
  { deepLinkOpenedProvider :: Maybe Text
  , deepLinkOpenedUrl :: Maybe URL
  }

deriveJSON (jsonOpts 14) ''DeepLinkOpened

deepLinkOpened :: Event DeepLinkOpened
deepLinkOpened = "Deep Link Opened"
instance EventValue DeepLinkOpened
instance EmptyEvent DeepLinkOpened where
  emptyEvent = DeepLinkOpened Nothing Nothing

-- ** A/B Testing Events

data ExperimentViewed = ExperimentViewed
  { experimentViewedExperimentId :: Maybe Text
  , experimentViewedExperimentName :: Maybe Text
  , experimentViewedVariationId :: Maybe Text
  , experimentViewedVariationName :: Maybe Text
  }

deriveJSON (jsonOpts 16) ''ExperimentViewed
instance EventValue ExperimentViewed
instance EmptyEvent ExperimentViewed where
  emptyEvent = ExperimentViewed Nothing Nothing Nothing Nothing

experimentViewed :: Event ExperimentViewed
experimentViewed = "Experiment Viewed"

-- ** V2 Ecommerce Events
-- *** Browsing Overview

data ProductSearched
  = ProductSearchedSimpleQuery Text
  | ProductSearchedQuery Object

productsSearched :: Event ProductSearched
productsSearched = "Products Searched"

data ProductListItem = ProductListItem
  { productListItemProductId :: Maybe Text
  , productListItemSku :: Maybe Text
  , productListItemCategory :: Maybe Text
  , productListItemName :: Maybe Text
  , productListItemBrand :: Maybe Text
  , productListItemVariant :: Maybe Text
  , productListItemPrice :: Maybe Double
  , productListItemQuantity :: Maybe Int
  , productListItemCoupon :: Maybe Text
  , productListItemPosition :: Maybe Int
  }

deriveJSON (jsonOpts 15) ''ProductListItem

data ProductListViewed = ProductListViewed
  { productListViewedListId :: Maybe Text
  , productListViewedCategory :: Maybe Text
  , productListViewedProducts :: Vector ProductListItem
  }

deriveJSON (jsonOpts 17) ''ProductListViewed
instance EventValue ProductListViewed
instance EmptyEvent ProductListViewed where
  emptyEvent = ProductListViewed Nothing Nothing mempty

productListViewed :: Event ProductListViewed
productListViewed = "Product List Viewed"

data ProductListFilter = ProductListFilter
  { productListFilterType :: Maybe Text
  , productListFilterValue :: Maybe Text
  }

deriveJSON (jsonOpts 17) ''ProductListFilter
instance EventValue ProductListFilter
instance EmptyEvent ProductListFilter where
  emptyEvent = ProductListFilter Nothing Nothing

data ProductListSort = ProductListSort
  { productListSortType :: Maybe Text
  , productListSortValue :: Maybe Text
  }

deriveJSON (jsonOpts 15) ''ProductListSort

data ProductListFiltered = ProductListFiltered
  { productListFilteredListId :: Maybe Text
  , productListFilteredCategory :: Maybe Text
  , productListFilteredFilters :: Vector ProductListFilter
  , productListFilteredSorts :: Vector ProductListSort
  , productListFilteredProducts :: Vector ProductListItem
  }

deriveJSON (jsonOpts 19) ''ProductListFiltered
instance EventValue ProductListFiltered
instance EmptyEvent ProductListFiltered where
  emptyEvent = ProductListFiltered Nothing mempty mempty mempty mempty

productListFiltered :: Event ProductListFiltered
productListFiltered = "Product List Filtered"

-- *** Promotions Overview

data PromotionViewed = PromotionViewed
  { promotionViewedPromotionId :: Maybe Text
  , promotionViewedCreative :: Maybe Text
  , promotionViewedName :: Maybe Text
  , promotionViewedPosition :: Maybe Text
  }

deriveJSON (jsonOpts 15) ''PromotionViewed
instance EventValue PromotionViewed
instance EmptyEvent PromotionViewed where
  emptyEvent = PromotionViewed Nothing Nothing Nothing Nothing

promotionViewed :: Event PromotionViewed
promotionViewed = "Promotion Viewed"

data PromotionClicked = PromotionClicked
  { promotionClickedPromotionId :: Maybe Text
  , promotionClickedCreative :: Maybe Text
  , promotionClickedName :: Maybe Text
  , promotionClickedPosition :: Maybe Text
  }

deriveJSON (jsonOpts 16) ''PromotionClicked
instance EventValue PromotionClicked
instance EmptyEvent PromotionClicked where
  emptyEvent = PromotionClicked Nothing Nothing Nothing Nothing

promotionClicked :: Event PromotionClicked
promotionClicked = "Promotion Clicked"

-- *** Core Ordering Overview

data ProductClicked = ProductClicked
  { productClickedProductId :: Maybe Text
  , productClickedSku :: Maybe Text
  , productClickedCategory :: Maybe Text
  , productClickedName :: Maybe Text
  , productClickedBrand :: Maybe Text
  , productClickedVariant :: Maybe Text
  , productClickedPrice :: Maybe Double
  , productClickedQuantity :: Maybe Int
  , productClickedCoupon :: Maybe Text
  , productClickedPosition :: Maybe Int
  }

deriveJSON (jsonOpts 14) ''ProductClicked
instance EventValue ProductClicked
instance EmptyEvent ProductClicked where
  emptyEvent = ProductClicked Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

productClicked :: Event ProductClicked
productClicked = "Product Clicked"

data ProductViewed = ProductViewed
  { productViewedProductId :: Maybe Text
  , productViewedSku :: Maybe Text
  , productViewedCategory :: Maybe Text
  , productViewedName :: Maybe Text
  , productViewedBrand :: Maybe Text
  , productViewedVariant :: Maybe Text
  , productViewedPrice :: Maybe Double
  , productViewedQuantity :: Maybe Int
  , productViewedCoupon :: Maybe Text
  , productViewedCurrency :: Maybe Text
  , productViewedPosition :: Maybe Int
  , productViewedValue :: Maybe Double
  }

deriveJSON (jsonOpts 13) ''ProductViewed
instance EventValue ProductViewed
instance EmptyEvent ProductViewed where
  emptyEvent = ProductViewed Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

productViewed :: Event ProductViewed
productViewed = "Product Viewed"

data ProductAdded = ProductAdded
  { productAddedCartId :: Maybe Text
  , productAddedProductId :: Maybe Text
  , productAddedSku :: Maybe Text
  , productAddedCategory :: Maybe Text
  , productAddedName :: Maybe Text
  , productAddedBrand :: Maybe Text
  , productAddedVariant :: Maybe Text
  , productAddedPrice :: Maybe Double
  , productAddedQuantity :: Maybe Int
  , productAddedCoupon :: Maybe Text
  , productAddedPosition :: Maybe Int
  }

deriveJSON (jsonOpts 12) ''ProductAdded
instance EventValue ProductAdded
instance EmptyEvent ProductAdded where
  emptyEvent = ProductAdded Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


productAdded :: Event ProductAdded
productAdded = "Product Added"

data ProductRemoved = ProductRemoved
  { productRemovedCartId :: Maybe Text
  , productRemovedProductId :: Maybe Text
  , productRemovedSku :: Maybe Text
  , productRemovedCategory :: Maybe Text
  , productRemovedName :: Maybe Text
  , productRemovedBrand :: Maybe Text
  , productRemovedVariant :: Maybe Text
  , productRemovedPrice :: Maybe Double
  , productRemovedQuantity :: Maybe Int
  , productRemovedCoupon :: Maybe Text
  , productRemovedPosition :: Maybe Int
  }

deriveJSON (jsonOpts 14) ''ProductRemoved
instance EventValue ProductRemoved
instance EmptyEvent ProductRemoved where
  emptyEvent = ProductRemoved Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


productRemoved :: Event ProductRemoved
productRemoved = "Product Removed"

data CartProduct = CartProduct
  { cartProductProductId :: Maybe Text
  , cartProductSku :: Maybe Text
  , cartProductCategory :: Maybe Text
  , cartProductName :: Maybe Text
  , cartProductBrand :: Maybe Text
  , cartProductVariant :: Maybe Text
  , cartProductPrice :: Maybe Double
  , cartProductQuantity :: Maybe Int
  , cartProductCoupon :: Maybe Text
  , cartProductPosition :: Maybe Int
  }

deriveJSON (jsonOpts 11) ''CartProduct
instance EventValue CartProduct
instance EmptyEvent CartProduct where
  emptyEvent = CartProduct Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data CartViewed = CartViewed
  { cartViewedCartId :: Maybe Text
  , cartViewedProducts :: Vector CartProduct
  }

deriveJSON (jsonOpts 10) ''CartViewed

cartViewed :: Event CartViewed
cartViewed = "Cart Viewed"

data CheckoutStarted = CheckoutStarted
  { checkoutStartedOrderId :: Maybe Text
  , checkoutStartedAffiliation :: Maybe Text
  , checkoutStartedValue :: Maybe Double
  , checkoutStartedRevenue :: Maybe Double
  , checkoutStartedShipping :: Maybe Double
  , checkoutStartedTax :: Maybe Double
  , checkoutStartedDiscount :: Maybe Double
  , checkoutStartedCoupon :: Maybe Text
  , checkoutStartedCurrency :: Maybe Text
  , checkoutStartedProducts :: Vector CartProduct
  }

deriveJSON (jsonOpts 15) ''CheckoutStarted
instance EventValue CheckoutStarted
instance EmptyEvent CheckoutStarted where
  emptyEvent = CheckoutStarted Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty


checkoutStarted :: Event CheckoutStarted
checkoutStarted = "Checkout Started"

data CheckoutStepViewed = CheckoutStepViewed
  { checkoutStepViewedCheckoutId :: Maybe Text
  , checkoutStepViewedStep :: Maybe Int
  , checkoutStepViewedShippingMethod :: Maybe Text
  , checkoutStepViewedPaymentMethod :: Maybe Text
  }

deriveJSON (jsonOpts 18) ''CheckoutStepViewed
instance EventValue CheckoutStepViewed
instance EmptyEvent CheckoutStepViewed where
  emptyEvent = CheckoutStepViewed Nothing Nothing Nothing Nothing

checkoutStepViewed :: Event CheckoutStepViewed
checkoutStepViewed = "Checkout Step Viewed"

data CheckoutStepCompleted = CheckoutStepCompleted
  { checkoutStepCompletedCheckoutId :: Maybe Text
  , checkoutStepCompletedStep :: Maybe Int
  , checkoutStepCompletedShippingMethod :: Maybe Text
  , checkoutStepCompletedPaymentMethod :: Maybe Text
  }

deriveJSON (jsonOpts 21) ''CheckoutStepCompleted
instance EventValue CheckoutStepCompleted
instance EmptyEvent CheckoutStepCompleted where
  emptyEvent = CheckoutStepCompleted Nothing Nothing Nothing Nothing

checkoutStepCompleted :: Event CheckoutStepCompleted
checkoutStepCompleted = "Checkout Step Completed"

data PaymentInfoEntered = PaymentInfoEntered
  { paymentInfoEnteredCheckoutId :: Maybe Text
  , paymentInfoEnteredStep :: Maybe Int
  , paymentInfoEnteredShippingMethod :: Maybe Text
  , paymentInfoEnteredPaymentMethod :: Maybe Text
  }

deriveJSON (jsonOpts 18) ''PaymentInfoEntered
instance EventValue PaymentInfoEntered
instance EmptyEvent PaymentInfoEntered where
  emptyEvent = PaymentInfoEntered Nothing Nothing Nothing Nothing

paymentInfoEntered :: Event PaymentInfoEntered
paymentInfoEntered = "Payment Info Entered"

data OrderCompleted = OrderCompleted
  { orderCompletedCheckoutId :: Maybe Text
  , orderCompletedOrderId :: Maybe Text
  , orderCompletedAffiliation :: Maybe Double
  , orderCompletedTotal :: Maybe Double
  , orderCompletedRevenue :: Maybe Double
  , orderCompletedShipping :: Maybe Double
  , orderCompletedTax :: Maybe Double
  , orderCompletedDiscount :: Maybe Double
  , orderCompletedCoupon :: Maybe Text
  , orderCompletedCurrency :: Maybe Text
  , orderCompletedProducts :: Vector CartProduct
  }

deriveJSON (jsonOpts 14) ''OrderCompleted
instance EventValue OrderCompleted
instance EmptyEvent OrderCompleted where
  emptyEvent = OrderCompleted Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

orderCompleted :: Event OrderCompleted
orderCompleted = "Order Completed"

data OrderUpdated = OrderUpdated
  { orderUpdatedOrderId :: Maybe Text
  , orderUpdatedAffiliation :: Maybe Double
  , orderUpdatedTotal :: Maybe Double
  , orderUpdatedRevenue :: Maybe Double
  , orderUpdatedShipping :: Maybe Double
  , orderUpdatedTax :: Maybe Double
  , orderUpdatedDiscount :: Maybe Double
  , orderUpdatedCoupon :: Maybe Text
  , orderUpdatedCurrency :: Maybe Text
  , orderUpdatedProducts :: Vector CartProduct
  }

deriveJSON (jsonOpts 12) ''OrderUpdated
instance EventValue OrderUpdated
instance EmptyEvent OrderUpdated where
  emptyEvent = OrderUpdated Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

orderUpdated :: Event OrderUpdated
orderUpdated = "Order Updated"

data OrderRefunded = OrderRefunded
  { orderRefundedOrderId :: Maybe Text
  }

deriveJSON (jsonOpts 13) ''OrderRefunded
instance EventValue OrderRefunded
instance EmptyEvent OrderRefunded where
  emptyEvent = OrderRefunded Nothing

orderRefunded :: Event OrderRefunded
orderRefunded = "Order Refunded"

data OrderCancelled = OrderCancelled
  { orderCancelledOrderId :: Maybe Text
  , orderCancelledAffiliation :: Maybe Double
  , orderCancelledTotal :: Maybe Double
  , orderCancelledRevenue :: Maybe Double
  , orderCancelledShipping :: Maybe Double
  , orderCancelledTax :: Maybe Double
  , orderCancelledDiscount :: Maybe Double
  , orderCancelledCoupon :: Maybe Text
  , orderCancelledCurrency :: Maybe Text
  , orderCancelledProducts :: Vector CartProduct
  }

deriveJSON (jsonOpts 14) ''OrderCancelled
instance EventValue OrderCancelled
instance EmptyEvent OrderCancelled where
  emptyEvent = OrderCancelled Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty

orderCancelled :: Event OrderCancelled
orderCancelled = "Order Cancelled"

-- *** Coupons Overview

data CouponEntered = CouponEntered
  { couponEnteredOrderId :: Maybe Text
  , couponEnteredCartId :: Maybe Text
  , couponEnteredCouponId :: Maybe Text
  }

deriveJSON (jsonOpts 13) ''CouponEntered
instance EventValue CouponEntered
instance EmptyEvent CouponEntered where
  emptyEvent = CouponEntered Nothing Nothing Nothing

couponEntered :: Event CouponEntered
couponEntered = "Coupon Entered"

data CouponApplied = CouponApplied
  { couponAppliedOrderId :: Maybe Text
  , couponAppliedCartId :: Maybe Text
  , couponAppliedCouponId :: Maybe Text
  , couponAppliedCouponName :: Maybe Text
  , couponAppliedDiscount :: Maybe Double
  }

deriveJSON (jsonOpts 13) ''CouponApplied
instance EventValue CouponApplied
instance EmptyEvent CouponApplied where
  emptyEvent = CouponApplied Nothing Nothing Nothing Nothing Nothing

couponApplied :: Event CouponApplied
couponApplied = "Coupon Applied"

data CouponDenied = CouponDenied
  { couponDeniedOrderId :: Maybe Text
  , couponDeniedCartId :: Maybe Text
  , couponDeniedCouponId :: Maybe Text
  , couponDeniedCouponName :: Maybe Text
  , couponDeniedReason :: Maybe Text
  }

deriveJSON (jsonOpts 12) ''CouponDenied
instance EventValue CouponDenied
instance EmptyEvent CouponDenied where
  emptyEvent = CouponDenied Nothing Nothing Nothing Nothing Nothing

couponDenied :: Event CouponDenied
couponDenied = "Coupon Denied"

data CouponRemoved = CouponRemoved
  { couponRemovedOrderId :: Maybe Text
  , couponRemovedCartId :: Maybe Text
  , couponRemovedCouponId :: Maybe Text
  , couponRemovedCouponName :: Maybe Text
  , couponRemovedDiscount :: Maybe Double
  }

deriveJSON (jsonOpts 13) ''CouponRemoved
instance EventValue CouponRemoved
instance EmptyEvent CouponRemoved where
  emptyEvent = CouponRemoved Nothing Nothing Nothing Nothing Nothing

couponRemoved :: Event CouponRemoved
couponRemoved = "Coupon Removed"

-- *** Wishlisting Overview

data ProductAddedToWishlist = ProductAddedToWishlist
  { productAddedToWishlistWishlistId :: Maybe Text
  , productAddedToWishlistWishlistName :: Maybe Text
  , productAddedToWishlistProductId :: Maybe Text
  , productAddedToWishlistSku :: Maybe Text
  , productAddedToWishlistCategory :: Maybe Text
  , productAddedToWishlistName :: Maybe Text
  , productAddedToWishlistBrand :: Maybe Text
  , productAddedToWishlistVariant :: Maybe Text
  , productAddedToWishlistPrice :: Maybe Double
  , productAddedToWishlistQuantity :: Maybe Int
  , productAddedToWishlistCoupon :: Maybe Text
  , productAddedToWishlistPosition :: Maybe Int
  }

deriveJSON (jsonOpts 22) ''ProductAddedToWishlist

productAddedToWishlist :: Event ProductAddedToWishlist
productAddedToWishlist = "Product Added to Wishlist"

data ProductRemovedFromWishlist = ProductRemovedFromWishlist
  { productRemovedFromWishlistWishlistId :: Maybe Text
  , productRemovedFromWishlistWishlistName :: Maybe Text
  , productRemovedFromWishlistProductId :: Maybe Text
  , productRemovedFromWishlistSku :: Maybe Text
  , productRemovedFromWishlistCategory :: Maybe Text
  , productRemovedFromWishlistName :: Maybe Text
  , productRemovedFromWishlistBrand :: Maybe Text
  , productRemovedFromWishlistVariant :: Maybe Text
  , productRemovedFromWishlistPrice :: Maybe Double
  , productRemovedFromWishlistQuantity :: Maybe Int
  , productRemovedFromWishlistCoupon :: Maybe Text
  , productRemovedFromWishlistPosition :: Maybe Int
  }

deriveJSON (jsonOpts 26) ''ProductRemovedFromWishlist

productRemovedFromWishlist :: Event ProductRemovedFromWishlist
productRemovedFromWishlist = "Product Removed from Wishlist"

data WishlistProductAddedToCart = WishlistProductAddedToCart
  { wishlistProductAddedToCartWishlistId :: Maybe Text
  , wishlistProductAddedToCartWishlistName :: Maybe Text
  , wishlistProductAddedToCartProductId :: Maybe Text
  , wishlistProductAddedToCartSku :: Maybe Text
  , wishlistProductAddedToCartCategory :: Maybe Text
  , wishlistProductAddedToCartName :: Maybe Text
  , wishlistProductAddedToCartBrand :: Maybe Text
  , wishlistProductAddedToCartVariant :: Maybe Text
  , wishlistProductAddedToCartPrice :: Maybe Double
  , wishlistProductAddedToCartQuantity :: Maybe Int
  , wishlistProductAddedToCartCoupon :: Maybe Text
  , wishlistProductAddedToCartPosition :: Maybe Int
  }

deriveJSON (jsonOpts 26) ''WishlistProductAddedToCart

wishlistProductAddedToCart :: Event WishlistProductAddedToCart
wishlistProductAddedToCart = "Wishlist Product Added to Cart"

-- *** Sharing Overview

data ProductShared = ProductShared
  { productSharedShareVia :: Maybe Text
  , productSharedShareMessage :: Maybe Text
  , productSharedRecipient :: Maybe Text
  , productSharedProductId :: Maybe Text
  , productSharedSku :: Maybe Text
  , productSharedCategory :: Maybe Text
  , productSharedName :: Maybe Text
  , productSharedBrand :: Maybe Text
  , productSharedVariant :: Maybe Text
  , productSharedPrice :: Maybe Double
  }

deriveJSON (jsonOpts 13) ''ProductShared

productShared :: Event ProductShared
productShared = "Product Shared"

data CartSharedProduct = CartSharedProduct
  { cartSharedProductProductId :: Maybe Text
  }

deriveJSON (jsonOpts 17) ''CartSharedProduct

data CartShared = CartShared
  { cartSharedShareVia :: Maybe Text
  , cartSharedShareMessage :: Maybe Text
  , cartSharedRecipient :: Maybe Text
  , cartSharedCartId :: Maybe Text
  , cartSharedProducts :: Vector CartSharedProduct
  }

deriveJSON (jsonOpts 10) ''CartShared

cartShared :: Event CartShared
cartShared = "Cart Shared"

-- *** Reviewing Overview

data ProductReviewed = ProductReviewed
  { productReviewedProductId :: Maybe Text
  , productReviewedReviewId :: Maybe Text
  , productReviewedReviewBody :: Maybe Text
  , productReviewedRating :: Maybe Text
  }

deriveJSON (jsonOpts 15) ''ProductReviewed

productReviewed :: Event ProductReviewed
productReviewed = "Product Reviewed"

-- ** Email Events

data EmailBounced = EmailBounced
  { emailBouncedEmailId :: Maybe Text
  , emailBouncedEmailSubject :: Maybe Text
  , emailBouncedCampaignId :: Maybe Text
  , emailBouncedCampaignName :: Maybe Text
  }

deriveJSON (jsonOpts 12) ''EmailBounced

emailBounced :: Event EmailBounced
emailBounced = "Email Bounced"

data EmailDelivered = EmailDelivered
  { emailDeliveredEmailId :: Maybe Text
  , emailDeliveredEmailSubject :: Maybe Text
  , emailDeliveredCampaignId :: Maybe Text
  , emailDeliveredCampaignName :: Maybe Text
  }

deriveJSON (jsonOpts 14) ''EmailDelivered

emailDelivered :: Event EmailDelivered
emailDelivered = "Email Delivered"

data EmailLinkClicked = EmailLinkClicked
  { emailLinkClickedEmailId :: Maybe Text
  , emailLinkClickedEmailSubject :: Maybe Text
  , emailLinkClickedCampaignId :: Maybe Text
  , emailLinkClickedCampaignName :: Maybe Text
  , emailLinkClickedLinkId :: Maybe Text
  , emailLinkClickedLinkUrl :: Maybe Text
  }

deriveJSON (jsonOpts 16) ''EmailLinkClicked

emailLinkClicked :: Event EmailLinkClicked
emailLinkClicked = "Email Link Clicked"

data EmailMarkedAsSpam = EmailMarkedAsSpam
  { emailMarkedAsSpamEmailId :: Maybe Text
  , emailMarkedAsSpamEmailSubject :: Maybe Text
  , emailMarkedAsSpamCampaignId :: Maybe Text
  , emailMarkedAsSpamCampaignName :: Maybe Text
  }

deriveJSON (jsonOpts 17) ''EmailMarkedAsSpam

emailMarkedAsSpam :: Event EmailMarkedAsSpam
emailMarkedAsSpam = "Email Marked as Spam"

data EmailOpened = EmailOpened
  { emailOpenedEmailId :: Maybe Text
  , emailOpenedEmailSubject :: Maybe Text
  , emailOpenedCampaignId :: Maybe Text
  , emailOpenedCampaignName :: Maybe Text
  }

deriveJSON (jsonOpts 11) ''EmailOpened

emailOpened :: Event EmailOpened
emailOpened = "Email Opened"

data Unsubscribed = Unsubscribed
  { unsubscribedEmailId :: Maybe Text
  , unsubscribedEmailSubject :: Maybe Text
  , unsubscribedCampaignId :: Maybe Text
  , unsubscribedCampaignName :: Maybe Text
  , unsubscribedListId :: Maybe Text
  , unsubscribedListName :: Maybe Text
  }

deriveJSON (jsonOpts 12) ''Unsubscribed

unsubscribed :: Event Unsubscribed
unsubscribed = "Unsubscribed"

-- ** Live Chat Events

data LiveChatConversationStarted = LiveChatConversationStarted
  { liveChatConversationStartedAgentId :: Maybe Text
  , liveChatConversationStartedAgentName :: Maybe Text
  , liveChatConversationStartedAgentUsername :: Maybe Text
  , liveChatConversationStartedConversationId :: Maybe Text
  , liveChatConversationStartedConversationDuration :: Maybe Double
  , liveChatConversationStartedMessageId :: Maybe Text
  }

deriveJSON (jsonOpts 27) ''LiveChatConversationStarted

liveChatConversationStarted :: Event LiveChatConversationStarted
liveChatConversationStarted = "Live Chat Conversation Started"

data LiveChatConversationEnded = LiveChatConversationEnded
  { liveChatConversationEndedAgentId :: Maybe Text
  , liveChatConversationEndedAgentName :: Maybe Text
  , liveChatConversationEndedAgentUsername :: Maybe Text
  , liveChatConversationEndedConversationId :: Maybe Text
  , liveChatConversationEndedConversationDuration :: Maybe Double
  , liveChatConversationEndedMessageId :: Maybe Text
  }

deriveJSON (jsonOpts 25) ''LiveChatConversationEnded

liveChatConversationEnded :: Event LiveChatConversationEnded
liveChatConversationEnded = "Live Chat Conversation Ended"

data LiveChatMessageSent = LiveChatMessageSent
  { liveChatMessageSentAgentId :: Maybe Text
  , liveChatMessageSentAgentName :: Maybe Text
  , liveChatMessageSentAgentUsername :: Maybe Text
  , liveChatMessageSentConversationId :: Maybe Text
  , liveChatMessageSentConversationDuration :: Maybe Double
  , liveChatMessageSentMessageId :: Maybe Text
  }

deriveJSON (jsonOpts 19) ''LiveChatMessageSent

liveChatMessageSent :: Event LiveChatMessageSent
liveChatMessageSent = "Live Chat Message Sent"

data LiveChatMessageReceived = LiveChatMessageReceived
  { liveChatMessageReceivedAgentId :: Maybe Text
  , liveChatMessageReceivedAgentName :: Maybe Text
  , liveChatMessageReceivedAgentUsername :: Maybe Text
  , liveChatMessageReceivedConversationId :: Maybe Text
  , liveChatMessageReceivedConversationDuration :: Maybe Double
  , liveChatMessageReceivedMessageId :: Maybe Text
  }

deriveJSON (jsonOpts 23) ''LiveChatMessageReceived

liveChatMessageReceived :: Event LiveChatMessageReceived
liveChatMessageReceived = "Live Chat Message Received"

segmentBatchReaper :: String -> Int -> (ServantError -> IO ()) -> IO (Reaper (DL.DList (Message Object)) [Message Object])
segmentBatchReaper key batchSize errHandler = do
  run <- mkRunner key
  let settings = defaultReaperSettings
        { reaperAction = \dl -> do
            t <- getCurrentTime
            u <- UUID.nextRandom
            let (thisBatch, remaining) = splitAt batchSize $ DL.toList dl
            forM_ thisBatch (LBS8.putStrLn . encode)
            r <- run (BatchedMsg u thisBatch t)
            case r of
              Left err -> do
                errHandler err
                -- TODO figure out how to work in exponential backoff here
                return (DL.fromList (thisBatch <> remaining) <>)
              Right _ -> return (DL.fromList remaining <>)
            return (DL.fromList remaining <>)
        , reaperCons = \l wl -> (DL.fromList l <> wl)
        , reaperNull = null . DL.toList
        , reaperEmpty = mempty
        , reaperDelay = 3000000
        }
  mkReaper settings

send :: (MonadIO m, SegmentMsg msg) => SegmentClient -> Context -> msg -> m ()
send c ctxt m = liftIO $ do
  t <- getCurrentTime
  reaperAdd (segmentBatchHandler c) $ pure $
    Message t ctxt (HM.insert "type" (toJSON $ segmentType m) $ segmentObject m)

-- sendImmediate :: SegmentMsg m => SegmentClient -> m -> IO (Either ServantError SegmentResponse)
-- sendImmediate = undefined

coerceTraits :: HM.HashMap (Trait Value) Value -> Object
coerceTraits = unsafeCoerce

identify :: MonadIO m => SegmentClient -> Context -> SegmentId -> HM.HashMap (Trait Value) Value -> m ()
identify c ctxt sid traits = send c ctxt (Identify sid $ coerceTraits traits)

-- | NB: while there is a ToJSON instance constraint here, the JSON value *must* be an object.
-- see issue 544 on aeson's GitHub page
track :: (MonadIO m, EventValue a, ToJSON a) => SegmentClient -> Context -> SegmentId -> Event a -> a -> Freeform -> m ()
track c ctxt sid (Event e) d ff = send c ctxt (Track sid e (val <> ff))
  where
    (Object val) = toJSON d

page :: MonadIO m => SegmentClient -> Context -> Maybe SegmentId -> Maybe Text -> PageProperties -> Freeform -> m ()
page c ctxt msid n props ff = send c ctxt (Page msid n (val <> ff))
  where
    (Object val) = toJSON props

screen :: MonadIO m => SegmentClient -> Context -> Maybe SegmentId -> Maybe Text -> Freeform -> m ()
screen c ctxt msid n props = send c ctxt (Screen msid n props)

group :: MonadIO m => SegmentClient -> Context -> SegmentId -> Text -> HM.HashMap (Trait Value) Value -> m ()
group c ctxt sid gid traits = send c ctxt (Group sid gid $ coerceTraits traits)

alias :: MonadIO m => SegmentClient -> Context -> SegmentId -> SegmentId -> m ()
alias c ctxt oldId newId = send c ctxt (Alias newId oldId)

type SegmentApi
  = BasicAuth "realm" ()
  :> ("batch" :> ReqBody '[JSON] BatchedMsg
  :> Post '[JSON] SegmentResponse)


mkRunner :: String -> IO (BatchedMsg -> IO (Either ServantError SegmentResponse))
mkRunner key = do
  manager <- getGlobalManager
  let baseURL = BaseUrl Https "api.segment.io" 443 "/v1"
      basicAuthData = BasicAuthData (BS8.pack key) ""
      appCall m = runClientM m (ClientEnv manager baseURL Nothing)
  return $ \msg -> appCall $ segmentClient basicAuthData msg


segmentApi :: Proxy SegmentApi
segmentApi = Proxy

segmentClient :: BasicAuthData -> BatchedMsg -> ClientM SegmentResponse
segmentClient = client segmentApi

mkClient :: String -> IO SegmentClient
mkClient tok = do
  r <- segmentBatchReaper tok 10 (const $ return ())
  return $ SegmentClient r

instance ToJSON BatchedMsg where
  toJSON (BatchedMsg uuid msgs sentAt) =
    object
    [ "context"   .= defaultContext
    , "batch"     .= msgs
    , "type"      .= ("batch" :: Text)
    , "messageId" .= UUID.toText uuid
    , "sentAt"    .= sentAt
      -- timestamp is meaningless here, so omit.
    ]

instance {-# OVERLAPPING #-} ToJSON (Message Object) where
  toJSON (Message t msg context) = Object (commonJson <> msg)
    where
      commonJson = HM.fromList
        [ ("timestamp", toJSON t)
        , ("context", toJSON context)
        ]

instance {-# OVERLAPPABLE #-} SegmentMsg m => ToJSON (Message m) where
  toJSON (Message t context msg) = toJSON (Message t context (segmentObject msg))

defaultContext :: Value
defaultContext = [aesonQQ|
{
  "library" : {
    "name": "segment-api",
    "version" : #{showVersion version}
  }
}
|]


{-
test c = do
  u <- UUID.nextRandom
  t <- getCurrentTime
  send c emptyIdentify (emptyCommonMsg t $ IdentifiedUser "weorew") emptyFreeform
-}
