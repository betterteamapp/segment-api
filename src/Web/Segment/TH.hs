module Web.Segment.TH where

import Data.Aeson.Types
import Data.Aeson.TH

jsonOpts :: Int -> Options
jsonOpts c = defaultOptions
  { omitNothingFields = True
  , unwrapUnaryRecords = True
  , fieldLabelModifier  = camelTo2 '_' . drop c
  }
