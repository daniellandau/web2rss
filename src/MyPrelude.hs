module MyPrelude (module Prelude, identity) where

import Prelude hiding (id)
import qualified Prelude as P

identity :: a -> a
identity = P.id
