{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Uni
  ( Inj(..)
  , Uni(..)
  , runUni
  ) where

import Control.Monad.Trans.Except

runUni :: forall t m a . Monad m
       => ExceptT (Uni t) m a
       -> m (Either (Uni t) a)
runUni = runExceptT

data Uni (a:: [*]) where
  That  :: Uni b -> Uni (a ': b)
  This  :: a -> Uni (a ': b)


class Inj o i where
  inj :: i -> o

instance {-# OVERLAPPABLE #-} Inj a a where
  inj = id

instance Inj (Maybe a) a where
  inj = Just


instance {-# OVERLAPPABLE #-} Inj (Uni b) c => Inj (Uni (a ': b)) c where
  inj = That . inj

instance {-# OVERLAPS #-} Inj (Uni (a ': b)) a where
  inj = This

