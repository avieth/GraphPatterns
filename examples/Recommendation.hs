{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Proxy
import Data.UUID
import Data.UUID.V4
import Data.Relational
import Data.Relational.Interpreter
import Data.Relational.RelationalF
import Data.Relational.PostgreSQL
import Data.GraphPatterns.MList
import Data.GraphPatterns.Vertex
import Data.GraphPatterns.Edge
import Data.GraphPatterns.Language
import Data.GraphPatterns.Interpreter
import Data.GraphPatterns.Engines.RelationalGraph

data Customer = Customer T.Text
  deriving (Eq, Show)

data Widget = Widget UUID
  deriving (Eq, Show)

data Bought = Bought
  deriving (Show)

instance
    ( Functor m
    , Applicative m
    , MonadIO m
    , ContainsDatabase db GraphDatabase
    ) => Vertex (RelationalGraph db m) Customer
  where
    toEngineVertexInsertion _ (Customer name) = M.singleton "name" name
    fromEngineVertex _ _ (_, map) = case M.toList map of
        [("name", x)] -> return (Customer x)
        _ -> mzero

instance
    ( Functor m
    , Applicative m
    , MonadIO m
    , ContainsDatabase db GraphDatabase
    ) => Vertex (RelationalGraph db m) Widget
  where
    toEngineVertexInsertion _ (Widget uuid) = M.singleton "id" (uuidToText uuid)
    fromEngineVertex _ _ (_, map) = case M.toList map of
        [("id", x)] -> case textToUUID x of
            Just uuid -> return (Widget uuid)
            Nothing -> mzero
        _ -> mzero

uuidToText :: UUID -> T.Text
uuidToText = T.pack . toString

textToUUID :: T.Text -> Maybe UUID
textToUUID = fromString . T.unpack

instance 
    ( Functor m
    , Applicative m
    , MonadIO m
    , ContainsDatabase db GraphDatabase
    ) => DeterminesVertex (RelationalGraph db m) Customer Customer
  where
    toEngineVertexInformation _ _ (Customer name) = M.fromList [("name", name)]

instance 
    ( Functor m
    , Applicative m
    , MonadIO m
    , ContainsDatabase db GraphDatabase
    ) => DeterminesVertex (RelationalGraph db m) Widget Widget
  where
    toEngineVertexInformation _ _ (Widget uuid) = M.fromList [("id", uuidToText uuid)]

-- This instance allows us to get every widget
instance 
    ( Functor m
    , Applicative m
    , MonadIO m
    , ContainsDatabase db GraphDatabase
    ) => DeterminesVertex (RelationalGraph db m) Widget ()
  where
    toEngineVertexInformation _ _ () = M.empty

instance
    ( Functor m
    , Applicative m
    , MonadIO m
    , ContainsDatabase db GraphDatabase
    ) => Edge (RelationalGraph db m) Bought
  where
    type EdgeTarget (RelationalGraph db m) Bought = Widget
    type EdgeSource (RelationalGraph db m) Bought = Customer
    toEngineEdgeInsertion _ _ = M.empty
    fromEngineEdge _ _ (id, map) = return Bought

instance
    ( Functor m
    , Applicative m
    , MonadIO m
    , ContainsDatabase db GraphDatabase
    ) => DeterminesLocalEdge (RelationalGraph db m) Bought ()
  where
    toEngineEdgeInformationLocal _ _ _ = M.empty

thingsBought
  :: (Monad m)
  => Customer
  -> GraphQuery (RelationalGraph GraphDatabase IO) m (V (RelationalGraph GraphDatabase IO) Widget)
thingsBought customer = do
    buyer <- vertex (Proxy :: Proxy Customer) customer
    thingBought <- adjacentOut (Proxy :: Proxy Bought) () buyer
    return thingBought

recommend
  :: ( MonadPlus m )
  => Customer
  -> GraphQuery (RelationalGraph GraphDatabase IO) m (V (RelationalGraph GraphDatabase IO) Widget)
recommend customer = do
    buyer <- vertex (Proxy :: Proxy Customer) customer
    thingBought <- adjacentOut (Proxy :: Proxy Bought) () buyer
    otherBuyer <- adjacentIn (Proxy :: Proxy Bought) () thingBought
    guard (v buyer /= v otherBuyer)
    otherThingBought <- adjacentOut (Proxy :: Proxy Bought) () otherBuyer
    guard (v otherThingBought /= v thingBought)
    return otherThingBought

addCustomer
  :: Monad m
  => Customer
  -> GraphMutation (RelationalGraph GraphDatabase IO) m (V (RelationalGraph GraphDatabase IO) Customer)
addCustomer customer = putVertex customer

addWidget
  :: Monad m
  => Widget
  -> GraphMutation (RelationalGraph GraphDatabase IO) m (V (RelationalGraph GraphDatabase IO) Widget)
addWidget widget = putVertex widget

addBought
  :: Monad m
  => Customer
  -> Widget
  -> GraphPatterns (RelationalGraph GraphDatabase IO) m (E (RelationalGraph GraphDatabase IO) Bought)
addBought customer widget = do
    vcustomer <- query $ vertex (Proxy :: Proxy Customer) customer
    vwidget <- query $ vertex (Proxy :: Proxy Widget) widget
    mutation $ putEdge Bought vcustomer vwidget

step1
  :: GraphPatterns
       (RelationalGraph GraphDatabase IO)
       (RelationalGraphInterpreter GraphDatabase IO)
       t
  -> RelationalGraphInterpreter GraphDatabase IO t
step1 = iterT graphPatternsInterpreter

step2
  :: RelationalGraphInterpreter GraphDatabase IO t
  -> FreeT (RelationalF GraphDatabase) IO [t]
step2 = ml_tolist . runRGInterpreter

step3
  :: FreeT (RelationalF GraphDatabase) IO [t]
  -> PostgresT IO [t]
step3 = iterTM (interpreter (Proxy :: Proxy (PostgresInterpreter IO)))

go connInfo = runPostgresT connInfo id . step3 . step2 . step1
