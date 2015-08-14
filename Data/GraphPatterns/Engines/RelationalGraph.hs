{-|
Module      : 
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.GraphPatterns.Engines.RelationalGraph (

    RelationalGraph
  , RelationalGraphInterpreter
  , runRGInterpreter
  , Properties
  , IdColumn
  , KeyColumn
  , ValueColumn
  , SourceColumn
  , TargetColumn
  , VertexPropertySchema
  , EdgePropertySchema
  , AdjacencySchema
  , VertexPropertyTable
  , EdgePropertyTable
  , AdjacencyTable
  , GraphDatabase

  ) where

import GHC.TypeLits (Symbol)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.FInterpreter
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Proxy
import Data.UUID
import Data.UUID.V4
import Data.Relational
import Data.Relational.Interpreter
import Data.Relational.RelationalF
import Data.Relational.PostgreSQL
import Data.GraphPatterns.MList
import Data.GraphPatterns.GraphEngine

data RelationalGraph (db :: [(Symbol, [(Symbol, *)])]) (m :: * -> *) = RG

-- | Interpreting a "relational graph" is all about dumping it to RelationalF
--   terms and then running those in MList-style, i.e. fanning out according
--   to edge and vertex counts.
newtype RelationalGraphInterpreter db (m :: * -> *) t = RGInterpreter {
    runRGInterpreter :: MList (FreeT (RelationalF db) m) t
  } deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

instance MonadTrans (RelationalGraphInterpreter db) where
    lift = RGInterpreter . lift . lift

instance FTrans (RelationalGraphInterpreter db) where
    transInterp interp =
          RGInterpreter
        . ml_fromlist'
        . FreeT
        . interp
        . fmap (runFreeT . ml_tolist . runRGInterpreter)

type Properties = M.Map T.Text T.Text

type IdColumn = '( "id", UUID )
type KeyColumn = '( "key", T.Text )
type ValueColumn = '( "value", T.Text )
type SourceColumn = '( "source", UUID )
type TargetColumn = '( "target", UUID )

idColumn :: Column IdColumn
idColumn = column

keyColumn :: Column KeyColumn
keyColumn = column

valueColumn :: Column ValueColumn
valueColumn = column

sourceColumn :: Column SourceColumn
sourceColumn = column

targetColumn :: Column TargetColumn
targetColumn = column

type VertexManifestSchema = '[ IdColumn ]
type EdgeManifestSchema = '[ IdColumn ]
type VertexPropertySchema = '[ IdColumn, KeyColumn, ValueColumn ]
type EdgePropertySchema = '[ IdColumn, KeyColumn, ValueColumn ]
type AdjacencySchema = '[ IdColumn, SourceColumn, TargetColumn ]

vertexManifestSchema :: Schema VertexManifestSchema
vertexManifestSchema = idColumn :| EndSchema

edgeManifestSchema :: Schema EdgeManifestSchema
edgeManifestSchema = schema Proxy

vertexPropertySchema :: Schema VertexPropertySchema
vertexPropertySchema = idColumn :| keyColumn :| valueColumn :| EndSchema

edgePropertySchema :: Schema EdgePropertySchema
edgePropertySchema = idColumn :| keyColumn :| valueColumn :| EndSchema

adjacencySchema :: Schema AdjacencySchema
adjacencySchema = idColumn :| sourceColumn :| targetColumn :| EndSchema

type VertexManifestTable = '( "vertex_manifest", VertexManifestSchema )
type EdgeManifestTable = '( "edge_manifest", EdgeManifestSchema )
type VertexPropertyTable = '( "vertex_properties", VertexPropertySchema )
type EdgePropertyTable = '( "edge_properties", EdgePropertySchema )
type AdjacencyTable = '( "adjacency", AdjacencySchema )

vertexManifestTable :: Table VertexManifestTable
vertexManifestTable = table vertexManifestSchema

edgeManifestTable :: Table EdgeManifestTable
edgeManifestTable = table edgeManifestSchema

vertexPropertyTable :: Table VertexPropertyTable
vertexPropertyTable = table vertexPropertySchema

edgePropertyTable :: Table EdgePropertyTable
edgePropertyTable = table edgePropertySchema

adjacencyTable :: Table AdjacencyTable
adjacencyTable = table adjacencySchema

type GraphDatabase = '[
    VertexManifestTable
  , EdgeManifestTable
  , VertexPropertyTable
  , EdgePropertyTable
  , AdjacencyTable
  ]

rg :: Monad m => FreeT (RelationalF db) m t -> RelationalGraphInterpreter db m t
rg = RGInterpreter . lift

rgl :: Monad m => FreeT (RelationalF db) m [t] -> RelationalGraphInterpreter db m t
rgl = RGInterpreter . ml_fromlist'

makeProperties :: [Row '[KeyColumn, ValueColumn]] -> Properties
makeProperties = M.fromList . fmap makeProperty
  where
    makeProperty :: Row '[KeyColumn, ValueColumn] -> (T.Text, T.Text)
    makeProperty (keyField :&| valueField :&| EndRow) =
        (fieldValue keyField, fieldValue valueField)

rowToId :: Row '[IdColumn] -> UUID
rowToId row = case row of
    r :&| EndRow -> fieldValue r

getVertexProperties
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> m Properties
getVertexProperties _ id = do
    let term :: RelationalF db [Row '[KeyColumn, ValueColumn]]
        term = rfrelation (Selection selection)
    rows <- liftF (injectFunctor term)
    return (makeProperties rows)
  where
    selection
      :: Select
           VertexPropertyTable
           '[ KeyColumn, ValueColumn ]
           '[ KeyColumn, ValueColumn ]
           '[ '[ IdColumn ] ]
    selection = select condition
    condition :: Condition '[ '[ IdColumn ] ]
    condition = idColumn .==. id .||. false .&&. true

getEdgeProperties
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> m Properties
getEdgeProperties _ id = do
    let term :: RelationalF db [Row '[KeyColumn, ValueColumn]]
        term = rfrelation (Selection selection)
    rows <- liftF (injectFunctor term)
    return (makeProperties rows)
  where
    selection
      :: Select
           EdgePropertyTable
           '[ KeyColumn, ValueColumn ]
           '[ KeyColumn, ValueColumn ]
           '[ '[ IdColumn ] ]
    selection = select condition
    condition :: Condition '[ '[ IdColumn ] ]
    condition = idColumn .==. id .||. false .&&. true

getEdgeSourceId
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> m UUID
getEdgeSourceId _ id = do
    let term :: RelationalF db [Row '[SourceColumn]]
        term = rfrelation (Selection selection)
    rows <- liftF (injectFunctor term)
    case rows of
      [ (idField :&| EndRow) ] -> return (fieldValue idField)
      _ -> error "Edge has n /= 1 sources! Wtf?"
  where
    selection
      :: Select
           AdjacencyTable
           '[ SourceColumn ]
           '[ SourceColumn ]
           '[ '[ IdColumn ] ]
    selection = select condition
    condition :: Condition '[ '[ IdColumn ] ]
    condition = idColumn .==. id .||. false .&&. true

getEdgeTargetId
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> m UUID
getEdgeTargetId _ id = do
    let term :: RelationalF db [Row '[TargetColumn]]
        term = rfrelation (Selection selection)
    rows <- liftF (injectFunctor term)
    case rows of
      [ (idField :&| EndRow) ] -> return (fieldValue idField)
      _ -> error "Edge has n /= 1 targets! Wtf?"
  where
    selection
      :: Select
           AdjacencyTable
           '[ TargetColumn ]
           '[ TargetColumn ]
           '[ '[ IdColumn ] ]
    selection = select condition
    condition :: Condition '[ '[ IdColumn ] ]
    condition = idColumn .==. id .||. false .&&. true

selectVertexIdsWithProperty
  :: ( )
  => (T.Text, T.Text)
  -> Select
       VertexPropertyTable
       '[ IdColumn ]
       '[ IdColumn ]
       '[ '[KeyColumn], '[ValueColumn] ]
selectVertexIdsWithProperty (key, value) = select condition
  where
    condition :: Condition '[ '[KeyColumn], '[ValueColumn] ]
    condition =      keyColumn .==. key .||. false
                .&&. valueColumn .==. value .||. false
                .&&. true

selectVertexIdsWithProperties
  :: ( ContainsDatabase db GraphDatabase )
  => [(T.Text, T.Text)]
  -> Relation db '[ IdColumn ]
selectVertexIdsWithProperties pairs = case pairs of
    [] -> selectVertexManifest
    x : rest -> Intersection
                  (Selection (selectVertexIdsWithProperty x))
                  (selectVertexIdsWithProperties rest)

selectVertexManifest
  :: ( ContainsDatabase db GraphDatabase )
  => Relation db '[ IdColumn ]
selectVertexManifest = Selection select
  where
    select :: Select VertexManifestTable '[ IdColumn ] '[ IdColumn ] '[ ]
    select = selectAll

getVertexIdsWithProperties
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> Properties
  -> m [UUID]
getVertexIdsWithProperties _ properties = do
    let term :: RelationalF db [Row '[IdColumn]]
        term = rfrelation (selectVertexIdsWithProperties (M.toList properties))
    rows <- liftF (injectFunctor term)
    let outcome = fmap rowToId rows
    return outcome

selectEdgeIdsWithProperty
  :: ( )
  => (T.Text, T.Text)
  -> Select
       EdgePropertyTable
       '[ IdColumn ]
       '[ IdColumn ]
       '[ '[KeyColumn], '[ValueColumn] ]
selectEdgeIdsWithProperty (key, value) = select condition
  where
    condition :: Condition '[ '[KeyColumn], '[ValueColumn] ]
    condition =      keyColumn .==. key .||. false
                .&&. valueColumn .==. value .||. false
                .&&. true

selectEdgeIdsWithProperties
  :: ( ContainsDatabase db GraphDatabase )
  => [(T.Text, T.Text)]
  -> Relation db '[ IdColumn ]
selectEdgeIdsWithProperties pairs = case pairs of
    [] -> Selection (selectAll :: Select EdgeManifestTable '[ IdColumn ] '[ IdColumn ] '[])
    x : rest -> Intersection
                  (Selection (selectEdgeIdsWithProperty x))
                  (selectEdgeIdsWithProperties rest)

getEdgeIdsWithProperties
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> Properties
  -> m [UUID]
getEdgeIdsWithProperties _ properties = do
    let term :: RelationalF db [Row '[IdColumn]]
        term = rfrelation (selectVertexIdsWithProperties (M.toList properties))
    rows <- liftF (injectFunctor term)
    return $ fmap rowToId rows

selectEdgeIdsWithTarget
  :: ( ContainsDatabase db GraphDatabase )
  => UUID
  -> Relation db '[ IdColumn ]
selectEdgeIdsWithTarget targetId = Selection (Select tbl prj prj cond)
  where
    tbl :: Table AdjacencyTable
    tbl = table (schema Proxy)
    prj :: Project '[ IdColumn ]
    prj = projection Proxy
    cond :: Condition '[ '[ TargetColumn ] ]
    cond = targetColumn .==. targetId .||. false .&&. true

selectEdgeIdsWithSource
  :: ( ContainsDatabase db GraphDatabase )
  => UUID
  -> Relation db '[ IdColumn ]
selectEdgeIdsWithSource sourceId = Selection (Select tbl prj prj cond)
  where
    tbl :: Table AdjacencyTable
    tbl = table (schema Proxy)
    prj :: Project '[ IdColumn ]
    prj = projection Proxy
    cond :: Condition '[ '[ SourceColumn ] ]
    cond = sourceColumn .==. sourceId .||. false .&&. true

getOutgoingEdges
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> Properties
  -> UUID
  -> m [UUID]
getOutgoingEdges _ edgeproperties vertexId = do
    let term :: RelationalF db [Row '[IdColumn]]
        term = rfrelation $ Intersection
                 (selectEdgeIdsWithProperties (M.toList edgeproperties))
                 (selectEdgeIdsWithSource vertexId)
    rows <- liftF (injectFunctor term)
    return $ fmap rowToId rows

getIncomingEdges
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> Properties
  -> UUID
  -> m [UUID]
getIncomingEdges _ edgeproperties vertexId = do
    let term :: RelationalF db [Row '[IdColumn]]
        term = rfrelation $ Intersection
                 (selectEdgeIdsWithProperties (M.toList edgeproperties))
                 (selectEdgeIdsWithTarget vertexId)
    rows <- liftF (injectFunctor term)
    return $ fmap rowToId rows

insertVertexManifest
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> m ()
insertVertexManifest _ id = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfinsert insertion
    insertion :: Insert VertexManifestTable
    insertion = Insert vertexManifestTable row
    row :: Row VertexManifestSchema
    row = fromColumnAndValue idColumn id :&| EndRow

insertEdgeManifest
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> m ()
insertEdgeManifest _ id = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfinsert insertion
    insertion :: Insert EdgeManifestTable
    insertion = Insert edgeManifestTable row
    row :: Row EdgeManifestSchema
    row = fromColumnAndValue idColumn id :&| EndRow

deleteVertexManifest
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> m ()
deleteVertexManifest _ id = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfdelete deletion
    deletion :: Delete VertexManifestTable '[ '[ IdColumn ] ]
    deletion = Delete vertexManifestTable condition
    condition :: Condition '[ '[ IdColumn ] ]
    condition = idColumn .==. id .||. false .&&. true

deleteEdgeManifest
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> m ()
deleteEdgeManifest _ id = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfdelete deletion
    deletion :: Delete EdgeManifestTable '[ '[ IdColumn ] ]
    deletion = Delete edgeManifestTable condition
    condition :: Condition '[ '[ IdColumn ] ]
    condition = idColumn .==. id .||. false .&&. true

insertVertexProperty
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> (T.Text, T.Text)
  -> m ()
insertVertexProperty _ id (key, value) = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfinsert insertion
    insertion :: Insert VertexPropertyTable
    insertion = Insert vertexPropertyTable row
    row :: Row VertexPropertySchema
    row =     fromColumnAndValue idColumn id
          :&| fromColumnAndValue keyColumn key
          :&| fromColumnAndValue valueColumn value
          :&| EndRow

insertEdgeProperty
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> (T.Text, T.Text)
  -> m ()
insertEdgeProperty _ id (key, value) = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfinsert insertion
    insertion :: Insert EdgePropertyTable
    insertion = Insert edgePropertyTable row
    row :: Row EdgePropertySchema
    row =     fromColumnAndValue idColumn id
          :&| fromColumnAndValue keyColumn key
          :&| fromColumnAndValue valueColumn value
          :&| EndRow

deleteVertexProperty
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> T.Text
  -> m ()
deleteVertexProperty _ id key = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfdelete deletion
    deletion :: Delete VertexPropertyTable '[ '[ IdColumn ], '[ KeyColumn ] ]
    deletion = Delete vertexPropertyTable condition
    condition :: Condition '[ '[ IdColumn ], '[ KeyColumn ] ]
    condition = idColumn .==. id .||. false .&&. keyColumn .==. key .||. false .&&. true

deleteAllVertexProperties
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> m ()
deleteAllVertexProperties _ id = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfdelete deletion
    deletion :: Delete VertexPropertyTable '[ '[ IdColumn ] ]
    deletion = Delete vertexPropertyTable condition
    condition :: Condition '[ '[ IdColumn ] ]
    condition = idColumn .==. id .||. false .&&. true

deleteEdgeProperty
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> T.Text
  -> m ()
deleteEdgeProperty _ id key = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfdelete deletion
    deletion :: Delete EdgePropertyTable '[ '[ IdColumn ], '[ KeyColumn ] ]
    deletion = Delete edgePropertyTable condition
    condition :: Condition '[ '[ IdColumn ], '[ KeyColumn ] ]
    condition = idColumn .==. id .||. false .&&. keyColumn .==. key .||. false .&&. true

deleteAllEdgeProperties
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> m ()
deleteAllEdgeProperties _ id = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfdelete deletion
    deletion :: Delete EdgePropertyTable '[ '[ IdColumn ] ]
    deletion = Delete edgePropertyTable condition
    condition :: Condition '[ '[ IdColumn ] ]
    condition = idColumn .==. id .||. false .&&. true

insertEdgeAdjacency
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> UUID
  -> UUID
  -> m ()
insertEdgeAdjacency _ edgeId sourceId targetId = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfinsert insertion
    insertion :: Insert AdjacencyTable
    insertion = Insert adjacencyTable row
    row :: Row AdjacencySchema
    row =     fromColumnAndValue idColumn edgeId
          :&| fromColumnAndValue sourceColumn sourceId
          :&| fromColumnAndValue targetColumn targetId
          :&| EndRow

deleteEdgeAdjacency
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> UUID
  -> UUID
  -> m ()
deleteEdgeAdjacency _ edgeId sourceId targetId = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfdelete deletion
    deletion :: Delete AdjacencyTable '[ '[ IdColumn ], '[ SourceColumn ], '[ TargetColumn ] ]
    deletion = Delete adjacencyTable condition
    condition :: Condition '[ '[ IdColumn ], '[ SourceColumn ], '[ TargetColumn ] ]
    condition =
             idColumn .==. edgeId .||. false
        .&&. sourceColumn .==. sourceId .||. false
        .&&. targetColumn .==. targetId .||. false
        .&&. true

deleteAllEdgeAdjacency
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> m ()
deleteAllEdgeAdjacency _ edgeId = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfdelete deletion
    deletion :: Delete AdjacencyTable '[ '[ IdColumn ] ]
    deletion = Delete adjacencyTable condition
    condition :: Condition '[ '[ IdColumn ] ]
    condition =
             idColumn .==. edgeId .||. false
        .&&. true

deleteAllVertexAdjacency
  :: forall f m db .
     ( Functor f
     , MonadFree f m
     , InjectFunctor (RelationalF db) f
     , ContainsDatabase db GraphDatabase
     )
  => Proxy db
  -> UUID
  -> m ()
deleteAllVertexAdjacency _ vertexId = liftF (injectFunctor term)
  where
    term :: RelationalF db ()
    term = rfdelete deletion
    deletion :: Delete AdjacencyTable '[ '[ SourceColumn, TargetColumn ] ]
    deletion = Delete adjacencyTable condition
    condition :: Condition '[ '[ SourceColumn, TargetColumn ] ]
    condition =
             sourceColumn .==. vertexId .||. targetColumn .==. vertexId .||. false
        .&&. true

instance
    ( Functor m
    , Applicative m
    , Monad m
    , MonadIO m
    , ContainsDatabase db GraphDatabase
    )
    => GraphEngine (RelationalGraph db m)
  where

    type GraphEngineMonad (RelationalGraph db m) = RelationalGraphInterpreter db m

    type EngineVertex (RelationalGraph db m) v = (UUID, Properties)
    type EngineEdge (RelationalGraph db m) e = (UUID, Properties)

    type EngineVertexInsertion (RelationalGraph db m) v = Properties
    type EngineEdgeInsertion (RelationalGraph db m) e = Properties

    type EngineVertexInformation (RelationalGraph db m) v = Properties
    type EngineEdgeInformation (RelationalGraph db m) e = Properties

    getTargetVertex proxyG proxyE proxyV (edgeId, _) = do
        targetId <- rg $ getEdgeTargetId (Proxy :: Proxy db) edgeId
        properties <- rg $ getVertexProperties (Proxy :: Proxy db) targetId
        return (targetId, properties)

    getSourceVertex proxyG proxyE proxyV (edgeId, _) = do
        sourceId <- rg $ getEdgeSourceId (Proxy :: Proxy db) edgeId
        properties <- rg $ getVertexProperties (Proxy :: Proxy db) sourceId
        return (sourceId, properties)

    getVertices proxyG proxyV properties = do
        vertexId <- rgl $ getVertexIdsWithProperties (Proxy :: Proxy db) properties
        properties <- rg $ getVertexProperties (Proxy :: Proxy db) vertexId
        return (vertexId, properties)

    getEdges proxyG proxyE properties = do
        edgeId <- rgl $ getEdgeIdsWithProperties (Proxy :: Proxy db) properties
        properties <- rg $ getEdgeProperties (Proxy :: Proxy db) edgeId
        return (edgeId, properties)

    getEdgesIn proxyG proxyE proxyV edgeproperties (vertexId, _) = do
        edgeId <- rgl $ getIncomingEdges (Proxy :: Proxy db) edgeproperties vertexId
        properties <- rg $ getEdgeProperties (Proxy :: Proxy db) edgeId
        return (edgeId, properties)

    getEdgesOut proxyG proxyE proxyV edgeproperties (vertexId, _) = do
        edgeId <- rgl $ getOutgoingEdges (Proxy :: Proxy db) edgeproperties vertexId
        properties <- rg $ getEdgeProperties (Proxy :: Proxy db) edgeId
        return (edgeId, properties)

    insertVertex proxyG proxyV properties = do
        id :: UUID <- liftIO nextRandom 
        let engineVertex = (id, properties)
        rg $ insertVertexManifest (Proxy :: Proxy db) id
        rg $ forM_ (M.toList properties) (insertVertexProperty (Proxy :: Proxy db) id)
        return engineVertex

    insertEdge proxyG proxyE proxySrc proxyTgt properties (srcid, _) (tgtid, _)= do
        id :: UUID <- liftIO nextRandom
        let engineEdge = (id, properties)
        rg $ insertEdgeManifest (Proxy :: Proxy db) id
        rg $ forM_ (M.toList properties) (insertEdgeProperty (Proxy :: Proxy db) id)
        rg $ insertEdgeAdjacency (Proxy :: Proxy db) id srcid tgtid
        return engineEdge

    -- Delete all properties for the vertex, and re-insert.
    updateVertex proxyG proxyV (id, properties) properties' = do
        rg $ deleteAllVertexProperties (Proxy :: Proxy db) id
        rg $ forM_ (M.toList properties) (insertEdgeProperty (Proxy :: Proxy db) id)
        return (id, properties')

    -- Delete all properties for the edge, and re-insert.
    updateEdge proxyG proxyE (id, properties) properties' = do
        rg $ deleteAllEdgeProperties (Proxy :: Proxy db) id
        rg $ forM_ (M.toList properties) (insertEdgeProperty (Proxy :: Proxy db) id)
        return (id, properties')

    -- Here we just want to delete everything in vertex properties which has
    -- thie vertex's id, and everything from adjacenecy which has its id as
    -- source or target.
    -- Also from the manifest.
    deleteVertex proxyG proxyV (id, properties) = do
        rg $ deleteAllVertexProperties (Proxy :: Proxy db) id
        rg $ deleteAllVertexAdjacency (Proxy :: Proxy db) id
        rg $ deleteVertexManifest (Proxy :: Proxy db) id
        return ()

    -- Here we delete everything in edge properties which has this edge's id,
    -- and everything from adjacency which has its id.
    -- Also from the manifest.
    deleteEdge proxyG proxyE (id, properties) = do
        rg $ deleteAllEdgeProperties (Proxy :: Proxy db) id
        rg $ deleteAllEdgeAdjacency (Proxy :: Proxy db) id
        rg $ deleteEdgeManifest (Proxy :: Proxy db) id
        return ()
