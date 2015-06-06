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
  :: forall db m .
     ( ContainsDatabase db GraphDatabase
     , Monad m
     )
  => UUID
  -> FreeT (RelationalF db) m Properties
getVertexProperties id = do
    rows <- rfrelation (Selection selection)
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
  :: ( ContainsDatabase db GraphDatabase
     , Monad m
     )
  => UUID
  -> FreeT (RelationalF db) m Properties
getEdgeProperties id = do
    rows <- rfrelation (Selection selection)
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
  :: ( ContainsDatabase db GraphDatabase
     , Monad m
     )
  => UUID
  -> FreeT (RelationalF db) m UUID
getEdgeSourceId id = do
    rows <- rfrelation (Selection selection)
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
  :: ( ContainsDatabase db GraphDatabase
     , Monad m
     )
  => UUID
  -> FreeT (RelationalF db) m UUID
getEdgeTargetId id = do
    rows <- rfrelation (Selection selection)
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
  :: ( ContainsDatabase db GraphDatabase
     , Monad m
     )
  => Properties
  -> FreeT (RelationalF db) m [UUID]
getVertexIdsWithProperties properties = do
    rows <- rfrelation (selectVertexIdsWithProperties (M.toList properties))
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
  :: ( ContainsDatabase db GraphDatabase
     , Monad m
     )
  => Properties
  -> FreeT (RelationalF db) m [UUID]
getEdgeIdsWithProperties properties = do
    rows <- rfrelation (selectEdgeIdsWithProperties (M.toList properties))
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
  :: ( ContainsDatabase db GraphDatabase
     , Monad m
     )
  => Properties
  -> UUID
  -> FreeT (RelationalF db) m [UUID]
getOutgoingEdges edgeproperties vertexId = do
    rows <- rfrelation $ Intersection
                (selectEdgeIdsWithProperties (M.toList edgeproperties))
                (selectEdgeIdsWithSource vertexId)
    return $ fmap rowToId rows

getIncomingEdges
  :: ( ContainsDatabase db GraphDatabase
     , Monad m
     )
  => Properties
  -> UUID
  -> FreeT (RelationalF db) m [UUID]
getIncomingEdges edgeproperties vertexId = do
    rows <- rfrelation $ Intersection
                (selectEdgeIdsWithProperties (M.toList edgeproperties))
                (selectEdgeIdsWithTarget vertexId)
    return $ fmap rowToId rows

insertVertexManifest
  :: ( ContainsDatabase db GraphDatabase
     , Monad m
     )
  => UUID
  -> FreeT (RelationalF db) m ()
insertVertexManifest id = rfinsert insertion
  where
    insertion :: Insert VertexManifestTable
    insertion = Insert vertexManifestTable row
    row :: Row VertexManifestSchema
    row = fromColumnAndValue idColumn id :&| EndRow

insertEdgeManifest
  :: ( ContainsDatabase db GraphDatabase
     , Monad m
     )
  => UUID
  -> FreeT (RelationalF db) m ()
insertEdgeManifest id = rfinsert insertion
  where
    insertion :: Insert EdgeManifestTable
    insertion = Insert edgeManifestTable row
    row :: Row EdgeManifestSchema
    row = fromColumnAndValue idColumn id :&| EndRow

insertVertexProperty
  :: ( ContainsDatabase db GraphDatabase
     , Monad m
     )
  => UUID
  -> (T.Text, T.Text)
  -> FreeT (RelationalF db) m ()
insertVertexProperty id (key, value) = rfinsert insertion
  where
    insertion :: Insert VertexPropertyTable
    insertion = Insert vertexPropertyTable row
    row :: Row VertexPropertySchema
    row =     fromColumnAndValue idColumn id
          :&| fromColumnAndValue keyColumn key
          :&| fromColumnAndValue valueColumn value
          :&| EndRow

insertEdgeProperty
  :: ( ContainsDatabase db GraphDatabase
     , Monad m
     )
  => UUID
  -> (T.Text, T.Text)
  -> FreeT (RelationalF db) m ()
insertEdgeProperty id (key, value) = rfinsert insertion
  where
    insertion :: Insert EdgePropertyTable
    insertion = Insert edgePropertyTable row
    row :: Row EdgePropertySchema
    row =     fromColumnAndValue idColumn id
          :&| fromColumnAndValue keyColumn key
          :&| fromColumnAndValue valueColumn value
          :&| EndRow

insertEdgeAdjacency
  :: ( ContainsDatabase db GraphDatabase
     , Monad m
     )
  => UUID
  -> UUID
  -> UUID
  -> FreeT (RelationalF db) m ()
insertEdgeAdjacency edgeId sourceId targetId = rfinsert insertion
  where
    insertion :: Insert AdjacencyTable
    insertion = Insert adjacencyTable row
    row :: Row AdjacencySchema
    row =     fromColumnAndValue idColumn edgeId
          :&| fromColumnAndValue sourceColumn sourceId
          :&| fromColumnAndValue targetColumn targetId
          :&| EndRow

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
        targetId <- rg $ getEdgeTargetId edgeId
        properties <- rg $ getVertexProperties targetId
        return (targetId, properties)

    getSourceVertex proxyG proxyE proxyV (edgeId, _) = do
        sourceId <- rg $ getEdgeSourceId edgeId
        properties <- rg $ getVertexProperties sourceId
        return (sourceId, properties)

    getVertices proxyG proxyV properties = do
        vertexId <- rgl $ getVertexIdsWithProperties properties
        properties <- rg $ getVertexProperties vertexId
        return (vertexId, properties)

    getEdges proxyG proxyE properties = do
        edgeId <- rgl $ getEdgeIdsWithProperties properties
        properties <- rg $ getEdgeProperties edgeId
        return (edgeId, properties)

    getEdgesIn proxyG proxyE proxyV edgeproperties (vertexId, _) = do
        edgeId <- rgl $ getIncomingEdges edgeproperties vertexId
        properties <- rg $ getEdgeProperties edgeId
        return (edgeId, properties)

    getEdgesOut proxyG proxyE proxyV edgeproperties (vertexId, _) = do
        edgeId <- rgl $ getOutgoingEdges edgeproperties vertexId
        properties <- rg $ getEdgeProperties edgeId
        return (edgeId, properties)

    insertVertex proxyG proxyV properties = do
        id :: UUID <- liftIO nextRandom 
        let engineVertex = (id, properties)
        rg $ insertVertexManifest id
        rg $ forM_ (M.toList properties) (insertVertexProperty id)
        return engineVertex

    insertEdge proxyG proxyE proxySrc proxyTgt properties (srcid, _) (tgtid, _)= do
        id :: UUID <- liftIO nextRandom
        let engineEdge = (id, properties)
        rg $ insertEdgeManifest id
        rg $ forM_ (M.toList properties) (insertEdgeProperty id)
        rg $ insertEdgeAdjacency id srcid tgtid
        return engineEdge
