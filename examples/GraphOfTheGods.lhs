Here we run through Thinkaurelius's getting started documentation (found here
https://github.com/thinkaurelius/titan/wiki/Getting-Started) and offer
alternatives in GraphPatterns.

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> 
> module GraphOfTheGods where
>
> import Data.GraphPatterns.Types
> import Data.GraphPatterns.Edge
> import Data.GraphPatterns.Vertex
> import Data.GraphPatterns.Language
>
> import Data.GraphPatterns.Engines.FunctionalGraph
>
> import Data.Proxy
>
> -- Shouldn't be needed, but we have no engines so we just define StupidEngine.
> import Data.GraphPatterns.GraphEngine
> 
> import Control.Applicative
> import Control.Monad
> import Data.Traversable (traverse)
> import Data.Map as M
> import Data.Monoid

First things first: let's describe our types and describe their properties
within a graph.

> newtype Name = Name String
>   deriving (Eq, Show)
>
> nameToString :: Name -> String
> nameToString (Name s) = s
>
> type Age = Integer
> -- ^ Titans can be very old; must use big Integer.
> type Time = Integer
> type Place = (Double, Double)
> newtype Reason = Reason String
>   deriving (Eq)
> 
> data Titan = Titan Name Age
>  deriving (Show)
>
> data God = God Name Age
>  deriving (Show)
>
> data Human = Human Name Age
>  deriving (Show)
>
> data Demigod = Demigod Name Age
>  deriving (Show)
>
> data Monster = Monster Name
>  deriving (Show)
>
> data Location = Location Name
>  deriving (Show)
>
> data IsFather = IsFather
>
> data IsMother = IsMother
>
> data IsBrother = IsBrother
>
> data IsPet = IsPet
>
> data Battled = Battled Time Place
>
> data Lives = Lives (Maybe Reason)
>
> data HasBattled = HasBattled
>
> data AnyBattled = AnyBattled
>
> data LivesIn = LivesIn

Building the graph of the gods.
We start by defining all of the vertices, i.e. normal Haskell data.

> hercules :: Demigod
> hercules = Demigod (Name "Hercules") 30
>
> saturn :: Titan
> saturn = Titan (Name "Saturn") 10000
>
> jupiter :: God
> jupiter = God (Name "Jupiter") 5000
>
> alcmene :: Human
> alcmene = Human (Name "Alcmene") 45
>
> pluto :: God
> pluto = God (Name "Pluto") 4000
>
> neptune :: God
> neptune = God (Name "Neptune") 4500
>
> nemean :: Monster
> nemean = Monster (Name "Nemean")
>
> hydra :: Monster
> hydra = Monster (Name "Hydra")
>
> cerberus :: Monster
> cerberus = Monster (Name "Cerberus")
>
> sky :: Location
> sky = Location (Name "Sky")
>
> sea :: Location
> sea = Location (Name "Sea")
>
> tartarus :: Location
> tartarus = Location (Name "Tartarus")
>
> graphOfTheGods :: GraphPatterns FunctionalGraph ()
> graphOfTheGods = do
>   vhercules <- mutation $ putVertex hercules
>   vsaturn <- mutation $ putVertex saturn
>   vjupiter <- mutation $ putVertex jupiter
>   valcmene <- mutation $ putVertex alcmene
>   vpluto <- mutation $ putVertex pluto
>   vneptune <- mutation $ putVertex neptune
>   vnemean <- mutation $ putVertex nemean
>   vhydra <- mutation $ putVertex hydra
>   vcerberus <- mutation $ putVertex cerberus
>   vsky <- mutation $ putVertex sky
>   vsea <- mutation $ putVertex sea
>   vtartarus <- mutation $ putVertex tartarus
>   mutation $ putEdge IsMother vhercules valcmene
>   mutation $ putEdge IsFather vhercules vjupiter
>   mutation $ putEdge IsFather vjupiter vsaturn
>   -- TODO could define a convenience function to put two symmetric edges.
>   mutation $ putEdge IsBrother vjupiter vpluto
>   mutation $ putEdge IsBrother vpluto vjupiter
>   mutation $ putEdge IsBrother vjupiter vneptune
>   mutation $ putEdge IsBrother vneptune vjupiter
>   mutation $ putEdge IsBrother vpluto vneptune
>   mutation $ putEdge IsBrother vneptune vpluto
>   mutation $ putEdge IsPet vpluto vcerberus
>   mutation $ putEdge (Battled 1 (38.1, 23.7)) vhercules vnemean
>   mutation $ putEdge (Battled 2 (37.7, 23.9)) vhercules vhydra
>   mutation $ putEdge (Battled 12 (39, 22)) vhercules vcerberus
>   mutation $ putEdge (Lives (Just (Reason "loves fresh breezes"))) vjupiter vsky
>   mutation $ putEdge (Lives (Just (Reason "loves waves"))) vneptune vsea
>   mutation $ putEdge (Lives (Just (Reason "no fear of death"))) vpluto vtartarus
>   mutation $ putEdge (Lives Nothing) vcerberus vtartarus
>   return ()

The first query: Saturn's grandchild Hercules.

  gremlin> saturn = g.V('name','saturn').next()
  ==>v[4]
  gremlin> saturn.map()
  ==>name=saturn
  ==>age=10000
  ==>type=titan
  gremlin> saturn.in('father').in('father').name
  ==>hercules

How to express this in GraphPatterns?

> saturn' :: GraphPatterns FunctionalGraph (V (EngineVertex FunctionalGraph Titan) Titan)
> saturn' = query $ vertex (Proxy :: Proxy Titan) (Name "Saturn")
>
> saturnsGrandchild :: GraphPatterns FunctionalGraph Demigod
> saturnsGrandchild = do
>   vsaturn <- saturn'
>   saturnsChild <- query $ adjacentIn (Proxy :: Proxy IsFather) (Proxy :: Proxy God) IsFather vsaturn
>   saturnsGrandchild <- query $ adjacentIn (Proxy :: Proxy IsFather) (Proxy :: Proxy Demigod) IsFather saturnsChild
>   return $ v Proxy saturnsGrandchild

Note a key difference between GraphPatterns and Titan, or between a typeful
graph representation and an untyped one: we have to know the type of
Titan's child, and the type of that child's child, or we're out of luck.
A typeful graph representation is therefore not necessarily suitable in any
given application of an untyped one! It's also a lot more verbose.

Later in the document, Hecules's status as a demigod is shown by examining
the status of his parents. 

  gremlin> hercules.out('father','mother')
  ==>v[16]
  ==>v[28]
  gremlin> hercules.out('father','mother').name
  ==>jupiter
  ==>alcmene
  gremlin> hercules.out('father','mother').type
  ==>god
  ==>human
  gremlin> hercules.type
  ==>demigod

It's interesting to note that with a typeful graph representation, a query
like this can degenerate into nonsense: in this example, we've included
the fact that Hercules is a demigod _statically_. We _already_ know it, so
why ask the graph to prove it? But we could have ditched the titan, god,
demigod, human, monster types altogether and then a query like this _would_
make sense. Indeed, we could recover a Titan-style graph by giving Vertex
and Edge instances to some Any type!

Next we find all places within some circle. Here's the gremlin query:

  gremlin> g.E.has('place',WITHIN,Geoshape.circle(37.97,23.72,50))
  ==>e[6J-o-2i][24-battled->36]
  ==>e[6L-o-2i][24-battled->40]
  gremlin> g.E.has('place',WITHIN,Geoshape.circle(37.97,23.72,50)).collect {
       it.bothV.name.next(2) 
  }
  ==>[hercules, hydra]
  ==>[hercules, nemean]

In GraphPatterns, we would write:

> type Circle = (Double, Double, Double)
>
> inArea :: Circle -> GraphPatterns FunctionalGraph (Demigod, Monster)
> inArea circle = do
>     ebattled <- query $ edge (Proxy :: Proxy Battled) AnyBattled
>     guard $ contains circle (battlePlace (e Proxy ebattled))
>     vdemigod <- query $ source (Proxy :: Proxy Demigod) (Proxy :: Proxy Monster) ebattled
>     vmonster <- query $ target (Proxy :: Proxy Demigod) (Proxy :: Proxy Monster) ebattled
>     return (v Proxy vdemigod, v Proxy vmonster)
>   where
>     contains (x1, y1, radiusSq) (x2, y2) = (x1 - x2) ** 2.0 + (y1 - y2) ** 2.0 <= radiusSq
>     battlePlace (Battled _ place) = place



> instance Vertex FunctionalGraph Titan where
>   toEngineVertexInsertion = undefined
>   fromEngineVertex = undefined
>
> instance Vertex FunctionalGraph God where
>   toEngineVertexInsertion = undefined
>   fromEngineVertex = undefined
> 
> instance Vertex FunctionalGraph Demigod where
>   toEngineVertexInsertion = undefined
>   fromEngineVertex = undefined
>
> instance Vertex FunctionalGraph Human where
>   toEngineVertexInsertion = undefined
>   fromEngineVertex = undefined
>
> instance Vertex FunctionalGraph Monster where
>   toEngineVertexInsertion = undefined
>   fromEngineVertex = undefined
>
> instance Vertex FunctionalGraph Location where
>   toEngineVertexInsertion = undefined
>   fromEngineVertex = undefined
>
> instance Edge FunctionalGraph IsFather where
>   toEngineEdgeInsertion = undefined
>   fromEngineEdge = undefined
>
> instance Edge FunctionalGraph IsMother where
>   toEngineEdgeInsertion = undefined
>   fromEngineEdge = undefined
>
> instance Edge FunctionalGraph IsBrother where
>   toEngineEdgeInsertion = undefined
>   fromEngineEdge = undefined
>
> instance Edge FunctionalGraph IsPet where
>   toEngineEdgeInsertion = undefined
>   fromEngineEdge = undefined
>
> instance Edge FunctionalGraph Battled where
>   toEngineEdgeInsertion = undefined
>   fromEngineEdge = undefined
>
> instance Edge FunctionalGraph Lives where
>   toEngineEdgeInsertion = undefined
>   fromEngineEdge = undefined
>
> instance EdgeRelated IsFather Titan Titan where
>   type EdgeCardinalitySource IsFather Titan Titan = Single
>   type EdgeCardinalityTarget IsFather Titan Titan = Multi
>
> instance EdgeRelated IsFather God Titan where
>   type EdgeCardinalitySource IsFather God Titan = Single
>   type EdgeCardinalityTarget IsFather God Titan = Multi
>
> instance EdgeRelated IsFather Demigod Titan where
>   type EdgeCardinalitySource IsFather Demigod Titan = Single
>   type EdgeCardinalityTarget IsFather Demigod Titan = Multi
>
> instance EdgeRelated IsFather Demigod Human where
>   type EdgeCardinalitySource IsFather Demigod Human = Single
>   type EdgeCardinalityTarget IsFather Demigod Human = Multi
>
> instance EdgeRelated IsFather Demigod God where
>   type EdgeCardinalitySource IsFather Demigod God = Single
>   type EdgeCardinalityTarget IsFather Demigod God = Multi
>
> instance EdgeRelated IsFather God God where
>   type EdgeCardinalitySource IsFather God God = Single
>   type EdgeCardinalityTarget IsFather God God = Multi
>
> instance EdgeRelated IsFather Human Human where
>   type EdgeCardinalitySource IsFather Human Human = Single
>   type EdgeCardinalityTarget IsFather Human Human = Multi
>
> instance EdgeRelated IsMother Demigod Human where
>   type EdgeCardinalitySource IsMother Demigod Human = Single
>   type EdgeCardinalityTarget IsMother Demigod Human = Multi
>
> instance EdgeRelated IsMother Demigod God where
>   type EdgeCardinalitySource IsMother Demigod God = Single
>   type EdgeCardinalityTarget IsMother Demigod God = Multi
>
> instance EdgeRelated IsMother God God where
>   type EdgeCardinalitySource IsMother God God = Single
>   type EdgeCardinalityTarget IsMother God God = Multi
>
> instance EdgeRelated IsMother Human Human where
>   type EdgeCardinalitySource IsMother Human Human = Single
>   type EdgeCardinalityTarget IsMother Human Human = Multi
>
> instance EdgeRelated IsBrother Titan Titan where
>   type EdgeCardinalitySource IsBrother Titan Titan = Multi
>   type EdgeCardinalityTarget IsBrother Titan Titan = Multi
>   
> instance EdgeRelated IsBrother God God where
>   type EdgeCardinalitySource IsBrother God God = Multi
>   type EdgeCardinalityTarget IsBrother God God = Multi
>
> instance EdgeRelated IsBrother Demigod Demigod where
>   type EdgeCardinalitySource IsBrother Demigod Demigod = Multi
>   type EdgeCardinalityTarget IsBrother Demigod Demigod = Multi
>
> instance EdgeRelated IsBrother Human Human where
>   type EdgeCardinalitySource IsBrother Human Human = Multi
>   type EdgeCardinalityTarget IsBrother Human Human = Multi
>
> instance EdgeRelated IsPet God Monster where
>   type EdgeCardinalitySource IsPet God Monster = Multi
>   type EdgeCardinalityTarget IsPet God Monster = Single
>
> instance EdgeRelated Battled Demigod Monster where
>   type EdgeCardinalitySource Battled Demigod Monster = Multi
>   type EdgeCardinalityTarget Battled Demigod Monster = Multi
>
> instance EdgeRelated Lives Titan Location where
>   type EdgeCardinalitySource Lives Titan Location = Single
>   type EdgeCardinalityTarget Lives Titan Location = Multi
>
> instance EdgeRelated Lives God Location where
>   type EdgeCardinalitySource Lives God Location = Single
>   type EdgeCardinalityTarget Lives God Location = Multi
>
> instance EdgeRelated Lives Monster Location where
>   type EdgeCardinalitySource Lives Monster Location = Single
>   type EdgeCardinalityTarget Lives Monster Location = Multi
>
> instance DeterminesVertex FunctionalGraph Titan Name where
>   type VertexUniqueness FunctionalGraph Titan Name = Unique
>   toEngineVertexInformation = undefined
>
> instance DeterminesVertex FunctionalGraph God Name where
>   type VertexUniqueness FunctionalGraph God Name = Unique
>   toEngineVertexInformation = undefined
>
> instance DeterminesEdge FunctionalGraph Battled AnyBattled where
>   type EdgeUniqueness FunctionalGraph Battled AnyBattled = NotUnique
>   toEngineEdgeInformation = undefined
>
> instance DeterminesLocalEdge FunctionalGraph IsFather God Titan IsFather where
>   toEngineEdgeInformationLocal = undefined
>
> instance DeterminesLocalEdge FunctionalGraph IsFather Demigod God IsFather where
>   toEngineEdgeInformationLocal = undefined
>   

> {-
>
> testBeingInsert :: Being -> GraphPatterns FunctionalGraph ()
> testBeingInsert being = mutation $ putVertex being (Proxy :: Proxy Being) >> return ()
>
> testBeingQuery :: Being -> GraphPatterns FunctionalGraph Being
> testBeingQuery being = query $ do
>     vert <- vertex Proxy being
>     return $ v vert
>
> testBeingNameQuery :: Name -> GraphPatterns FunctionalGraph Being
> testBeingNameQuery name = query $ do
>     vert <- vertex Proxy name
>     return $ v vert
>
> getByName :: DeterminesVertex m Name thing => Name -> GraphPatterns m thing
> getByName name = do
>   vert <- query $ vertex Proxy name
>   return $ v vert

The titan example in gremlin is as follows:

  hercules = saturn.as('x').in('father').loop('x'){it.loops < 3}.next()

but check out how this can be expressed in GraphPatterns:

> herculesQ :: GraphPatterns FunctionalGraph Being
> herculesQ = do
>   saturn <- query $ vertex Proxy saturn
>   -- We could also grab it by name.
>   --saturn <- query $ vertex Proxy (Name "Saturn")
>   someGuy <- query $ adjacentIn (Proxy :: Proxy IsFather) IsFather saturn
>   hercules <- query $ adjacentIn (Proxy :: Proxy IsFather) IsFather someGuy
>   return $ v hercules

> -- Proof that our vertex types are actually vertices in FunctionalGraph.
> -- We don't care about values right now (only types) so we leave it undefined.
> instance Vertex FunctionalGraph Being where
>   fromEngineVertex (FGVertex (_, v)) = haskellType *> beingType
>     where
>       haskellType = case M.lookup "haskellType" properties of
>         Just "Being" -> Just ()
>         Nothing -> Nothing
>       beingType = case M.lookup "type" properties of
>         Just "Titan" -> (Just $ \name age -> BTitan (Titan name age)) <*> beingName <*> beingAge
>         Just "God" -> (Just $ \name age -> BGod (God name age)) <*> beingName <*> beingAge
>         Just "Human" -> (Just $ \name age -> BHuman (Human name age)) <*> beingName <*> beingAge
>         Just "Demigod" -> (Just $ \name age -> BDemigod (Demigod name age)) <*> beingName <*> beingAge
>         Just "Monster" -> (Just $ \name _ -> BMonster (Monster name)) <*> beingName
>         _ -> Nothing
>       beingName = fmap makeName (M.lookup "name" properties)
>       beingAge = (M.lookup "age" properties) >>= makeAge
>       properties = vertexProperties v
>       makeName :: MapValue -> Name
>       makeName = Name
>       makeAge :: MapValue -> Maybe Age
>       makeAge val = case reads val of
>         [(i,"")] -> Just i
>         _ -> Nothing
>   toEngineVertexInsertion = FGVertexInsertion . VertexLabel . beingProperties
>
> beingProperties :: Being -> M.Map MapKey MapValue
> beingProperties b =
>        M.singleton "haskellType" "Being"
>     <> M.singleton "type" beingType
>     <> M.singleton "name" (nameToString (beingName b))
>     <> age
>   where
>     beingType = case b of
>       BTitan _ -> "Titan"
>       BGod _ -> "God"
>       BHuman _ -> "Human"
>       BDemigod _ -> "Demigod"
>       BMonster _ -> "Monster"
>     age = case b of
>       BTitan (Titan _ i) = i
>       BGod (God _ i) = i
>       BHuman (Human _ i) = i
>       BDemigod (Demigod _ i) = i
>       BMonster _ -> M.empty
>     ageMap i = M.singleton "age" (show i)
>

> instance Vertex FunctionalGraph Location where
>   fromEngineVertex (FGVertex (_, v)) = haskellType *> location
>     where
>       haskellType = case M.lookup "haskellType" properties of
>         Just "Location" -> Just ()
>         Nothing -> Nothing
>       location = fmap Location locationName
>       locationName = fmap Name (M.lookup "name" properties)
>       properties = vertexProperties v
>   toEngineVertexInsertion = FGVertexInsertion . VertexLabel . locationProperties
>
> locationProperties :: Location -> M.Map MapKey MapValue
> locationProperties location =
>        M.singleton "haskellType" "Location"
>     <> M.singleton "name" (nameToString (locationName location))
>
> monsterProperties :: Monster -> M.Map MapKey MapValue
> monsterProperties monster =
>        M.singleton "haskellType" "Being"
>     <>
>     <> M.singleton "name" (nameToString (monsterName monster))
>
> instance DeterminesVertex FunctionalGraph Being Being where
>   type VertexUniqueness FunctionalGraph Being Being = Unique
>   toEngineVertexInformation _ _ = FGVertexInfo . beingProperties
>
> instance DeterminesVertex FunctionalGraph Monster Being where
>   type VertexUniqueness FunctionalGraph Monster Being = Unique
>   toEngineVertexInformation _ _ = FGVertexInfo . monsterProperties
>
> instance DeterminesVertex FunctionalGraph Location Location where
>   type VertexUniqueness FunctionalGraph Location Location = Unique
>   toEngineVertexInformation _ _ = FGVertexInfo . locationProperties
>
> instance DeterminesVertex FunctionalGraph Titan Being where
>   type VertexUniqueness FunctionalGraph Titan Being = NotUnique
>   toEngineVertexInformation _ _ (Titan name age) = FGVertexInfo map
>     where
>       map = M.singleton "haskellType" "Being"
>          <> M.singleton "type" "Titan"
>          <> M.singleton "name" (nameToString name)
>          <> M.singleton "age" (show age)
>
> instance DeterminesVertex FunctionalGraph God Being where
>   type VertexUniqueness FunctionalGraph God Being = NotUnique
>   toEngineVertexInformation _ _ (God name age) = FGVertexInfo map
>     where
>       map = M.singleton "haskellType" "Being"
>          <> M.singleton "type" "God"
>          <> M.singleton "name" (nameToString name)
>          <> M.singleton "age" (show age)
>
> instance DeterminesVertex FunctionalGraph Human Being where
>   type VertexUniqueness FunctionalGraph Human Being = NotUnique
>   toEngineVertexInformation _ _ (Human name age) = FGVertexInfo map
>     where
>       map = M.singleton "haskellType" "Being"
>          <> M.singleton "type" "Human"
>          <> M.singleton "name" (nameToString name)
>          <> M.singleton "age" (show age)
>
> instance DeterminesVertex FunctionalGraph Demigod Being where
>   type VertexUniqueness FunctionalGraph Demigod Being = NotUnique
>   toEngineVertexInformation _ _ (Demigod name age) = FGVertexInfo map
>     where
>       map = M.singleton "haskellType" "Being"
>          <> M.singleton "type" "Demigod"
>          <> M.singleton "name" (nameToString name)
>          <> M.singleton "age" (show age)
>
> instance DeterminesVertex FunctionalGraph Name Being where
>   type VertexUniqueness FunctionalGraph Name Being = NotUnique
>   toEngineVertexInformation _ _ name = FGVertexInfo map
>     where
>       map = M.singleton "haskellType" "Being"
>          <> M.singleton "name" (nameToString name)
>
> instance DeterminesVertex FunctionalGraph Name Monster where
>   type VertexUniqueness FunctionalGraph Name Monster = NotUnique
>   toEngineVertexInformation _ _ name = FGVertexInfo map
>     where
>       map = M.singleton "haskellType" "Monster"
>          <> M.singleton "name" (nameToString name)
>
> instance DeterminesVertex FunctionalGraph Name Location where
>   type VertexUniqueness FunctionalGraph Name Location = NotUnique
>   toEngineVertexInformation _ _ name = FGVertexInfo map
>     where
>       map = M.singleton "haskellType" "Location"
>          <> M.singleton "name" (nameToString name)

> instance DeterminesEdge FunctionalGraph Reason Lives where
>   type EdgeUniqueness FunctionalGraph Reason Lives = NotUnique
>   toEngineEdgeInformation = undefined
>
> -- Proof that our edge types are indeed edges in FunctionalGraph. Again, we leave
> -- the implementation undefined.
> instance Edge FunctionalGraph IsFather where
>   type EdgeSource FunctionalGraph IsFather = Being
>   type EdgeTarget FunctionalGraph IsFather = Being
>   type EdgeCardinalitySource FunctionalGraph IsFather = Single
>   type EdgeCardinalityTarget FunctionalGraph IsFather = Multi
>   -- ^ These cardinalities tell us that the target of an IsFather edge can
>   -- have more than one in-incident IsFather edge, but that the source has
>   -- at most one out-incident IsFather edge.
>   toEngineEdgeInsertion = FGEdgeInsertion . EdgeLabel . const (M.singleton "haskellType" "IsFather")
>   fromEngineEdge (FGEdge (_, _, e)) = haskellType *> pure IsFather
>     where
>       haskellType = case M.lookup "haskellType" properties of
>         Just "IsFather" -> Just ()
>         Nothing -> Nothing
>       properties = edgeProperties e
>
> instance Edge FunctionalGraph IsMother where
>   type EdgeSource FunctionalGraph IsMother = Being
>   type EdgeTarget FunctionalGraph IsMother = Being
>   type EdgeCardinalitySource FunctionalGraph IsMother = Single
>   type EdgeCardinalityTarget FunctionalGraph IsMother  = Multi
>   toEngineEdgeInsertion = FGEdgeInsertion . EdgeLabel . const (M.singleton "haskellType" "IsMother")
>   fromEngineEdge (FGEdge (_, _, e)) = haskellType *> pure IsMother
>     where
>       haskellType = case M.lookup "haskellType" properties of
>         Just "IsMother" -> Just ()
>         Nothing -> Nothing
>       properties = edgeProperties e
>
> instance Edge FunctionalGraph IsBrother where
>   type EdgeSource FunctionalGraph IsBrother = Being
>   type EdgeTarget FunctionalGraph IsBrother = Being
>   type EdgeCardinalitySource FunctionalGraph IsBrother = Multi
>   type EdgeCardinalityTarget FunctionalGraph IsBrother = Multi
>   -- ^ Unlike IsMother or IsFather, one can have many brothers, and many
>   -- people can have the same being as a brother. This is a many-to-many
>   -- edge.
>   toEngineEdgeInsertion = FGEdgeInsertion . EdgeLabel . const (M.singleton "haskellType" "IsBrother")
>   fromEngineEdge (FGEdge (_, _, e)) = haskellType *> pure IsBrother
>     where
>       haskellType = case M.lookup "haskellType" properties of
>         Just "IsBrother" -> Just ()
>         Nothing -> Nothing
>       properties = edgeProperties e
>
> instance Edge FunctionalGraph IsPet where
>   type EdgeSource FunctionalGraph IsPet = Being
>   type EdgeTarget FunctionalGraph IsPet = Monster
>   type EdgeCardinalitySource FunctionalGraph IsPet  = Multi
>   type EdgeCardinalityTarget FunctionalGraph IsPet = Multi
>   -- ^ We declare that a pet can have multiple owners.
>   toEngineEdgeInsertion = undefined
>   fromEngineEdge = undefined
>
> instance Edge FunctionalGraph Battled where
>   type EdgeSource FunctionalGraph Battled = Being
>   type EdgeTarget FunctionalGraph Battled = Monster
>   type EdgeCardinalitySource FunctionalGraph Battled = Multi
>   type EdgeCardinalityTarget FunctionalGraph Battled = Multi
>   -- ^ A being can battle many monsters, and a monster can be battled by many
>   -- beings.
>   toEngineEdgeInsertion = undefined
>   fromEngineEdge = undefined

Problem: Lives needs to take Either Being Monster as its EdgeSource.
BUT we want to be able to put an edge of this type at a Being or a Monster, without
upping them into the sum. In this way, we don't force users to describe
their vertices as the least upper bound of everything that is a valid choice
for the edges into or out of that vertex.

Solution: SubVertex class in Vertex.hs

> instance Edge FunctionalGraph Lives where
>   type EdgeSource FunctionalGraph Lives = Either Monster Being
>   type EdgeTarget FunctionalGraph Lives = Location
>   type EdgeCardinalitySource FunctionalGraph Lives = Single
>   type EdgeCardinalityTarget FunctionalGraph Lives = Multi
>   -- ^ A being lives only in one place (lives, not lived) but many beings
>   -- can live in one place.
>   toEngineEdgeInsertion = undefined
>   fromEngineEdge = undefined
>
> instance DeterminesEdge FunctionalGraph IsFather IsFather where
>   type EdgeUniqueness FunctionalGraph IsFather IsFather = NotUnique
>   toEngineEdgeInformation proxy1 proxy2 IsFather = FGEdgeInfo map
>     where
>       map = M.singleton "haskellType" "IsFather"
>
> -- The type IsFather determines IsFather edges going into or out of a Being.
> instance DeterminesLocalEdge FunctionalGraph Being IsFather IsFather where
>   type EdgeDirection FunctionalGraph Being IsFather IsFather = Both
>   toEngineEdgeInformationLocal proxy1 _ proxy2 determiner = toEngineEdgeInformation proxy1 proxy2 determiner
>
> instance DeterminesEdge FunctionalGraph IsMother IsMother where
>   type EdgeUniqueness FunctionalGraph IsMother IsMother = NotUnique
>   toEngineEdgeInformation proxy1 proxy2 IsMother = FGEdgeInfo map
>     where
>       map = M.singleton "haskellType" "IsMother"
>
> -- The type IsMother determines IsMother edges going into or out of a Being
> instance DeterminesLocalEdge FunctionalGraph Being IsMother IsMother where
>   type EdgeDirection FunctionalGraph Being IsMother IsMother = Both
>   toEngineEdgeInformationLocal proxy1 _ proxy2 determiner = toEngineEdgeInformation proxy1 proxy2 determiner
>
> instance DeterminesEdge FunctionalGraph IsBrother IsBrother where
>   type EdgeUniqueness FunctionalGraph IsBrother IsBrother = NotUnique
>   toEngineEdgeInformation proxy1 proxy2 IsBrother = FGEdgeInfo map
>     where
>       map = M.singleton "haskellType" "IsBrother"
>
> instance DeterminesLocalEdge FunctionalGraph Being IsBrother IsBrother where
>   type EdgeDirection FunctionalGraph Being IsBrother IsBrother = Both
>   toEngineEdgeInformationLocal proxy1 _ proxy2 determiner = toEngineEdgeInformation proxy1 proxy2 determiner
>
> instance DeterminesLocalEdge FunctionalGraph Being IsPet IsPet where
>   type EdgeDirection FunctionalGraph Being IsPet IsPet = Out
>   toEngineEdgeInformationLocal = undefined
>
> instance DeterminesLocalEdge FunctionalGraph Being Battled HasBattled where
>   type EdgeDirection FunctionalGraph Being Battled HasBattled = Out
>   toEngineEdgeInformationLocal = undefined

> instance DeterminesLocalEdge FunctionalGraph Location Lives LivesIn where
>   type EdgeDirection FunctionalGraph Location Lives LivesIn = In
>   toEngineEdgeInformationLocal = undefined
>
> instance DeterminesLocalEdge FunctionalGraph (Either Monster Being) Lives LivesIn where
>   type EdgeDirection FunctionalGraph (Either Monster Being) Lives LivesIn = Out
>   toEngineEdgeInformationLocal = undefined
>
> instance SubVertex FunctionalGraph Being (Either Monster Being) where
>   subVertexInjection _ = Right
>   subVertexRetraction _ (Right b) = Just b
>   subVertexRetraction _ (Left _) = Nothing
>
> instance SubVertex FunctionalGraph Monster (Either Monster Being) where
>   subVertexInjection _ = Left
>   subVertexRetraction _ (Right _) = Nothing
>   subVertexRetraction _ (Left m) = Just m
>
> instance Vertex FunctionalGraph (Either Monster Being) where
>   toEngineVertexInsertion v =
>     toEngineVertexInsertion (subVertexInjection (Proxy :: Proxy FunctionalGraph) v)
>   fromEngineVertex ev =
>     fromEngineVertex ev >>= subVertexRetraction (Proxy :: Proxy FunctionalGraph)

> -- We can grab everybody named Hercules
> herculesByName :: GraphQueries FunctionalGraph Being
> herculesByName = v <$> vertex (Proxy :: Proxy Being) (Name "Hercules")

We now wish to prove that Hercules is a demigod.
Here's the gremlin recipe:

  gremlin> hercules.out('father','mother').type
  ==>god
  ==>human

but how can we express this in GraphPatterns? Well, we can be a little more
particular about our types:

> data Status = IsGod | IsHuman | IsDemigod
>
> getStatus :: Being -> Status
> getStatus (BTitan _) = IsGod
> getStatus (BGod _) = IsGod
> getStatus (BHuman _) = IsHuman
> getStatus (BDemigod _) = IsDemigod
>
> breed IsGod IsGod = IsGod
> breed IsGod _ = IsDemigod
> breed IsHuman IsHuman = IsHuman
> breed _ _ = IsDemigod

  hercules.out('father','mother')

> parentsOf :: Being -> GraphQueries FunctionalGraph (Being, Being)
> parentsOf being = do
>   beingVertex <- vertex (Proxy :: Proxy Being) being
>   mother <- v <$> adjacentOut (Proxy :: Proxy IsMother) IsMother beingVertex
>   father <- v <$> adjacentOut (Proxy :: Proxy IsFather) IsFather beingVertex
>   return (mother, father)
>
> lift2 :: (a -> b) -> (a, a) -> (b, b)
> lift2 f (x, y) = (f x, f y)
>
> apply2 :: (a -> b -> c) -> (a, b) -> c
> apply2 f (x, y) = f x y

  hercules.out('father','mother').name

> namesOfParents :: Being -> GraphQueries FunctionalGraph (Name, Name)
> namesOfParents = fmap (lift2 beingName) . parentsOf

Note that what this example calls type, we call status. In our world, a type
is not a value on the vertex.

  hercules.out('father','mother').type

> statusOfParents :: Being -> GraphQueries FunctionalGraph (Status, Status)
> statusOfParents = fmap (lift2 getStatus) . parentsOf

Ok, we can compute this one purely, but for the sake of demonstration, we
express it in GraphPatterns as well.

  hercules.type

> statusOfPerson :: Being -> GraphQueries FunctionalGraph Status
> statusOfPerson = fmap (apply2 breed) . statusOfParents

Now we move away from lineage and on to violence.

  hercules.out('battled')

> monstersBattled :: Being -> GraphQueries FunctionalGraph Monster
> monstersBattled being = do
>   beingVertex <- vertex (Proxy :: Proxy Being) being
>   v <$> adjacentOut (Proxy :: Proxy Battled) HasBattled beingVertex

This one doesn't really translate to our system; why ask for the map when
we know statically what data a Monster determines?

  hercules.out('battled').map

> namesOfMonstersBattled :: Being -> GraphQueries FunctionalGraph Name
> namesOfMonstersBattled = fmap monsterName . monstersBattled

Aha, here's a good one

  hercules.outE('battled').has('time',T.gt,1).inV.name

> battledRecently :: Time -> Being -> GraphQueries FunctionalGraph Monster
> battledRecently time being = do
>   beingVertex :: V FunctionalGraph Being <- vertex (Proxy :: Proxy Being) being
>   battledEdge :: E FunctionalGraph Battled <- outgoing HasBattled beingVertex
>   let battled :: Battled = e battledEdge
>   -- Observe the use of MonadPlus to cast out the undesirables.
>   guard $ timeBattled battled > time
>   v <$> target battledEdge
>
> nameOfBattledRecently :: Time -> Being -> GraphQueries FunctionalGraph Name
> nameOfBattledRecently time = fmap monsterName . battledRecently time

Let's move on to the next section. This one is just like finding Hercules.

  pluto = g.V('name','pluto').next()

> pluto' :: GraphQueries FunctionalGraph Being
> pluto' = v <$> vertex (Proxy :: Proxy Being) (Name "Pluto")

  pluto.out('lives').in('lives').name

> roomates :: Being -> GraphQueries FunctionalGraph Being
> roomates being = do
>   beingVertex <- vertex (Proxy :: Proxy Being) being
>   edge :: E FunctionalGraph Lives <- outgoing LivesIn beingVertex
>   location <- target edge
>   v <$> adjacentIn (Proxy :: Proxy Lives) LivesIn location

  pluto.out('lives').in('lives').except([pluto]).name
  pluto.as('x').out('lives').in('lives').except('x').name

> roomatesProper :: Being -> GraphQueries FunctionalGraph Being
> roomatesProper being = do
>   roomate <- roomates being
>   guard $ (beingName roomate) /= (beingName being)
>   -- ^ No Eq on Beings, so we settle for Eq on names... which is not a good
>   --   Eq instance, but oh well.
>   return roomate

  pluto.out('brother').out('lives').name

> brothersPlace :: Being -> GraphQueries FunctionalGraph Location
> brothersPlace being = do
>   beingVertex <- vertex (Proxy :: Proxy Being) being
>   brother <- adjacentOut (Proxy :: Proxy IsBrother) IsBrother beingVertex
>   v <$> adjacentOut (Proxy :: Proxy Lives) LivesIn brother
>
> nameOfBrothersPlace :: Being -> GraphQueries FunctionalGraph Name
> nameOfBrothersPlace = fmap locationName . brothersPlace

  pluto.out('brother').as('god').out('lives').as('place').select

> brotherAndAbode :: Being -> GraphQueries FunctionalGraph (Being, Location)
> brotherAndAbode being = do
>   beingVertex <- vertex (Proxy :: Proxy Being) being
>   godVertex <- undefined -- adjacentOut (Proxy :: Proxy IsBrother) IsBrother beingVertex
>   placeVertex <- undefined -- adjacentOut (Proxy :: Proxy Lives) LivesIn godVertex
>   god <- v godVertex
>   place <- v placeVertex
>   return (god, place)

pluto.out('brother').as('god').out('lives').as('place').select{it.name}

> namesOfBrotherAndAbode :: Being -> GraphQueries FunctionalGraph (Name, Name)
> namesOfBrotherAndAbode being = do
>   (brother, location) <- brotherAndAbode being
>   return (beingName brother, locationName location)

  pluto.outE('lives').reason

> reasonForLivingThere :: Being -> GraphQueries FunctionalGraph Reason
> reasonForLivingThere being = do
>   beingVertex <- vertex (Proxy :: Proxy Being) being
>   edge <- e <$> outgoing LivesIn beingVertex
>   return $ why edge

  g.E.has('reason',CONTAINS,'loves')

> --lovesLivingThere :: GraphQueries FunctionalGraph Lives
> lovesLivingThere = edge (Proxy :: Proxy Lives) (Reason "Loves")

  g.E.has('reason',CONTAINS,'loves').collect{
      [it.outV.name.next(),it.reason,it.inV.name.next()] 
  }

> lovesLivingThere' :: GraphQueries FunctionalGraph (Name, Reason, Name)
> lovesLivingThere' = do
>   lives <- lovesLivingThere
>   being <- source lives
>   location <- target lives
>   return (beingName . v $ being, why . e $ lives, locationName . v $ location)

> -}
