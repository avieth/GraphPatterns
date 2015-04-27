Here we run through Thinkaurelius's getting started documentation (found here
https://github.com/thinkaurelius/titan/wiki/Getting-Started) and offer
alternatives in GraphPatterns.

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
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
>
> import Debug.Trace

First things first: let's describe our types and describe their properties
within a graph.

> newtype Name = Name String
>   deriving (Eq, Show)
>
> type Age = Integer
> -- ^ Titans can be very old; must use big Integer.
> type Time = Integer
> type Place = (Double, Double)
> newtype Reason = Reason String
>   deriving (Eq, Show, Read)
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
>   deriving (Show)
>
> data Lives = Lives (Maybe Reason)
>   deriving (Show)
>
> data HasBattled = HasBattled
>
> data AnyBattled = AnyBattled
>
> data LivesIn = LivesIn
>
> data AnyLives = AnyLives

Building the graph of the gods.

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
> saturnsGrandchild :: GraphPatterns FunctionalGraph (V (EngineVertex FunctionalGraph Demigod) Demigod)
> saturnsGrandchild = do
>   vsaturn <- saturn'
>   saturnsChild <- query $ adjacentIn (Proxy :: Proxy IsFather) (Proxy :: Proxy God) IsFather vsaturn
>   saturnsGrandchild <- query $ adjacentIn (Proxy :: Proxy IsFather) (Proxy :: Proxy Demigod) IsFather saturnsChild
>   return saturnsGrandchild

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

In GraphPatterns, we would use a guard to rule out those edges which are
not in the given circle:

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
>     -- Our circle position is in lat/long and its radius in kilometers.
>     -- We approximate 111km per degree.
>     contains (x1, y1, radius) (x2, y2) = sqrt (((x1 - x2) * 111.0) ** 2.0 + ((y1 - y2) * 111.0) ** 2.0) <= radius
>     battlePlace (Battled _ place) = place

The next example explores various battles involving Hercules.

  gremlin> hercules.out('battled')
  ==>v[36]
  ==>v[40]
  ==>v[44]
  gremlin> hercules.out('battled').map
  ==>{name=nemean, type=monster}
  ==>{name=hydra, type=monster}
  ==>{name=cerberus, type=monster}
  gremlin> hercules.outE('battled').has('time',T.gt,1).inV.name
  ==>hydra
  ==>cerberus

> herculesBattles :: GraphPatterns FunctionalGraph (V (EngineVertex FunctionalGraph Monster) Monster)
> herculesBattles = do
>     vhercules <- saturnsGrandchild
>     ebattled <- query $ outgoing (Proxy :: Proxy Battled) (Proxy :: Proxy Monster) HasBattled vhercules
>     guard $ timeGreaterThan1 (e Proxy ebattled)
>     vmonster <- query $ target (Proxy :: Proxy Demigod) (Proxy :: Proxy Monster) ebattled
>     return vmonster
>   where
>     timeGreaterThan1 (Battled time _) = time > 1

Now we look at beings who live on Tartarus.

  gremlin> pluto = g.V('name','pluto').next()
  ==>v[32]
  gremlin> // who are pluto's cohabitants?
  gremlin> pluto.out('lives').in('lives').name                
  ==>pluto
  ==>cerberus
  gremlin> // pluto can't be his own cohabitant
  gremlin> pluto.out('lives').in('lives').except([pluto]).name
  ==>cerberus
  gremlin> pluto.as('x').out('lives').in('lives').except('x').name
  ==>cerberus

With our type model, this one does not directly translate, because we must
know what kind of thing we're looking for. Since Pluto is a God and Cerberus
is a Monster, there's no need to eliminate Pluto from the result set. We
write instead:

> monstersOnTartarus :: GraphPatterns FunctionalGraph (V (EngineVertex FunctionalGraph Monster) Monster)
> monstersOnTartarus = do
>     vpluto <- query $ vertex (Proxy :: Proxy God) (Name "Pluto")
>     -- We traverse outgoing in lives and then back across incoming.
>     wherePlutoLives <- query $ adjacentOut (Proxy :: Proxy Lives) (Proxy :: Proxy Location) LivesIn vpluto
>     monsterWhoLivesWithPluto <- query $ adjacentIn (Proxy :: Proxy Lives) (Proxy :: Proxy Monster) LivesIn wherePlutoLives
>     return monsterWhoLivesWithPluto

Where do Pluto's brothers live? In gremlin we would write:

  gremlin> // where do pluto's brothers live?
  gremlin> pluto.out('brother').out('lives').name
  ==>sky
  ==>sea
  gremlin> // which brother lives in which place?
  gremlin> pluto.out('brother').as('god').out('lives').as('place').select
  ==>[god:v[16], place:v[8]]
  ==>[god:v[20], place:v[12]]
  gremlin> // what is the name of the brother and the name of the place?
  gremlin> pluto.out('brother').as('god').out('lives').as('place').select{it.name}
  ==>[god:jupiter, place:sky]
  ==>[god:neptune, place:sea]

But here in typeful GraphPatterns, we express it thus:

> vpluto :: GraphPatterns FunctionalGraph (V (EngineVertex FunctionalGraph God) God)
> vpluto = query $ vertex (Proxy :: Proxy God) (Name "Pluto")
>
> plutosBrothers :: GraphPatterns FunctionalGraph (Name, Name)
> plutosBrothers = do
>     vpluto <- vpluto
>     brother <- query $ adjacentOut (Proxy :: Proxy IsBrother) (Proxy :: Proxy God) IsBrother vpluto
>     whereHeLives <- query $ adjacentOut (Proxy :: Proxy Lives) (Proxy :: Proxy Location) LivesIn brother
>     return (brotherName (v Proxy brother), locationName (v Proxy whereHeLives))
>   where
>     brotherName (God n _) = n
>     locationName (Location n) = n

The final example: reasons for living somewhere:

  gremlin> pluto.outE('lives').reason
  ==>no fear of death
  gremlin> g.E.has('reason',CONTAINS,'loves')
  ==>e[6t-g-2q][16-lives->8]
  ==>e[6z-k-2q][20-lives->12]
  gremlin> g.E.has('reason',CONTAINS,'loves').collect{
      [it.outV.name.next(),it.reason,it.inV.name.next()] 
  }
  ==>[jupiter, loves fresh breezes, sky]
  ==>[neptune, loves waves, sea]

Here we don't just check for any old edge that has a reason field containing
the word loves. Instead, we pick out all of the Lives edges because we know
statically that they determine a Maybe Reason, and we go from there:

> reasons :: GraphPatterns FunctionalGraph (God, Maybe Reason, Location)
> reasons = do
>     elives <- query $ edge (Proxy :: Proxy Lives) AnyLives
>     let reason = why (e Proxy elives)
>     guard $ contains "loves" reason
>     god <- query $ source (Proxy :: Proxy God) (Proxy :: Proxy Location) elives
>     location <- query $ target (Proxy :: Proxy God) (Proxy :: Proxy Location) elives
>     return (v Proxy god, reason, v Proxy location)
>   where
>     why (Lives x) = x
>     contains str Nothing = False
>     contains str (Just (Reason str')) = substring str str'
>
> substring :: String -> String -> Bool
> substring [] _ = True
> substring (x : xs) [] = False
> substring (x : xs) (y : ys) = x == y && substring xs ys || substring (x : xs) (ys)

Now the boring part: declaring the interface between our domain and the
FunctionalGraph.

> instance Vertex FunctionalGraph Titan where
>   toEngineVertexInsertion (Titan (Name name) age) = FGVertexInsertion (VertexLabel $
>       M.fromList [("haskellType", "Titan"), ("name", name), ("age", show age)]
>     )
>   fromEngineVertex (FGVertex (_, evertex)) = return (rightType >> (Titan <$> name <*> age))
>     where
>       propertyMap = vertexProperties evertex
>       rightType = M.lookup "haskellType" propertyMap >>= guard . ((==) "Titan")
>       name = Name <$> M.lookup "name" propertyMap
>       age :: Maybe Age
>       age = do
>         ageString <- M.lookup "age" propertyMap
>         case reads ageString of
>           [(i, "")] -> Just i
>           _ -> trace ("oops" ++ ageString) Nothing
>
> instance Vertex FunctionalGraph God where
>   toEngineVertexInsertion (God (Name name) age) = FGVertexInsertion (VertexLabel $
>       M.fromList [("haskellType", "God"), ("name", name), ("age", show age)]
>     )
>   fromEngineVertex (FGVertex (_, evertex)) = return (rightType >> (God <$> name <*> age))
>     where
>       propertyMap = vertexProperties evertex
>       rightType = M.lookup "haskellType" propertyMap >>= guard . ((==) "God")
>       name = Name <$> M.lookup "name" propertyMap
>       age = do
>         ageString <- M.lookup "age" propertyMap
>         case reads ageString of
>           [(i, "")] -> Just i
>           _ -> Nothing
> 
> instance Vertex FunctionalGraph Demigod where
>   toEngineVertexInsertion (Demigod (Name name) age) = FGVertexInsertion (VertexLabel $
>       M.fromList [("haskellType", "Demigod"), ("name", name), ("age", show age)]
>     )
>   fromEngineVertex (FGVertex (_, evertex)) = return (rightType >> (Demigod <$> name <*> age))
>     where
>       propertyMap = vertexProperties evertex
>       rightType = M.lookup "haskellType" propertyMap >>= guard . ((==) "Demigod")
>       name = Name <$> M.lookup "name" propertyMap
>       age = do
>         ageString <- M.lookup "age" propertyMap
>         case reads ageString of
>           [(i, "")] -> Just i
>           _ -> Nothing
>
> instance Vertex FunctionalGraph Human where
>   toEngineVertexInsertion (Human (Name name) age) = FGVertexInsertion (VertexLabel $
>       M.fromList [("haskellType", "Human"), ("name", name), ("age", show age)]
>     )
>   fromEngineVertex (FGVertex (_, evertex)) = return (rightType >> (Human <$> name <*> age))
>     where
>       propertyMap = vertexProperties evertex
>       rightType = M.lookup "haskellType" propertyMap >>= guard . ((==) "Human")
>       name = Name <$> M.lookup "name" propertyMap
>       age = do
>         ageString <- M.lookup "age" propertyMap
>         case reads ageString of
>           [(i, "")] -> Just i
>           _ -> Nothing
>
> instance Vertex FunctionalGraph Monster where
>   toEngineVertexInsertion (Monster (Name name)) = FGVertexInsertion (VertexLabel $
>       M.fromList [("haskellType", "Monster"), ("name", name)]
>     )
>   fromEngineVertex (FGVertex (_, evertex)) = return (rightType >> (Monster <$> name))
>     where
>       propertyMap = vertexProperties evertex
>       rightType = M.lookup "haskellType" propertyMap >>= guard . ((==) "Monster")
>       name = Name <$> M.lookup "name" propertyMap
>
> instance Vertex FunctionalGraph Location where
>   toEngineVertexInsertion (Location (Name name)) = FGVertexInsertion (VertexLabel $
>       M.fromList [("haskellType", "Location"), ("name", name)]
>     )
>   fromEngineVertex (FGVertex (_, evertex)) = return (rightType >> (Location <$> name))
>     where
>       propertyMap = vertexProperties evertex
>       rightType = M.lookup "haskellType" propertyMap >>= guard . ((==) "Location")
>       name = Name <$> M.lookup "name" propertyMap
>
> instance Edge FunctionalGraph IsFather where
>   toEngineEdgeInsertion IsFather = FGEdgeInsertion (EdgeLabel $
>       M.fromList [("haskellType", "IsFather")]
>     )
>   fromEngineEdge (FGEdge (_, _, eedge)) = return (rightType >> pure IsFather)
>     where
>       propertyMap = edgeProperties eedge
>       rightType = M.lookup "haskellType" propertyMap >>= guard . ((==) "IsFather")
>
> instance Edge FunctionalGraph IsMother where
>   toEngineEdgeInsertion IsMother = FGEdgeInsertion (EdgeLabel $
>       M.fromList [("haskellType", "IsMother")]
>     )
>   fromEngineEdge (FGEdge (_, _, eedge)) = return (rightType >> pure IsMother)
>     where
>       propertyMap = edgeProperties eedge
>       rightType = M.lookup "haskellType" propertyMap >>= guard . ((==) "IsMother")
>
> instance Edge FunctionalGraph IsBrother where
>   toEngineEdgeInsertion IsBrother = FGEdgeInsertion (EdgeLabel $
>       M.fromList [("haskellType", "IsBrother")]
>     )
>   fromEngineEdge (FGEdge (_, _, eedge)) = return (rightType >> pure IsBrother)
>     where
>       propertyMap = edgeProperties eedge
>       rightType = M.lookup "haskellType" propertyMap >>= guard . ((==) "IsBrother")
>
> instance Edge FunctionalGraph IsPet where
>   toEngineEdgeInsertion IsPet = FGEdgeInsertion (EdgeLabel $
>       M.fromList [("haskellType", "IsPet")]
>     )
>   fromEngineEdge (FGEdge (_, _, eedge)) = return (rightType >> pure IsPet)
>     where
>       propertyMap = edgeProperties eedge
>       rightType = M.lookup "haskellType" propertyMap >>= guard . ((==) "IsPet")
>
> instance Edge FunctionalGraph Battled where
>   toEngineEdgeInsertion (Battled time place) = FGEdgeInsertion (EdgeLabel $
>       M.fromList [("haskellType", "Battled"), ("time", show time), ("place", show place)]
>     )
>   fromEngineEdge (FGEdge (_, _, eedge)) = return (rightType >> (Battled <$> time <*> place))
>     where
>       propertyMap = edgeProperties eedge
>       rightType = M.lookup "haskellType" propertyMap >>= guard . ((==) "Battled")
>       time = do
>         timeString <- M.lookup "time" propertyMap
>         case reads timeString of
>           [(d, "")] -> Just d
>           _ -> Nothing
>       place = do
>         placeString <- M.lookup "place" propertyMap
>         case reads placeString of
>           [(pair, "")] -> Just pair
>           _ -> Nothing
>
> instance Edge FunctionalGraph Lives where
>   toEngineEdgeInsertion (Lives reason) = FGEdgeInsertion (EdgeLabel $
>       M.fromList [("haskellType", "Lives"), ("reason", show reason)]
>     )
>   fromEngineEdge (FGEdge (_, _, eedge)) = return (rightType >> (Lives <$> reason))
>     where
>       propertyMap = edgeProperties eedge
>       rightType = M.lookup "haskellType" propertyMap >>= guard . ((==) "Lives")
>       reason = do
>         reasonString <- M.lookup "reason" propertyMap
>         case reads reasonString of
>           [(r, "")] -> Just r
>           _ -> Nothing
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
>   -- We'll say Titan names are unique, unlike Human names.
>   type VertexUniqueness FunctionalGraph Titan Name = Unique
>   toEngineVertexInformation (Name n) = FGVertexInfo $ M.singleton "name" n
>
> instance DeterminesVertex FunctionalGraph God Name where
>   type VertexUniqueness FunctionalGraph God Name = NotUnique
>   toEngineVertexInformation (Name n) = FGVertexInfo $ M.singleton "name" n
>
> instance DeterminesEdge FunctionalGraph Battled AnyBattled where
>   type EdgeUniqueness FunctionalGraph Battled AnyBattled = NotUnique
>   toEngineEdgeInformation _ = FGEdgeInfo $ M.singleton "haskellType" "Battled"
>
> instance DeterminesEdge FunctionalGraph Lives AnyLives where
>   type EdgeUniqueness FunctionalGraph Lives AnyLives = NotUnique
>   toEngineEdgeInformation _ = FGEdgeInfo $ M.singleton "haskellType" "Lives"
>
> instance DeterminesLocalEdge FunctionalGraph IsFather God Titan IsFather where
>   toEngineEdgeInformationLocal _ _ IsFather = FGEdgeInfo $ M.singleton "haskellType" "IsFather"
>
> instance DeterminesLocalEdge FunctionalGraph IsFather Demigod God IsFather where
>   toEngineEdgeInformationLocal _ _ IsFather = FGEdgeInfo $ M.singleton "haskellType" "IsFather"
>   
> instance DeterminesLocalEdge FunctionalGraph Battled Demigod Monster HasBattled where
>   toEngineEdgeInformationLocal _ _ HasBattled = FGEdgeInfo $ M.singleton "haskellType" "Battled"
>
> instance DeterminesLocalEdge FunctionalGraph Lives God Location LivesIn where
>   toEngineEdgeInformationLocal _ _ LivesIn = FGEdgeInfo $ M.singleton "haskellType" "Lives"
>
> instance DeterminesLocalEdge FunctionalGraph Lives Monster Location LivesIn where
>   toEngineEdgeInformationLocal _ _ LivesIn = FGEdgeInfo $ M.singleton "haskellType" "Lives"
>
> instance DeterminesLocalEdge FunctionalGraph Lives Location Monster LivesIn where
>   toEngineEdgeInformationLocal _ _ LivesIn = FGEdgeInfo $ M.singleton "haskellType" "Lives"
>
> instance DeterminesLocalEdge FunctionalGraph IsBrother God God IsBrother where
>   toEngineEdgeInformationLocal _ _ IsBrother = FGEdgeInfo $ M.singleton "haskellType" "IsBrother"
