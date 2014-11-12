Here we run through Thinkaurelius's getting started documentation (found here
https://github.com/thinkaurelius/titan/wiki/Getting-Started) and offer
alternatives in GraphPatterns.

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
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

First things first: let's describe our types and describe their properties
within a graph.

> newtype Name = Name String
>   deriving (Eq)
> type Age = Integer
> -- ^ Titans can be very old; must use big Integer.
> type Time = Integer
> type Place = String
> newtype Reason = Reason String
>   deriving (Eq)
> 
> data Titan = Titan Name Age
> data God = God Name Age
> data Human = Human Name Age
> data Demigod = Demigod Name Age
>
> -- Here's our vertex types.
> data Being = BTitan Titan | BGod God | BHuman Human | BDemigod Demigod
> data Monster = Monster Name
> data Location = Location Name
>
> beingName :: Being -> Name
> beingName (BTitan (Titan name _)) = name
> beingName (BGod (God name _)) = name
> beingName (BHuman (Human name _)) = name
> beingName (BDemigod (Demigod name _)) = name
>
> monsterName :: Monster -> Name
> monsterName (Monster name) = name
>
> locationName :: Location -> Name
> locationName (Location name) = name
>
> -- Here's our edge types. We frame them in the language of relationships.
> data IsFather = IsFather
> data IsMother = IsMother
> data IsBrother = IsBrother
> data IsPet = IsPet
> data Battled = Battled Time Place
> data Lives = Lives Reason
>
> timeBattled :: Battled -> Time
> timeBattled (Battled t _) = t
>
> why :: Lives -> Reason
> why (Lives r) = r
>
> -- This is a type used to determine, locally, all edges of Battled type.
> -- It serves the same purpose as a Titan edge label.
> -- Note that for edge types which carry no data, it makes sense to use them
> -- as labels as well, because we can provide them without giving any extra
> -- information (their value constructor is a 0-ary function).
> data HasBattled = HasBattled
> data LivesIn = LivesIn
>
> -- Proof that our vertex types are actually vertices in FunctionalGraph.
> -- We don't care about values right now (only types) so we leave it undefined.
> instance Vertex FunctionalGraph Being where
>   fromEngineVertex (FGVertex (_, v)) = undefined
>   toEngineVertexInsertion b = undefined
>
> instance DeterminesVertex FunctionalGraph Being Being where
>   type VertexUniqueness FunctionalGraph Being Being = NotUnique
>   toEngineVertexInformation = undefined
>

FGVertexInsertion map
     where map = M.fromlist [
                   , ("type", beingType b)
                   , ("name", beingName b)
                   , ("age", show $ beingAge b)
                   ]

>
> instance Vertex FunctionalGraph Monster
>
> instance Vertex FunctionalGraph Location
>
> -- A Name determines possibly many Beings.
> instance DeterminesVertex FunctionalGraph Name Being where
>   type VertexUniqueness FunctionalGraph Name Being = NotUnique
>   toEngineVertexInformation = undefined
>
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
>
> instance Edge FunctionalGraph IsMother where
>   type EdgeSource FunctionalGraph IsMother = Being
>   type EdgeTarget FunctionalGraph IsMother = Being
>   type EdgeCardinalitySource FunctionalGraph IsMother = Single
>   type EdgeCardinalityTarget FunctionalGraph IsMother  = Multi
>
> instance Edge FunctionalGraph IsBrother where
>   type EdgeSource FunctionalGraph IsBrother = Being
>   type EdgeTarget FunctionalGraph IsBrother = Being
>   type EdgeCardinalitySource FunctionalGraph IsBrother = Multi
>   type EdgeCardinalityTarget FunctionalGraph IsBrother = Multi
>   -- ^ Unlike IsMother or IsFather, one can have many brothers, and many
>   -- people can have the same being as a brother. This is a many-to-many
>   -- edge.
>
> instance Edge FunctionalGraph IsPet where
>   type EdgeSource FunctionalGraph IsPet = Being
>   type EdgeTarget FunctionalGraph IsPet = Monster
>   type EdgeCardinalitySource FunctionalGraph IsPet  = Multi
>   type EdgeCardinalityTarget FunctionalGraph IsPet = Multi
>   -- ^ We declare that a pet can have multiple owners.
>
> instance Edge FunctionalGraph Battled where
>   type EdgeSource FunctionalGraph Battled = Being
>   type EdgeTarget FunctionalGraph Battled = Monster
>   type EdgeCardinalitySource FunctionalGraph Battled = Multi
>   type EdgeCardinalityTarget FunctionalGraph Battled = Multi
>   -- ^ A being can battle many monsters, and a monster can be battled by many
>   -- beings.
>
> instance Edge FunctionalGraph Lives where
>   type EdgeSource FunctionalGraph Lives = Being
>   type EdgeTarget FunctionalGraph Lives = Location
>   type EdgeCardinalitySource FunctionalGraph Lives = Single
>   type EdgeCardinalityTarget FunctionalGraph Lives = Multi
>   -- ^ A being lives only in one place (lives, not lived) but many beings
>   -- can live in one place.
>
> -- The type IsFather determines IsFather edges going into or out of a Being.
> instance DeterminesLocalEdge FunctionalGraph Being IsFather IsFather where
>   type EdgeDirection FunctionalGraph Being IsFather IsFather = Both
>   toEngineEdgeInformationLocal = undefined
>
> -- The type IsMother determines IsMother edges going into or out of a Being
> instance DeterminesLocalEdge FunctionalGraph Being IsMother IsMother where
>   type EdgeDirection FunctionalGraph Being IsMother IsMother = Both
>   toEngineEdgeInformationLocal = undefined
>
> instance DeterminesLocalEdge FunctionalGraph Being IsBrother IsBrother where
>   type EdgeDirection FunctionalGraph Being IsBrother IsBrother = Both
>   toEngineEdgeInformationLocal = undefined
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
> instance DeterminesLocalEdge FunctionalGraph Being Lives LivesIn where
>   type EdgeDirection FunctionalGraph Being Lives LivesIn = Out
>   toEngineEdgeInformationLocal = undefined

The titan example in gremlin is as follows:

  hercules = saturn.as('x').in('father').loop('x'){it.loops < 3}.next()

but check out how this can be expressed in GraphPatterns:

> -- We can grab everybody named Hercules
> herculesByName :: GraphQueries FunctionalGraph Being
> herculesByName = v <$> vertex (Proxy :: Proxy Being) (Name "Hercules")
>
> -- Or we can grab everybody named Saturn and find their grandchildren.
> -- Hercules should be in there :)
> herculesByGrandfather :: GraphQueries FunctionalGraph Being
> herculesByGrandfather = do
>   saturn <- vertex (Proxy :: Proxy Being) (Name "Saturn")
>   v <$> adjacentIn (Proxy :: Proxy IsFather) IsFather saturn
>   --fmap concat $ traverse (adjacentIn (Proxy :: Proxy IsFather) IsFather) saturns

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

> pluto :: GraphQueries FunctionalGraph Being
> pluto = v <$> vertex (Proxy :: Proxy Being) (Name "Pluto")

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
