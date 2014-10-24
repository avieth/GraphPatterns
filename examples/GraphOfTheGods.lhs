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
> import Data.Proxy
>
> -- Shouldn't be needed, but we have no engines so we just define StupidEngine.
> import Data.GraphPatterns.GraphEngine
> 
> import Control.Applicative
> import Control.Monad
> import Data.Traversable (traverse)

First things first: let's describe our types. We need a GraphEngine but since
we're not interested in actually doing any work, just describing it, we'll
use the unimplemented StupidGraph.

> data StupidGraph a = StupidGraph
> instance Functor StupidGraph
> instance Applicative StupidGraph
> instance Monad StupidGraph
> instance GraphEngine StupidGraph
>
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
> -- Proof that our vertex types are actually vertices in StupidGraph.
> -- We don't care about values right now (only types) so we leave it undefined.
> instance Vertex StupidGraph Being where
>   toEngineVertex = undefined
>   fromEngineVertex = undefined
>
> instance Vertex StupidGraph Monster where
>   toEngineVertex = undefined
>   fromEngineVertex = undefined
>
> instance Vertex StupidGraph Location where
>   toEngineVertex = undefined
>   fromEngineVertex = undefined
>
> -- A Name determines possibly many Beings.
> instance DeterminesVertex StupidGraph Name Being where
>   type VertexUniqueness StupidGraph Name Being = NotUnique
>   toEngineVertexInformation = undefined
>
> instance DeterminesEdge StupidGraph Reason Lives where
>   type EdgeUniqueness StupidGraph Reason Lives = NotUnique
>   toEngineEdgeInformation = undefined
>
> -- Proof that our edge types are indeed edges in StupidGraph. Again, we leave
> -- the implementation undefined.
> instance Edge StupidGraph IsFather where
>   type EdgeSource StupidGraph IsFather = Being
>   type EdgeTarget StupidGraph IsFather = Being
>   type EdgeCardinalitySource StupidGraph IsFather = Single
>   type EdgeCardinalityTarget StupidGraph IsFather = Multi
>   -- ^ These cardinalities tell us that the target of an IsFather edge can
>   -- have more than one in-incident IsFather edge, but that the source has
>   -- at most one out-incident IsFather edge.
>   toEngineEdge = undefined
>   fromEngineEdge = undefined
>
> instance Edge StupidGraph IsMother where
>   type EdgeSource StupidGraph IsMother = Being
>   type EdgeTarget StupidGraph IsMother = Being
>   type EdgeCardinalitySource StupidGraph IsMother = Single
>   type EdgeCardinalityTarget StupidGraph IsMother  = Multi
>   toEngineEdge = undefined
>   fromEngineEdge = undefined
>
> instance Edge StupidGraph IsBrother where
>   type EdgeSource StupidGraph IsBrother = Being
>   type EdgeTarget StupidGraph IsBrother = Being
>   type EdgeCardinalitySource StupidGraph IsBrother = Multi
>   type EdgeCardinalityTarget StupidGraph IsBrother = Multi
>   -- ^ Unlike IsMother or IsFather, one can have many brothers, and many
>   -- people can have the same being as a brother. This is a many-to-many
>   -- edge.
>   toEngineEdge = undefined
>   fromEngineEdge = undefined
>
> instance Edge StupidGraph IsPet where
>   type EdgeSource StupidGraph IsPet = Being
>   type EdgeTarget StupidGraph IsPet = Monster
>   type EdgeCardinalitySource StupidGraph IsPet  = Multi
>   type EdgeCardinalityTarget StupidGraph IsPet = Multi
>   -- ^ We declare that a pet can have multiple owners.
>   toEngineEdge = undefined
>   fromEngineEdge = undefined
>
> instance Edge StupidGraph Battled where
>   type EdgeSource StupidGraph Battled = Being
>   type EdgeTarget StupidGraph Battled = Monster
>   type EdgeCardinalitySource StupidGraph Battled = Multi
>   type EdgeCardinalityTarget StupidGraph Battled = Multi
>   -- ^ A being can battle many monsters, and a monster can be battled by many
>   -- beings.
>   toEngineEdge = undefined
>   fromEngineEdge = undefined
>
> instance Edge StupidGraph Lives where
>   type EdgeSource StupidGraph Lives = Being
>   type EdgeTarget StupidGraph Lives = Location
>   type EdgeCardinalitySource StupidGraph Lives = Single
>   type EdgeCardinalityTarget StupidGraph Lives = Multi
>   -- ^ A being lives only in one place (lives, not lived) but many beings
>   -- can live in one place.
>   toEngineEdge = undefined
>   fromEngineEdge = undefined
>
> -- The type IsFather determines IsFather edges going into or out of a Being.
> instance DeterminesLocalEdge StupidGraph Being IsFather IsFather where
>   type EdgeDirection StupidGraph Being IsFather IsFather = Both
>   toEngineEdgeInformationLocal = undefined
>
> -- The type IsMother determines IsMother edges going into or out of a Being
> instance DeterminesLocalEdge StupidGraph Being IsMother IsMother where
>   type EdgeDirection StupidGraph Being IsMother IsMother = Both
>   toEngineEdgeInformationLocal = undefined
>
> instance DeterminesLocalEdge StupidGraph Being IsBrother IsBrother where
>   type EdgeDirection StupidGraph Being IsBrother IsBrother = Both
>   toEngineEdgeInformationLocal = undefined
>
> instance DeterminesLocalEdge StupidGraph Being IsPet IsPet where
>   type EdgeDirection StupidGraph Being IsPet IsPet = Out
>   toEngineEdgeInformationLocal = undefined
>
> instance DeterminesLocalEdge StupidGraph Being Battled HasBattled where
>   type EdgeDirection StupidGraph Being Battled HasBattled = Out
>   toEngineEdgeInformationLocal = undefined

> instance DeterminesLocalEdge StupidGraph Location Lives LivesIn where
>   type EdgeDirection StupidGraph Location Lives LivesIn = In
>   toEngineEdgeInformationLocal = undefined
>
> instance DeterminesLocalEdge StupidGraph Being Lives LivesIn where
>   type EdgeDirection StupidGraph Being Lives LivesIn = Out
>   toEngineEdgeInformationLocal = undefined

The titan example in gremlin is as follows:

  hercules = saturn.as('x').in('father').loop('x'){it.loops < 3}.next()

but check out how this can be expressed in GraphPatterns:

> -- We can grab everybody named Hercules
> herculesByName :: GraphPatterns StupidGraph Being
> herculesByName = vertex (Proxy :: Proxy Being) (Name "Hercules")
>
> -- Or we can grab everybody named Saturn and find their grandchildren.
> -- Hercules should be in there :)
> herculesByGrandfather :: GraphPatterns StupidGraph Being
> herculesByGrandfather = do
>   saturn <- vertex (Proxy :: Proxy Being) (Name "Saturn")
>   adjacentIn (Proxy :: Proxy IsFather) IsFather saturn
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

> parentsOf :: Being -> GraphPatterns StupidGraph (Being, Being)
> parentsOf being = do
>   mother <- adjacentOut (Proxy :: Proxy IsMother) IsMother being
>   father <- adjacentOut (Proxy :: Proxy IsFather) IsFather being
>   return (mother, father)
>
> lift2 :: (a -> b) -> (a, a) -> (b, b)
> lift2 f (x, y) = (f x, f y)
>
> apply2 :: (a -> b -> c) -> (a, b) -> c
> apply2 f (x, y) = f x y

  hercules.out('father','mother').name

> namesOfParents :: Being -> GraphPatterns StupidGraph (Name, Name)
> namesOfParents = fmap (lift2 beingName) . parentsOf

Note that what this example calls type, we call status. In our world, a type
is not a value on the vertex.

  hercules.out('father','mother').type

> statusOfParents :: Being -> GraphPatterns StupidGraph (Status, Status)
> statusOfParents = fmap (lift2 getStatus) . parentsOf

Ok, we can compute this one purely, but for the sake of demonstration, we
express it in GraphPatterns as well.

  hercules.type

> statusOfPerson :: Being -> GraphPatterns StupidGraph Status
> statusOfPerson = fmap (apply2 breed) . statusOfParents

Now we move away from lineage and on to violence.

  hercules.out('battled')

> monstersBattled :: Being -> GraphPatterns StupidGraph Monster
> monstersBattled = adjacentOut (Proxy :: Proxy Battled) HasBattled

This one doesn't really translate to our system; why ask for the map when
we know statically what data a Monster determines?

  hercules.out('battled').map

> namesOfMonstersBattled :: Being -> GraphPatterns StupidGraph Name
> namesOfMonstersBattled = fmap monsterName . monstersBattled

Aha, here's a good one

  hercules.outE('battled').has('time',T.gt,1).inV.name

> battledRecently :: Time -> Being -> GraphPatterns StupidGraph Monster
> battledRecently time being = do
>   battled :: Battled <- outgoing HasBattled being
>   -- Observe the use of MonadPlus to cast out the undesirables.
>   guard $ timeBattled battled > time
>   target battled
>
> nameOfBattledRecently :: Time -> Being -> GraphPatterns StupidGraph Name
> nameOfBattledRecently time = fmap monsterName . battledRecently time

Let's move on to the next section. This one is just like finding Hercules.

  pluto = g.V('name','pluto').next()

> pluto :: GraphPatterns StupidGraph Being
> pluto = vertex (Proxy :: Proxy Being) (Name "Pluto")

  pluto.out('lives').in('lives').name

> roomates :: Being -> GraphPatterns StupidGraph Being
> roomates being = do
>   edge :: Lives <- outgoing LivesIn being
>   location <- target edge
>   adjacentIn (Proxy :: Proxy Lives) LivesIn location

  pluto.out('lives').in('lives').except([pluto]).name
  pluto.as('x').out('lives').in('lives').except('x').name

> roomatesProper :: Being -> GraphPatterns StupidGraph Being
> roomatesProper being = do
>   roomate <- roomates being
>   guard $ (beingName roomate) /= (beingName being)
>   -- ^ No Eq on Beings, so we settle for Eq on names... which is not a good
>   --   Eq instance, but oh well.
>   return roomate

  pluto.out('brother').out('lives').name

> brothersPlace :: Being -> GraphPatterns StupidGraph Location
> brothersPlace being = do
>   brother <- adjacentOut (Proxy :: Proxy IsBrother) IsBrother being
>   adjacentOut (Proxy :: Proxy Lives) LivesIn brother
>
> nameOfBrothersPlace :: Being -> GraphPatterns StupidGraph Name
> nameOfBrothersPlace = fmap locationName . brothersPlace

  pluto.out('brother').as('god').out('lives').as('place').select

> brotherAndAbode :: Being -> GraphPatterns StupidGraph (Being, Location)
> brotherAndAbode being = do
>   god <- adjacentOut (Proxy :: Proxy IsBrother) IsBrother being
>   place <- adjacentOut (Proxy :: Proxy Lives) LivesIn god
>   return (god, place)

pluto.out('brother').as('god').out('lives').as('place').select{it.name}

> namesOfBrotherAndAbode :: Being -> GraphPatterns StupidGraph (Name, Name)
> namesOfBrotherAndAbode being = do
>   (brother, location) <- brotherAndAbode being
>   return (beingName brother, locationName location)

  pluto.outE('lives').reason

> reasonForLivingThere :: Being -> GraphPatterns StupidGraph Reason
> reasonForLivingThere being = do
>   edge :: Lives <- outgoing LivesIn being
>   return $ why edge

  g.E.has('reason',CONTAINS,'loves')

> lovesLivingThere :: GraphPatterns StupidGraph Lives
> lovesLivingThere = edge (Proxy :: Proxy Lives) (Reason "Loves")

  g.E.has('reason',CONTAINS,'loves').collect{
      [it.outV.name.next(),it.reason,it.inV.name.next()] 
  }

> lovesLivingThere' :: GraphPatterns StupidGraph (Name, Reason, Name)
> lovesLivingThere' = do
>   lives <- lovesLivingThere
>   being <- source lives
>   location <- target lives
>   return (beingName being, why lives, locationName location)
