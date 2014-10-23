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
> type Age = Integer
> -- ^ Titans can be very old; must use big Integer.
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
> -- Here's our edge types. We frame them in the language of relationships.
> data IsFather = IsFather
> data IsMother = IsMother
> data IsBrother = IsBrother
> data IsPet = IsPet
> data Battled = Battled
> data Lives = Lives
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
> -- The type IsFather determines IsFather edges going into a Being.
> instance DeterminesLocalEdge StupidGraph Being IsFather IsFather where
>   type EdgeDirection StupidGraph Being IsFather IsFather = In
>   toEngineEdgeInformationLocal = undefined
>
> -- The type IsMother determines IsMother edges going into a Being
> instance DeterminesLocalEdge StupidGraph Being IsMother IsMother where
>   type EdgeDirection StupidGraph Being IsMother IsMother = In
>   toEngineEdgeInformationLocal = undefined
>
> -- Brotherhood is bidirectional; we use two determiners. A bit of a wart but
> -- oh well, should work just fine.
> data IsBrotherIn = BrotherIn
> data IsBrotherOut = IsBrotherOut
> instance DeterminesLocalEdge StupidGraph Being IsBrother IsBrotherIn where
>   type EdgeDirection StupidGraph Being IsBrother IsBrotherIn = In
>   toEngineEdgeInformationLocal = undefined
> instance DeterminesLocalEdge StupidGraph Being IsBrother IsBrotherOut where
>   type EdgeDirection StupidGraph Being IsBrother IsBrotherOut = Out
>   toEngineEdgeInformationLocal = undefined
>
> instance DeterminesLocalEdge StupidGraph Being IsPet IsPet where
>   type EdgeDirection StupidGraph Being IsPet IsPet = Out
>   toEngineEdgeInformationLocal = undefined
>
> instance DeterminesLocalEdge StupidGraph Being Battled Battled where
>   type EdgeDirection StupidGraph Being Battled Battled = Out
>   toEngineEdgeInformationLocal = undefined

> instance DeterminesLocalEdge StupidGraph Location Lives Lives where
>   type EdgeDirection StupidGraph Location Lives Lives = In
>   toEngineEdgeInformationLocal = undefined

The titan example in gremlin is as follows:

  hercules = saturn.as('x').in('father').loop('x'){it.loops < 3}.next()

but check out how this can be expressed in GraphPatterns:

> -- We can grab everybody named Hercules
> herculesByName :: GraphPatterns StupidGraph [Being]
> herculesByName = vertex (Proxy :: Proxy Being) (Name "Hercules")
>
> -- Or we can grab everybody named Saturn and find their grandchildren.
> -- Hercules should be in there :)
> herculesByGrandfather :: GraphPatterns StupidGraph [Being]
> herculesByGrandfather = do
>   saturns <- vertex (Proxy :: Proxy Being) (Name "Saturn")
>   fmap concat $ traverse (adjacentIn (Proxy :: Proxy IsFather) IsFather) saturns

Excuse the fmap concat and traverse. This should be cleaner. Perhaps something
like

  herculesByGrandfather' :: GraphPatterns StupidGraph [Being]
  herculesByGrandfather' = do
    saturns <- vertex (Proxy :: Proxy Being) (Name "Saturn")
    fatherEdges <- incoming FatherOf saturns
    source fatherEdges

Or better yet

    saturns <- vertex (Proxy :: Proxy Being) (Name "Saturn")
    adjacentIn (Proxy :: Proxy IsFather) FatherOf saturns

with the understanding that adjacentIn can be applied to more than one
thing... Will have to change the def'n of the GraphPatterns monad.

We need to prove that Hercules is a demigod. That's to say, we must find that
precisely one of his parents is a god and the other is human. Here's the
gremlin session:

  gremlin> hercules.out('father','mother').type
  ==>god
  ==>human

but how can we express this in GraphPatterns?

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
>
> statusOfParents vertex = do
>   mothers <- adjacentOut (Proxy :: Proxy IsMother) IsMother vertex
>   fathers <- adjacentOut (Proxy :: Proxy IsFather) IsFather vertex
>   return $ (getStatus $ head mothers, getStatus $ head fathers)
>   -- ^ Note the wart on the design: we still treat everything as a list, when
>   --   we ought to hide this; we know MotherOf determines at most one vertex
>   --   so there's no need to get back a list; Maybe is more appropriate.
>   --   Note as well how the use of head completely subverts our goal of
>   --   type safety! Oh well, we'll fix this up in future developments.
>
> statusOfPerson vertex = do
>   (motherStatus, fatherStatus) <- statusOfParents vertex
>   return $ motherStatus `breed` fatherStatus
