Consider f.

> -- f :: (a -> a) -> ()
> f :: forall a. (a -> a) -> ()
> f _ = ()

So for any a, f can be fed a function which has the type (a -> a):

< () == f (\n -> n+2)
< () == f (++"hi")
< () == f id

Consider g.

> g :: (forall a. a -> a) -> ()
> g _ = ()

So g can be fed a function which has the type (a -> a) for any a.
Only the identity has type (forall a. a -> a):

< () == g id
< () == g (+2) -- ERROR
