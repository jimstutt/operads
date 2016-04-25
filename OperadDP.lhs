
> {-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeFamilies #-}

> module Main where

> import Data.Monoid
> import Control.Monad.Writer
> import Control.Monad.State

> main :: IO ()
> main = do
>   (print .  show)  ex1
>   (print . show) ex2
>   (print . show) ex3
>   (print . show) ex4
>   (print . show) ex5
>   (print . show) ex6
>   (print . show) ex7
>   return()

Let's define a simple tree type:


> data Tree a = Leaf a | Tree [Tree a] deriving (Eq,Show)


Sometimes we want to apply a function to every element of the tree. That's provided by the fmap member of the Functor type class.


> instance Functor Tree where
>     fmap f (Leaf x) = Leaf (f x)
>     fmap f (Tree ts) = Tree $ map (fmap f) ts

> class FunctorM c where
>     fmapM :: Monad m => (a -> m b) -> c a -> m (c b)


(I could have used Data.Traversable but that entails Data.Foldable and that's too much work.)

AMP needs an Applicative instance. But there are several options for Tree semantics.


> instance Applicative Tree where
>   pure = return
>   fa <*> ma = do
>     f <- fa
>     a <- ma
>     return (f a)

And now we can implement a monadic version of Tree's Functor instance:


> instance FunctorM Tree where
>     fmapM f (Leaf x) = do
>         y <- f x
>         return (Leaf y)
>     fmapM f (Tree ts) = do
>         ts' <- mapM (fmapM f) ts
>         return (Tree ts')


We can use fmapM to extract the list of elements of a container type:


> serialise a = runWriter $ fmapM putElement a
>               where putElement x = tell [x]

> type Shape t = t ()


So we have:


> serialise :: FunctorM t => t a -> (Shape t,[a])


Keeping the shape around allows is to invert serialise to give:


> deserialise :: FunctorM t => Shape t -> [a] -> (t a, [a])
> deserialise t = runState (fmapM getElement t) where
>   getElement () = do
>       x:xs <- get
>       put xs
>       return x

> size :: FunctorM t => t a -> Int
> size a = getSum $ execWriter $ fmapM incCounter a
>          where incCounter _ = tell (Sum 1)


Let's try an example. Here's an empty tree:


> tree1 = Tree [Tree [Leaf (),Leaf ()],Leaf ()]


We can pack a bunch of integers into it:


> ex1 = fst $ deserialise tree1 [1,2,3]


And get them back out again:


> ex2 = serialise ex1



> instance Monad Tree where
>     return x = Leaf x
>     t >>= f = join (fmap f t) where
>         join (Leaf t) = t
>         join (Tree ts) = Tree (fmap join ts)
>


> class Operad a where
>     degree :: a -> Int
>     identity :: a
>     o :: a -> [a] -> a


o is the composition operation. If f has degree n then we can apply it to a list of n more objects. So we only expect to evaluate f `o` fs successfully if degree f == length fs.

There are many identities we'd expect to hold. For example f `o` [identities,...,identity] == f, because adding plain sections of pipe has no effect. We also expect some associativity conditions coming from the fact that it doesn't matter what order we build a pipe assembly in, it'll still function the same way.

We can follow this pipe metaphor quite closely to define what I think of as the prototype Operad. A Fn a is a function that takes n inputs of type a and returns one of type a. As we can't easily introspect and find out how many arguments such a function expects, we also store the degree of the function with the function:


> data Fn a = F { deg::Int, fn::[a] -> a }

> instance Show a => Show (Fn a) where
>     show (F n _) = "<degree " ++ show n ++ " function>"

unconcat is a kind of inverse to concat. You give a list of integers and it chops up a list into pieces with lengths corresponding to your integers. We use this to unpack the arguments to f `o` gs into pieces suitable for the elements of gs to consume.


> unconcat [] [] = []
> unconcat (n:ns) xs = take n xs : unconcat ns (drop n xs)

> instance Operad (Fn a) where
>     degree = deg
>     f `o` gs = let n = sum (map degree gs)
>                in F n (fn f . zipWith fn gs . unconcat (map degree gs))
>     identity = F 1 head


Now compute an example, f(f1,f2,f3) applied to [1,2,3]. It should give 1+1+2*3=8.


> ex3 = fn (f `o`  [f1,f2]) [1,2,3] where
>   f = F 2 (\[x,y] -> x+y)
>   f1 = F 1 (\[x] -> x+1)
>   f2 = F 2 (\[x,y] -> x*y)


(That's a lot like lambda calculus without names. Operads are a bit like n-ary combinators.)

> data V m = V { unV :: [m] } deriving (Eq,Show)


This becomes an operad by allowing the monoid value in a 'parent' scheme multiply the values in a 'child'.


> instance Monoid m => Operad (V m) where
>     degree (V ps) = length ps
>     (V as) `o` bs = V $ op as (map unV bs) where
>         op [] [] = []
>         op (a:as) (b:bs) = map (mappend a) b ++ op as bs
>     identity = V [mempty]


For example, if d1 cuts the real line in half, and d2 cuts it into thirds, then d1(d1,d2) will cut it into five pieces of lengths 1/4,1/4,1/6,1/6,1/6:


> ex4 = d1 `o` [d1,d2] where
>   d1 = V [Product (1/2),Product (1/2)]
>   d2 = V [Product (1/3),Product (1/3),Product (1/3)]

If the elements in a V are non-negative and sum to 1 we can think of them as probability distributions. The composition A(A1,...,An) is the distribution of all possible outcomes you can get by selecting a value i in the range {1..n} using distribution A and then selecting a second
value conditionally from distribution Ai. We connect with the recent n-category post on entropy.

In fact we can compute the entropy of a distrbution as follows:


> h (V ps) = - (sum $ map (\(Product x) -> xlogx x) ps) where
>   xlogx 0 = 0
>   xlogx x = x*log x/log 2


We can now look at the 'aside' in that post. From an element of V we can produce a function that computes a corresponding linear combination (at least for Num types):


> linear (V ps) xs = sum $ zipWith (*) (map (\(Product x) -> x) ps) xs


We can now compute the entropy of a distribution in two different ways:


> (ex5,ex6) = (h (d1 `o` [d1,d2]),h d1 + linear d1 (map h [d1,d2])) where
>   d1 = V [Product 0.5,Product 0.5]
>   d2 = V [Product 0.25,Product 0.75]


Now according to this paper on operads we can build a monad from an operad. Here's the construction:


> data MonadWrapper op a = M { shape::op, value :: [a] } deriving (Eq,Show)


(The field names aren't from the paper but they do give away what's actually going on...)

The idea is that an element of this construction consists of an element of the operad of degree n, and an n element list. It's a functor in an obvious way:


> instance Functor (MonadWrapper o) where
>     fmap f (M o xs) = M o (map f xs)


It's also a FunctorM:


> instance FunctorM (MonadWrapper o) where
>   fmapM f (M s c) = do
>       c' <- mapM f c
>       return $ M s c'


Need Applicative:

>{-
> instance Operad o => Applicative (MonadWrapper o) where
>   pure = return
>   mf <*> ma = do
>     f <- mf
>     a <- ma
>     return (f a)

>--We can make the construction a monad as follows:


> instance Operad o => Monad (MonadWrapper o) where
>     return x = M identity [x]
>     p >>= f = join (fmap f p) where
>         join (M p xs) = M (p `o` map shape xs) (concatMap value xs)
>-}

Now for something to be a monad there are various laws that needs to be satisfied. These follow from the rules (which I haven't explicitly stated) for an operad. When I first looked at that paper I was confused - it seemed that the operad part and the list part didn't interact with each other. And then I suddenly realised what was happening. But hang on for a moment...

Tree shapes make nice operads. The composition rule just grafts child trees into the leaves of the parent:


> instance Operad (Tree ()) where
>     degree t = length (snd (serialise t))
>     identity = Leaf ()
>     t `o` ts = let (r,[]) = deserialise t ts in r >>= id


We can write that more generically so it works with more than trees:

> data OperadWrapper m = O { unO::Shape m }


> instance (FunctorM m,Monad m) => Operad (OperadWrapper m) where
>     degree (O t) = size t
>     identity = O (return ())
>     (O t) `o` ts = let (r,[]) = deserialise t (map unO ts) in O (r >>= id)


So let's use the construction above to make a monad. But what actually is this monad? Each element is a pair with (1) a tree shape of degree n and (2) an n-element list. In other words, it's just a serialised tree. We can define these isomorphisms to make that clearer:


> iso1 :: FunctorM t => t x -> MonadWrapper (t ()) x
> iso1 t = uncurry M (serialise t)

> iso2 :: FunctorM t => MonadWrapper (t ()) x -> t x
> iso2 (M shape contents) = let (tree,[]) = deserialise shape contents in tree


So, for example:


> ex7 = iso2 (iso1 tree) where
>   tree = Tree [Tree [Leaf "Birch",Leaf "Oak"],Leaf "Cypress",Leaf "Binary"]


That construction won't work for all monads, just those monads that come from operads. I'll leave you to characterise those.

And now we have it: a way to think about operads from a computational perspective. They're the shapes of certain monadic serialisable containers. Operadic composition is the just the same grafting operation used in the join operation, using values () as the graft points.

I have a few moments spare so let's actually do something with an operad. First we need the notion of a free operad. This is basically just a set of 'pipe' parts that we can stick together with no equations holding apart from those inherent in the definition of an operad. This is different from the V operad where many different ways of apply the o operator can result in the same result. We can use any set of parts, as long as we can associate an integer with each part:


> class Graded a where
>   grade :: a -> Int


The free operad structure is just a tree:


> data FreeOperad a = I | B a [FreeOperad a] deriving Show


I will be the identity, but it will also serve as a 'terminator' like the way there's always a [] at the end of a list.

An easy way to make a single part an element of an operad:


> b n = let d = grade n in B n (replicate d I)


Here's the instance:


> instance Graded a => Operad (FreeOperad a) where
>   degree I = 1
>   degree (B _ xs) = sum (map degree xs)
>   identity = I
>   I `o` [x] = x
>   B a bs `o` xs = let arities = map degree bs
>                   in B a $ zipWith o bs (unconcat arities xs)


Now I'm going to use this to make an operad and then a monad:


> instance Graded [a] where
>   grade = length

> type DecisionTree = MonadWrapper (FreeOperad [Float])


What we get is a lot like the probability monad except it doesn't give the final probabilities. Instead, it gives the actual tree of possibilities. (I must also point out this hpaste by wli.)

>-- test :: MonadWrapper (FreeOperad [Product Double]) Int
> test = do
>   a <- M (b [Product 0.5,Product 0.5]) [1,2::Int]
>   b <- M (b [Product (1/3.0),Product (1/3.0),Product (1/3.0)]) [1,2,3::Int]
>   return $ a+b

Now we can 'flattten' this tree so that the leaves have the final probabilities:


> flatten :: Monoid m => FreeOperad [m] -> V m
> flatten I = V [mempty]
> flatten (B ms fs) = V $ concat $ zipWith (map . mappend) ms (map (unV . flatten) fs)


This is a morphism of operads. (You can probably guess the definition of such a thing.) It induces a morphism of monads:


> liftOp :: (Operad a,Operad b) => (a -> b) -> MonadWrapper a x -> MonadWrapper b x
> liftOp f (M shape values) = M (f shape) values


liftOp flatten test will give you the final probabilities.

There may just be a possible application of this stuff. The point of separating shape from data is performance. You can store all of your data in flat arrays and do most of your work there. It means you can write fast tight loops and only rebuild the original datatype if needed at the end. If you're lucky you can precompute the shape of the result, allowing you to preallocate a suitable chunk of memory for your final answer to go into. What the operad does is allow you to extend this idea to monadic computations, for suitable monads. If the 'shape' of the computation is independent of the details of the computation, you can use an operad to compute that shape, and then compute the contents of the corresponding array separately. If you look at the instance for MonadWrapper you'll see that the part of the computation that deals with the data is simply a concatMap.

BTW In some papers the definition restricts the degree to ≥1. But that's less convenient for computer science applications. If it really bothers you then you can limit yourself to thinking about containers that contain at least one element.
Posted by Dan Piponi at Saturday, October 25, 2008  

