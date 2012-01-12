{-
    Zachary Weaver (c) 2011
    26/12/2011
    Util.hs
-}

module Util
    ( (??)
    , (|>)
    , sq
    , loop
    ) where

(??) :: a -> a -> Bool -> a
(a ?? b) tf = if tf then a else b
infix 5 ??

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

sq :: Num a => a -> a
sq x = x * x

loop :: (Monad m) => m Bool -> m ()
loop act = act >>= loop act ?? return ()
