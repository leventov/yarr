
module Data.Yarr.WorkTypes where

-- | Abstracts interval works: 'Fill's, 'Walk's.
--
-- To be passed to functions from "Data.Yarr.Utils.Fork" module
-- or called directly.
type Work sh a =
       sh   -- ^ Lower bound
    -> sh   -- ^ Upper bound
    -> IO a -- ^ Result

-- | Curried version of 'StatefulWalk'. Identical to 'Work', indeed.
type Walk sh a =
       sh   -- ^ Lower bound (start for left walks, end for right ones)
    -> sh   -- ^ Upper bound (end or start)
    -> IO a -- ^ Result

-- | Alias to frequently used get-write-from-to arguments combo.
--
-- To be passed as 1st parameter of all 'Data.Yarr.Eval.Load'ing functions
-- from "Data.Yarr.Eval" module.
type Fill sh a =
       (sh -> IO a)       -- ^ Indexing function
    -> (sh -> a -> IO ()) -- ^ Writing function
    -> Work sh ()         -- ^ Curried result function -- worker


-- | Generalizes both partially applied left and right folds,
-- as well as walks with mutable state.
--
-- To be passed to walk runners from "Data.Yarr.Walk" module.
type StatefulWalk sh a s = 
       IO s         -- ^ Initial state
    -> (sh -> IO a) -- ^ Indexing function
    -> Walk sh s    -- ^ Curried result function -- walker,
                    -- emits final state

-- | Generalizes left folds.
--
-- To be passed to fold combinators from "Data.Yarr.Walk" module.
type Foldl sh a b =
       (b -> sh -> a -> IO b) -- ^ Generalized left reduce
    -> StatefulWalk sh a b    -- ^ Curried result stateful walk

-- | Generalizes right folds.
--
-- To be passed to fold combinators from "Data.Yarr.Walk" module.
type Foldr sh a b =
       (sh -> a -> b -> IO b) -- ^ Generalized right reduce
    -> StatefulWalk sh a b    -- ^ Curried result stateful walk