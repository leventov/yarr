
module Data.Yarr.WorkTypes where

-- | Generalizes interval works: 'Fill's, 'StatefulWork's.
--
-- To be passed to functions from "Data.Yarr.Utils.Fork" module
-- and called directly.
type Work sh a =
       sh   -- ^ Start (lower index)
    -> sh   -- ^ End (higher index)
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
-- as well as works on mutable state.
--
-- To be passed to fold runners from "Data.Yarr.Work" module.
type StatefulWork sh a s = 
       IO s         -- ^ Initial state
    -> (sh -> IO a) -- ^ Indexing function
    -> Work sh s    -- ^ Curried result function -- worker,
                    -- emits final state

-- | Generalizes left to right folds.
--
-- To be passed to fold combinators from "Data.Yarr.Work" module.
type Foldl sh a b =
       (b -> sh -> a -> IO b) -- ^ Generalized left reduce
    -> StatefulWork sh a b    -- ^ Curried result stateful work

-- | Generalizes right to left folds.
--
-- To be passed to fold combinators from "Data.Yarr.Work" module.
type Foldr sh a b =
       (sh -> a -> b -> IO b) -- ^ Generalized right reduce
    -> StatefulWork sh a b    -- ^ Curried result stateful work