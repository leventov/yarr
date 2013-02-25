
module Data.Yarr.WorkTypes where

-- | Generalizes interval works: 'Fill's, 'Fold's.
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
       (sh -> IO a)       -- ^ Get
    -> (sh -> a -> IO ()) -- ^ Write
    -> Work sh ()         -- ^ Curried result function -- worker


-- | Generalizes both partially applied left and right folds.
--
-- To be passed to fold runners from "Data.Yarr.Fold" module.
type Fold sh a b = 
       IO b         -- ^ Zero
    -> (sh -> IO a) -- ^ Get
    -> Work sh b    -- ^ Curried result function -- worker

-- | Generalizes left to right folds.
--
-- To be passed to fold combinators from "Data.Yarr.Fold" module.
type Foldl sh a b =
       (b -> sh -> a -> IO b) -- ^ Generalized left reduce
    -> Fold sh a b            -- ^ Curried result fold

-- | Generalizes right to left folds.
--
-- To be passed to fold combinators from "Data.Yarr.Fold" module.
type Foldr sh a b =
       (sh -> a -> b -> IO b) -- ^ Generalized right reduce
    -> Fold sh a b            -- ^ Curried result fold