module Pers.Types
    ( FirstL
    ) where
-- import           GHC.Exts (Constraint)

type family FirstL (x :: [(a,b)]) where
    FirstL '[] = '[]
    FirstL ('(a,b) ': xs) = a ': FirstL xs

-- type family ConstraintL (x :: [Constraint]) :: Constraint where
--     ConstraintL '[] = ()
--     ConstraintL (a ': as) = (a, ConstraintL as)

-- type family MapL (xs :: [a]) b c :: Constraint where
--     MapL '[] b c = ()
--     MapL (a ': as) b (c a b :: Constraint) = (c a b,
