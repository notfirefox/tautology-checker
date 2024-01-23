-- prop data type
data Prop where
    Const :: Bool -> Prop
    Var :: Char -> Prop
    Not :: Prop -> Prop
    And :: Prop -> Prop -> Prop
    Or :: Prop -> Prop -> Prop
    Imply :: Prop -> Prop -> Prop

-- more types
type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

-- find function
find :: (Eq k) => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- evaluates a expression given substitution rules
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q

-- returns all vars for a given proposition
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- creates bools for n variables
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
  where
    bss = bools (n - 1)

-- remove duplicates from a list
rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

-- creates a list of all possible substitutions
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)

-- returns true if the proposition is a tautology
isTautology :: Prop -> Bool
isTautology p = and [eval s p | s <- substs p]

main = do
    -- A && !A
    let p1 = And (Var 'A') (Not (Var 'A'))
    putStrLn $ "isTautology p1: " ++ show (isTautology p1)

    -- (A && B) => A
    let p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
    putStrLn $ "isTautology p2: " ++ show (isTautology p2)

    -- A => (A && B)
    let p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
    putStrLn $ "isTautology p3: " ++ show (isTautology p3)

    -- (A && (A => B)) => B
    let p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
    putStrLn $ "isTautology p4: " ++ show (isTautology p4)
