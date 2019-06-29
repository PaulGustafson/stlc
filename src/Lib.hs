module Lib where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy

data Ty                           -- types
  = Atom 
  | Fun Ty Ty
  deriving (Show, Eq)

newtype IBool = IBool Bool        -- internal bool

data TExp v                       -- typed expressions
  = TVar v Ty
  | TLit IBool 
  | TLam v Ty (TExp v) Ty
  | TApp (TExp v) (TExp v) Ty

type Context v = v -> Maybe Ty

data PTy                          -- partially defined types
  = FullT Ty
  | Hole
  | PFun PTy PTy
  deriving (Show)

data PExp v                       -- partially typed expression
  = FullE (TExp v)
  | PVar v PTy
  | PLam v PTy (PExp v) PTy
  | PApp (PExp v) (PExp v) PTy

type PContext v = v -> PTy

type Error = String

type Failable = Either Error

type FComp v = StateT (PContext v) Failable   -- failable partial context

-- type Exp = ExpA String 

-- instance Show Exp where
--     show (Var s) = s
--     show (Lam s e) = "\\" + s + " -> " + (show e)
--     show (App e1 e2) = "(" + (show e1) +") (" + (show e2) + ")"



reifyT :: PTy -> Maybe Ty
reifyT (FullT t) = Just t
reifyT Hole = Nothing
reifyT (PFun pa pb) = liftM2 Fun (reifyT pa) (reifyT pb)

unreifyT :: Ty -> PTy
unreifyT Atom = FullT Atom
unreifyT (Fun a b) = PFun (unreifyT a) (unreifyT b)

reifyE :: PExp v -> Maybe (TExp v)
reifyE (FullE e) = Just e
reifyE (PVar x pt) = liftM (TVar x) (reifyT pt)
reifyE (PLam x pt1 e pt2) = liftM3 (TLam x) (reifyT pt1) (reifyE e) (reifyT pt2)
reifyE (PApp e1 e2 pt) = liftM3 TApp (reifyE e1) (reifyE e2) (reifyT pt)


insert :: Eq v => (v, Ty) -> Context v -> Context v
insert h c = \ x ->
  if x == fst h
  then Just (snd h)
  else c x

insertP :: Eq v => (v, PTy) -> PContext v -> PContext v
insertP h c = \ x ->
  if x == fst h
  then snd h
  else c x


typeof :: TExp v -> Ty
typeof (TVar x t) = t
typeof (TLit _) = Atom
typeof (TLam _ a _ b) = Fun a b
typeof (TApp _ _ t) = t


check :: Eq v => Context v -> TExp v -> Bool
check c (TVar x t) = case c x of
  Just t2 -> t2 == t
  Nothing -> True
check c (TLit _) = True
check c (TLam x a e b) = check (insert (x, a) c) e
check c (TApp e1 e2 t) = case e1 of
  TLam x a e3 b -> (a == typeof e2) && (b == t) && (check c e1) && (check c e2)
  _ -> False



-- ascribe :: Context v -> UExp v -> PExp v
-- ascribe c (UVar x) = PVar x (c x)
-- ascribe c (ULit x) = PLit x
-- ascribe c (ULam x e) = PLam x Nothing 

unify :: PTy -> PTy -> Failable PTy
unify (FullT t) x = case unify (unreifyT t) x of
  Left err -> throwError err
  Right t2 -> Right $ FullT t
unify Hole x = Right x
unify (PFun a b) x = case unifyFun a b x of
  Left err -> Left err
  Right (a2, b2) -> Right $ PFun a2 b2
  

unifyFun :: PTy -> PTy -> PTy -> Failable (PTy, PTy)
unifyFun a b t = case t of
  Hole -> Right (a, b)
  PFun a2 b2 -> liftM2 (,) (unify a a2) (unify b b2)
  FullT t1 -> case t1 of
    Atom -> throwError $ "Can't unify " ++ (show $ PFun a b) ++ " with atom"
    Fun a2 b2 ->  liftM2 (,) (unify a (FullT a2)) (unify b (FullT b2))


-- ascribe :: (Eq v, Show v) => PContext v -> PExp v -> (Failable PExp, PContext v)
-- ascribe c (PVar x t) = case (unify (c x) t) of
--   Left err -> (throwError err, insertP (x, Hole) c)
--   Right t2 -> (Right (PVar t2), insertP


-- infer :: (Eq v, Show v) => PContext v -> PExp v -> (Failable PTy, PContext v)
-- infer c (PVar x t) = case (unify (c x) t) of
--   Left err -> (throwError err, insertP (x, Hole) c)
--   Right t2 -> (Right t2, insertP (x, t2) c)
-- infer c (PLit b) = (Right PAtom, c)
-- infer c (PLam x t1 e t2) =
--   let c2 = insertP (x, t1) c 
--       te = join $ liftM2 unify (fst $ infer c2 e) (Right t2) in
--     (liftM2 PFun (Right t1) te, c)
-- infer c (PApp e1 e2 b) =
--   let ab = fst $ infer c e1
--       a = fst $ infer c e2
--       ut1p = unifyFun a b ab
--       ut2 = liftM fst ut1p in
    


    
--   in
      
  
-- infer c (

  

-- infer :: (Show v) => Context v -> UExp v -> Failable (PTy, Context v)
-- infer c (UVar x) = Right (c x, c)
-- infer c (ULit IBool) = Right (Just Atom, c)
-- infer c (ULam x e) = case (infer e) of
--   Left err -> Left err
--   Right (mt, c2) -> case mt of
--     Nothing -> Right (Nothing, c)
--     Just t  -> case t of
--       Atom 
-- infer c (UApp e1 e2) = case infer e1 of
--   Nothing -> Right $ Nothing
--   Just Atom -> Left  $$ "Can't apply atom " + (show e1) + " to " + (show e2)
--   Just (Fun a b) ->
    

-- -- Ascribes as many types as possible, given a context
-- ascribe :: (Eq v, Show v, Show b) => Context v b -> ExpA v b -> Failable (ExpA v b)
-- ascribe c e = case e of
--   Var x -> case (c x) of
--     Just a -> if check e a
--               then Right $ Asc e a
--               else Left $ "Type mismatch " ++ (show e) ++ "does not have type " ++ (show a)
--     Nothing -> e
--   Lit l -> Right (Asc e Atom)
--   Lam x e2 -> case (synth e) of
--     Just f -> case f of
--       Fun a b -> Right $ Lam x (ascribe (append (x, a) c) e2)
--       _       -> Left $ "Expected function"
--     Nothing -> Lam x (ascribe c e2)
--   App e1 e2 -> case (synth e) of
--     Just a -> Right $ Asc (App (ascribe c e1) (ascribe c e2)) a
--     Nothing -> Right $ App (ascribe c e1) (ascribe c e2)
--   Asc e2 t  -> if check e2 t
--                then case t of
--                       Atom -> e
--                       Fun a d -> Right $ Asc (ascribe (a
  
  
    
    

-- synth :: ExpA v b ->  Maybe (TyA b)
-- synth (Var x) = Nothing
-- synth (Lit x) = Just Atom
-- synth (Lam x e) = Nothing 
-- synth (App x y) = case (synth x) of
--   Just tx -> case tx of
--     Fun a c -> if (check y a)
--       then (Just c)
--       else Nothing
--     _ -> Nothing
--   _ -> Nothing
-- synth (Asc x y) = case (check x y) of
--   True -> Just y
--   False -> Nothing

-- TODO: replace == with unification
-- TODO: Use dependent types to replace bool with a base type enumeration




