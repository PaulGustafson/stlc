module Lib where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy

data Ty t                          -- type on a base type enumeration $t$
  = Atom t
  | Fun (Ty t) (Ty t)
  deriving (Show, Eq)

type Asc e t  = (e, Ty t)             -- ascription of expression to type

type Checker e t = e -> t -> Bool

  
data TExp v t e 
  -- typed expressions of variable type v, base type t, base expression type e
  = TLit e t 
  | TVar (Asc v t)
  | TLam (Asc v t)  (Asc (TExp v t e) t)
  | TApp (Asc (TExp v t e, TExp v t e) t)


type Context v t = v -> Maybe (Ty t)

data PAtom t                        -- atomic partial types
  = Full (Ty t)
  | Hole 

type PTy t = Ty (PAtom t)           -- partial types

type PExp v t e = TExp v (PAtom t) e  -- partially typed expressions

data IType = TBool 
newtype IBool = IBool Bool           -- internal bool

newtype Var = Var String

type PContext v t = v -> PTy t

type Error = String

type Failable = Either Error

type FComp v t = StateT (PContext v t) Failable   -- failable partial context

type Exp = TExp String IType IBool 

-- instance Show Exp where
--     show (Var s) = s
--     show (Lam s e) = "\\" + s + " -> " + (show e)
--     show (App e1 e2) = "(" + (show e1) +") (" + (show e2) + ")"



-- reifyT :: PTy t -> Maybe (Ty t)
-- reifyT (Atom Hole) = Nothing
-- reifyT (Atom (Full x)) = Just x
-- reifyT (Fun a b) = liftM2 Fun (reifyT a) (reifyT b)


-- unreifyT :: Ty t -> PTy t
-- unreifyT x = Atom (Full x)

-- reifyA :: Asc e (PAtom t) -> Maybe (Asc e t)
-- reifyA (exp, pty) =
--   case pty of
--     Atom pAtom -> case pAtom of
--       Hole -> Nothing
--       Full ty -> Just (exp, ty)
--     Fun a b -> 


-- reifyE :: PExp v t e -> Maybe (TExp v t e)
-- reifyE (TLit pExp pAtom) =
--   case pAtom of
--     Hole -> Nothing
--     Full ty -> case ty of
--       Atom atomicType -> Just $ TLit pExp atomicType
--       Fun a b -> case pExp of
--         TLam x e2 -> Just $ TLam 
  
  

-- reifyE :: PExp v -> Maybe (TExp v)
-- reifyE (FullE e) = Just e
-- reifyE (PLit b) = Just $ TLit b
-- reifyE (PVar x pt) = liftM (TVar x) (reifyT pt)
-- reifyE (PLam x pt1 e pt2) = liftM3 (TLam x) (reifyT pt1) (reifyE e) (reifyT pt2)
-- reifyE (PApp e1 e2 pt) = liftM3 TApp (reifyE e1) (reifyE e2) (reifyT pt)

-- -- no FullE output
-- unreifyE :: TExp v -> PExp v
-- unreifyE (TVar x t) = PVar x (FullT t)
-- unreifyE TLit b = PLit b
-- unreifyE TLam x a e b = PLam (PVar 


insert :: Eq v => (v, Ty t) -> Context v t -> Context v t
insert h c = \ x ->
  if x == fst h
  then Just (snd h)
  else c x

-- insertP :: Eq v => (v, PTy) -> PContext v -> PContext v
-- insertP h c = \ x ->
--   if x == fst h
--   then snd h
--   else c x


typeof :: TExp v t e -> Ty t
typeof (TLit exp ty) = Atom ty
typeof (TVar (x, ty)) = ty
typeof (TLam (x, t1) (exp, t2)) = Fun t1 t2
typeof (TApp (_, ty)) = ty


check :: (Eq t, Eq v) => Checker e t -> Context v t -> TExp v t e -> Bool
check ch con texp = case texp of
  TLit exp ty -> ch exp ty
  TVar (x, ty) -> case con x of
    Just t2 -> t2 == ty
    Nothing -> True
  TLam (x, t1) (exp, t2) -> check ch (insert (x, t1) con) exp
  TApp ((e1, e2), ty) -> case e1 of
    TLam (x, a) (e3, b) -> (a == typeof e2) && (b == ty) && (check ch con e1) && (check ch con e2)
    _ -> False



-- unify :: PTy -> PTy -> Failable PTy
-- unify (FullT t) x = case unify (unreifyT t) x of
--   Left err -> throwError err
--   Right t2 -> Right $ FullT t
-- unify Hole x = Right x
-- unify (PFun a b) x = case unifyFun a b x of
--   Left err -> Left err
--   Right (a2, b2) -> case (a2,b2) of
--     (FullT a3, FullT b3) -> Right $ FullT $ Fun a3 b3
--     _                    -> Right $ PFun a2 b2
  

-- unifyFun :: PTy -> PTy -> PTy -> Failable (PTy, PTy)
-- unifyFun a b t = case t of
--   Hole -> Right (a, b)
--   PFun a2 b2 -> liftM2 (,) (unify a a2) (unify b b2)
--   FullT t1 -> case t1 of
--     Atom -> throwError $ "Can't unify " ++ (show $ PFun a b) ++ " with atom"
--     Fun a2 b2 ->  liftM2 (,) (unify a (FullT a2)) (unify b (FullT b2))


-- ascribe :: (Eq v, Show v) => PContext v -> PExp v -> (Failable (PExp v), PContext v)
-- ascribe c fe@(FullE e) = ascribe c (unreifyE e)
-- ascribe c (PVar x t) = case (unify (c x) t) of
--   Left err -> (throwError err, insertP (x, Hole) c)
--   Right t2 -> (Right (PVar x t2), insertP (x, t2) c)
-- ascribe c (PLam x a e b) =
--   let c2 = insertP (x, a) c
  



  


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




