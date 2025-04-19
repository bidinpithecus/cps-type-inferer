module Lambda.Typing where

import Control.Monad.State (MonadState (get, put), State)
import Data.List(nub, union)
import Utils.Typing (Id, greekVar)

-- ADT representing Lambda expressions
-- x
-- \x. e
-- e e
data Expr
  = Var Id
  | Lam Id Expr
  | App Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Var var) = var
  show (Lam var expr) = "λ" ++ var ++ ". " ++ show expr
  show (App expr1 expr2) =
    let e1 = case expr1 of
          Lam _ _ -> "(" ++ show expr1 ++ ")"
          _ -> show expr1
        e2 = case expr2 of
          App _ _ -> "(" ++ show expr2 ++ ")"
          Lam _ _ -> "(" ++ show expr2 ++ ")"
          _ -> show expr2
     in e1 ++ " " ++ e2

-- Fresh variables
type FreshM a = State (Int, Int) a

freshVar :: FreshM Id
freshVar = do
  (vCount, kCount) <- get
  put (vCount + 1, kCount)
  return $ "v" ++ show vCount

freshCont :: FreshM Id
freshCont = do
  (vCount, kCount) <- get
  put (vCount, kCount + 1)
  return $ "k" ++ show kCount

data SimpleType =
  TVar Id
  | TArr SimpleType SimpleType
  deriving (Eq)

type Index  = Int
newtype TI a = TI (Index -> (a, Index))
type Subst  = [(Id, SimpleType)]
data Assump = Id :>: SimpleType deriving (Eq, Show)

instance Show SimpleType where
   show (TVar i) = i
   show (TArr (TVar i) t) = i ++ " -> "++ show t
   show (TArr t t') = "(" ++ show t ++ ")" ++ " -> " ++ show t'

instance Functor TI where
   fmap f (TI m) = TI (\e -> let (a, e') = m e in (f a, e'))

instance Applicative TI where
   pure a = TI (\e -> (a, e))
   TI fs <*> TI vs = TI (\e -> let (f, e') = fs e; (a, e'') = vs e' in (f a, e''))

instance Monad TI where
   TI m >>= f  = TI (\e -> let (a, e') = m e; TI fa = f a in fa e')

freshTVar :: TI SimpleType
freshTVar = TI (\e -> let v = greekVar e in (TVar v, e + 1))

runTI :: TI a -> a
runTI (TI m) = let (t, _) = m 0 in t

(-->) :: SimpleType -> SimpleType -> SimpleType
t --> t' = TArr t t'

infixr 4 @@
(@@)       :: Subst -> Subst -> Subst
s1 @@ s2    = [ (u, apply s1 t) | (u,t) <- s2 ] ++ s1

dom :: [Assump] -> [Id]
dom = map (\(i:>:_)->i)

(/+/) :: [Assump] -> [Assump] -> [Assump]
as /+/ as' = as' ++ filter compl as
   where
     is = dom as'
     compl (i:>:_) = i `notElem` is

class Subs t where
  apply :: Subst -> t -> t
  tv    :: t -> [Id]

instance Subs SimpleType where
  apply s (TVar u)  =
                    case lookup u s of
                       Just t  -> t
                       Nothing -> TVar u
  apply s (TArr l r) =  TArr (apply s l) (apply s r)

  tv (TVar u)  = [u]
  tv (TArr l r) = tv l `union` tv r

instance Subs a => Subs [a] where
  apply s     = map (apply s)
  tv          = nub . concatMap tv

instance Subs Assump where
  apply s (i:>:t) = i:>:apply s t
  tv (_:>:t) = tv t
