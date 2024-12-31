-- Should be imported qualified; there are a number of Prelude conflicts.
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

module Expr where

import Control.Monad ((<=<))
import Data.Ratio (denominator)
import qualified Terminals
import Terminals (Domain, Terminal)

data Expr
  = Terminal Terminal
  | Sin Expr
  | Cos Expr
  | Arcsin Expr
  | Arccos Expr
  | Power Expr Expr
  | Add Expr Expr
  | Multiply Expr Expr
  deriving (Eq, Show)

pattern Variable :: Domain -> String -> Expr
pattern Variable domain name = Terminal (Terminals.Variable domain name)

pattern Constant :: Terminals.Constant -> Expr
pattern Constant const = Terminal (Terminals.Constant const)

pattern Rational :: Rational -> Expr
pattern Rational r = Terminal (Terminals.Constant (Terminals.Rational r))

pattern Pi :: Expr
pattern Pi = Terminal (Terminals.Constant Terminals.Pi)

pattern E :: Expr
pattern E = Terminal (Terminals.Constant Terminals.E)

-- | Completely simplified Exprs are sums of product terms with the order:
-- 1. Constants sorted by delegation
-- 2. Variables potentially in Powers sorted by Variable
-- 3. Trig functions potentially in Powers sorted by contents
-- 5. Arctrig functions potentially with Powers sorted by contents
-- 6. Unreduceable Powers sorted by contents
instance Ord Expr where
    compare _ _ = error "not implemented yet"

realVariable, negativeVariable, positiveVariable :: String -> Maybe Expr
-- | can have any real value, including zero
realVariable = error "not implemented yet"
-- | can have any negative value, including zero
negativeVariable = error "not implemented yet"
-- | can have any positive value, including zero
positiveVariable = error "not implemented yet"

nzVariable, nzNegativeVariable, nzPositiveVariable :: String -> Maybe Expr
-- | can have any real value, excluding zero.
nzVariable = error "not implemented yet"
-- | can have any negative value, excluding zero.
nzNegativeVariable = error "not implemented yet"
-- | can have any positive value, excluding zero.
nzPositiveVariable = error "not implemented yet"

data Arity = Nullary | Unary | Binary deriving (Eq, Show)

arity :: Expr -> Arity
arity (Terminal _)   = Nullary
arity (Sin _)        = Unary
arity (Cos _)        = Unary
arity (Arcsin _)     = Unary
arity (Arccos _)     = Unary
arity (Power _ _)    = Binary
arity (Add _ _)      = Binary
arity (Multiply _ _) = Binary

domain :: Expr -> Domain
domain = error "not implemented yet"

-- nonrecursive sort
sort :: Expr -> Expr
-- the const should be the last thing used to determine sort order.
sort (Add a b)
  | a < b = Add a b
  | otherwise = Add b a
sort (Multiply a@(Power aBase _) b)
  | aBase < b = Multiply a b
  | otherwise = Multiply b a
sort _ = error "not fully implemented yet"

data Fixity = Sum | Prod | Pow | Peak deriving (Eq, Ord, Show)

fixity :: Expr -> Fixity
fixity (Add _ _)      = Sum
fixity (Multiply _ _) = Prod
fixity (Power _ _)    = Pow
fixity (Rational r) | denominator r /= 1 = Prod
fixity _ = Peak

pretty :: Expr -> String
pretty (Sin e)        = mconcat ["sin(", pretty e, ")"]
pretty (Cos e)        = mconcat ["cos(", pretty e, ")"]
pretty (Arcsin e)     = mconcat ["arcsin(", pretty e, ")"]
pretty (Arccos e)     = mconcat ["arccos(", pretty e, ")"]
pretty (Terminal t)   = Terminals.pretty t
pretty (Power a b)    = mconcat [prettyWithFixity Pow a, "^", prettyWithFixity Pow b]
pretty (Multiply a b) = mconcat [prettyWithFixity Prod a, "*", prettyWithFixity Prod b]
pretty (Add a b)      = mconcat [prettyWithFixity Sum a, "+", prettyWithFixity Sum b]

prettyWithFixity :: Fixity -> Expr -> String
prettyWithFixity parentFixity e
  | parentFixity < fixity e = pretty e
  | otherwise               = mconcat ["(", pretty e, ")"]

fromString :: String -> Expr
fromString = error "not implemented yet"

-- | What latex is parsable should be fully documented.
fromLatex :: String -> Expr
fromLatex = error "not implemented yet"

-- | I don't think I'm going to need to run this more than once... if everything
-- below is optimally simplified, why would things above be unoptimally
-- simplified? Is there anything here that would introduce nonoptimal
-- simplification?
--
-- Returns Nothing when a variable has an empty domain or a domain doesn't match
-- an operation on it.
simplify :: Expr -> Maybe Expr
simplify = resolveDomains <=< \case
-- e@(Constant _)   -> Just e
-- simplify v@(Variable _ _) -> simplifyVariable v
-- simplify (Sin e)          -> simplifySin e
-- simplify (Cos e)          -> simplifyCos e
-- simplify (Arcsin e)       -> simplifyArcsin e
-- simplify (Arccos e)       -> simplifyArccos e
-- simplify (Add a b)        -> simplifySum a b
-- simplify (Multiply a b)   -> simplifyProduct a b
-- simplify (Power a b)      -> simplifyPower a b

-- Get all variable names, and then set the domain of each variable with name n to the
-- intersection of the domains of all variables with name n.
resolveDomains :: Expr -> Maybe Expr
resolveDomains = error "not implemented yet"

simplifyVariable :: Expr -> Maybe Expr
simplifyVariable = error "not implemented yet"

simplifyPower :: Expr -> Expr -> Maybe Expr
simplifyPower = error "not implemented yet"

simplifyProduct :: Expr -> Expr -> Maybe Expr
simplifyProduct = error "not implemented yet"

-- merge two sorted lists into a single sorted list.
-- when `resolvePred`, run `resolve`.
merge :: Ord a => (a -> a -> Bool) -> (a -> a -> a) -> [a] -> [a] -> [a]
merge resolvePred resolve = error "not implemented yet"

isMultiple :: Expr -> Expr -> Bool
isMultiple = error "not implemented yet"

-- | assumes that x and y are multiples and completely simplified.
addMultiples :: Expr -> Expr -> Expr
addMultiples (Multiply (Rational a) x) (Multiply (Rational a) y) =
  simplifyProduct (Rational $ a + b) x
addMultiples (Multiply (Rational a) x) y = 
  simplifyProduct (Rational $ a + 1) x
addMultiples x y | x == y = simplifyProduct (Rational 2) x
addMultiples _ _ = error "contract broken; inputs are not multiples"

-- simplifies forms:
-- 0 + x, x + 0 -> x
-- cx + x, x + cx -> (c + 1)x
-- ax + bx -> (a + b)x
-- new plan: make sorted lists, merge the sorted lists.
simplifySum :: Expr -> Expr -> Maybe Expr
simplifySum a b = 
  let listToPair [a,b] = (a,b)
      listToPair _ = error "impossible state"
      s = listToPair <$> mapM simplify [a, b]
  in
  fmap sort $ s >>= \case

      -- 0 + x = x + 0 = x
      (Rational 0, x) -> Just x
      (x, Rational 0) -> Just x

      -- x + x = 2x
      (x, y) | x == y -> simplifyProduct (Rational 2) x

      -- rx + x = x + rx = (r + 1)x
      (Multiply (Rational r) x, y) | x == y ->
          simplifyProduct (Rational $ r + 1) x
      (x, Multiply (Rational r) y) | x == y ->
          simplifyProduct (Rational $ r + 1) x

      -- ax + bx -> (a + b)x
      (Multiply (Rational a) x, Multiply (Rational b) y) | x == y ->
          simplifyProduct (Rational $ a + b) x

      -- merge mergeable terms
      (xs@(Add _ _), y) -> merge resolve (terms xs) [x2]
      ()
      -- (Add (Multiply (Rational r1) a undefined, undefined)) -> undefined
      -- (x + y) + (ax)


simplifyArccos :: Expr -> Maybe Expr
simplifyArccos = error "not implemented yet"

simplifySin :: Expr -> Maybe Expr
simplifySin = error "not implemented yet"

simplifyCos :: Expr -> Maybe Expr
simplifyCos = error "not implemented yet"

simplifyArcsin :: Expr -> Maybe Expr
simplifyArcsin = error "not implemented yet"

simplifyTan :: Expr -> Maybe Expr
simplifyTan = error "not implemented yet"

simplifyArctan :: Expr -> Maybe Expr
simplifyArctan = error "not implemented yet"
