module Terminals where

import Data.Char (isAlpha, isAlphaNum)
import Data.Ratio (denominator, numerator)
import Data.Set ()

piRational, eRational :: Rational
piRational = toRational (pi :: Double)
eRational  = toRational (exp 1 :: Double)

-- By having nonzero, we can enable operations like division to be performed safely on
-- unkown constants.
data Terminal
  = Constant Constant
  | Variable Domain String
  deriving (Eq, Ord, Show)


-- Ord should not be used to find which element is greater.
data Constant
  = Rational Rational
  | E
  | Pi
  deriving (Eq, Show)

instance Ord Constant where
  compare (Rational a) (Rational b) = compare a b
  compare (Rational a) Pi           = compare a piRational
  compare Pi (Rational a)           = compare piRational a
  compare (Rational a) E            = compare a eRational
  compare E (Rational a)            = compare eRational a
  compare Pi E                      = GT
  compare E Pi                      = LT
  compare Pi Pi                     = EQ
  compare E E                       = EQ

data Inclusivity = Inclusive | Exclusive deriving (Eq, Ord, Show)

data Bound
  = Real Inclusivity Constant
  | NegInfinity
  | PosInfinity
  deriving (Eq, Ord, Show)

-- | BoundSeq[0] to BoundSeq[1] is included in the Domain,
-- BoundSeq[1] to BoundSeq[2] is not included in the Domain, etc.
-- Note that BoundSeq should never have an unsorted state.
-- Consider changing the container type to enforce that.
--
-- Reduction rules:
-- BoundSeq[i] == BoundSeq[i+1], at least one inclusive => both eliminated
--
-- As long as the sequence is never unsorted, that reduction rule should be
-- sufficient for BoundSeq to be valid.
newtype BoundSeq = BoundSeq [Bound] deriving (Eq, Ord, Show)

-- | FiniteDomain is just a finite set of values that a variable could have.
newtype FiniteDomain = Set Constant deriving (Eq, Ord, Show)

data Domain = Domain BoundSeq FiniteDomain deriving (Eq, Ord, Show)

-- | returns Nothing when the top bound is greater than or equal to the bottom bound.
addRange :: Bound -> Bound -> Domain -> Maybe Domain
addRange = error "not implemented yet"

-- | returns Nothing when the top bound is greater than or equal to the bottom bound.
subtractRange :: Bound -> Bound -> Domain -> Maybe Domain
subtractRange = error "not implemented yet"

addValue :: Constant -> Domain -> Domain
addValue = error "not implemented yet"

removeValue :: Constant -> Domain -> Domain
removeValue = error "not implemented yet"

-- | nonempty ensures that the domain is nonempty, and that the left bound
-- is less than or equal to than the right bound.
nonempty :: Bound -> Bound -> Bool
-- invalid infinite bounds
nonempty _ NegInfinity = False
nonempty PosInfinity _ = False
-- empty range
nonempty (Real Exclusive a) (Real Exclusive b) | a == b = False
-- left bound is greater than right bound
nonempty (Real _ a) (Real _ b) | a > b = False
-- otherwise
nonempty _ _ = True

-- | makes a variable. names are case sensitive.
withDomain :: Domain -> String -> Maybe Terminal
withDomain _ "" = Nothing
withDomain domain name@(c:cs)
  | isAlpha c && all isAlphaNum cs = Just $ Variable domain name
  | otherwise = Nothing

pretty :: Terminal -> String
pretty (Constant (Rational r))
  | denominator r == 1 = show $ numerator r
  | otherwise = mconcat [show (numerator r), "/", show (denominator r)]
pretty (Constant Pi) = "pi"
pretty (Constant E) = "e"
pretty (Variable _ name) = name
