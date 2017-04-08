-- | 
-- Module      :  Line
-- Description :  Functions for testing intersections between line segments
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- See http://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/
-- I didn't test all this, I just trusted the article and converted it to haskell
-- 

module Line
  ( intersects
  , intersectionPoint
  , intersectionPoints
  , distance
  , normal
  , SDL.V2(..)
  , Seg(..)
  )
where

import Lens.Micro ((^.))
import SDL (crossZ, V2(..), _x, _y, normalize, Epsilon)


data Seg a = Seg (SDL.V2 a) (SDL.V2 a) deriving Show

data Orientation = Colinear | Clockwise | Counterclockwise deriving (Eq, Show)


-- Checks if, of three colinear points, point q is on
-- line segment pr 
onSegment :: Ord a => Seg a -> SDL.V2 a -> Bool
onSegment (Seg p r) q = (q^._x) <= max (p^._x) (r^._x)
                     && (q^._x) >= min (p^._x) (r^._x)
                     && (q^._y) <= max (p^._y) (r^._y)
                     && (q^._x) >= min (p^._y) (r^._y)


-- | Find orientation of an ordered triplet (p, q, r)
orientation :: (Num a, Eq a, Ord a) => SDL.V2 a -> SDL.V2 a -> SDL.V2 a -> Orientation
orientation p q r
  | val > 0   = Clockwise
  | val < 0   = Counterclockwise
  | val == 0  = Colinear
  where val = (q^._y - p^._y) * (r^._x - q^._x)
            - (q^._x - p^._x) * (r^._y - q^._y)


-- | Finds whether two line segments intersect
intersects :: (Num a, Eq a, Ord a) => Seg a -> Seg a -> Bool
intersects (Seg p q) (Seg r s) =
  let o1 = orientation p q r
      o2 = orientation p q s
      o3 = orientation r s p
      o4 = orientation r s q
  in ((p-q) /= 0 || onSegment (Seg r s) p) &&
     ((r-s) /= 0 || onSegment (Seg p q) r) &&
     ((o1 /= o2 && o3 /= o4) ||
      (o1 == Colinear && onSegment (Seg p r) q) ||
      (o2 == Colinear && onSegment (Seg p s) q) ||
      (o3 == Colinear && onSegment (Seg r p) s) ||
      (o4 == Colinear && onSegment (Seg r q) s))


-- | Finds the intersection point of two line segments
-- Returns the normalised distance along line segment a
-- If the value is < 0 or > 1 the segments do not intersect
-- but the result still makes sense (as in, the distance from
-- a's first point to the intersection with b is result * length a)
intersectionPoint :: (Num a, Eq a, Ord a, Fractional a) => Seg a -> Seg a -> a
intersectionPoint a b = fst $ intersectionPoints a b


-- | Finds the intersection point of two line segments
-- Returns the normalised distance along line segment a
-- If the value is < 0 or > 1 the segments do not intersect
-- but the result still makes sense (as in, the distance from
-- a's first point to the intersection with b is result * length a)
intersectionPoints :: (Num a, Eq a, Ord a, Fractional a) => Seg a -> Seg a -> (a, a)
intersectionPoints (Seg p rf) (Seg q sf) =
  let r = rf - p
      s = sf - q
      t = ((q - p) `SDL.crossZ` s) / (r `SDL.crossZ` s)
      u = ((q - p) `SDL.crossZ` r) / (r `SDL.crossZ` s)
  in (t, u)


-- | Finds the distance between a point and a line segment
-- https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
distance :: (Num a, Fractional a, Floating a) => Seg a -> SDL.V2 a -> a
distance (Seg p q) r =
  let x0 = r ^. _x
      y0 = r ^. _y
      x1 = p ^. _x
      y1 = p ^. _y
      x2 = q ^. _x
      y2 = q ^. _y
  in abs ((y2 - y1) * x0 - (x2 - x1) * y0 + (x2 * y1) - (y2 * x1))
   / sqrt ((y2 - y1) * (y2 - y1) + (x2 - x1) * (x2 - x1))


-- | Normal of a line segment, defined in a clockwise way along the
-- vector ab
normal :: (Num a, Floating a, SDL.Epsilon a) => Seg a -> SDL.V2 a
normal (Seg (SDL.V2 x1 y1) (SDL.V2 x2 y2)) = SDL.normalize $ SDL.V2 (y1-y2) (x2-x1)
