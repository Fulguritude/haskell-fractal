module Math where

import Text.Printf
import GHC.Float

{- Global config -}

type GFloat  = Float -- Double
type Geom    = Perplex
type Point2D = (GFloat, GFloat)

toFrac :: Real a => a -> GFloat
toFrac = realToFrac


{- Math declarations -}

{-
data Norm =
	Manhattan |
	Euclidean |
	Supremum  |
	Spacetime |
	Default    -- that which is natural for a given geometry for various geometries
	deriving (Eq, Enum, Show)
-}

class (Show a, Eq a) => Ring a where
	zero :: a
	unit :: a
	neg  :: a -> a
	(+.) :: a -> a -> a
	(-.) :: a -> a -> a
	(*.) :: a -> a -> a

class Geom2D a where
	conj    :: a -> a
	getx    :: a -> GFloat
	gety    :: a -> GFloat
	dot     :: a -> a -> GFloat
	quad    :: a -> GFloat
	mod     :: a -> GFloat
	arg     :: a -> GFloat


{-
class ConvertGen f1 f2 a where
	from_1d :: f1 -> a  -- for scaling, goes to real part
	into_2d :: a -> f2
	from_2d :: f2 -> a
class ConvertGen  GFloat   Point2D  a => Convert a
class ConvertGen [GFloat] [Point2D] a => ConvertPoly a
-}

class Convert a where
	from_1d :: (Real f) =>  f     -> a  -- for scaling, goes to real part
	from_2d :: (Real f) => (f, f) -> a
	into_2d :: a -> Point2D
class ConvertPoly a where
	from_1ds :: (Real f) => [ f    ] -> a  -- for scaling, goes to real part
	from_2ds :: (Real f) => [(f, f)] -> a
	into_2ds :: a -> [Point2D]

class (Ring a, Geom2D a, Convert a) => Ring2D a

data Pairwise = Pairwise { xx :: GFloat, yy :: GFloat }
data Complex  = Complex  { cr :: GFloat, ci :: GFloat }
data Dual     = Dual     { dr :: GFloat, de :: GFloat }
data Perplex  = Perplex  { pr :: GFloat, pj :: GFloat }
data CPolar   = CPolar   { cm :: GFloat, ch :: GFloat }
data DPolar   = DPolar   { dm :: GFloat, dh :: GFloat } -- meaningful with e^(a+b eps) = e^a + b (e^a) eps
data PPolar   = PPolar   { pm :: GFloat, ph :: GFloat }
-- data Tropical
-- data Average

instance Show Pairwise where show z = printf "Pairwise { xx: % 7.6f, : yy: % 7.6f }" (xx z) (yy z) 
instance Show Complex  where show z = printf "Complex  { cr: % 7.6f, : ci: % 7.6f }" (cr z) (ci z) 
instance Show Dual     where show z = printf "Dual     { dr: % 7.6f, : de: % 7.6f }" (dr z) (de z) 
instance Show Perplex  where show z = printf "Perplex  { pr: % 7.6f, : pj: % 7.6f }" (pr z) (pj z) 

instance Eq Pairwise where
	(==) a b = xx a == xx b && yy a == yy b
	(/=) a b = xx a /= xx b || yy a /= yy b
instance Eq Complex  where
	(==) a b = cr a == cr b && ci a == ci b
	(/=) a b = cr a /= cr b || ci a /= ci b
instance Eq Dual     where
	(==) a b = dr a == dr b && de a == de b
	(/=) a b = dr a /= dr b || de a /= de b
instance Eq Perplex  where
	(==) a b = pr a == pr b && pj a == pj b
	(/=) a b = pr a /= pr b || pj a /= pj b

instance Ring Pairwise where
	zero     = Pairwise { xx = 0.0,             yy = 0.0             }
	unit     = Pairwise { xx = 1.0,             yy = 0.0             }
	neg  a   = Pairwise { xx = -(xx a),         yy = -(yy a)         }
	(+.) a b = Pairwise { xx = (xx a) + (xx b), yy = (yy a) + (yy b) }
	(-.) a b = Pairwise { xx = (xx a) - (xx b), yy = (yy a) - (yy b) }
	(*.) a b = Pairwise { xx = (xx a) * (xx b), yy = (yy a) * (yy b) }
instance Ring Complex  where
	zero     = Complex { cr = 0.0,             ci = 0.0            }
	unit     = Complex { cr = 1.0,             ci = 0.0            }
	neg  a   = Complex { cr = -(cr a),         ci = -(ci a)        }
	(+.) a b = Complex { cr = (cr a) + (cr b), ci = (ci a) + (ci b)}
	(-.) a b = Complex { cr = (cr a) - (cr b), ci = (ci a) - (ci b)}
	(*.) a b =
		Complex {
			cr = (cr a) * (cr b) - (ci a) * (ci b),
			ci = (cr a) * (ci b) + (cr b) * (ci a)
		}
instance Ring Dual     where
	zero     = Dual { dr = 0.0,             de = 0.0            }
	unit     = Dual { dr = 1.0,             de = 0.0            }
	neg  a   = Dual { dr = -(dr a),         de = -(de a)        }
	(+.) a b = Dual { dr = (dr a) + (dr b), de = (de a) + (de b)}
	(-.) a b = Dual { dr = (dr a) - (dr b), de = (de a) - (de b)}
	(*.) a b =
		Dual {
			dr = (dr a) * (dr b),
			de = (dr a) * (de b) + (dr b) * (de a)
		}
instance Ring Perplex  where
	zero     = Perplex { pr = 0.0,             pj = 0.0             }
	unit     = Perplex { pr = 1.0,             pj = 0.0             }
	neg  a   = Perplex { pr = -(pr a),         pj = -(pj a)         }
	(+.) a b = Perplex { pr = (pr a) + (pr b), pj = (pj a) + (pj b) }
	(-.) a b = Perplex { pr = (pr a) - (pr b), pj = (pj a) - (pj b) }
	(*.) a b =
		Perplex {
			pr = (pr a) * (pr b) + (pj a) * (pj b),
			pj = (pr a) * (pj b) + (pr b) * (pj a)
		}
{-
instance Ring2D CPolar   where
	(+.) a b = CPolar   {md = (md a) * (md b), th = (th a) + (th b)}
	(-.) a b = CPolar   {md = (md a) / (md b), th = (th a) - (th b)}
	(*.) a b =
		Polar   {
			md = (md a) * (md b) - (th a) * (th b),
			th = (md a) * (th b) + (md a) * (th b)
		}
-}

instance Geom2D Pairwise where
	conj a   = Pairwise { xx = (xx a), yy = -(yy a)}
	getx     = xx
	gety     = yy
	dot  a b = xx a * xx b + yy a * yy b
	quad a   = dot (a) (a)
	mod  a   = sqrt (quad a)
	arg  a   = atan2 (yy a) (xx a)
instance Geom2D Complex  where
	conj a   = Complex { cr = (cr a), ci = -(ci a)}
	getx     = cr
	gety     = ci
	dot  a b = cr a * cr b + ci a * ci b
	quad a   = dot (a) (a)
	mod  a   = sqrt (quad a)
	arg  a   = atan2 (ci a) (cr a)
instance Geom2D Dual     where
	conj a   = Dual { dr = (dr a), de = -(de a)}
	getx     = dr
	gety     = de
	dot  a b = dr a * dr b
	quad a   = dot (a) (a)
	mod      = dr
	arg  a   = (de a) / (dr a)
instance Geom2D Perplex  where
	conj a   = Perplex { pr = (pr a), pj = -(pj a)}
	getx     = pr
	gety     = pj
	dot  a b = pr a * pr b - pj a * pj b
	quad a   = dot (a) (a)
	mod  a   = let qnorm = quad a in if qnorm >= 0.0 then sqrt(qnorm) else -sqrt(-qnorm)
	arg  a   = atanh ((pj a) / (pr a))

instance Convert Pairwise where
	from_1d x = Pairwise { xx = toFrac x, yy = 0.0 }
	from_2d p = Pairwise { xx = toFrac (fst p), yy = toFrac (snd p) }
	into_2d a = (xx a, yy a)
instance Convert Complex  where
	from_1d x = Complex { cr = toFrac x, ci = 0.0 }
	from_2d p = Complex { cr = toFrac (fst p), ci = toFrac (snd p) }
	into_2d a = (cr a, ci a)
instance Convert Dual     where
	from_1d x = Dual { dr = toFrac x, de = 0.0 }
	from_2d p = Dual { dr = toFrac (fst p), de = toFrac (snd p) }
	into_2d a = (dr a, de a)
instance Convert Perplex  where
	from_1d x = Perplex { pr = toFrac x, pj = 0.0 }
	from_2d p = Perplex { pr = toFrac (fst p), pj = toFrac (snd p) }
	into_2d a = (pr a, pj a)



newtype Polynomial a = Polynomial [a] deriving (Show, Eq)

class (Ring a) => PolynomialOps a where
	evaluate   :: Polynomial a -> a -> a
	degree     :: Polynomial a -> Int
	fix_degree :: Polynomial a -> Polynomial a

add_poly_arr :: (Ring a, Geom2D a) => [a] -> [a] -> [a]
add_poly_arr (  []) (  ys) = ys
add_poly_arr (  xs) (  []) = xs
add_poly_arr (x:xs) (y:ys) = (x +. y) : add_poly_arr (xs) (ys)

add_poly ::  (Ring a, Geom2D a) => Polynomial a -> Polynomial a -> Polynomial a
add_poly (Polynomial ps) (Polynomial qs) = Polynomial (add_poly_arr ps qs)

sub_poly_arr :: (Ring a, Geom2D a) => [a] -> [a] -> [a]
sub_poly_arr (  []) (  ys) = map neg ys
sub_poly_arr (  xs) (  []) = xs
sub_poly_arr (x:xs) (y:ys) = (x -. y) : sub_poly_arr (xs) (ys)

sub_poly ::  (Ring a, Geom2D a) => Polynomial a -> Polynomial a -> Polynomial a
sub_poly (Polynomial ps) (Polynomial qs) = Polynomial (sub_poly_arr ps qs)

mul_poly_arr :: (Ring a, Geom2D a, Convert a) => [a] -> [a] -> [a]
mul_poly_arr (   _) ([]) = []
mul_poly_arr (  []) ( _) = []
mul_poly_arr (x:xs) (ys) =
	let convolution_term  = map (x *.) (ys)        in
	let moved_convolution = from_1d 0 : mul_poly_arr (xs) (ys) in
	add_poly_arr (convolution_term) (moved_convolution)

mul_poly ::  (Ring a, Geom2D a, Convert a) => Polynomial a -> Polynomial a -> Polynomial a
mul_poly (Polynomial ps) (Polynomial qs) = Polynomial (mul_poly_arr ps qs)

instance (Ring a, Geom2D a, Convert a) => Ring (Polynomial a) where
	zero = Polynomial ([])
	unit = let i = from_1d 1 in Polynomial ([i])
	neg  (Polynomial xs) = Polynomial (map neg xs)
	(+.) = add_poly
	(-.) = sub_poly
	(*.) = mul_poly

instance (Convert a) => ConvertPoly (Polynomial a) where
	from_1ds xs = Polynomial (map from_1d xs)
	from_2ds zs = Polynomial (map from_2d zs)
	into_2ds p  =
		case p of
			Polynomial (  []) -> []
			Polynomial (z:zs) -> into_2d (z) : into_2ds (Polynomial (zs))



g_evaluate   :: Ring a => a -> Polynomial a -> a -> a
g_evaluate z0 (Polynomial (  [])) (_) = z0
g_evaluate z0 (Polynomial (c:cs)) (z) = c +. (z *. g_evaluate (z0) (Polynomial cs) (z))

g_degree     :: Polynomial a -> Int
g_degree (Polynomial []) = 0
g_degree (Polynomial cs) = (length cs) - 1

g_fix_degree :: Ring a => a -> Polynomial a -> Polynomial a
g_fix_degree (z0) (Polynomial cs) = Polynomial (reverse (dropWhile (== z0) (reverse cs)))

instance PolynomialOps Pairwise where evaluate = g_evaluate zero; degree = g_degree; fix_degree = g_fix_degree zero
instance PolynomialOps Dual     where evaluate = g_evaluate zero; degree = g_degree; fix_degree = g_fix_degree zero
instance PolynomialOps Complex  where evaluate = g_evaluate zero; degree = g_degree; fix_degree = g_fix_degree zero
instance PolynomialOps Perplex  where evaluate = g_evaluate zero; degree = g_degree; fix_degree = g_fix_degree zero
