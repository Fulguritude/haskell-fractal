{-# LANGUAGE ScopedTypeVariables #-}

module Main(main) where


-- import Numeric.Algebra
import Text.Printf
import Debug.Trace
import Graphics.Gloss
-- import Data.Colour.Palette



{- Global config -}

type Dwell   = Int   -- Word
type GFloat  = Float -- Double
type Geom    = Complex
type Point2D = (GFloat, GFloat)

-- Render config
g_canv_w, g_canv_h :: Int
g_canv_w = 512
g_canv_h = 512

-- ETF config

g_geom_w, g_geom_h :: GFloat
g_anchor, g_spread :: Point2D
g_geom_w = 2.0
g_geom_h = 2.0
g_anchor = (0.0, 0.0)
g_spread = (g_geom_w, g_geom_h) -- absolute spread in manhattan absolute distance from the anchor

g_protocol :: DwellProtocol
g_protocol = Mandelbrot

g_algorithm :: DwellChoiceAlgorithm
g_algorithm = Naive

g_max_dwell :: Dwell
g_max_dwell = 32




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
	from_1d :: GFloat -> a  -- for scaling, goes to real part
	into_2d :: a -> Point2D
	from_2d :: Point2D -> a

class (Ring a, Geom2D a) => Ring2D a
-- data Ring2D a = (Ring a, Geom2D a) => Ring2D a
-- class Ring2D a where deriving (Ring a, Geom2D a) 

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
			ci = (cr a) * (ci b) + (cr a) * (ci b)
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
			de = (dr a) * (de b) + (dr a) * (de b)
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
			pj = (pr a) * (pj b) + (pr a) * (pj b)
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
	conj a = Pairwise { xx = (xx a), yy = -(yy a)}
	getx   = xx
	gety   = yy

	dot  a b = xx a * xx b + yy a * yy b
	quad a   = dot (a) (a)
	mod  a   = sqrt (quad a)
	arg  a   = atan2 (yy a) (xx a)

	from_1d x = Pairwise { xx = x, yy = 0.0 }
	into_2d a = (xx a, yy a)
	from_2d p = Pairwise { xx = (fst p), yy = (snd p) }
instance Geom2D Complex  where
	conj a = Complex { cr = (cr a), ci = -(ci a)}
	getx   = cr
	gety   = ci

	dot  a b = cr a * cr b + ci a * ci b
	quad a   = dot (a) (a)
	mod  a   = sqrt (quad a)
	arg  a   = atan2 (ci a) (cr a)

	from_1d x = Complex { cr = x, ci = 0.0 }
	into_2d a = (cr a, ci a)
	from_2d p = Complex { cr = (fst p), ci = (snd p) }
instance Geom2D Dual     where
	conj a = Dual { dr = (dr a), de = -(de a)}
	getx   = dr
	gety   = de

	dot  a b = dr a * dr b
	quad a   = dot (a) (a)
	mod      = dr
	arg  a   = (de a) / (dr a)

	from_1d x = Dual { dr = x, de = 0.0 }
	into_2d a = (dr a, de a)
	from_2d p = Dual { dr = (fst p), de = (snd p) }
instance Geom2D Perplex  where
	conj a = Perplex { pr = (pr a), pj = -(pj a)}
	getx   = pr
	gety   = pj

	dot  a b = pr a * pr b - pj a * pj b
	quad a   = dot (a) (a)
	mod  a   = let qnorm = quad a in if qnorm >= 0.0 then sqrt(qnorm) else -sqrt(-qnorm)
	arg  a   = atanh ((pj a) / (pr a))

	from_1d x = Perplex { pr = x, pj = 0.0 }
	into_2d a = (pr a, pj a)
	from_2d p = Perplex { pr = (fst p), pj = (snd p) }



newtype Polynomial a = Polynomial [a] deriving (Show, Eq)

class (Ring a) => PolynomialOps a where
	evaluate   :: Polynomial a -> a -> a
	degree     :: Polynomial a -> Int
	fix_degree :: Polynomial a -> Polynomial a

add_poly :: (Ring a, Geom2D a) => [a] -> [a] -> [a]
add_poly (  []) (  ys) = ys
add_poly (  xs) (  []) = xs
add_poly (x:xs) (y:ys) = (x +. y) : add_poly (xs) (ys)

sub_poly :: (Ring a, Geom2D a) => [a] -> [a] -> [a]
sub_poly (  []) (  ys) = map neg ys
sub_poly (  xs) (  []) = xs
sub_poly (x:xs) (y:ys) = (x -. y) : sub_poly (xs) (ys)

mul_poly :: (Ring a, Geom2D a) => [a] -> [a] -> [a]
mul_poly (   _) ([]) = []
mul_poly (  []) ( _) = []
mul_poly (x:xs) (ys) =
	let convolution_term  = map (x *.) (ys)        in
	let moved_convolution = from_1d 0.0 : mul_poly (xs) (ys) in 
	add_poly (convolution_term) (moved_convolution)

instance (Ring a, Geom2D a) => Ring (Polynomial a) where
	zero = Polynomial ([])
	unit = let i = from_1d 1.0 in Polynomial ([i])
	neg  (Polynomial xs) = Polynomial (map neg xs)
	(+.) (Polynomial xs) (Polynomial ys) = Polynomial (add_poly (xs) (ys))
	(-.) (Polynomial xs) (Polynomial ys) = Polynomial (sub_poly (xs) (ys))
	(*.) (Polynomial xs) (Polynomial ys) = Polynomial (mul_poly (xs) (ys))

instance PolynomialOps Pairwise where
	evaluate (Polynomial (  [])) (_) = zero
	evaluate (Polynomial (c:cs)) (z) = c +. (z *. evaluate (Polynomial cs) (z))

	degree (Polynomial []) = 0
	degree (Polynomial cs) = (length cs) - 1

	fix_degree (Polynomial cs) = Polynomial (reverse (dropWhile (== zero) (reverse cs)))
instance PolynomialOps Dual where
	evaluate (Polynomial (  [])) (_) = zero
	evaluate (Polynomial (c:cs)) (z) = c +. (z *. evaluate (Polynomial cs) (z))

	degree (Polynomial []) = 0
	degree (Polynomial cs) = (length cs) - 1

	fix_degree (Polynomial cs) = Polynomial (reverse (dropWhile (== zero) (reverse cs)))
instance PolynomialOps Complex where
	evaluate (Polynomial (  [])) (_) = zero
	evaluate (Polynomial (c:cs)) (z) = c +. (z *. evaluate (Polynomial cs) (z))

	degree (Polynomial []) = 0
	degree (Polynomial cs) = (length cs) - 1

	fix_degree (Polynomial cs) = Polynomial (reverse (dropWhile (== zero) (reverse cs)))
instance PolynomialOps Perplex where
	evaluate (Polynomial (  [])) (_) = zero
	evaluate (Polynomial (c:cs)) (z) = c +. (z *. evaluate (Polynomial cs) (z))

	degree (Polynomial []) = 0
	degree (Polynomial cs) = (length cs) - 1

	fix_degree (Polynomial cs) = Polynomial (reverse (dropWhile (== zero) (reverse cs)))




{- Espace-time fractal declarations -}

data DwellProtocol =
	Julia       |
	Mandelbrot  |
	Newton      |
	Burningship |
	Duquesne
	deriving (Eq, Enum, Show)

data DwellChoiceAlgorithm =
	Naive             |
	MarianiSilverQuad
	deriving (Eq, Enum, Show)

data ETF a = ETF {
	protocol       :: DwellProtocol,
	algorithm      :: DwellChoiceAlgorithm,
	compute_dwell  :: DwellFunction a,
	compute_dwells :: DwellAlgorithm a,
	anchor         :: a,
	spread         :: a,
	radius         :: GFloat,
	radius_sqrd    :: GFloat,
	iter_poly      :: Polynomial a
}


-- TODO add constraint
type DwellFunction  a = ETF a -> a -> Dwell
type DwellAlgorithm a = ETF a -> Ring2DArray a -> DwellArray




compute_dwell_mandelbrot :: forall a. (Geom2D a, PolynomialOps a) => DwellFunction a
compute_dwell_mandelbrot (etf) (z) =
	let lim  = radius_sqrd etf in
	let
		coefs :: [a]
		coefs =
			case iter_poly etf of
			Polynomial (  []) -> [z, from_1d 0.0, from_1d 2.0]
			Polynomial (_:cs) -> cs
	in
	let z0 = zero in
	let poly = trace (show z0) $ Polynomial (z : coefs) in
	let eval_poly = evaluate (poly) in
	let
		iterate_dwell :: a -> Dwell -> Dwell
		iterate_dwell (z_n) (dwell) =
			if dwell >= g_max_dwell
				then
					g_max_dwell
				else
					let z_np1 = eval_poly (z_n) in
					let qnorm = quad z_np1 in
					if qnorm > lim
						then dwell
						else iterate_dwell (z_np1) (dwell + 1)
	in
	let result = iterate_dwell (z0) (0) in
	result


get_dwell_function ::  (Geom2D a, PolynomialOps a) => DwellProtocol -> DwellFunction a
get_dwell_function (proto) =
	case proto of
		Julia       -> compute_dwell_mandelbrot --julia
		Mandelbrot  -> compute_dwell_mandelbrot --mandelbrot
		Newton      -> compute_dwell_mandelbrot --newton
		Burningship -> compute_dwell_mandelbrot --burningship
		Duquesne    -> compute_dwell_mandelbrot --duquesne

get_iter_algorithm :: DwellChoiceAlgorithm -> DwellAlgorithm a
get_iter_algorithm (algorithm) =
	let
		result :: DwellAlgorithm a
		result (etf) (point_array) =
			let point_matrix = points point_array in
			let dwell_matrix = (map.map) (compute_dwell etf etf) (point_matrix) in
			let dwell_array = DwellArray { daw = raw point_array, dah = rah point_array, dwells = dwell_matrix } in
			dwell_array
	in
	result



{- Render declarations -}

type Palette = [Color]

data RenderParams = RenderParams {
	window_dims   :: (Int, Int),
	zoom          :: GFloat,
	palette       :: Palette,
	is_static     :: Bool
}

data Ring2DArray a = Ring2DArray {
	raw    :: Int,
	rah    :: Int,
	points :: [[a]]
}

data DwellArray = DwellArray {
	daw    :: Int,
	dah    :: Int,
	dwells :: [[Int]]
}

data PixelArray = PixelArray {
	paw    :: Int,
	pah    :: Int,
	pixels :: [[Color]]
}

picture_from_pixelarray :: PixelArray -> Picture
picture_from_pixelarray pixel_array =
	let vert_offset = fromIntegral(pah pixel_array) / 2.0 in
	let horz_offset = fromIntegral(paw pixel_array) / 2.0 in
	let result      =
		pictures
		[
			translate
				(x - horz_offset)
				(y - vert_offset)
				(color pixel (rectangleSolid 1 1))
			|
			(row,   y) <- zip (pixels pixel_array) [0..],
			(pixel, x) <- zip row                  [0..]
		]
	in
	result




fromIntegral2D :: (Int, Int) -> Point2D
fromIntegral2D (x, y) = (fromIntegral x, fromIntegral y)

transform_affine_1d ::
	Point2D ->
	Point2D ->
	GFloat ->
	GFloat
transform_affine_1d
		(old_range_begin, old_range_final)
		(new_range_begin, new_range_final)
		value
	=
		let old_range  = old_range_final - old_range_begin in
		let new_range  = new_range_final - new_range_begin in
		let normalized = (value - old_range_begin) / old_range in
		let result     = (normalized * new_range) + new_range_begin in
		result

transform_affine_2d ::
	(Point2D, Point2D) ->
	(Point2D, Point2D) ->
	Point2D ->
	Point2D
transform_affine_2d
		(old_range_topleft_, old_range_botright)
		(new_range_topleft_, new_range_botright)
		point
	=
		let old_x = fst point in
		let old_y = snd point in
		let old_range_x = (fst old_range_topleft_, fst old_range_botright) in
		let old_range_y = (snd old_range_topleft_, snd old_range_botright) in
		let new_range_x = (fst new_range_topleft_, fst new_range_botright) in
		let new_range_y = (snd new_range_topleft_, snd new_range_botright) in
		let new_x = transform_affine_1d (old_range_x) (new_range_x) (old_x) in
		let new_y = transform_affine_1d (old_range_y) (new_range_y) (old_y) in
		let result = (new_x, new_y) in
		result

get_geompoint_from_windowcoord ::
	Geom2D a =>
	(Point2D, Point2D) ->
	(Point2D, Point2D) ->
	(Int, Int) ->
	a
get_geompoint_from_windowcoord (old_range) (new_range) (coordinates) =
	let point  = fromIntegral2D (coordinates) in
	let result = transform_affine_2d (old_range) (new_range) (point) in
	from_2d result

get_all_points ::
	Geom2D a =>
	(Int, Int) ->
	(Point2D, Point2D) ->
	(Point2D, Point2D) ->
	Ring2DArray a
get_all_points (wind_dims) (old_range) (new_range) =
	let (win_w, win_h) = wind_dims in
	let get_geompoint  = get_geompoint_from_windowcoord (old_range) (new_range) in
	let build_horz (y) = [ get_geompoint (x, y) | x <- [ 0 .. win_w ] ] in
	let	point_matrix   = [ build_horz       (y) | y <- [ 0 .. win_h ] ] in
	let result         = Ring2DArray { raw = win_w, rah = win_h, points = point_matrix } in
	result

map_colors :: Palette -> DwellArray -> PixelArray
map_colors (palette) (dwell_array) =
	let dwell_matrix = dwells dwell_array in
	let pixel_matrix = (map.map) (palette !!) (dwell_matrix) in
	let pixel_array = PixelArray { paw = daw dwell_array, pah = dah dwell_array, pixels = pixel_matrix } in
	pixel_array

render_fractal :: (Ring a, Geom2D a) => RenderParams -> ETF a -> Picture
render_fractal (params) (etf) =
	let dims  = window_dims (params) in
	let win_w = fst dims in
	let win_h = snd dims in
	let wind_topleft_ = (0.0, 0.0) in
	let wind_topright = fromIntegral2D (win_w, win_h) in
	let wind_range = (wind_topleft_, wind_topright) in

	let geom_anchor = anchor (etf) in
	let geom_spread = spread (etf) in
	let geom_topleft_ = into_2d (geom_anchor -. geom_spread) in
	let geom_topright = into_2d (geom_anchor +. geom_spread) in
	let geom_range = (geom_topleft_, geom_topright) in

	let point_array = get_all_points (dims) (wind_range) (geom_range) in
	let dwell_array = (compute_dwells etf etf) (point_array) in
	let pixel_array = map_colors (palette params) (dwell_array) in
	let result      = picture_from_pixelarray (pixel_array) in
	result


main :: IO ()
main =
	let sc = 1.0 / fromIntegral(g_max_dwell) in
	let
		render_params :: RenderParams
		render_params = RenderParams {
			window_dims = (g_canv_w, g_canv_h),
			zoom        = 1.0,
			palette     = [makeColor (fromIntegral(x)*sc) (fromIntegral(x)*sc) (fromIntegral(x)*sc) (255)  | x <- [ 0 .. g_max_dwell  ] ],
			is_static   = True
		}
	in
	let
		etf :: ETF Geom
		etf = ETF {
			protocol       = g_protocol,
			algorithm      = g_algorithm,
			compute_dwell  = get_dwell_function (g_protocol),
			compute_dwells = get_iter_algorithm (g_algorithm),
			anchor         = from_2d (g_anchor),
			spread         = from_2d (g_spread),
			radius         = 2.0,
			radius_sqrd    = 4.0,
			iter_poly      = Polynomial ([from_1d 0.0, from_1d 0.0, from_1d 2.0])
		}
	in
	let 
		drawing :: Picture
		drawing = render_fractal (render_params) (etf)
	in
	let
		window :: Display
		window = InWindow "Fractal Renderer" (window_dims render_params) (0, 0)
	in
	let
		background :: Color
		background = black
	in
	display window background drawing














{-
create_gradient :: Int -> Int -> Picture
create_gradient (width) (height) =
	let normalize_color (pos) (x) = fromIntegral (x) / fromIntegral (pos) in
	let normalize_w               = normalize_color (width)  in
	let normalize_h               = normalize_color (height) in
	let pixel_from_rg (r) (g) = makeColor (normalize_h (r)) (normalize_w (g)) (0) (255) in
	let build_greens  (r)     = [ pixel_from_rg (r) (g) | g <- [ 0 .. width  ] ] in
	let	color_matrix          = [ build_greens  (r)     | r <- [ 0 .. height ] ] in
	let pixel_array           = PixelArray { w = width, h = height, pixels = color_matrix } in
	let result                = picture_from_pixelarray (pixel_array) in
	result
-}