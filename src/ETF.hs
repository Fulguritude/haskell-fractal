{-# LANGUAGE ScopedTypeVariables #-}

module ETF where


import Debug.Trace

import Math


{- Global config -}
type Dwell   = Int   -- Word

g_geom_w, g_geom_h :: GFloat
g_anchor, g_spread :: Point2D
g_geom_w = 2.0
g_geom_h = 2.0
g_anchor = (0.0, 0.0)
g_spread = (g_geom_w, g_geom_h) -- absolute spread in supremum absolute distance from the anchor

g_protocol :: DwellProtocol
g_protocol = Mandelbrot

g_algorithm :: DwellChoiceAlgorithm
g_algorithm = Naive

g_max_dwell :: Dwell
g_max_dwell = 32



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

-- TODO add constraint
type DwellFunction  a = ETF a -> a -> Dwell
type DwellAlgorithm a = ETF a -> Ring2DArray a -> DwellArray




compute_dwell_mandelbrot :: forall a. (Geom2D a, Convert a, PolynomialOps a) => DwellFunction a
compute_dwell_mandelbrot (etf) (z) =
	let lim  = radius_sqrd etf in
	let
		coefs :: [a]
		coefs =
			case iter_poly etf of
			Polynomial (  []) -> [z, zero, from_1d 2.0]
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


get_dwell_function ::  (Geom2D a, Convert a, PolynomialOps a) => DwellProtocol -> DwellFunction a
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
