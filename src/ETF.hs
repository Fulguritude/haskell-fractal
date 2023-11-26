{-# LANGUAGE ScopedTypeVariables #-}

module ETF (module ETF) where


-- import Debug.Trace

import Math
import Data.Bifunctor




{- Global config -}

type Dwell   = Int   -- Word




{- Espace-time fractal declarations -}

data DwellProtocol =
	Julia        |
	Mandelbrot   |
	PseudoNewton |
	Burningship  |
	Duquesne
	deriving (Eq, Enum, Show)

data DwellChoiceAlgorithm =
	Naive              |
	MarianiSilverQuad  |
	MarianiSilverPoint
	deriving (Eq, Enum, Show)

data ETF a = ETF {
	r2algebra      :: R2Algebra,
	protocol       :: DwellProtocol,
	algorithm      :: DwellChoiceAlgorithm,
	max_dwell      :: Dwell,
	compute_dwell  :: DwellFunction a,
	compute_dwells :: DwellAlgorithm a,
	has_escaped    :: GFloat -> Bool,
	anchor         :: a,
	spread         :: a,
	parameter      :: a,
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



build_poly_mandelbrot :: (Ring a, Convert a) => ETF a -> a -> Polynomial a
build_poly_mandelbrot (etf) (z) =
	let
		coefs =
			case iter_poly etf of
			Polynomial (  []) -> [zero, zero, from_1d (2.0 :: GFloat)]
			Polynomial (_:cs) -> cs
	in
	let poly = Polynomial (z : coefs) in  -- poly = trace (show z0) $ Polynomial (z : coefs) in
	poly


build_has_escaped :: GFloat -> R2Algebra -> (GFloat -> Bool)
build_has_escaped (lim) (r2alg) =
	case r2alg of
		R2A_Pairwise -> ( > lim )
		R2A_Complex  -> ( > lim )
		R2A_Dual     -> ( > lim )
		R2A_Perplex  -> ( \x -> x > lim || x < -lim )
		R2A_Tropical -> ( > lim )
		R2A_Average  -> ( > lim )
		-- R2A_CPolar   ->
		-- R2A_DPolar   ->
		-- R2A_PPolar   ->
		_            -> ( > lim )

build_iterate_dwell ::
	forall a. (Geom2D a) =>
	Dwell ->
	(a -> a) ->
	(GFloat -> Bool) ->
	(a -> Dwell -> Dwell)
build_iterate_dwell
	(maxdwell)
	(iterate_sequence)
	(hasescaped)
	=
	let
		iterate_dwell :: a -> Dwell -> Dwell
		iterate_dwell (z_n) (dwell) =
			if dwell >= maxdwell
				then
					maxdwell
				else
					let z_np1 = iterate_sequence (z_n) in
					let qnorm = quad z_np1 in
					if hasescaped (qnorm)
						then dwell
						else iterate_dwell (z_np1) (dwell + 1)
	in
	iterate_dwell



compute_dwell_julia :: (Geom2D a, PolynomialOps a) => DwellFunction a
compute_dwell_julia (etf) (z) =
	let poly          = iter_poly etf in
	let eval_poly     = evaluate (poly) in
	let iterate_dwell = build_iterate_dwell (max_dwell etf) (eval_poly) (has_escaped etf) in
	let result        = iterate_dwell (z) (0) in
	result


compute_dwell_mandelbrot :: (Convert a, Geom2D a, PolynomialOps a) => DwellFunction a
compute_dwell_mandelbrot (etf) (z) =
	let poly          = build_poly_mandelbrot (etf) (z) in
	let eval_poly     = evaluate (poly) in
	let iterate_dwell = build_iterate_dwell (max_dwell etf) (eval_poly) (has_escaped etf) in
	let z0            = zero in
	let result        = iterate_dwell (z0) (0) in
	result


compute_dwell_pseudonewton :: (Convert a, Geom2D a, PolynomialOps a) => DwellFunction a
compute_dwell_pseudonewton (etf) (z) =
	let poly           = iter_poly etf in
	let param          = from_1d (-1 :: GFloat) *. (parameter etf) in
	let eval_poly (zz) = (param *. evaluate (poly) (zz)) +. zz in
	let iterate_dwell  = build_iterate_dwell (max_dwell etf) (eval_poly) (has_escaped etf) in
	let result         = iterate_dwell (z) (0) in
	result


compute_dwell_burningship :: (Convert a, Geom2D a, PolynomialOps a) => DwellFunction a
compute_dwell_burningship (etf) (z) =
	let poly           = build_poly_mandelbrot (etf) (z) in
	let
		eval_poly (zz) =
			let zz2    = into_2d (zz) in
			let zz2abs = bimap abs abs zz2 in
			let zzabs  = from_2d zz2abs in
			evaluate (poly) (zzabs)
	in
	let iterate_dwell  = build_iterate_dwell (max_dwell etf) (eval_poly) (has_escaped etf) in
	let z0             = zero in
	let result         = iterate_dwell (z0) (0) in
	result

compute_dwell_duquesne :: (Convert a, Geom2D a, PolynomialOps a) => DwellFunction a
compute_dwell_duquesne (etf) (z) =
	let poly          = iter_poly etf in
	let
		eval_poly (zz) =
			let zz1    = evaluate (poly) (zz) in
			let zz2    = into_2d (zz1) in
			let zz3    = from_2d (bimap abs negate zz2) in
			let zz4    = evaluate (poly) (zz3) in
			let result = from_1d (0.5 :: GFloat) *. (zz3 +. zz4) in
			result
	in
	let iterate_dwell = build_iterate_dwell (max_dwell etf) (eval_poly) (has_escaped etf) in
	let result        = iterate_dwell (z) (0) in
	result



get_dwell_function ::  (Convert a, Geom2D a, PolynomialOps a) => DwellProtocol -> DwellFunction a
get_dwell_function (proto) =
	case proto of
		Julia        -> compute_dwell_julia
		Mandelbrot   -> compute_dwell_mandelbrot
		PseudoNewton -> compute_dwell_pseudonewton
		Burningship  -> compute_dwell_burningship
		Duquesne     -> compute_dwell_duquesne




get_iter_algorithm :: DwellChoiceAlgorithm -> DwellAlgorithm a
get_iter_algorithm (algo) =
	let
		naive_alg :: DwellAlgorithm a
		naive_alg (etf) (point_array) =
			let point_matrix = points point_array in
			let dwell_matrix = (map.map) (compute_dwell etf etf) (point_matrix) in
			let dwell_array = DwellArray { daw = raw point_array, dah = rah point_array, dwells = dwell_matrix } in
			dwell_array
	in
	let
		ms_quad :: DwellAlgorithm a
		ms_quad (etf) (point_array) =
			let dwell_array = DwellArray { daw = raw point_array, dah = rah point_array, dwells =  [] } in
			dwell_array
	in
	let
		ms_point :: DwellAlgorithm a
		ms_point (etf) (point_array) =
			let dwell_array = DwellArray { daw = raw point_array, dah = rah point_array, dwells =  [] } in
			dwell_array
	in
	let
		result :: DwellAlgorithm a
		result =
			case algo of
				Naive              -> naive_alg
				MarianiSilverQuad  -> ms_quad
				MarianiSilverPoint -> ms_point
	in
	result
