{-# LANGUAGE ScopedTypeVariables #-}

module ETF_Protocols (module ETF_Protocols) where


import Data.Bifunctor

import Math
import ETF_Types


build_poly_mandelbrot :: (Ring a, Convert a) => ETF a -> a -> Polynomial a
build_poly_mandelbrot (etf) (z) =
	let
		coefs =
			case iter_poly etf of
			Polynomial (  []) -> [zero, zero, from_1d (2.0 :: GFloat)]
			Polynomial (_:cs) -> cs
	in
	let poly = Polynomial (z : coefs) in  -- poly = trace (show z) $ Polynomial (z : coefs) in
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
	Int ->
	(a -> IterationData a -> IterationData a)
build_iterate_dwell
	(maxdwell)
	(iterate_sequence)
	(hasescaped)
	(depth)
	=
	let
		iterate_dwell_rec :: a -> Dwell -> [a] -> (Dwell, [a])
		iterate_dwell_rec (z_n) (dwell) (values) =
			if dwell >= maxdwell
				then
					(maxdwell, z_n : values)
				else
					let z_np1 = iterate_sequence (z_n) in
					let qnorm = quad z_np1 in
					if hasescaped (qnorm)
						then (dwell, z_np1 : values)
						else iterate_dwell_rec (z_np1) (dwell + 1) (z_n : values)
	in
	let
		iterate_dwell :: a -> IterationData a -> IterationData a
		iterate_dwell (z0) (iter_data) =
			let maybe_calced = id_calced iter_data in
			case maybe_calced of
				Just _  -> iter_data
				Nothing ->
					let results = iterate_dwell_rec (z0) (0) ([]) in
					let (dwell, values) = results in
					let result = IterationData {
						id_coord  = id_coord iter_data,
						id_pos    = id_pos   iter_data,
						id_values = Just values,
						id_depth  = Just depth,
						id_calced = Just True,
						id_dwell  = Just dwell						
					}
					in
					result
	in
	iterate_dwell



compute_dwell_julia :: (Geom2D a, PolynomialOps a) => DwellFunction a
compute_dwell_julia (etf) (iter_data) =
	let poly          = iter_poly etf in
	let eval_poly     = evaluate (poly) in
	let iterate_dwell = build_iterate_dwell (max_dwell etf) (eval_poly) (has_escaped etf) (0) in
	let z0            = id_pos iter_data in
	let result        = iterate_dwell (z0) (iter_data) in
	result


compute_dwell_mandelbrot :: (Convert a, Geom2D a, PolynomialOps a) => DwellFunction a
compute_dwell_mandelbrot (etf) (iter_data) =
	let poly          = build_poly_mandelbrot (etf) (id_pos iter_data) in
	let eval_poly     = evaluate (poly) in
	let iterate_dwell = build_iterate_dwell (max_dwell etf) (eval_poly) (has_escaped etf) (0) in
	let z0            = zero in
	let result        = iterate_dwell (z0) (iter_data) in
	result


compute_dwell_pseudonewton :: (Convert a, Geom2D a, PolynomialOps a) => DwellFunction a
compute_dwell_pseudonewton (etf) (iter_data) =
	let poly           = iter_poly etf in
	let param          = from_1d (-1 :: GFloat) *. (parameter etf) in
	let eval_poly (zz) = (param *. evaluate (poly) (zz)) +. zz in
	let iterate_dwell  = build_iterate_dwell (max_dwell etf) (eval_poly) (has_escaped etf) (0) in
	let z0             = id_pos iter_data in
	let result         = iterate_dwell (z0) (iter_data) in
	result


compute_dwell_burningship :: (Convert a, Geom2D a, PolynomialOps a) => DwellFunction a
compute_dwell_burningship (etf) (iter_data) =
	let poly           = build_poly_mandelbrot (etf) (id_pos iter_data) in
	let
		eval_poly (zz) =
			let zz2    = into_2d (zz) in
			let zz2abs = bimap abs abs zz2 in
			let zzabs  = from_2d zz2abs in
			evaluate (poly) (zzabs)
	in
	let iterate_dwell  = build_iterate_dwell (max_dwell etf) (eval_poly) (has_escaped etf) (0) in
	let z0             = zero in
	let result         = iterate_dwell (z0) (iter_data) in
	result

compute_dwell_duquesne :: (Convert a, Geom2D a, PolynomialOps a) => DwellFunction a
compute_dwell_duquesne (etf) (iter_data) =
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
	let iterate_dwell = build_iterate_dwell (max_dwell etf) (eval_poly) (has_escaped etf) (0) in
	let z0            = id_pos iter_data in
	let result        = iterate_dwell (z0) (iter_data) in
	result



get_dwell_function ::  (Convert a, Geom2D a, PolynomialOps a) => DwellProtocol -> DwellFunction a
get_dwell_function (proto) =
	case proto of
		Julia        -> compute_dwell_julia
		Mandelbrot   -> compute_dwell_mandelbrot
		PseudoNewton -> compute_dwell_pseudonewton
		Burningship  -> compute_dwell_burningship
		Duquesne     -> compute_dwell_duquesne
