{-# LANGUAGE ScopedTypeVariables #-}

module ETF (module ETF) where


import Debug.Trace
import System.Exit

import Math
import Data.Bifunctor
import Data.List ( sortBy, unionBy )  --, nubBy )
import Data.Maybe




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

data DwellAlgorithmChoice =
	Naive              |
	MarianiSilverQuad  |
	MarianiSilverPoint
	--	({-points_per_rank-} Int)
	--	({-depth-} Int)
	deriving (Eq, Enum, Show)

data ETF a = ETF {
	r2algebra      :: R2Algebra,
	protocol       :: DwellProtocol,
	algorithm      :: DwellAlgorithmChoice,
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

data IterationData a = IterationData {
	id_coord  :: (Int, Int),
	id_pos    :: a,
	id_values :: Maybe [a],
	id_depth  :: Maybe Int,
	id_calced :: Maybe Bool,
	id_dwell  :: Maybe Dwell
} deriving (Show)


data Ring2DArray a = Ring2DArray {
	raw    :: Int,
	rah    :: Int,
	points :: [[a]]
}

data DwellArray a = DwellArray {
	daw    :: Int,
	dah    :: Int,
	dwells :: [[IterationData a]]
}

-- TODO add constraint
type DwellFunction  a = ETF a -> (IterationData a) -> (IterationData a)
type DwellAlgorithm a = ETF a -> Ring2DArray (IterationData a) -> DwellArray a



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




-- data QuadTree a = Branch {
-- 	qt_tl :: QuadTree a,
-- 	qt_tr :: QuadTree a,
-- 	qt_bl :: QuadTree a,
-- 	qt_br :: QuadTree a
-- } | Leaf a




naive_alg :: DwellAlgorithm a
naive_alg (etf) (point_data) =
	let point_matrix = points point_data in
	let dwell_matrix = (map.map) (compute_dwell etf etf) (point_matrix) in
	let dwell_data = DwellArray { daw = raw point_data, dah = rah point_data, dwells = dwell_matrix } in
	dwell_data



data QuadBounds =
	QuadBounds {
		qb_anch_top_l :: (Int, Int),
		qb_size_top_l :: (Int, Int),
		qb_anch_top_r :: (Int, Int),
		qb_size_top_r :: (Int, Int),
		qb_anch_bot_l :: (Int, Int),
		qb_size_bot_l :: (Int, Int),
		qb_anch_bot_r :: (Int, Int),
		qb_size_bot_r :: (Int, Int)
	}
	deriving (Show)

ms_get_subquad_bounds :: (Int, Int) -> (Int, Int) -> QuadBounds
ms_get_subquad_bounds (anch) (size) =
	-- computes, for each new quadrant, the corresponding top-left point,
	-- here called its "anchor", and the vector "size" which, from the anchor,
	-- points to the bottom-right point, such that topleft + size = botright
	let (w_size_half, w_size_parity) = divMod (fst size) (2) in
	let (h_size_half, h_size_parity) = divMod (snd size) (2) in

	let w_half        = w_size_half + w_size_parity in
	let h_half        = h_size_half + h_size_parity in
	let w_even_offset = 1 - w_size_parity in
	let h_even_offset = 1 - h_size_parity in
	let w_ini_anchor  = fst anch in
	let h_ini_anchor  = snd anch in
	let w_mid_anchor  = w_ini_anchor + w_size_half - w_even_offset in
	let h_mid_anchor  = h_ini_anchor + h_size_half - h_even_offset in

	let anch_top_l = ( w_ini_anchor           , h_ini_anchor           ) in
	let anch_top_r = ( w_mid_anchor           , h_ini_anchor           ) in
	let anch_bot_l = ( w_ini_anchor           , h_mid_anchor           ) in
	let anch_bot_r = ( w_mid_anchor           , h_mid_anchor           ) in
	let size_top_l = ( w_half                 , h_half                 ) in
	let size_top_r = ( w_half + w_even_offset , h_half                 ) in
	let size_bot_l = ( w_half                 , h_half + h_even_offset ) in
	let size_bot_r = ( w_half + w_even_offset , h_half + h_even_offset ) in
	let result = QuadBounds
		{
			qb_anch_top_l = anch_top_l,
			qb_anch_top_r = anch_top_r,
			qb_anch_bot_l = anch_bot_l,
			qb_anch_bot_r = anch_bot_r,
			qb_size_top_l = size_top_l,
			qb_size_top_r = size_top_r,
			qb_size_bot_l = size_bot_l,
			qb_size_bot_r = size_bot_r
		}
	in
	result

fill_iteration_data :: Dwell -> Int -> IterationData a -> IterationData a
fill_iteration_data (dwell) (depth) (iteration_data) =
	let result = IterationData {
		id_coord  = id_coord iteration_data,
		id_pos    = id_pos   iteration_data,
		id_values = Nothing,
		id_depth  = Just depth,
		id_calced = Just False,
		id_dwell  = Just dwell
	}
	in
	result

ms_quad :: forall a. (Show a) => DwellAlgorithm a
ms_quad (etf) (points_data) =
	let
		build_filter_points ::
			(Int -> Int -> Int -> Int -> Int -> Int -> Bool) ->
			[IterationData a] ->
			(Int, Int) ->
			(Int, Int) ->
			[IterationData a]
		build_filter_points (predicate) (point_array) (anch) (size) =
			let (l, t) = anch in
			let (w, h) = size in
			let (r, b) = (l + w, t + h) in
			let is_boundary (p) = let (x, y) = id_coord p in predicate x y t (b-1) l (r-1) in
			let result = filter (is_boundary) (point_array) in
			result
	in
	let filter_boundary_points = build_filter_points (\x y t b l r -> (y == t) || (x == l) || (x == r) || (y == b)) in
	let filter_interior_points = build_filter_points (\x y t b l r -> (y >  t) && (x >  l) && (x <  r) && (y <  b)) in
	let filter_enclosed_points = build_filter_points (\x y t b l r -> (y >= t) && (x >= l) && (x <= r) && (y <= b)) in
	let
		ms_quad_rec ::
			[IterationData a] ->
			Int ->
			(Int, Int) ->
			(Int, Int) ->
			[IterationData a]
		ms_quad_rec [] _ _ _ =
			[]
		ms_quad_rec
			(point_array)
			(depth)
			(anch)
			(size)
			=
			let boundary       = trace ("\n\nAnchor " ++ show anch ++ " | Size " ++ show size ++ " | Depth " ++ show depth) $ filter_boundary_points (point_array) (anch) (size) in  -- TODO a one-pass optimization of boundary and interior
			let interior       = filter_interior_points (point_array) (anch) (size) in
			let ms_figure      =
				trace ("Boundary " ++ show (map id_coord boundary)) $
				trace ("Interior " ++ show (map id_coord interior)) $
				fmap (compute_dwell etf etf) (boundary) in
			let fig_dwells     = fmap ((fromMaybe (-1)) . id_dwell) (ms_figure) in
			let dwell : dwells = fig_dwells in
			let should_fill    = trace ("dwell " ++ show dwell ++ " | dwells " ++ show dwells) $ all (== dwell) (dwells) in
			let result
				| fst size <= 2 || snd size <= 2 =
					trace ("end_ " ++ show (length boundary) ++ " + " ++ show (length interior) ++ " = " ++ show (length ms_figure)) $
					ms_figure
				| should_fill =
					let filled_interior = fmap (fill_iteration_data (dwell) (depth)) (interior) in
					trace ("fill " ++ show (length boundary) ++ " + " ++ show (length interior) ++ " = " ++ show (length (ms_figure ++ filled_interior))) $
					ms_figure ++ filled_interior
				| otherwise =
					let updated_pointarray = ms_figure ++ interior in
					let
						QuadBounds {
							qb_anch_top_l = anch_top_l,
							qb_size_top_l = size_top_l,
							qb_anch_top_r = anch_top_r,
							qb_size_top_r = size_top_r,
							qb_anch_bot_l = anch_bot_l,
							qb_size_bot_l = size_bot_l,
							qb_anch_bot_r = anch_bot_r,
							qb_size_bot_r = size_bot_r
						} = ms_get_subquad_bounds (anch) (size)
					in
					let
						compute_quadrant (array) (sub_anch) (sub_size) =
							let sub_array = filter_enclosed_points (array) (sub_anch) (sub_size) in  -- TODO a one-pass optimization of subdivision rather than 4
							ms_quad_rec (sub_array) (depth + 1) (sub_anch) (sub_size)
					in
					let array_top_l = compute_quadrant (updated_pointarray) (anch_top_l) (size_top_l) in
					let array_top_r = compute_quadrant (updated_pointarray) (anch_top_r) (size_top_r) in
					let array_bot_l = compute_quadrant (updated_pointarray) (anch_bot_l) (size_bot_l) in
					let array_bot_r = compute_quadrant (updated_pointarray) (anch_bot_r) (size_bot_r) in
					-- let combined_array = foldl (unionBy (\p q -> id_coord p == id_coord q)) [] [array_top_l, array_top_r, array_bot_l, array_bot_r] in
					let combined_array = concat [array_top_l, array_top_r, array_bot_l, array_bot_r] in
					trace ("quad " ++
						show (length boundary) ++ " + " ++
						show (length interior) ++ " = " ++
						show (length array_top_l) ++ " + " ++
						show (length array_top_r) ++ " + " ++
						show (length array_bot_l) ++ " + " ++
						show (length array_bot_r) ++ " = " ++
						show (length (combined_array))) $
					combined_array
			in
			result
	in
	let Ring2DArray {raw = w, rah = h, points = point_matrix} = points_data in
	let point_array   = concat (point_matrix) in
	let calced_matrix = ms_quad_rec (point_array) (0) (0, 0) (w, h) in
	let
		split_every _ [] = []
		split_every (n) (list) =
			let (hh, tt) = splitAt (n) (list) in
			hh : (split_every n tt)
	in
	let
		lexsort a b =
			let (xa, ya) = id_coord a in
			let (xb, yb) = id_coord b in
			let result
				| ya < yb   = LT
				| ya > yb   = GT
				| xa < xb   = LT
				| xa > xb   = GT
				| otherwise = EQ
			in
			result
	in
	let sorted_matrix = sortBy (lexsort) (calced_matrix) in
	--let nubbed_matrix = nubBy (\a b -> lexsort a b == EQ) (sorted_matrix) in
	let dwell_matrix  = split_every (w) (sorted_matrix) in -- (nubbed_matrix) in
	-- trace ("ms_result " ++ show dwell_matrix) $ 
	let dwell_data = DwellArray { daw = w, dah = h, dwells = dwell_matrix } in
	dwell_data

get_iter_algorithm :: forall a. (Show a) => DwellAlgorithmChoice -> DwellAlgorithm a
get_iter_algorithm (algo) =
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
