{-# LANGUAGE ScopedTypeVariables #-}

module ETF_Algorithms (module ETF_Algorithms) where



import Data.Maybe
import Data.List ( sortBy ) --, unionBy )  --, nubBy )
-- import Data.HashMap.Strict (fromList, (!))

import Math
import ETF_Types
import GeomCoordUtils



naive_alg :: (Convert a) => DwellAlgorithm a
naive_alg (etf) (dims) =
	let points_data  = get_all_points (dims) in
	let point_matrix = points points_data in
	let dwell_matrix = (map.map) (compute_dwell etf etf) (point_matrix) in
	let dwell_data = DwellArray { daw = raw points_data, dah = rah points_data, dwells = dwell_matrix } in
	dwell_data



-- beginning, middle, end
data QuadBounds =
	QuadBounds {
		qb_xb :: Int,
		qb_xm :: Int,
		qb_xe :: Int,
		qb_yb :: Int,
		qb_ym :: Int,
		qb_ye :: Int,
		qb_sl :: Int,
		qb_sr :: Int,
		qb_st :: Int,
		qb_sb :: Int
	}
	deriving (Show)

ms_get_subquad_bounds :: Coord2D -> Coord2D -> QuadBounds
ms_get_subquad_bounds (anch) (size) =
	-- computes relevant axes coordinates for horz and vert,
	-- beginning, middle, end; and sizes for x left, x right,
	-- y top, y bottom
	let (w_size_half, w_size_parity) = divMod (fst size) (2) in
	let (h_size_half, h_size_parity) = divMod (snd size) (2) in
{-
	let w_half        = w_size_half + w_size_parity in
	let h_half        = h_size_half + h_size_parity in
	let w_even_offset = 1 - w_size_parity in
	let h_even_offset = 1 - h_size_parity in
	let w_ini_anchor  = fst anch in
	let h_ini_anchor  = snd anch in
	let w_mid_anchor  = w_ini_anchor + w_size_half - w_even_offset in
	let h_mid_anchor  = h_ini_anchor + h_size_half - h_even_offset in
	let w_half_offset = w_half + w_even_offset in
	let h_half_offset = h_half + h_even_offset in

	let anch_top_l = ( w_ini_anchor  , h_ini_anchor  ) in
	let anch_top_r = ( w_mid_anchor  , h_ini_anchor  ) in
	let anch_bot_l = ( w_ini_anchor  , h_mid_anchor  ) in
	let anch_bot_r = ( w_mid_anchor  , h_mid_anchor  ) in
	let size_top_l = ( w_half        , h_half        ) in
	let size_top_r = ( w_half_offset , h_half        ) in
	let size_bot_l = ( w_half        , h_half_offset ) in
	let size_bot_r = ( w_half_offset , h_half_offset ) in
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
-}
	let w_half        = w_size_half + w_size_parity in
	let h_half        = h_size_half + h_size_parity in
	let w_even_offset = 1 - w_size_parity in
	let h_even_offset = 1 - h_size_parity in

	let w_ini_anchor = fst anch in
	let h_ini_anchor = snd anch in
	let w_mid_anchor = w_ini_anchor + w_size_half - w_even_offset in
	let h_mid_anchor = h_ini_anchor + h_size_half - h_even_offset in
	let w_half_offset = w_half + w_even_offset in
	let h_half_offset = h_half + h_even_offset in
	let w_end_anchor = w_mid_anchor + w_half_offset - 1 in
	let h_end_anchor = h_mid_anchor + h_half_offset - 1 in
	let result =
		QuadBounds {
			qb_xb = w_ini_anchor,
			qb_yb = h_ini_anchor,
			qb_xm = w_mid_anchor,
			qb_ym = h_mid_anchor,
			qb_xe = w_end_anchor,
			qb_ye = h_end_anchor,
			qb_sl = w_half,
			qb_sr = w_half_offset,
			qb_st = h_half,
			qb_sb = h_half_offset
		}
	in
	result


{-
	Labelling system:
	- start from center point (c)
	- 1 go up left
	- 2 go up right
	- 3 go down left
	- 4 go down right
	- u go up
	- d go down
	- l go left
	- r go right

	11    1u  uu   2u   22
		.______.______.
		|      |      |
	1l  |  1   u   2  | 2r
		|      |      |
		.__l___c___r__. rr
		|      |      |
	3l  |  3   d   4  | 4r
		|      |      |
		.______.______.
	33    3d   dd  4d   44
-}

data CalcPointDivision a =
	CalcPointDivision {
		-- bd_boundary :: [a],
		bd_point_11 ::  a,
		bd_point_22 ::  a,
		bd_point_33 ::  a,
		bd_point_44 ::  a,
		bd_point_uu ::  a,
		bd_point_dd ::  a,
		bd_point_ll ::  a,
		bd_point_rr ::  a,
		bd_line__1u :: [a],
		bd_line__2u :: [a],
		bd_line__3d :: [a],
		bd_line__4d :: [a],
		bd_line__1l :: [a],
		bd_line__2r :: [a],
		bd_line__3l :: [a],
		bd_line__4r :: [a],
		-- bd_interior :: [a],
		bd_point_cc ::  a,
		bd_line__cu :: [a],
		bd_line__cd :: [a],
		bd_line__cl :: [a],
		bd_line__cr :: [a]
	}
	deriving (Show)

type CheckCoords      = CalcPointDivision Coord2D
type UncalcedPoints a = CalcPointDivision ({- Coord2D, -} IterationData a)
type CalcedPoints   a = CalcPointDivision ({- Coord2D, -} IterationData a)

get_points_to_check ::
	QuadBounds ->
	CheckCoords
get_points_to_check (quad_bounds) =
	let 
		QuadBounds {
			qb_xb = x_ini,
			qb_yb = y_ini,
			qb_xm = x_mid,
			qb_ym = y_mid,
			qb_xe = x_end,
			qb_ye = y_end
		} = quad_bounds
	in
	let
		point_11 = (x_ini, y_ini)
		point_22 = (x_end, y_ini)
		point_33 = (x_end, y_ini)
		point_44 = (x_end, y_end)
	
		point_uu = (x_mid, y_ini)
		point_dd = (x_mid, y_end)
		point_ll = (x_ini, y_mid)
		point_rr = (x_end, y_mid)

		point_cc = (x_mid, y_mid)
	in
	let
		line__1u = [(x     , y_ini ) | x <- [x_ini + 1 .. x_mid - 1]]
		line__2u = [(x     , y_ini ) | x <- [x_mid + 1 .. x_end - 1]]
		line__3d = [(x     , y_end ) | x <- [x_ini + 1 .. x_mid - 1]]
		line__4d = [(x     , y_end ) | x <- [x_mid + 1 .. x_end - 1]]
		line__1l = [(x_ini , y     ) | y <- [y_ini + 1 .. y_mid - 1]]
		line__3l = [(x_ini , y     ) | y <- [y_mid + 1 .. y_end - 1]]
		line__2r = [(x_end , y     ) | y <- [y_ini + 1 .. y_mid - 1]]
		line__4r = [(x_end , y     ) | y <- [y_mid + 1 .. y_end - 1]]

		line__cu = [( x_mid , y     ) | y <- [y_ini + 1 .. y_mid - 1]]
		line__cd = [( x_mid , y     ) | y <- [y_mid + 1 .. y_end - 1]]
		line__cl = [( x     , y_mid ) | x <- [x_ini + 1 .. x_mid - 1]]
		line__cr = [( x     , y_mid ) | x <- [x_mid + 1 .. x_end - 1]]
	in
{-
	let
		boundary =
			concat
			[
				[point_11],
				[point_22],
				[point_33],
				[point_44],
				[point_uu],
				[point_dd],
				[point_ll],
				[point_rr],
				line__1u,
				line__2u,
				line__3d,
				line__4d,
				line__1l,
				line__2r,
				line__3l,
				line__4r
			]
	in
	let
		interior =
			concat
			[
				[point_cc],
				line__cu,
				line__cd,
				line__cl,
				line__cr
			]
	in
-}
	let
		result =
			CalcPointDivision
			{
				--bd_boundary = boundary,
				bd_point_11 = point_11,
				bd_point_22 = point_22,
				bd_point_33 = point_33,
				bd_point_44 = point_44,
				bd_point_uu = point_uu,
				bd_point_dd = point_dd,
				bd_point_ll = point_ll,
				bd_point_rr = point_rr,
				bd_line__1u = line__1u,
				bd_line__2u = line__2u,
				bd_line__3d = line__3d,
				bd_line__4d = line__4d,
				bd_line__1l = line__1l,
				bd_line__2r = line__2r,
				bd_line__3l = line__3l,
				bd_line__4r = line__4r,
				--bd_interior = interior,
				bd_point_cc = point_cc,
				bd_line__cu = line__cu,
				bd_line__cd = line__cd,
				bd_line__cl = line__cl,
				bd_line__cr = line__cr
			}
	in
	result

{-
extract_sublist_from_keys :: IDCollection a -> [Coord2D] -> [(Coord2D, IterationData a)]  -- IDCollection a
extract_sublist_from_keys (points_data) (keys) =
	[ (p, points_data ! p) | p <- keys ] --fromList
-}
uncalcedpoints_of_checkcoords :: (Coord2D -> IterationData a) -> CheckCoords -> UncalcedPoints a
uncalcedpoints_of_checkcoords (get_geompoint) (check_coords) =
	let get_geompoints = map get_geompoint in
	let result =
		CalcPointDivision {
			bd_point_11 = get_geompoint (bd_point_11 check_coords),
			bd_point_22 = get_geompoint (bd_point_22 check_coords),
			bd_point_33 = get_geompoint (bd_point_33 check_coords),
			bd_point_44 = get_geompoint (bd_point_44 check_coords),
			bd_point_uu = get_geompoint (bd_point_uu check_coords),
			bd_point_dd = get_geompoint (bd_point_dd check_coords),
			bd_point_ll = get_geompoint (bd_point_ll check_coords),
			bd_point_rr = get_geompoint (bd_point_rr check_coords),
			bd_line__1u = get_geompoints (bd_line__1u check_coords),
			bd_line__2u = get_geompoints (bd_line__2u check_coords),
			bd_line__3d = get_geompoints (bd_line__3d check_coords),
			bd_line__4d = get_geompoints (bd_line__4d check_coords),
			bd_line__1l = get_geompoints (bd_line__1l check_coords),
			bd_line__2r = get_geompoints (bd_line__2r check_coords),
			bd_line__3l = get_geompoints (bd_line__3l check_coords),
			bd_line__4r = get_geompoints (bd_line__4r check_coords),
			-- bd_interior = get_geompoint (bd_interior check_coords),
			bd_point_cc = get_geompoint (bd_point_cc check_coords),
			bd_line__cu = get_geompoints (bd_line__cu check_coords),
			bd_line__cd = get_geompoints (bd_line__cd check_coords),
			bd_line__cl = get_geompoints (bd_line__cl check_coords),
			bd_line__cr = get_geompoints (bd_line__cr check_coords)
		}
	in
	result

calcedpoints_of_uncalcedpoints :: (IterationData a -> IterationData a) -> UncalcedPoints a -> CalcedPoints a
calcedpoints_of_uncalcedpoints (dwell_function) (uncalced_points) =
	-- let dwellfunc_pair (p, iter_data) = (p, dwell_function iter_data) in
	-- let map_dwellfunc_pair            = fmap (dwellfunc_pair) in
	let result =
		CalcPointDivision {
			bd_point_11 = dwell_function (bd_point_11 uncalced_points),
			bd_point_22 = dwell_function (bd_point_22 uncalced_points),
			bd_point_33 = dwell_function (bd_point_33 uncalced_points),
			bd_point_44 = dwell_function (bd_point_44 uncalced_points),
			bd_point_uu = dwell_function (bd_point_uu uncalced_points),
			bd_point_dd = dwell_function (bd_point_dd uncalced_points),
			bd_point_ll = dwell_function (bd_point_ll uncalced_points),
			bd_point_rr = dwell_function (bd_point_rr uncalced_points),
			bd_line__1u = map (dwell_function) (bd_line__1u uncalced_points),
			bd_line__2u = map (dwell_function) (bd_line__2u uncalced_points),
			bd_line__3d = map (dwell_function) (bd_line__3d uncalced_points),
			bd_line__4d = map (dwell_function) (bd_line__4d uncalced_points),
			bd_line__1l = map (dwell_function) (bd_line__1l uncalced_points),
			bd_line__2r = map (dwell_function) (bd_line__2r uncalced_points),
			bd_line__3l = map (dwell_function) (bd_line__3l uncalced_points),
			bd_line__4r = map (dwell_function) (bd_line__4r uncalced_points),
			bd_point_cc = dwell_function (bd_point_cc uncalced_points),
			bd_line__cu = map (dwell_function) (bd_line__cu uncalced_points),
			bd_line__cd = map (dwell_function) (bd_line__cd uncalced_points),
			bd_line__cl = map (dwell_function) (bd_line__cl uncalced_points),
			bd_line__cr = map (dwell_function) (bd_line__cr uncalced_points)
		}
	in
	result


{-
split_boundary_and_interior :: IDCollection a -> [Coord2D] -> [Coord2D] -> (IDCollection a, IDCollection a)
split_boundary_and_interior (points_data) (boundary_points) (interior_points) =
	-- is partition or partitionWithKey with some anch/size predicate more performant here ?
	let boundary = fromList [ (p, points_data ! p) | p <- boundary_points ] in
	let interior = fromList [ (p, points_data ! p) | p <- interior_points ] in
	(boundary, interior)
-}


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

ms_quad :: forall a. (Convert a) => DwellAlgorithm a
ms_quad (etf) (dims) =
	let dwell_function = compute_dwell etf etf in
	let get_geompoint  = build_get_geompoint (dims) in
	let
		check_boundary :: IDCollection a -> (Dwell, Bool)
		check_boundary (boundary) =
			let fig_dwells = fmap ((fromMaybe (-1)) . id_dwell) (boundary) in
			case fig_dwells of
				[] ->
					(-1, False)
				dwell : dwells ->
					let should_fill = all (== dwell) (dwells) in
					(dwell, should_fill)
	in
	let
		ms_quad_rec ::
			IDCollection a ->
			Int ->
			Coord2D ->
			Coord2D ->
			IDCollection a
		ms_quad_rec
			(boundary)
			(depth)
			(anch)
			(size)
			=
			let result =
				if fst size <= 2 || snd size <= 2 then
					[]
				else
					let (dwell, should_fill) = check_boundary (boundary) in
					if should_fill then
						let interior_anch = (fst anch + 1, snd anch + 1) in
						let interior_size = (fst size - 2, snd size - 2) in
						let interior = get_all_geompoints_in_block (get_geompoint) (interior_anch) (interior_size) in
						map (fill_iteration_data (dwell) (depth)) (interior)
					else
						let quad_bounds     = ms_get_subquad_bounds (anch) (size) in
						let check_coords    = get_points_to_check (quad_bounds) in
						let uncalced_points = uncalcedpoints_of_checkcoords (get_geompoint) (check_coords) in
						let calced_points   = calcedpoints_of_uncalcedpoints (dwell_function) (uncalced_points) in
						let
							CalcPointDivision {
								bd_point_11 = p11,
								bd_point_22 = p22,
								bd_point_33 = p33,
								bd_point_44 = p44,
								bd_point_uu = puu,
								bd_point_dd = pdd,
								bd_point_ll = pll,
								bd_point_rr = prr,
								bd_line__1u = l1u,
								bd_line__2u = l2u,
								bd_line__3d = l3d,
								bd_line__4d = l4d,
								bd_line__1l = l1l,
								bd_line__2r = l2r,
								bd_line__3l = l3l,
								bd_line__4r = l4r,
								bd_point_cc = pcc,
								bd_line__cu = lcu,
								bd_line__cd = lcd,
								bd_line__cl = lcl,
								bd_line__cr = lcr
							} = calced_points
						in
						let bd_tl1 = concat [[p11, puu, pll, pcc], lcu, lcl, l1u, l1l] in
						let bd_tr2 = concat [[p22, puu, prr, pcc], lcu, lcr, l2u, l2r] in
						let bd_bl3 = concat [[p33, pdd, pll, pcc], lcd, lcl, l3d, l3l] in
						let bd_br4 = concat [[p44, pdd, prr, pcc], lcd, lcr, l4d, l4r] in

						let
							QuadBounds {
								qb_xb = xb, qb_yb = yb,
								qb_xm = xm, qb_ym = ym,
								qb_xe =  _, qb_ye =  _,
								qb_sl = sl, qb_st = st,
								qb_sr = sr, qb_sb = sb
							} = quad_bounds
						in
						let anch1 = (xb, yb) ; size1 = (sl, st) in
						let anch2 = (xm, yb) ; size2 = (sr, st) in
						let anch3 = (xb, ym) ; size3 = (sl, sb) in
						let anch4 = (xm, ym) ; size4 = (sr, sb) in
						let interior_tl1 = ms_quad_rec (bd_tl1) (depth + 1) (anch1) (size1) in
						let interior_tr2 = ms_quad_rec (bd_tr2) (depth + 1) (anch2) (size2) in
						let interior_bl3 = ms_quad_rec (bd_bl3) (depth + 1) (anch3) (size3) in
						let interior_br4 = ms_quad_rec (bd_br4) (depth + 1) (anch4) (size4) in
						let result_interior = concat
							[
								[pcc],
								lcd, lcl, lcr, lcu,
								interior_tl1,
								interior_tr2,
								interior_bl3,
								interior_br4
							]
						in
						if depth > 0 then
							result_interior
						else
							let result_boundary = concat
								[
									[p11, p22, p33, p44, puu, pdd, pll, prr],
									l1l, l1u, l2r, l2u, l3d, l3l, l4d, l4r
								]
							in
							result_interior ++ result_boundary
			in
			result
	in
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

	let (w, h)        = (intoIntegral2D . snd . dad_rend_range) (dims) in
	let calced_matrix = ms_quad_rec ([]) (0) (0, 0) (w, h) in
	let sorted_matrix = sortBy (lexsort) (calced_matrix) in
	--let nubbed_matrix = nubBy (\a b -> lexsort a b == EQ) (sorted_matrix) in
	let dwell_matrix  = split_every (w) (sorted_matrix) in -- (nubbed_matrix) in
	-- trace ("ms_result " ++ show dwell_matrix) $ 
	let dwell_data = DwellArray { daw = w, dah = h, dwells = dwell_matrix } in
	dwell_data

get_iter_algorithm :: forall a. (Show a, Convert a) => DwellAlgorithmChoice -> DwellAlgorithm a
get_iter_algorithm (algo) =
	let
		ms_point :: DwellAlgorithm a
		ms_point (etf) (point_array) =
			let dwell_array = DwellArray { daw = 0, dah = 0, dwells =  [] } in
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
