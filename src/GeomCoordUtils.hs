module GeomCoordUtils (module GeomCoordUtils) where

import Math

import ETF_Types



fromIntegral2D :: Coord2D -> Point2D
fromIntegral2D (x, y) = (fromIntegral x, fromIntegral y)
intoIntegral2D :: Point2D -> Coord2D
intoIntegral2D (x, y) = (floor x, floor y)

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
	Range ->
	Range ->
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

get_geompoint_of_rendercoord ::
	(Convert a) =>
	RendRange ->
	GeomRange ->
	Coord2D ->
	IterationData a
get_geompoint_of_rendercoord (old_range) (new_range) (coordinates) =
	let old_point = fromIntegral2D (coordinates) in
	let new_point = transform_affine_2d (old_range) (new_range) (old_point) in
	let result = IterationData {
		id_coord  = coordinates,
		id_pos    = from_2d new_point,
		id_values = Nothing,
		id_depth  = Nothing,
		id_calced = Nothing,
		id_dwell  = Nothing
	}
	in
	result

build_get_geompoint :: 
	(Convert a) =>
	DwellAlgorithmDims ->
	(Coord2D -> IterationData a)
build_get_geompoint (dims) =
	get_geompoint_of_rendercoord (dad_rend_range dims) (dad_geom_range dims)

get_all_geompoints_in_block :: (Coord2D -> IterationData a) -> Coord2D -> Coord2D -> [IterationData a]
get_all_geompoints_in_block (get_geompoint) (anch) (size) =
	let (anch_x, anch_y) = anch in
	let (size_w, size_h) = size in
	let build_horz (y) = [ get_geompoint (x, y) | x <- [ anch_x .. anch_x + size_w - 1 ] ] in
	let	point_matrix   = [ build_horz       (y) | y <- [ anch_y .. anch_y + size_h - 1 ] ] in
	let result = concat point_matrix in
	result


get_all_points ::
	(Convert a) =>
	DwellAlgorithmDims ->
	Ring2DArray (IterationData a)
get_all_points (dims) =
	let DwellAlgorithmDims {
		-- dad_rend_size  = rend_dims,
		dad_rend_range = old_range,
		dad_geom_range = new_range
	} = dims in
	let (anch_x, anch_y) = intoIntegral2D (fst old_range) in
	let (size_w, size_h) = intoIntegral2D
		(
			((fst . snd) old_range - (fst . fst) old_range),
			((snd . snd) old_range - (snd . fst) old_range)
		)
	in
	let get_geompoint  = get_geompoint_of_rendercoord (old_range) (new_range) in
	let build_horz (y) = [ get_geompoint (x, y) | x <- [ anch_x .. anch_x + size_w - 1 ] ] in
	let	point_matrix   = [ build_horz       (y) | y <- [ anch_y .. anch_y + size_h - 1 ] ] in
	let result         = Ring2DArray { raw = size_w, rah = size_h, points = point_matrix } in
	result



get_wind_dims :: RenderParams -> (Coord2D, GeomRange)
get_wind_dims (params) =
	let dims  = window_dims (params) in
	let wind_topleft_ = (0.0, 0.0) in
	let wind_topright = fromIntegral2D (dims) in
	let wind_range = (wind_topleft_, wind_topright) in
	(dims, wind_range)

get_geom_range :: (Ring a, Convert a) => ETF a -> GeomRange
get_geom_range (etf) =
	let geom_anchor   = anchor (etf) in
	let geom_spread   = spread (etf) in
	let geom_topleft_ = into_2d (geom_anchor -. geom_spread) in
	let geom_topright = into_2d (geom_anchor +. geom_spread) in
	let geom_range    = (geom_topleft_, geom_topright) in
	geom_range

get_dwell_algorithm_dims :: (Ring a, Convert a) => ETF a -> RenderParams -> DwellAlgorithmDims
get_dwell_algorithm_dims (etf) (params) =
	let (dims, wind_range) = get_wind_dims (params) in
	let geom_range = get_geom_range (etf) in
	let result =
		DwellAlgorithmDims
		{
			-- dad_rend_size  = dims,
			dad_rend_range = wind_range,
			dad_geom_range = geom_range 
		}
	in
	result
