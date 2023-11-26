{-# LANGUAGE ScopedTypeVariables #-}

module Render where

-- import Debug.Trace
import Graphics.Gloss
-- import Data.Colour.Palette

import ETF
import Math




{- Render declarations -}

type Palette = [Color]

data RenderParams = RenderParams {
	window_dims   :: (Int, Int),
	zoom          :: GFloat,
	palette       :: Palette,
	is_static     :: Bool
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
	(Convert a) =>
	(Point2D, Point2D) ->
	(Point2D, Point2D) ->
	(Int, Int) ->
	a
get_geompoint_from_windowcoord (old_range) (new_range) (coordinates) =
	let point  = fromIntegral2D (coordinates) in
	let result = transform_affine_2d (old_range) (new_range) (point) in
	from_2d result

get_all_points ::
	(Convert a) =>
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
map_colors (palette_array) (dwell_array) =
	let dwell_matrix = dwells dwell_array in
	let pixel_matrix = (map.map) (palette_array !!) (dwell_matrix) in
	let pixel_array = PixelArray { paw = daw dwell_array, pah = dah dwell_array, pixels = pixel_matrix } in
	pixel_array

render_fractal :: (Ring a, Convert a) => RenderParams -> ETF a -> Picture
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
