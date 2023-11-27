{-# LANGUAGE ScopedTypeVariables #-}

module Render (module Render) where

-- import Debug.Trace
import Graphics.Gloss
import Data.Maybe
-- import Data.Colour.Palette

import ETF
import Math




{- Render declarations -}

type Palette = [Color]

data RenderParams = RenderParams {
	window_dims :: (Int, Int),
	zoom        :: GFloat,
	palette     :: Palette,
	show_axes   :: Bool,
	ms_calcs    :: Bool,
	hover_paths :: Bool,
	is_static   :: Bool
}

data PixelArray = PixelArray {
	paw    :: Int,
	pah    :: Int,
	pixels :: [[Color]]
}

picture_of_pixelarray :: PixelArray -> Picture
picture_of_pixelarray pixel_array =
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
intoIntegral2D :: Point2D -> (Int, Int)
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

get_geompoint_of_windowcoord ::
	(Convert a) =>
	(Point2D, Point2D) ->
	(Point2D, Point2D) ->
	(Int, Int) ->
	IterationData a
get_geompoint_of_windowcoord (old_range) (new_range) (coordinates) =
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



get_all_points ::
	(Convert a) =>
	(Int, Int) ->
	(Point2D, Point2D) ->
	(Point2D, Point2D) ->
	Ring2DArray (IterationData a)
get_all_points (wind_dims) (old_range) (new_range) =
	let (win_w, win_h) = wind_dims in
	let get_geompoint  = get_geompoint_of_windowcoord (old_range) (new_range) in
	let build_horz (y) = [ get_geompoint (x, y) | x <- [ 0 .. win_w - 1 ] ] in
	let	point_matrix   = [ build_horz       (y) | y <- [ 0 .. win_h - 1 ] ] in
	let result         = Ring2DArray { raw = win_w, rah = win_h, points = point_matrix } in
	result



-- source, MIT license: https://bottosson.github.io/posts/oklab/

type RGB = Color
type LAB = Color

oklab_of_srgb :: RGB -> LAB 
oklab_of_srgb (c) =
	let (r, g, b, a) = rgbaOfColor c in
	let l = 0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b in
	let m = 0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b in
	let s = 0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b in

	let l_ = cbrt(l) in
	let m_ = cbrt(m) in
	let s_ = cbrt(s) in

	let result =
		makeColor
			(0.2104542553 * l_ + 0.7936177850 * m_ - 0.0040720468 * s_)
			(1.9779984951 * l_ - 2.4285922050 * m_ + 0.4505937099 * s_)
			(0.0259040371 * l_ + 0.7827717662 * m_ - 0.8086757660 * s_)
			(a)
	in
	result

srgb_of_oklab :: LAB -> RGB
srgb_of_oklab (c) =
	let (l, m, s, a) = rgbaOfColor c in

	let r_ = l + 0.3963377774 * m + 0.2158037573 * s in
	let g_ = l - 0.1055613458 * m - 0.0638541728 * s in
	let b_ = l - 0.0894841775 * m - 1.2914855480 * s in

	let r = r_ * r_ * r_ in
	let g = g_ * g_ * g_ in
	let b = b_ * b_ * b_ in

	let result =
		makeColor
			( 4.0767416621 * r - 3.3077115913 * g + 0.2309699292 * b)
			(-1.2684380046 * r + 2.6097574011 * g - 0.3413193965 * b)
			(-0.0041960863 * r - 0.7034186147 * g + 1.7076147010 * b)
			(a)
	in
	result


map_colors :: Palette -> DwellArray a -> PixelArray
map_colors (palette_array) (dwell_array) =
	let extract_dwell x = fromMaybe 0 (id_dwell x) in
	let dwell_matrix    = (map.map) (extract_dwell) (dwells dwell_array) in
	let pixel_matrix    = (map.map) (palette_array !!) (dwell_matrix) in
	let pixel_array     = PixelArray { paw = daw dwell_array, pah = dah dwell_array, pixels = pixel_matrix } in
	pixel_array



render_fractal :: (Ring a, Convert a) => RenderParams -> ETF a -> Picture
render_fractal (params) (etf) =
	let dims  = window_dims (params) in
	let win_w = fst dims in
	let win_h = snd dims in
	let wind_topleft_ = (0.0, 0.0) in
	let wind_topright = fromIntegral2D (win_w, win_h) in
	let wind_range = (wind_topleft_, wind_topright) in

	let geom_anchor   = anchor (etf) in
	let geom_spread   = spread (etf) in
	let geom_topleft_ = into_2d (geom_anchor -. geom_spread) in
	let geom_topright = into_2d (geom_anchor +. geom_spread) in
	let geom_range    = (geom_topleft_, geom_topright) in

	let point_array = get_all_points (dims) (wind_range) (geom_range) in
	let dwell_array = (compute_dwells etf etf) (point_array) in
	let pixel_array = map_colors (palette params) (dwell_array) in
	let result      = picture_of_pixelarray (pixel_array) in
	result
