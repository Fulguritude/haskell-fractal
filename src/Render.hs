{-# LANGUAGE ScopedTypeVariables #-}

module Render (module Render) where


-- import Debug.Trace
import Graphics.Gloss
import Data.Maybe
-- import Data.Colour.Palette

import Math
import ETF_Types
import GeomCoordUtils


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



map_colors_naive :: Palette -> DwellArray a -> PixelArray
map_colors_naive (palette_array) (dwell_array) =
	let extract_dwell x = fromMaybe 0 (id_dwell x) in
	let dwell_matrix    = (map.map) (extract_dwell) (dwells dwell_array) in
	let pixel_matrix    = (map.map) (palette_array !!) (dwell_matrix) in
	let pixel_array     = PixelArray { paw = daw dwell_array, pah = dah dwell_array, pixels = pixel_matrix } in
	pixel_array


map_colors_ms :: Dwell -> Palette -> DwellArray a -> PixelArray
map_colors_ms (max_dwell) (palette_array) (dwell_array) =
	-- todo : alternative palette for squares based on depth ?
	let extract_dwell x = if fromMaybe (False) (id_calced x) then max_dwell - 1 else fromMaybe 0 (id_dwell x) in
	let dwell_matrix    = (map.map) (extract_dwell) (dwells dwell_array) in
	let pixel_matrix    = (map.map) (palette_array !!) (dwell_matrix) in
	let pixel_array     = PixelArray { paw = daw dwell_array, pah = dah dwell_array, pixels = pixel_matrix } in
	pixel_array



render_fractal :: (Ring a, Convert a) => RenderParams -> ETF a -> Picture
render_fractal (params) (etf) =
	let dims        = get_dwell_algorithm_dims (etf) (params) in
	let dwell_array = (compute_dwells etf etf dims) in
	let map_colors  = if algorithm etf == Naive then map_colors_naive else map_colors_ms (max_dwell etf) in
	let pixel_array = map_colors (palette params) (dwell_array) in
	let result      = picture_of_pixelarray (pixel_array) in
	result
