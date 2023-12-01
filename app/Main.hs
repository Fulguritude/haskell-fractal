{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



-- import Numeric.Algebra
import Graphics.Gloss

import Math
import ETF_Types
import ETF_Protocols
import ETF_Algorithms
import Render
import Config

main :: IO ()
main =
	let
		render_params :: RenderParams
		render_params = RenderParams {
			window_dims = (g_canv_w, g_canv_h),
			zoom        = g_zoom,
			palette     = g_palette,
			show_axes   = True,
			ms_calcs    = True,
			hover_paths = True,
			is_static   = True
		}
	in
	let
		etf :: ETF Geom
		etf = ETF {
			r2algebra      = g_r2algebra,
			protocol       = g_protocol,
			algorithm      = g_algorithm,
			max_dwell      = g_max_dwell,
			compute_dwell  = get_dwell_function (g_protocol),
			compute_dwells = get_iter_algorithm (g_algorithm),
			has_escaped    = build_has_escaped (g_radius_sqrd) (g_r2algebra),
			anchor         = from_2d (g_anchor),
			spread         = from_2d (g_spread),
			parameter      = from_2d (g_parameter),
			radius         = g_radius,
			radius_sqrd    = g_radius_sqrd,
			iter_poly      = g_iter_poly
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
