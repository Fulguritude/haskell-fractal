{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where


-- import Numeric.Algebra
import Graphics.Gloss

import Math
import ETF
import Render


main :: IO ()
main =
	let sc = 1.0 / fromIntegral(g_max_dwell) in
	let conv x = fromIntegral(x) * sc in
	let
		render_params :: RenderParams
		render_params = RenderParams {
			window_dims = (g_canv_w, g_canv_h),
			zoom        = 1.0,
			palette     = [
				makeColor (conv(x)) (conv(x)) (conv(x)) (255)
				| x <- [ 0 .. g_max_dwell  ]
			],
			is_static   = True
		}
	in
	let
		etf :: ETF Geom
		etf = ETF {
			protocol       = g_protocol,
			algorithm      = g_algorithm,
			compute_dwell  = get_dwell_function (g_protocol),
			compute_dwells = get_iter_algorithm (g_algorithm),
			anchor         = from_2d (g_anchor),
			spread         = from_2d (g_spread),
			radius         = 2.0,
			radius_sqrd    = 4.0,
			iter_poly      = from_1ds [0.0, 0.0, 2.0]
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
