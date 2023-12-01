{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



-- import Numeric.Algebra
import Graphics.Gloss

import Math
import ETF_Types
import ETF_Protocols
import ETF_Algorithms
import Render



{- Global config -}

g_r2algebra :: R2Algebra
g_r2algebra = R2A_Complex
type Geom   =     Complex

g_radius, g_radius_sqrd :: GFloat
g_geom_w, g_geom_h      :: GFloat
g_anchor, g_spread      :: Point2D
g_parameter             :: Point2D
g_geom_w      = 2.0
g_geom_h      = 2.0
g_anchor      = (0.0, 0.0)
g_spread      = (g_geom_w, g_geom_h) -- absolute spread in supremum absolute distance from the anchor
g_radius      = 2.0
g_radius_sqrd = g_radius * g_radius
g_parameter   = (2.0, 1.0)


g_protocol :: DwellProtocol
g_protocol = Mandelbrot

g_algorithm :: DwellAlgorithmChoice
g_algorithm = MarianiSilverQuad

g_max_dwell :: Dwell
g_max_dwell = 8

g_iter_poly :: Polynomial (Geom)
g_iter_poly = from_1ds ([0, 0, 2] :: [GFloat])

g_canv_w, g_canv_h :: Int
g_canv_w = 256
g_canv_h = 256

g_palette :: [Color]
g_palette =
	let sc = 1.0 / fromIntegral(g_max_dwell) in
	let conv x = fromIntegral(x) * sc in
	[
		let cx = conv(x) in makeColor (cx) (cx) (cx) (255)
		| x <- [ 0 .. g_max_dwell  ]
	]

g_zoom :: GFloat
g_zoom = 1.0



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
