-- {-# LANGUAGE OverloadedRecordFields #-}

module Main(main) where

-- import Numeric.Algebra
import Graphics.Gloss
-- import Data.Colour.Palette

type GFloat = Float

g_width, g_height, g_offset :: Int
g_width  = 1024
g_height = 512
g_offset = 100

window :: Display
window = InWindow "Fractal Renderer" (g_width, g_height) (0, 0)

background :: Color
background = black

data PixelArray = PixelArray {
	w      :: Int,
	h      :: Int,
	pixels :: [[Color]]
}

type Palette = [Color]

class Ring a where
	(+.) :: a -> a -> a
	(-.) :: a -> a -> a
	(*.) :: a -> a -> a

data Pairwise = Pairwise { xx :: GFloat, yy :: GFloat }
data Complex  = Complex  { cr :: GFloat, ci :: GFloat }
data Dual     = Dual     { dr :: GFloat, de :: GFloat }
data Perplex  = Perplex  { pr :: GFloat, pi :: GFloat }
data Polar    = Polar    { md :: GFloat, th :: GFloat }

instance Ring Pairwise where
    (+.) a b = Pairwise {xx = (xx a) + (xx b), yy = (yy a) + (yy b)}
    (-.) a b = Pairwise {xx = (xx a) - (xx b), yy = (yy a) - (yy b)}
    (*.) a b = Pairwise {xx = (xx a) * (xx b), yy = (yy a) * (yy b)}

instance Ring Complex where
    (+.) a b = Complex {cr = (cr a) + (cr b), ci = (ci a) + (ci b)}
    (-.) a b = Complex {cr = (cr a) - (cr b), ci = (ci a) - (ci b)}
    (*.) a b =
		Complex {
			cr = (cr a) * (cr b) - (ci a) * (ci b),
			ci = (cr a) * (ci b) + (cr a) * (ci b)
		}




newtype Polynomial a = Polynomial [a]  deriving (Show, Eq)

-- instance Polynomial a => Polynomial (Ring a) where
--     (+.) (Ring x) (Ring ys) = Ring (xs + ys)
--     (-.) (Ring x) (Ring ys) = Ring (xs + ys)
--     (*.) (Ring x) (Ring ys) = Ring (xs + ys)





data IterationProtocol =
	Julia       |
	Mandelbrot  |
	Newton      |
	Burningship |
	Duquesne    |
	None
	deriving (Eq, Enum, Show)

data ETF a = ETF {
	protocol      :: IterationProtocol,
	compute_dwell :: DwellFunction a,
	zoom          :: GFloat,
	anchor        :: a,
	radius        :: GFloat,
	radius_sqrd   :: GFloat,
	-- is_static     :: Bool,
	palette       :: Palette,
	-- param         :: Ring,
	-- cur_coef      :: Int,
	iter_poly     :: Polynomial a
}

type DwellFunction a = ETF a -> a -> Int






picture_from_pixelarray :: PixelArray -> Picture
picture_from_pixelarray pixel_array =
	let vert_offset = fromIntegral(h pixel_array) / 2.0 in
	let horz_offset = fromIntegral(w pixel_array) / 2.0 in
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


{-
create_gradient :: Int -> Int -> Picture
create_gradient (width) (height) =
	let normalize_color (pos) (x) = fromIntegral (x) / fromIntegral (pos) in
	let normalize_w               = normalize_color (width)  in
	let normalize_h               = normalize_color (height) in
	let pixel_from_rg (r) (g) = makeColor (normalize_h (r)) (normalize_w (g)) (0) (255) in
	let build_greens  (r)     = [ pixel_from_rg (r) (g) | g <- [ 0 .. width  ] ] in
	let	color_matrix          = [ build_greens  (r)     | r <- [ 0 .. height ] ] in
	let pixel_array           = PixelArray { w = width, h = height, pixels = color_matrix } in
	let result                = picture_from_pixelarray (pixel_array) in
	result
-}

-- create_mandelbrot :: Int -> Int -> 

drawing :: Picture
drawing = create_gradient g_width g_height

main :: IO ()
main = display window background drawing
