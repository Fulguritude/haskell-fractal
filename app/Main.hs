module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width  = 1024
height = 512
offset = 100

-- bob :: String = 3

window :: Display
window = InWindow "Fractal Renderer" (width, height) (0, 0)

background :: Color
background = black

data PixelArray = PixelArray {
	w      :: Int,
	h      :: Int,
	pixels :: [[Color]]
}

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


drawing :: Picture
drawing = create_gradient width height

main :: IO ()
main = display window background drawing
