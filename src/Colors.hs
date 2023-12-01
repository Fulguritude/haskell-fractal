
module Colors (module Colors) where

import Graphics.Gloss

import Math (cbrt)


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