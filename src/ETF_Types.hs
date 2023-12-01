module ETF_Types (module ETF_Types) where


import Graphics.Gloss (Color)

import Math
-- import Data.HashMap.Strict (HashMap)  -- https://mmhaskell.com/data-structures/hash-map




{- Global config -}

type Dwell = Int   -- Word
type Range = (Point2D, Point2D)
type RendRange = Range  -- (Coord2D, Coord2D)
type GeomRange = Range




{- Espace-time fractal declarations -}

data DwellProtocol =
	Julia        |
	Mandelbrot   |
	PseudoNewton |
	Burningship  |
	Duquesne
	deriving (Eq, Enum, Show)

data DwellAlgorithmChoice =
	Naive              |
	MarianiSilverQuad  |
	MarianiSilverPoint
	--	({-points_per_rank-} Int)
	--	({-depth-} Int)
	deriving (Eq, Enum, Show)

data ETF a = ETF {
	r2algebra      :: R2Algebra,
	protocol       :: DwellProtocol,
	algorithm      :: DwellAlgorithmChoice,
	max_dwell      :: Dwell,
	compute_dwell  :: DwellFunction a,
	compute_dwells :: DwellAlgorithm a,
	has_escaped    :: GFloat -> Bool,
	anchor         :: a,
	spread         :: a,
	parameter      :: a,
	radius         :: GFloat,
	radius_sqrd    :: GFloat,
	iter_poly      :: Polynomial a
}

data IterationData a = IterationData {
	id_coord  :: Coord2D,
	id_pos    :: a,
	id_values :: Maybe [a],
	id_depth  :: Maybe Int,
	id_calced :: Maybe Bool,
	id_dwell  :: Maybe Dwell
} deriving (Show)


data Ring2DArray a = Ring2DArray {
	raw    :: Int,
	rah    :: Int,
	points :: [[a]]
}

data DwellArray a = DwellArray {
	daw    :: Int,
	dah    :: Int,
	dwells :: [[IterationData a]]
}

data DwellAlgorithmDims = DwellAlgorithmDims {
	-- dad_rend_size   :: Coord2D,
	dad_rend_range  :: RendRange,
	dad_geom_range  :: GeomRange
}

-- TODO add constraint
type DwellFunction  a = ETF a -> (IterationData a) -> (IterationData a)
type DwellAlgorithm a = ETF a -> DwellAlgorithmDims -> DwellArray a





-- data QuadTree a = Branch {
-- 	qt_tl :: QuadTree a,
-- 	qt_tr :: QuadTree a,
-- 	qt_bl :: QuadTree a,
-- 	qt_br :: QuadTree a
-- } | Leaf a


type IDCollection a = [IterationData a]
-- type IDCollection a = HashMap Coord2D (IterationData a)





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
