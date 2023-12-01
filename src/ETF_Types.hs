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
	r2algebra      :: R2Algebra,            -- ^ choice of an R²-algebra A, such as complex numbers
	protocol       :: DwellProtocol,        -- ^ choice of an iteration protocol (a way of using a polynomial to make a recurrent numeric sequence N -> A) 
	algorithm      :: DwellAlgorithmChoice, -- ^ choice of a rendering method, typically calculate all points (naive) or Mariani-Silver
	max_dwell      :: Dwell,                -- ^ number of iterations after which a point is considered to have converged
	compute_dwell  :: DwellFunction a,      -- ^ function that is the consequence of the choice in the `protocol` enum
	compute_dwells :: DwellAlgorithm a,     -- ^ function that is the consequence of the choice in the `algorithm` enum
	has_escaped    :: GFloat -> Bool,       -- ^ predicate to test whether a given (pseudo)norm corresponds to divergence
	anchor         :: a,                    -- ^ corresponding center of the render window in the A algebra
	spread         :: a,                    -- ^ spread around the anchor in terms of the supremum norm for respective coordinates
	parameter      :: a,                    -- ^ parameter for (pseudo)newton protocol
	radius         :: GFloat,               -- ^ radius of divergence
	radius_sqrd    :: GFloat,               -- ^ radius of divergence squared
	iter_poly      :: Polynomial a          -- ^ polynomial that is used as the basis of the recurrent numeric sequence
}

data IterationData a = IterationData {
	id_coord  :: Coord2D,    -- ^ pixel coordinates of a point in the render window
	id_pos    :: a,          -- ^ point coordinates in the R²-algebra A
	id_values :: Maybe [a],  -- ^ values of the sequence N -> A calculated for this point
	id_depth  :: Maybe Int,  -- ^ depth of the Mariani-Silver algorithm at which this point was found
	id_calced :: Maybe Bool, -- ^ whether this point's dwell was calculated fully, or inferred via the Mariani-Silver algorithm
	id_dwell  :: Maybe Dwell -- ^ the dwell value for the given point 
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




type IDCollection a = [IterationData a]
-- type IDCollection a = HashMap Coord2D (IterationData a)





{- Render declarations -}

type Palette = [Color]

data RenderParams = RenderParams {
	window_dims :: Coord2D, -- ^ window width and height
	zoom        :: GFloat,  -- ^ level of zoom required for the given render (scales inversely with ETF.spread)
	palette     :: Palette, -- ^ an array of colors for the render
	show_axes   :: Bool,    -- ^ whether to display the i and j axes of the cartesian plan in which the R²-algebra A is represented
	ms_calcs    :: Bool,    -- ^ whether to show figures calculated by Mariani-Silver
	hover_paths :: Bool,    -- ^ whether to show the path that a point takes when evaluated, via a series of bresenham lines
	is_static   :: Bool     -- ^ whether the movement interactivity is disabled, in order to allow for keys to affect other parameters
}

data PixelArray = PixelArray {
	paw    :: Int,
	pah    :: Int,
	pixels :: [[Color]]
}
