import qualified Data.HashMap.Lazy as HM
import Data.Hashable

data Robot
  = OreRob
  | ClayRob
  | ObsRob
  | GeodeRob
  deriving (Eq, Enum)
instance Hashable Robot where
  hashWithSalt s rob = s + fromEnum rob

data Material
  = Ore
  | Clay
  | Obsidian
  | Geode
  deriving (Eq, Enum)
instance Hashable Material where
  hashWithSalt s mat = s + fromEnum mat


data Blueprint = Blueprint
  { index :: Int,
    nOreOre :: Int,
    nOreClay :: Int,
    nOreObs :: Int,
    nClayObs :: Int,
    nOreGeode :: Int,
    nObsGeode :: Int
  } deriving Show

readBlueprint :: String -> Blueprint
readBlueprint xs =
  Blueprint
    { index = i,
      nOreOre = noo,
      nOreClay = noc,
      nOreObs = noob,
      nClayObs = ncob,
      nOreGeode = nog,
      nObsGeode = nobg
    }
  where
    ws = words xs
    i = read $ filter (/= ':') $ ws !! 1
    noo = read $ ws !! 6
    noc = read $ ws !! 12
    noob = read $ ws !! 18
    ncob = read $ ws !! 21
    nog = read $ ws !! 27
    nobg = read $ ws !! 30


type RobInv = HM.HashMap Robot Int
type MatInv = HM.HashMap Material Int
type State = (RobInv, MatInv)

update :: Material -> MatInv -> MatInv
update mat = HM.insertWith (+) mat 1

-- updatePosMap :: Coord -> PosMap -> PosMap
-- updatePosMap c = M.insertWith (+) c 1




    -- start with one OreRobot
exec :: Blueprint -> State
exec bp = undefined
  where
    tickMinute :: State -> State
    tickMinute (robOld, matOld) = undefined
      where
        collectOre = HM.lookup OreRob robOld
        collectClay = HM.lookup ClayRob robOld
        collectObs = HM.lookup ObsRob robOld
        collectGeode = HM.lookup GeodeRob robOld
        buildOreRob = (>= 4) <$> collectOre -- needs to be bp value
        buildClayRob = (>= 4) <$> collectOre
