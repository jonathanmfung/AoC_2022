
import Data.Graph

type Name = String
type Pressure = Int
type ValveState = (Bool, Pressure)

type Valve = (ValveState, Name, [Name])

readValve :: String -> Valve
readValve xs = ((False, flow), name, to)
  where
    ws = words xs
    name = ws !! 1
    flow :: Int
    flow = read $ filter (/= ';') $ drop 5 $ ws !! 4
    to = map (filter (/= ',')) $ drop 9 ws

data NetworkState
  = NetworkState
      [Name] -- Open Valves
      Pressure -- total Pressure

data Action
  = Open -- Opens current Valve
  | Move Name

transition :: NetworkState -> Action -> NetworkState
transition = undefined



exInp =
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n\
  \Valve BB has flow rate=13; tunnels lead to valves CC, AA\n\
  \Valve CC has flow rate=2; tunnels lead to valves DD, BB\n\
  \Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n\
  \Valve EE has flow rate=3; tunnels lead to valves FF, DD\n\
  \Valve FF has flow rate=0; tunnels lead to valves EE, GG\n\
  \Valve GG has flow rate=0; tunnels lead to valves FF, HH\n\
  \Valve HH has flow rate=22; tunnel leads to valve GG\n\
  \Valve II has flow rate=0; tunnels lead to valves AA, JJ\n\
  \Valve JJ has flow rate=21; tunnel leads to valve II"


(graph, nodeFromVertex, vertexFromKey) =
  graphFromEdges $ readValve <$> lines exInp
nodeFromKey k = nodeFromVertex <$> vertexFromKey k
