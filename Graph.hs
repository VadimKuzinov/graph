import Data.Maybe (fromJust)
import Data.List (find)

data Node a = Node {label :: a, adjacent_list :: [Node a]} deriving (Eq, Show, Read)
data Graph a = Graph {node_list :: [Node a]} deriving (Eq, Show, Read)

makeGraph :: (Eq a) => [(a, [a])] -> Graph a
makeGraph links = Graph $ map snd nodeLookupList where
    makeNode (lbl, adj) = (lbl, Node lbl $ map lookupNode adj)
    nodeLookupList = map makeNode links
    lookupNode lbl = fromJust $ lookup lbl nodeLookupList

walk graph node_label func = do
    case find (\node -> label node == node_label) $ node_list graph of
        Just node -> func node
        Nothing   -> []

dfs node = [label node] ++ (concat $ map dfs $ adjacent_list node)
bfs node = [label node] ++ (map label $ adjacent_list node) ++ (concat $ map bfs $ adjacent_list node)

main :: IO ()
main = do
    let graph = makeGraph [('a', ['b', 'c', 'd', 'e']), ('b', ['d']), ('c', ['d', 'e']), ('d', ['e']), ('e', [])]

    print $ walk graph 'a' dfs
    print $ walk graph 'a' bfs