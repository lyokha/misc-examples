-- Problem:  http://lpaste.net/2725173830396936192
-- Solution: http://lpaste.net/133871
--
-- Using Kruskal / union-find algorithms for unweighted graphs, prints a dot
-- file for GraphViz (requires GraphViz package)
--
-- Compile:
--
--   ghc --make deredundancify.hs
--
-- Create original image:
--
--   ./deredundancify -o | dot -Tpng > original.png
--
-- Create resulting spanning tree:
--
--   ./deredundancify | dot -Tpng > deredundancified.png
--
--  Use options -l to render edges without arrows and -t to render tiny graphs

{-# LANGUAGE TupleSections #-}

import            Control.Monad
import            Control.Arrow
import            Data.List                            (sort, group)
import qualified  Data.Map                             as M
import            Data.UnionFind.IO
import            Data.Maybe                           (fromJust)
import            Data.Graph.Inductive                 hiding (Edge)
import            Data.GraphViz
import            Data.GraphViz.Attributes.Complete
import            Data.GraphViz.Attributes.Colors.X11
import            Data.GraphViz.Printing               (renderDot)
import            Data.Colour                          (dissolve)
import            Data.Text.Lazy                       (Text, unpack, pack)
import            System.Environment                   (getArgs)

type Vertex = Int
type Edge   = (Int, Int)

theGraph :: [Edge]
theGraph = [(24,25),(25,449),(36,37),(37,46),(46,47),(47,51),(51,52),(52,54),
 (54,55),(55,56),(55,57),(56,60),(57,137),(60,61),(61,64),(64,508),
 (67,68),(68,71),(71,73),(73,57),(73,80),(75,71),(76,71),(77,71),
 (80,82),(82,83),(83,231),(84,57),(84,85),(85,87),(87,89),(89,304),
 (92,93),(93,96),(96,99),(99,100),(100,103),(103,105),(105,106),
 (106,450),(109,110),(110,114),(114,115),(115,116),(116,117),
 (117,118),(118,453),(119,120),(120,516),(121,122),(122,127),
 (127,458),(128,129),(129,132),(132,133),(133,136),(136,519),
 (137,142),(141,57),(144,142),(146,142),(148,149),(149,463),
 (151,148),(158,148),(159,148),(160,148),(161,148),(162,148),
 (165,148),(166,148),(167,148),(168,148),(170,148),(171,148),
 (172,148),(173,148),(174,148),(175,148),(176,148),(177,148),
 (178,148),(179,148),(180,148),(181,148),(182,148),(183,148),
 (184,148),(185,186),(186,191),(191,192),(192,198),(198,199),
 (199,464),(203,198),(205,206),(207,208),(208,212),(212,213),
 (213,504),(217,218),(217,57),(218,225),(222,217),(223,217),
 (224,217),(225,226),(226,230),(227,57),(231,232),(232,235),
 (235,239),(236,57),(237,232),(239,240),(240,243),(243,84),(304,305),
 (305,309),(309,313),(313,314),(314,319),(316,57),(317,313),(319,320),
 (320,326),(323,319),(324,319),(326,57),(326,327),(327,330),(327,57),
 (330,332),(332,333),(333,205),(335,336),(336,337),(337,340),(340,341),
 (341,343),(343,344),(344,466),(349,350),(352,353),(353,354),(354,355),
 (355,356),(356,357),(357,358),(358,359),(359,360),(360,361),(361,467),
 (363,364),(364,470),(376,377),(377,481),(391,392),(392,405),(405,406),
 (406,407),(407,408),(408,411),(414,415),(415,417),(417,418),(418,422),
 (422,423),(423,425),(425,426),(426,431),(431,432),(432,433),(436,437),
 (437,443),(443,461),(448,24),(449,36),(450,451),(451,509),(453,454),
 (454,455),(455,456),(456,457),(457,119),(458,459),(459,460),(460,128),
 (463,185),(466,349),(467,468),(468,363),(470,487),(481,391),(487,376),
 (498,411),(504,217),(508,67),(509,109),(513,514),(514,515),(515,118),
 (519,520),(520,436),(531,203),(531,445),(531,440),(531,205),(532,506),
 (532,52),(532,310),(532,123),(532,237),(532,79),(532,56),(532,62),
 (532,129),(532,86),(532,75),(532,461),(532,30),(532,132),(532,505),
 (532,47),(532,134),(532,135),(532,232),(532,236),(532,239),(532,243),
 (532,448),(532,25),(532,26),(532,499),(532,28),(532,500),(532,501),
 (532,29),(532,37),(532,38),(532,39),(532,41),(532,42),(532,51),(532,54),
 (532,55),(532,59),(532,71),(532,76),(532,77),(532,518),(532,124),(532,36),
 (532,443),(532,24),(532,31),(532,507),(532,43),(532,78),(532,242),(532,502),
 (532,73),(532,40),(532,60),(532,206),(532,305),(532,312),(532,311),(532,27),
 (532,74),(532,84),(532,449),(532,235),(533,80),(533,90),(533,49),(533,83),
 (533,68),(533,46),(533,238),(533,81),(533,33),(533,65),(533,61),(533,67),
 (533,58),(533,48),(533,50),(533,32),(533,85),(533,88),(533,91),(533,82),
 (533,240),(533,508),(533,89),(533,87),(533,64),(533,241),(533,318),(534,353),
 (534,414),(534,433),(534,341),(534,419),(534,427),(534,431),(534,356),(534,489),
 (534,404),(534,498),(534,352),(534,385),(534,399),(534,503),(534,467),(534,481),
 (534,361),(534,473),(534,482),(534,470),(534,491),(534,409),(534,335),(534,358),
 (534,363),(534,382),(534,125),(534,336),(534,338),(534,384),(534,376),(534,383),
 (534,342),(534,349),(534,354),(534,357),(534,428),(534,432),(534,344),(534,466),
 (534,401),(534,340),(534,343),(534,345),(534,351),(534,360),(534,397),(534,398),
 (534,418),(534,416),(534,413),(534,476),(534,377),(534,485),(534,421),(534,475),
 (534,483),(534,405),(534,408),(534,407),(534,400),(534,487),(534,402),(534,406),
 (534,387),(534,388),(534,390),(534,391),(534,339),(534,386),(534,415),(534,410),
 (534,423),(534,411),(534,426),(534,412),(534,417),(534,420),(534,422),(534,425),
 (534,429),(534,328),(534,337),(534,330),(534,350),(534,355),(534,359),(534,468),
 (534,471),(534,472),(534,474),(534,477),(534,479),(534,484),(534,488),(534,490),
 (534,392),(534,492),(534,493),(534,494),(534,495),(534,496),(535,325),(535,323),
 (535,321),(535,315),(535,308),(535,304),(535,306),(535,313),(535,460),(535,314),
 (535,319),(535,228),(535,230),(535,225),(535,307),(535,229),(535,322),(535,324),
 (535,227),(536,326),(536,221),(536,231),(536,316),(536,327),(537,226),(537,211),
 (537,210),(537,222),(537,224),(537,529),(537,309),(537,208),(537,530),(537,219),
 (537,209),(537,212),(537,217),(537,223),(537,504),(537,320),(537,213),(537,218),
 (537,333),(537,220),(538,128),(538,130),(538,131),(539,167),(539,207),(539,174),
 (539,157),(539,159),(539,160),(539,162),(539,57),(539,138),(539,142),(539,143),
 (539,146),(539,195),(539,194),(539,459),(539,184),(539,185),(539,137),(539,200),
 (539,201),(539,180),(539,182),(539,175),(539,178),(539,179),(539,140),(539,144),
 (539,198),(539,187),(539,149),(539,192),(539,199),(539,202),(539,181),(539,193),
 (539,463),(539,186),(539,447),(539,164),(539,158),(539,150),(539,147),(539,173),
 (539,148),(539,329),(539,161),(539,166),(539,151),(539,141),(539,191),(539,171),
 (539,172),(539,176),(539,145),(539,165),(539,170),(539,177),(539,106),(539,183),
 (539,331),(539,168),(539,439),(539,462),(539,317),(541,516),(541,332),(541,437),
 (541,513),(541,455),(541,464),(541,107),(541,109),(541,438),(541,527),(541,116),
 (541,334),(541,436),(541,450),(541,136),(541,453),(541,446),(541,451),(541,105),
 (541,115),(541,456),(541,454),(541,110),(541,524),(541,525),(541,526),(541,528),
 (541,118),(541,458),(541,127),(541,457),(541,517),(541,515),(541,120),(541,509),
 (541,514),(541,111),(541,112),(541,114),(541,117),(541,119),(541,121),(541,452),
 (541,441),(541,444),(541,122),(541,133),(542,93),(542,97),(542,99),(542,101),
 (542,523),(542,520),(542,94),(542,521),(542,100),(542,96),(542,104),(542,92),
 (542,103),(542,519),(542,434)]

deredundancify :: [Edge] -> IO ([Vertex], [Edge])
deredundancify es = do
    let vs = map head . group . sort $ foldr (\(a, b) c -> a : b : c) [] es
    m  <- liftM M.fromList $ mapM (runKleisli $ arr id &&& Kleisli fresh) vs
    let testNotCycle e = do
            let ve = both (fromJust . flip M.lookup m) e
            notCycle <- uncurry (liftM2 (/=)) $ both descriptor ve
            when notCycle $ uncurry union ve
            return notCycle
            where both = join (***)
    rs <- filterM testNotCycle es
    return (vs, rs)

mkDot :: [Vertex] -> [Edge] -> [GlobalAttributes] -> DotGraph Node
mkDot vs es attrs =
    let lvs = map (id &&& pack . show) vs
        les = map (uncurry (,, ())) es
        gr  = mkGraph lvs les :: Gr Text ()
    in graphToDot nonClusteredParams {globalAttributes = attrs} gr

main = do
    args     <- getArgs
    (vs, rs) <- deredundancify theGraph
    let originalGraph   = "-o" `elem` args
        tinyGraph       = "-t" `elem` args
        noArrows        = "-l" `elem` args
        withOpacity     = ((toWC .) fromAColour .) . flip dissolve . x11Colour
        ecolor          = [Blue `withOpacity` 0.3]
        ncolor          = [Red  `withOpacity` 0.5]
        eattrs
            | tinyGraph = [edgeEnds NoDir, Color ecolor]
            | otherwise = [edgeEnds $ if noArrows then NoDir else Forward]
        nattrs
            | tinyGraph = [Shape PointShape, Width 0.04, Color ncolor]
            | otherwise = []
        gattrs
            | tinyGraph = [Size $ GSize 10 Nothing True]
            | otherwise = []
        attrs = [EdgeAttrs eattrs, NodeAttrs nattrs, GraphAttrs gattrs]
        dot   = mkDot vs (if originalGraph then theGraph else rs) attrs
    putStrLn $ unpack $ renderDot $ toDot dot

