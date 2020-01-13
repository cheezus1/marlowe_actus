module SimplexSolve(solveLEInt) where

import Data.List (genericIndex, transpose, genericTake, genericDrop, genericLength, nub, sort, find)

addSlack :: [[Rational]] -> Integer -> Integer -> [[Rational]]
addSlack [] _ _ = []
addSlack (h:t) x y = (hi ++ [if a == x then 1 else 0 | a <- [0..y]] ++ [he]):(addSlack t (x + 1) y)
  where
    hi = init h
    he = last h

addAux :: [[Rational]] -> [Bool] -> Integer -> Integer -> [[Rational]]
addAux [] _ _ _ = []
addAux (h:t) (c:r) n l = (hi ++ [if (n == a && c) then 1 else 0 | a <- [0..l]] ++ [he]):(addAux t r (n + incre) l)
  where
    incre = if c then 1 else 0
    hi = init h
    he = last h
addAux _ _ _ _ = error "Index shorter than rows in addAux"

bigM :: [[Rational]] -> [[Rational]]
bigM m = obj:fm
  where
   em = addSlack m 0 (numEqs - 1)
   im = map (\(x, y) -> if y then map (0 -) x else x) $ zip em auxIdx
   nm = addAux im auxIdx 0 (numAux - 1)
   fm = [0:x | x <- nm]
   obj = ((0:(genericTake (((genericLength $ head m) - 1) :: Integer) (repeat 0))) ++
             (genericTake (numEqs) (repeat 0)) ++
             (genericTake (numAux) (repeat (-1))) ++ [0])
   auxIdx = map ((< 0) . last) $ m
   numAux = genericLength $ filter id $ auxIdx
   numEqs = genericLength m

chooseColumnAux :: Maybe (Integer, Rational) -> [(Integer, Rational)] -> Maybe (Integer)
chooseColumnAux Nothing [] = Nothing
chooseColumnAux (Just (x,_)) [] = Just x
chooseColumnAux Nothing (n@(_, v):t)
 | v > 0 = chooseColumnAux (Just n) t
 | otherwise = chooseColumnAux Nothing t
chooseColumnAux c@(Just (_, cv)) (n@(_, nv):t)
 | (nv > cv) = chooseColumnAux (Just n) t
 | otherwise = chooseColumnAux c t

computeZj :: [[Rational]] -> [Rational] -> [Rational]
computeZj tm ce = map (\(x, y) -> sum $ zipWith (*) x y) $ zip tm (repeat ce)

addSameFst :: [(Integer, Rational)] -> [(Integer, Rational)]
addSameFst [] = []
addSameFst [a] = [a]
addSameFst (f@(l1,r1):(st@((l2,r2):t)))
 | l1 == l2 = addSameFst ((l1, r1 + r2):t)
 | otherwise = f:(addSameFst st)

computeCe :: [[Rational]] -> [Rational] -> [Rational]
computeCe tm cj = ce
  where
        (_, ce) = unzip $ addSameFst $ sort $ concat $ [case onlyOneOne l of
                                                          Nothing -> []
                                                          Just x -> [(x, cj `genericIndex` p)]
                                                        | (p, l) <- zip [(0 :: Integer)..] tm]


nullifyArtif :: [Rational] -> [Rational] -> [Rational]
nullifyArtif h l = zipWith (\x y -> if x /= 0 then -1 else y) (tail h) l

nullifyBases :: [[Rational]] -> [Rational] -> [Rational]
nullifyBases tm l = [case x of
                       Just _ -> -1
                       _ -> y
                     | (x, y) <- (zip (map onlyOneOne tm) l)]

chooseColumn :: [[Rational]] -> Maybe Integer
chooseColumn (h:t) = chooseColumnAux Nothing $ zip [1..] (init $ nullifyArtif h $ nullifyBases tm $ zipWith (-) cj zj)
  where
        (_:tm) = init $ transpose t
        cj = tail $ init $ h 
        ce = computeCe tm cj
        zj = computeZj tm ce
chooseColumn [] = error "Empty list in chooseColumn"

chooseRowAux :: Integer -> [Integer] -> [(Integer, [Rational])] -> [[Rational]]
                  -> Maybe ([[Rational]], [Rational], [[Rational]], Rational)
                  -> Maybe ([[Rational]], [Rational], [[Rational]])
chooseRowAux _ _ [] _ Nothing = Nothing 
chooseRowAux _ _ [] _ (Just (b, m, a, _)) = Just (a, m, reverse b)
chooseRowAux pc as ((_,h):t) acc Nothing
  | hh > 0 =
        chooseRowAux pc as t [] (Just (reverse acc, h, [], nv))
  | otherwise = chooseRowAux pc as t (h:acc) Nothing
  where nv = (last h) / hh
        hh = h `genericIndex` pc
chooseRowAux pc as ((c,h):t) [] (Just (b, m, a, v))
  | hh > 0 && ((nv < v) || ((nv == v) && (elem c as))) =
        chooseRowAux pc as t [] (Just ((b ++ (m:(reverse a))), h, [], nv))
  | otherwise =
        chooseRowAux pc as t [] (Just (b, m, (h:a), v))
  where nv = (last h) / hh 
        hh = h `genericIndex` pc
chooseRowAux _ _ _ _ _ = error "Accumulator not empty in chooseRowaux"

chooseRow :: Integer -> [Integer] -> [(Integer, [Rational])] -> Maybe ([[Rational]], [Rational], [[Rational]])
chooseRow pc as l = chooseRowAux pc as l [] Nothing

removeArtificial :: (Integer, Integer) -> [(Integer, Integer)] -> [[Rational]] -> [[Rational]]
removeArtificial (_, c) es m =
  case (find (\(_,x) -> x == c) es) of
    Just (ec, _) -> transpose ((genericTake ec tm) ++ (genericDrop (ec + 1) tm))
    Nothing -> m
  where tm = transpose m

solveSimplex :: [[Rational]] -> [[Rational]]
solveSimplex [] = error "No equations in solveSimplex"
solveSimplex m@(h:t) =
  case pivotColumnPair of
    Nothing -> m
    Just pc ->
       case chooseRow pc as (reverse (zip [0..] t)) of
         Just (prb, pr, pra) -> let normRow = map (/ (pr `genericIndex` pc)) pr in
                                let fixRow a = (let coef = (a `genericIndex` pc) in
                                                zipWith (\x y -> x - (y * coef)) a normRow) in
                                let nm = h:((map fixRow prb) ++ (normRow:(map fixRow pra))) in
                                solveSimplex (removeArtificial (pc,(genericLength prb)) es nm)
         Nothing -> (h:t)
  where pivotColumnPair = chooseColumn m 
        (_, as) = unzip es
        es = [(let Just a = y in (c, a))
              | (c, x, y) <- (zip3 [0..] h $ map onlyOneOne $ transpose t), x /= 0, y /= Nothing]

onlyOneOne :: [Rational] -> Maybe Integer
onlyOneOne l =
  case cleanList of
    [(p, x)] -> if x == 1 then Just p else Nothing
    _ -> Nothing
  where
    idxl = zip [0..] l
    cleanList = filter ((/= 0) . snd) idxl

calculateSols :: [[Rational]] -> [Rational]
calculateSols m = [case onlyOneOne l of
                     Just x -> last (m `genericIndex` (x + 1))
                     Nothing -> 0
                   | (_:l) <- transpose m]

checkSolutionLE :: Eq b => ([[Rational]], [a], [b]) -> [(b, Rational)] -> Bool
checkSolutionLE (m, _, c) s = all (\y -> (sum $ (zipWith (*) c3) $ init y) <= last y) m
   where c3 = [(case lookup e s of
                 Just x -> x
                 Nothing -> 0) | e <- c]

unBigM :: Eq b => [r] -> [b] -> [[Rational]] -> [[Rational]] -> Maybe [(b, Rational)]
unBigM r c m om
  | checkSolutionLE (om, r, c) esols = Just esols
  | otherwise = Nothing
  where
    x = (genericLength c) :: Integer
    (_:sols) = calculateSols m 
    esols = init $ zip c $ genericTake x sols


solveLE :: Eq b => ([[Rational]], [a], [b]) -> Maybe [(b, Rational)]
solveLE (_, [], c) = Just $ zip (init c) $ repeat 0
solveLE (_, _, []) = Just []
solveLE (m, r, c)
 | anyAux = unBigM r c sol m
 | otherwise = Just $ zip (init c) $ repeat 0
  where sol = solveSimplex $ bigM m
        auxIdx = map ((< 0) . last) $ m
        anyAux = any id $ auxIdx

splitSol :: [(Either b b, Rational)] -> [(b, Rational)]
splitSol [] = []
splitSol [_] = error "Single result in splitSol"
splitSol ((Left a, x):(Right _, y):t) = (a, x - y):(splitSol t)
splitSol ((Left _, _):(Left _, _):_) = error "Two Left in a row in splitSol"
splitSol ((Right _, _):_:_) = error "Right in odd position in splitSol"

solveLEBS :: Eq b => ([[Rational]], [a], [b]) -> Maybe [(b, Rational)]
solveLEBS (m, r, c) = case res of
                        Just sol -> Just $ splitSol sol
                        Nothing -> Nothing
  where res = solveLE fm 
        fm = ([init $ concat [[x, -x] | x <- l] | l <- m],
               r, init $ concat [[Left x, Right x] | x <- c])

nonFractional :: Rational -> Bool
nonFractional d = (toRational $ (ceiling d :: Integer)) == d

splitOrConvertSolutionAux :: [(b, Rational)] -> [(b, Integer)] -> Either (b, Rational) [(b, Integer)]
splitOrConvertSolutionAux [] l = Right l
splitOrConvertSolutionAux ((b,v):t) l
 | nonFractional v = splitOrConvertSolutionAux t ((b, (ceiling v)):l)
 | otherwise = Left (b, v)

splitOrConvertSolution :: [(b, Rational)] -> Either (b, Rational) [(b, Integer)]
splitOrConvertSolution s = splitOrConvertSolutionAux s []

genericElemIndex :: Eq a => a -> [a] -> Integer
genericElemIndex _ [] = error "Element not in list!"
genericElemIndex e (h:t)
 | e == h = 0
 | otherwise = 1 + (genericElemIndex e t)

addIE :: Integer -> Rational -> Rational -> Integer -> [Rational]
addIE p v n w = [if (x == w) then n else (if x == p then v else 0) | x <- [0..w]]

addLE :: Integer -> Rational -> Integer -> [Rational]
addLE p n w = addIE p 1 n w

addGE :: Integer -> Rational -> Integer -> [Rational]
addGE p n w = addIE p (-1) (-n) w

splitOrConvert :: Eq b => ([[Rational]], [Either Integer a], [b]) -> Maybe [(b, Rational)] -> Maybe (Either [([[Rational]], [Either Integer a], [b])] [(b, Integer)])
splitOrConvert (m, r, c) msol =
  case msol of
    Just sol ->
      case splitOrConvertSolution sol of
        Left (b, n) -> let t = toRational $ ((ceiling n) :: Integer) in
                       let f = toRational $ ((floor n) :: Integer) in
                       let p = genericElemIndex b c in
                       let w = genericLength r in
                         Just $ Left [(((addLE p f w):m), nel:r, c), (((addGE p t w):m), nel:r, c)]
        Right nsol -> Just $ Right nsol
    Nothing -> Nothing
  where nel = case r of
                [] -> Left 0
                ((Right _):_) -> Left 0
                ((Left a):_) -> Left (a + 1)

solveLEIntAux :: Eq a => Eq b => [([[Rational]], [Either Integer a], [b])] -> Maybe [(b, Integer)]
solveLEIntAux [] = Nothing
solveLEIntAux (h:t) =
  case splitOrConvert h rSol of
    Just (Left nh) -> solveLEIntAux (nub (t ++ nh))
    Just (Right s) -> Just s
    Nothing -> solveLEIntAux t
  where
   rSol = solveLEBS h

solveLEInt :: Eq a => Eq b => ([[Rational]], [a], [b]) -> Maybe [(b, Integer)]
solveLEInt (m, a, b) = solveLEIntAux [(m, map Right a, b)]

