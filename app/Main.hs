module Main where
import Control.Applicative
import System.Environment
import System.Exit
import System.IO
type Parser a = String -> [(a, String)]

parseDigit :: Parser String
parseDigit [] = []
parseDigit (x:xs)
    | (x >= '0' && x <= '9') = [([x], xs)]
    | otherwise = []

parseChar :: Char -> Parser Char
parseChar _ av@[] = [('0', av)]
parseChar char av@(x:xs)
    | (char == x) = [(x, xs)]
    | otherwise = []

parseAnyChar :: String -> Parser Char
parseAnyChar [] _ = []
parseAnyChar (char:str) all@(c:cs)
    | (char == c) = [(c, cs)]
    | otherwise = parseAnyChar str all

parseOr :: (Eq a) => Parser a -> Parser a -> Parser a
parseOr pa pb av 
    | (pa av) /= [] = (pa av)
    | otherwise = (pb av)

andWith :: (Eq a, Eq b) => (a -> b -> c) -> Parser a -> Parser b -> Parser c
andWith lmb pa pb av
    | _pb == [] = []
    | otherwise = [(lmb (fst $ head _pa) (fst $ head _pb), snd $ head _pb)]
    where _pa = (pa av)
          _pb = (pb (snd $ head _pa))

parseAndWith :: (Eq a, Eq b) => (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith lmb pa pb av
    | _pa == [] = []
    | otherwise = andWith lmb pa pb av
    where _pa = (pa av)

parseMany :: (Eq a) => Parser a -> Parser [a]
parseMany _ av@[] = [([], av)]
parseMany pa av@(x:xs)
    | c /=  [] = [ ( (fst $ head c):(fst $ head n), snd $ head n ) ]
    | otherwise = [([], av)]
    where c = pa av
          n = parseMany pa xs

parseSome :: (Eq a) => Parser a -> Parser [a]
parseSome _ [] = []
parseSome pa av@(x:xs)
    | c /=  [] = [ ( (fst $ head c):(fst $ head n), snd $ head n ) ]
    | otherwise = []
    where c = pa av
          n = parseMany pa xs

parseInt :: Parser Float
parseInt exprt@[] = []
parseInt exprt@(_:xs)
    | signe /= [] = [((read $ fst $ head other :: Float) * (-1), snd $ head other)]
    | (res == []) = []
    | otherwise = [(read $ fst $ head res :: Float, snd $ head res)]
    where res = parseSome (parseAnyChar ('.':['0'..'9'])) exprt
          other = parseSome (parseAnyChar ('.':['0'..'9'])) xs
          signe = (parseChar '-' exprt)

doExConditionTwo :: [(String, String)] -> [(Char, String)] -> [(Float, String)]
doExConditionTwo res exp2 = [(read exprt :: Float, (snd $ head rest2))]
    where rest2 = parseSome (parseAnyChar (['0'..'9'])) (snd $ head exp2)
          exprt = (fst $ head res) ++ [(fst $ head exp2)] ++ (fst $ head rest2)

exConditionTwo :: [(String, String)] -> [(Float, String)]
exConditionTwo res
    | exp2 == [] = []
    | otherwise = doExConditionTwo res exp2
    where exp2 = (parseChar 'e' (snd $ head res))

doexConditionOn :: [(String, String)] -> [(String, String)] -> [(Float, String)]
doexConditionOn res exp = [(read exprt :: Float, (snd $ head rest2))]
    where rest2 = parseSome (parseAnyChar (['0'..'9'])) (snd $ head exp)
          exprt = (fst $ head res) ++ (fst $ head exp) ++ (fst $ head rest2)

exConditionOn :: [(String, String)] -> [(Float, String)]
exConditionOn res
    | (exp == []) || (exp == [("00","")]) = exConditionTwo res
    | otherwise = doexConditionOn res exp
    where exp = parseAndWith (\ x y -> [x , y ]) (parseChar 'e') (parseChar '-') (snd $ head res)

parseEx :: Parser Float
parseEx exprt@[] = []
parseEx exprt@(_:xs)
    | (res == []) = []
    | (snd $ head res) == [] = [( read $ fst $ head res :: Float , [])]
    | otherwise = exConditionOn res
    where res = parseSome (parseAnyChar ('.':['0'..'9'])) exprt

doFactor :: [(Char, String)] -> [(Float, String)]
doFactor open = expr $ merge [((fst $ head rest),(snd $ head close))]
    where rest = expr (snd $ head open)
          close = parseChar ')' (snd $ head rest)

factor :: Parser Float -- | (parseAndWith (\ x y -> [x , y ]) (parseChar '(') (parseChar ')') exprt) /= [] = [(0, exprt)]
factor exprt
    | (open /= []) && (fst $ head open) == '(' = doFactor open
    | nb == [] = [(0, exprt)]
    | otherwise = nb 
    where open = parseChar '(' exprt
          nb = parseOr (parseEx) (parseInt) exprt

doExpon :: [(Float, String)] -> [(Char, String)] -> [(Float, String)]
doExpon fctor op = [( (fst $ head one)*res , (snd $ head one))]
    where fctor2 = expon (snd $ head op)
          res = if ((fst $ head fctor) < 0) then -1 else 1
          one = expon $ merge $ [( ((fst $ head fctor)**(fst $ head $ fctor2)), (snd $ head $ fctor2) )]

expon :: Parser Float
expon expert
    | op == [] = fctor
    | (fctor /= []) && (fst $ head op)  == '^' = doExpon fctor op
    | otherwise = factor expert
    where fctor = factor expert
          op = parseChar '^' (snd $ head fctor)

mul :: [(Float, String)] -> [(Char, String)] -> [(Float, String)]
mul expont op = term $ merge $ [( (fst $ head expont) * (fst $ head $ fctor2), (snd $ head $ fctor2) )]
    where fctor2 = expon (snd $ head op)

divi :: [(Float, String)] -> [(Char, String)] -> [(Float, String)]
divi expont op = term $ merge $ [( (fst $ head expont) / (fst $ head $ fctor2), (snd $ head $ fctor2) )]
    where fctor2 = expon (snd $ head op)

term :: Parser Float
term exprt
    | op == [] = expont
    | (expont /= []) && (fst $ head op)  == '*' = mul expont op
    | (expont /= []) &&  (fst $ head op) == '/' = divi expont op
    | otherwise = expon exprt
    where expont = expon exprt
          op = parseOr (parseChar '/') (parseChar '*') (snd $ head expont)

add :: [(Float, String)] -> [(Char, String)] -> [(Float, String)]
add trm op = expr $ merge $ [( (fst $ head trm) + (fst $ head $ fctor2), (snd $ head $ fctor2) )]
    where fctor2 = expr (snd $ head op)

sub :: [(Float, String)] -> [(Char, String)] -> [(Float, String)]
sub trm op = expr $ merge $ [( (fst $ head trm) - (fst $ head $ fctor2), (snd $ head $ fctor2) )]
    where fctor2 = expr (snd $ head op)

expr :: Parser Float
expr exprt
    | op == [] = trm
    | (trm /= []) && (fst $ head op)  == '+' = add trm op
    | (trm /= []) &&  (fst $ head op) == '-' = sub trm op
    | otherwise = term exprt
    where trm = term exprt
          op = parseOr (parseChar '+') (parseChar '-') (snd $ head trm)

addZero :: String -> Float -> String
addZero expret 0 = expret
addZero expret nb = addZero ("0" ++ expret) (nb - 1)

setNegativePower :: Int -> String -> String
setNegativePower _ [] = []
setNegativePower nb (x:xs)
    | nb == 0 = [x] ++ "." ++ (setNegativePower (nb + 1) xs)
    | x == '.' = (setNegativePower nb xs)
    | otherwise = [x] ++ (setNegativePower nb xs)

deleteComma :: String -> String
deleteComma [] = []
deleteComma (x:xs)
    | x == '.' = deleteComma xs
    | otherwise = x:deleteComma xs

setPositivePower :: String -> Float -> String
setPositivePower exprt 0 = exprt
setPositivePower [] nb = '0':setPositivePower [] (nb - 1)
setPositivePower (x:xs) nb = x:setPositivePower xs (nb - 1)

doConvertPower :: [(Float, String)] -> String
doConvertPower nb
    | signe == [] = setPositivePower (deleteComma $ show $ fst $ head nb) ((read (show $ fst $ head power) :: Float) + 1)
    | otherwise = setNegativePower 0 $ addZero (show $ fst $ head nb) (read (show $ fst $ head $ parseInt $ snd $ head signe) :: Float)
    where expont = parseChar 'e' (snd $ head nb)
          signe = parseChar '-' (snd $ head expont)
          power = parseInt (snd $ head expont)

convertPower :: String -> String
convertPower expert
    | (nb == []) || (snd $ head nb) == [] = expert
    | otherwise = doConvertPower nb
    where nb = parseInt expert

merge :: [(Float, String)] -> String
merge (x:xs) = (show $ fst x) ++ (snd x)  

cleanSpace :: String -> String
cleanSpace [] = []
cleanSpace (x:xs)
    | x == ' ' = cleanSpace xs
    | otherwise = x:(cleanSpace xs)

cleanSpaceWithSingleSpace :: String -> Int -> String
cleanSpaceWithSingleSpace [] _ = []
cleanSpaceWithSingleSpace (x:xs) nb
    | x == ' ' && nb == 0 = cleanSpaceWithSingleSpace xs 0
    | x == ' ' && nb == 1 = ' ':cleanSpaceWithSingleSpace xs 0
    | x == '-' || x == '+' = x:(cleanSpaceWithSingleSpace xs 0)
    | otherwise = x:(cleanSpaceWithSingleSpace xs 1)

errorHandling :: [String] -> IO ()
errorHandling av
    | (av == []) = exitWith (ExitFailure 84)
    | (head av) == [] = exitWith (ExitFailure 84)
    | otherwise = return ()

handlingResult :: String -> IO ()
handlingResult all 
    | all /= [] = exitWith (ExitFailure 84)
    | otherwise = return ()

noNumber :: String -> IO ()
noNumber [] = exitWith (ExitFailure 84)
noNumber (x:xs)
    | x >= '0' && x <= '9' = return ()
    | otherwise = noNumber xs

numberWithoutOp :: String -> Int -> IO ()
numberWithoutOp [] spaceNb
    | spaceNb > 0 = exitWith (ExitFailure 84)
    | otherwise = return ()
numberWithoutOp (x:xs) spaceNb
    | x == '-' || x == '+' || x == '/' || x == '*' || x == '^' = return ()
    | x == ' ' = numberWithoutOp xs (spaceNb + 1)
    | otherwise = numberWithoutOp xs spaceNb

getNum :: String -> String
getNum all@(x:xs)
    | x == '-' && all!!((length all) - 1) ==  ' ' =  tail $ init $ all
    | x == '-' && all!!((length all) - 1) /= ' ' = tail $ all
    | x /= '-' && all!!((length all) - 1) == ' ' = init $ all
    | x /= '-' && all!!((length all) - 1) /= ' ' = all
    | otherwise = all

main :: IO ()
main = do
    av <- getArgs
    errorHandling av
    noNumber (cleanSpace $ head av)
    numberWithoutOp (getNum $ cleanSpaceWithSingleSpace (head av) 0) 0
    let numsOP = expr (cleanSpace $ head av)
    handlingResult (snd $ head numsOP)
    putStrLn (convertPower $ show $ fst $ head numsOP)