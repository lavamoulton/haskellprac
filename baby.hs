doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
FizzBuzz xs = [ if x < 10 then "Fizz" else "Buzz" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]
getRightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
getRects = [ (a,b,c,d) | a <- [1..10], b <- [1..10], c <- [1..10], d <- [1..10], a == c, 
                        b==d, a*b == 20 ]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number Seven!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

first :: (a, b, c) -> a
first (x, _, _) = x
second :: (a, b, c) -> b
second (_, x, _) = x
third :: (a, b, c) -> c
third (_, _, x) = x

head' :: [a] -> a
head' [] = error "Can't call head on an empty list."
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element " ++ show x
tell (x:y:[]) = "The list has two elements " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are " ++ show x ++ " and " ++ show y

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

sum'' :: (Num a) => [a] -> a
sum'' [] = 0
sum'' (x:xs) = x + sum'' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
        | weight / height ^ 2 <= 18.5 = "You're underweight you emo"
        | weight / height ^ 2 <= 25.0 = "You're supposedly normal"
        | weight / height ^ 2 <= 30.0 = "You're fat"
        | otherwise                   = "You're a whale"

max' :: (Ord a) => a -> a -> a
max' a b
        | a > b     = a
        | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
        | a > b         = GT
        | a == b        = EQ
        | otherwise     = LT

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
        | bmi <= 18.5 = "You're underweight"
        | bmi <= 25.0 = "You're normal"
        | bmi <= 30.0 = "You're fat"
        | otherwise   = "You're a whale"
        where bmi = weight / height ^ 2

bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
        | bmi <= skinny = "You're underweight"
        | bmi <= normal = "You're normal"
        | bmi <= fat    = "You're fat"
        | otherwise     = "You're a whale"
        where bmi = weight / height ^ 2
              (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
        where (f:_) = firstname
              (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
        where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + 2 * topArea

calc1Bmis :: (RealFloat a) => [(a, a)] -> [a]
calc1Bmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

describeList' :: [a] -> String
describeList' xs = "The list is " ++ case xs of [] -> "empty."  
                                                [x] -> "a singleton list."
                                                xs -> " a longer list."





