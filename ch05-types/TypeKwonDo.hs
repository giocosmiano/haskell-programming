module TypeKwonDo where

-- exercise # 1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h i = g (f i)
--h = g . f -- or alternatively just using compose

-- exercise # 2

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w (q a)
--e = w . q -- or alternatively just using compose

-- exercise # 3

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform xy = (xz $ fst xy, yz $ snd xy)

-- exercise # 4

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g x' = fst $ g (f x')
