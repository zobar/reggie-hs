:m Reggie Control.Applicative
let a = parser (lit 'a')
foldl Reggie.read a ""
foldl Reggie.read a "a"
foldl Reggie.read a "aa"
foldl Reggie.read a "c"

data Base = A | C | G | T deriving Show
let a = parser (fmap (const A) (lit 'a'))
foldl Reggie.read a ""
foldl Reggie.read a "a"
foldl Reggie.read a "aa"
foldl Reggie.read a "c"

let a = fmap (const A) (lit 'a')
let c = fmap (const C) (lit 'c')
let ac = parser (a <|> c)
foldl Reggie.read ac ""
foldl Reggie.read ac "a"
foldl Reggie.read ac "ac"
foldl Reggie.read ac "c"
foldl Reggie.read ac "g"

let a = parser (pure A)
foldl Reggie.read a ""
foldl Reggie.read a "a"

let ac = parser ((,) <$> (lit 'a') <*> (lit 'c'))
foldl Reggie.read ac ""
foldl Reggie.read ac "a"
foldl Reggie.read ac "c"
foldl Reggie.read ac "ac"
foldl Reggie.read ac "acg"

data AOrAC = A | AC deriving Show
data CGOrG = CG | G deriving Show
let a = fmap (const A) (lit 'a')
let c = lit 'c'
let g = fmap (const G) (lit 'g')
let ac = (\_ _ -> AC) <$> a <*> c
let cg = (\_ _ -> CG) <$> c <*> g
foldl Reggie.read (parser ((,) <$> (a <|> ac) <*> (cg <|> g))) "acg"
foldl Reggie.read (parser ((,) <$> (a <|> ac) <*> (g <|> cg))) "acg"
foldl Reggie.read (parser ((,) <$> (ac <|> a) <*> (cg <|> g))) "acg"
foldl Reggie.read (parser ((,) <$> (ac <|> a) <*> (g <|> cg))) "acg"

data Base = A | C | G | T deriving Show
let a = fmap (const A) (lit 'a')
let as = parser (rep (:) [] a)
foldl Reggie.read as ""
foldl Reggie.read as "a"
foldl Reggie.read as "aa"
foldl Reggie.read as "aaa"

let ass = parser (rep (:) [] (rep (:) [] a))
foldl Reggie.read ass ""
foldl Reggie.read ass "a"
foldl Reggie.read ass "aa"
foldl Reggie.read ass "aaa"
