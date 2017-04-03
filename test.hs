:m Reggie Control.Applicative
let a = parser (lit 'a') :: Parser Char Maybe Char
foldl Reggie.read a ""
foldl Reggie.read a "a"
foldl Reggie.read a "aa"
foldl Reggie.read a "c"

data Base = A | C | G | T deriving Show
let a = parser (fmap (const A) (lit 'a')) :: Parser Char Maybe Base
foldl Reggie.read a ""
foldl Reggie.read a "a"
foldl Reggie.read a "aa"
foldl Reggie.read a "c"

let ac = parser ((,) <$> (lit 'a') <*> (lit 'c')) :: Parser Char Maybe (Char, Char)
foldl Reggie.read ac ""
foldl Reggie.read ac "a"
foldl Reggie.read ac "c"
foldl Reggie.read ac "ac"
foldl Reggie.read ac "acg"

let a = parser (pure A) :: Parser Char Maybe Base
foldl Reggie.read a ""
foldl Reggie.read a "a"

let a = fmap (const A) (lit 'a')
let c = fmap (const C) (lit 'c')
let ac = parser (a <|> c) :: Parser Char Maybe Base
foldl Reggie.read ac ""
foldl Reggie.read ac "a"
foldl Reggie.read ac "ac"
foldl Reggie.read ac "c"
foldl Reggie.read ac "g"

data AOrAC = A | AC deriving Show
data CGOrG = CG | G deriving Show
let a = fmap (const A) (lit 'a')
let c = lit 'c'
let g = fmap (const G) (lit 'g')
let ac = (\_ _ -> AC) <$> a <*> c
let cg = (\_ _ -> CG) <$> c <*> g
foldl Reggie.read (parser ((,) <$> (a <|> ac) <*> (cg <|> g))) "acg" :: Parser Char Maybe (AOrAC, CGOrG)
foldl Reggie.read (parser ((,) <$> (a <|> ac) <*> (g <|> cg))) "acg" :: Parser Char Maybe (AOrAC, CGOrG)
foldl Reggie.read (parser ((,) <$> (ac <|> a) <*> (cg <|> g))) "acg" :: Parser Char Maybe (AOrAC, CGOrG)
foldl Reggie.read (parser ((,) <$> (ac <|> a) <*> (g <|> cg))) "acg" :: Parser Char Maybe (AOrAC, CGOrG)

data Base = A | C | G | T deriving Show
let a = fmap (const A) (lit 'a')
let as = parser (rep (:) [] a) :: Parser Char Maybe [Base]
foldl Reggie.read as ""
foldl Reggie.read as "a"
foldl Reggie.read as "aa"
foldl Reggie.read as "aaa"

let ass = parser (rep (:) [] (rep (:) [] a)) :: Parser Char Maybe [[Base]]
foldl Reggie.read ass ""
foldl Reggie.read ass "a"
foldl Reggie.read ass "aa"
foldl Reggie.read ass "aaa"
