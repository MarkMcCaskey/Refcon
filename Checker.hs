module Checker where

import Data.Maybe
import EdictDB

type Conjugation = String

conjugate :: Word -> Conjugation -> Maybe Word
conjugate _ []         = Nothing
conjugate ([],_) _     = Nothing
conjugate w c
  | not (validC c)     = Just w
  | c == "Te"          = teForm $ Just w
  | c == "Imperitive"  = imperitive $ Just w
  | c == "TeIru"       = teIru $ Just w
  | c == "Past"        = pastTense $ Just w
  | c == "Tai"         = taiForm $ Just w
  | c == "Negative"    = negative $ Just w
  | c == "NegTai"      = negative . taiForm $ Just w
  | c == "NegPastTai"  = pastTense . negative . taiForm $ Just w
  | c == "Causitive"   = causitive $ Just w
  | c == "CausPass"    = passive . causitive $ Just w
  | c == "Passive"     = passive $ Just w
  | c == "Polite"      = polite w
  | c == "PoliteNeg"   = politeNeg w
  | c == "PolitePast"  = politePast w
  | c == "PNegPast"    = politeNegPast w
  | c == "PoVo"        = politeVolitional w
  | c == "Stem"        = stem $ Just w
  | otherwise          = Nothing

teForm :: Maybe Word -> Maybe Word
teForm Nothing = Nothing
teForm x
  | hasIorERu x    = Just (init y ++ "て",  'e') --This needs to check for preceding い or え
  | last y == 'く' = Just (init y ++ "いて", 'e')
  | last y == 'ぐ' = Just (init y ++ "いで", 'e')
  | last y == 'う' = Just (init y ++ "って", 'e')
  | last y == 'つ' = Just (init y ++ "って", 'e')
  | last y == 'る' = Just (init y ++ "って", 'e')
  | last y == 'ぬ' = Just (init y ++ "んで", 'e')
  | last y == 'む' = Just (init y ++ "んで", 'e')
  | last y == 'ぶ' = Just (init y ++ "んで", 'e')
  | last y == 'す' = Just (init y ++ "して", 'e')
  | otherwise      = Nothing
  where z = fromJust x
        y = fst z

hasIorERu :: Maybe Word -> Bool
hasIorERu Nothing = False
hasIorERu x = beginsWith x 'i' && beginsWith x 'e' && snd y == 'r'
  where y = fromJust x

beginsWith :: Maybe Word -> Char -> Bool --probably a bad name for this function, rename later
beginsWith Nothing _  = False
beginsWith x y
  | y == 'a' = beginsWithA z
  | y == 'i' = beginsWithI z
  | y == 'u' = beginsWithU z
  | y == 'e' = beginsWithE z
  | y == 'o' = beginsWithO z
  | otherwise = False --maybe not the best solution, possibly throw an error here
  where z = fromJust x

beginsWithA :: Word -> Bool
beginsWithA x
  | z == 'あ' = True
  | z == 'か' = True
  | z == 'が' = True
  | z == 'さ' = True
  | z == 'ざ' = True
  | z == 'た' = True
  | z == 'だ' = True
  | z == 'な' = True
  | z == 'は' = True
  | z == 'ば' = True
  | z == 'ぱ' = True
  | z == 'ま' = True
  | z == 'ら' = True
  | z == 'わ' = True
  | z == 'や' = True
  | otherwise = False
  where y = take 2 $ reverse . fst $ x
        z = y !! 2

beginsWithI :: Word -> Bool
beginsWithI x
  | z == 'い' = True
  | z == 'き' = True
  | z == 'ぎ' = True
  | z == 'し' = True
  | z == 'じ' = True
  | z == 'ち' = True
  | z == 'ぢ' = True
  | z == 'に' = True
  | z == 'ひ' = True
  | z == 'び' = True
  | z == 'ぴ' = True
  | z == 'み' = True
  | z == 'り' = True
  | otherwise = False
  where y = take 2 $ reverse . fst $ x
        z = y !! 2

beginsWithU :: Word -> Bool
beginsWithU x
  | z == 'う' = True
  | z == 'く' = True
  | z == 'ぐ' = True
  | z == 'す' = True
  | z == 'ず' = True
  | z == 'つ' = True
  | z == 'づ' = True
  | z == 'ぬ' = True
  | z == 'ふ' = True
  | z == 'ぶ' = True
  | z == 'ぷ' = True
  | z == 'む' = True
  | z == 'る' = True
  | z == 'ゆ' = True
  | otherwise = False
  where y = take 2 $ reverse . fst $ x
        z = y !! 2


beginsWithE :: Word -> Bool
beginsWithE x
  | z == 'え' = True
  | z == 'け' = True
  | z == 'げ' = True
  | z == 'せ' = True
  | z == 'ぜ' = True
  | z == 'て' = True
  | z == 'で' = True
  | z == 'ね' = True
  | z == 'へ' = True
  | z == 'べ' = True
  | z == 'ぺ' = True
  | z == 'め' = True
  | z == 'れ' = True
  | otherwise = False
  where y = take 2 $ reverse . fst $ x
        z = y !! 2

beginsWithO :: Word -> Bool
beginsWithO x
  | z == 'お' = True
  | z == 'こ' = True
  | z == 'ご' = True
  | z == 'そ' = True
  | z == 'ぞ' = True
  | z == 'と' = True
  | z == 'ど' = True
  | z == 'の' = True
  | z == 'ほ' = True
  | z == 'ぼ' = True
  | z == 'ぽ' = True
  | z == 'も' = True
  | z == 'ろ' = True
  | z == 'よ' = True
  | otherwise = False
  where y = take 2 $ reverse . fst $ x
        z = y !! 2

teIru :: Maybe Word -> Maybe Word
teIru Nothing = Nothing
teIru x
  | isNothing (teForm x) = Nothing
  | otherwise            = Just (y ++ "いる", 'r')
  where y = fst . fromJust . teForm $ x

pastTense :: Maybe Word -> Maybe Word
pastTense Nothing = Nothing
pastTense x
  | snd z == 'r'                   = Just (init (fst z) ++ "た", 'r')
  | snd z == 'u' && last y == 'て' = Just (init y ++ "た", 'u')
  | snd z == 'u' && last y == 'で' = Just (init y ++ "だ", 'u')
  | snd z == 'i'                   = Just (init (fst z) ++ "かった", 'i')
  | snd z == 'd'                   = Just (init (fst z) ++ "だった", 'd')
  | otherwise                      = Nothing
  where z = fromJust x
        y = fst . fromJust . teForm $ x

taiForm :: Maybe Word -> Maybe Word
taiForm Nothing = Nothing
taiForm x
  | snd z == 'r'              = Just (init (fst z) ++ "たい", 'i')
  | snd z == 'u' && y == 'う' = Just (init (fst z) ++ "いたい", 'i')
  | snd z == 'u' && y == 'く' = Just (init (fst z) ++ "きたい", 'i')
  | snd z == 'u' && y == 'ぐ' = Just (init (fst z) ++ "ぎたい", 'i')
  | snd z == 'u' && y == 'す' = Just (init (fst z) ++ "したい", 'i')
  | snd z == 'u' && y == 'つ' = Just (init (fst z) ++ "ちたい", 'i')
  | snd z == 'u' && y == 'ぬ' = Just (init (fst z) ++ "にたい", 'i')
  | snd z == 'u' && y == 'む' = Just (init (fst z) ++ "みたい", 'i')
  | snd z == 'u' && y == 'ぶ' = Just (init (fst z) ++ "びたい", 'i')
  | snd z == 'u' && y == 'る' = Just (init (fst z) ++ "りたい", 'i')
  | snd z == 'n'              = Just (init (fst z) ++ "したい", 'i')
  | otherwise                 = Nothing
  where z = fromJust x
        y = last (fst z)

negative :: Maybe Word -> Maybe Word
negative Nothing = Nothing
negative x
  | snd z == 'i'              = Just (init (fst z) ++ "くない", 'i')
  | snd z == 'r'              = Just (init (fst z) ++ "ない", 'i')
  | snd z == 'd'              = Just (init (fst z) ++ "じゃない", 'i')
  | snd z == 'n'              = Just (init (fst z) ++ "しない", 'i')
  | snd z == 'u' && y == 'う' = Just (init (fst z) ++ "わない", 'i')
  | snd z == 'u' && y == 'く' = Just (init (fst z) ++ "かない", 'i')
  | snd z == 'u' && y == 'ぐ' = Just (init (fst z) ++ "がない", 'i')
  | snd z == 'u' && y == 'す' = Just (init (fst z) ++ "さない", 'i')
  | snd z == 'u' && y == 'つ' = Just (init (fst z) ++ "たない", 'i')
  | snd z == 'u' && y == 'ぬ' = Just (init (fst z) ++ "なない", 'i')
  | snd z == 'u' && y == 'む' = Just (init (fst z) ++ "まない", 'i')
  | snd z == 'u' && y == 'ぶ' = Just (init (fst z) ++ "ばない", 'i')
  | snd z == 'u' && y == 'る' = Just (init (fst z) ++ "らない", 'i')
  | otherwise                 = Nothing
  where z = fromJust x
        y = last (fst z)

imperitive :: Maybe Word -> Maybe Word
imperitive Nothing = Nothing
imperitive x
  | snd z == 'n'              = Just (fst z ++ "しろ", 'r')
  | snd z == 'r'              = Just (init (fst z) ++ "れ",'r')
  | snd z == 'u' && y == 'う' = Just (init (fst z) ++ "え", 'u')
  | snd z == 'u' && y == 'く' = Just (init (fst z) ++ "け", 'u')
  | snd z == 'u' && y == 'ぐ' = Just (init (fst z) ++ "げ", 'u')
  | snd z == 'u' && y == 'す' = Just (init (fst z) ++ "せ", 'u')
  | snd z == 'u' && y == 'つ' = Just (init (fst z) ++ "て", 'u')
  | snd z == 'u' && y == 'ぬ' = Just (init (fst z) ++ "ね", 'u')
  | snd z == 'u' && y == 'む' = Just (init (fst z) ++ "め", 'u')
  | snd z == 'u' && y == 'ぶ' = Just (init (fst z) ++ "べ", 'u')
  | snd z == 'u' && y == 'る' = Just (init (fst z) ++ "れ", 'u')
  where z = fromJust x
        y = last (fst z)

negativeImperitive :: Maybe Word -> Maybe Word
negativeImperitive Nothing = Nothing
negativeImperitive x
  | snd z == 'n'                 = Just (fst z ++ "するな", 'n')
  | snd z == 'r' || snd z == 'u' = Just (fst z ++ "な", 'n')
  | otherwise                    = Nothing
  where z = fromJust x

volitional :: Maybe Word -> Maybe Word
volitional Nothing = Nothing
volitional x
  | snd z == 'n'              = Just (fst z ++ "しよう", 'u')
  | snd z == 'r'              = Just (init (fst z) ++ "よう",'r')
  | snd z == 'u' && y == 'う' = Just (init (fst z) ++ "おう", 'u')
  | snd z == 'u' && y == 'く' = Just (init (fst z) ++ "こう", 'u')
  | snd z == 'u' && y == 'ぐ' = Just (init (fst z) ++ "ごう", 'u')
  | snd z == 'u' && y == 'す' = Just (init (fst z) ++ "そう", 'u')
  | snd z == 'u' && y == 'つ' = Just (init (fst z) ++ "とう", 'u')
  | snd z == 'u' && y == 'ぬ' = Just (init (fst z) ++ "のう", 'u')
  | snd z == 'u' && y == 'む' = Just (init (fst z) ++ "もう", 'u')
  | snd z == 'u' && y == 'ぶ' = Just (init (fst z) ++ "ぼう", 'u')
  | snd z == 'u' && y == 'る' = Just (init (fst z) ++ "ろう", 'u')
  | snd z == 'i'              = Just (init (fst z) ++ "かろう", 'u')
  | snd z == 'a'              = Just (init (fst z) ++ "だろう", 'u')
  | otherwise                 = Nothing
  where z = fromJust x
        y = last (fst z)

potential :: Maybe Word -> Maybe Word
potential Nothing = Nothing
potential x
  | snd z == 'd'              = Just (fst z ++ "できる", 'r')
  | snd z == 'r'              = Just (init (fst z) ++ "られる",'r')
  | snd z == 'n'              = Just (init (fst z) ++ "せられる",'r')
  | snd z == 'u' && y == 'う' = Just (init (fst z) ++ "える", 'u')
  | snd z == 'u' && y == 'く' = Just (init (fst z) ++ "ける", 'u')
  | snd z == 'u' && y == 'ぐ' = Just (init (fst z) ++ "げる", 'u')
  | snd z == 'u' && y == 'す' = Just (init (fst z) ++ "せる", 'u')
  | snd z == 'u' && y == 'つ' = Just (init (fst z) ++ "てる", 'u')
  | snd z == 'u' && y == 'ぬ' = Just (init (fst z) ++ "ねる", 'u')
  | snd z == 'u' && y == 'む' = Just (init (fst z) ++ "める", 'u')
  | snd z == 'u' && y == 'ぶ' = Just (init (fst z) ++ "べる", 'u')
  | snd z == 'u' && y == 'る' = Just (init (fst z) ++ "れる", 'u')
  | snd z == 'i'              = Just (init (fst z) ++ "あり得る", 'u')
  | otherwise                 = Nothing
  where z = fromJust x
        y = last (fst z)

conditional :: Maybe Word -> Maybe Word
conditional Nothing = Nothing
conditional x       = Just (fst z ++ "ら", snd y)
  where z = fromJust . pastTense $ x
        y = fromJust x

passive :: Maybe Word -> Maybe Word
passive Nothing = Nothing
passive x
  | snd z == 'n'              = Just (fst z ++ "される", 'r')
  | snd z == 'r'              = Just (init (fst z) ++ "られる",'r')
  | snd z == 'u' && y == 'う' = Just (init (fst z) ++ "われる", 'u')
  | snd z == 'u' && y == 'く' = Just (init (fst z) ++ "かれる", 'u')
  | snd z == 'u' && y == 'ぐ' = Just (init (fst z) ++ "がれる", 'u')
  | snd z == 'u' && y == 'す' = Just (init (fst z) ++ "される", 'u')
  | snd z == 'u' && y == 'つ' = Just (init (fst z) ++ "たれる", 'u')
  | snd z == 'u' && y == 'ぬ' = Just (init (fst z) ++ "なれる", 'u')
  | snd z == 'u' && y == 'む' = Just (init (fst z) ++ "まれる", 'u')
  | snd z == 'u' && y == 'ぶ' = Just (init (fst z) ++ "ばれる", 'u')
  | snd z == 'u' && y == 'る' = Just (init (fst z) ++ "られる", 'u')
  | otherwise                 = Nothing
  where z = fromJust x
        y = last (fst z)

causitive :: Maybe Word -> Maybe Word
causitive Nothing = Nothing
causitive x
  | snd z == 'n'              = Just (fst z ++ "させる", 'r')
  | snd z == 'r'              = Just (init (fst z) ++ "させる",'r')
  | snd z == 'u' && y == 'う' = Just (init (fst z) ++ "わせる", 'u')
  | snd z == 'u' && y == 'く' = Just (init (fst z) ++ "かせる", 'u')
  | snd z == 'u' && y == 'ぐ' = Just (init (fst z) ++ "がせる", 'u')
  | snd z == 'u' && y == 'す' = Just (init (fst z) ++ "させる", 'u')
  | snd z == 'u' && y == 'つ' = Just (init (fst z) ++ "たせる", 'u')
  | snd z == 'u' && y == 'ぬ' = Just (init (fst z) ++ "なせる", 'u')
  | snd z == 'u' && y == 'む' = Just (init (fst z) ++ "ませる", 'u')
  | snd z == 'u' && y == 'ぶ' = Just (init (fst z) ++ "ばせる", 'u')
  | snd z == 'u' && y == 'る' = Just (init (fst z) ++ "らせる", 'u')
  | otherwise                 = Nothing
  where z = fromJust x 
        y = last (fst z)

causitivePassive :: Maybe Word -> Maybe Word
causitivePassive Nothing = Nothing
causitivePassive x = passive . causitive $ x

provisionalConditional :: Maybe Word -> Maybe Word
provisionalConditional Nothing = Nothing
provisionalConditional x
  | snd z == 'n'              = Just (fst z ++ "すれば", 'u')
  | snd z == 'r'              = Just (init (fst z) ++ "れば",'r')
  | snd z == 'u' && y == 'う' = Just (init (fst z) ++ "えば", 'u')
  | snd z == 'u' && y == 'く' = Just (init (fst z) ++ "けば", 'u')
  | snd z == 'u' && y == 'ぐ' = Just (init (fst z) ++ "げば", 'u')
  | snd z == 'u' && y == 'す' = Just (init (fst z) ++ "せば", 'u')
  | snd z == 'u' && y == 'つ' = Just (init (fst z) ++ "てば", 'u')
  | snd z == 'u' && y == 'ぬ' = Just (init (fst z) ++ "ねば", 'u')
  | snd z == 'u' && y == 'む' = Just (init (fst z) ++ "めば", 'u')
  | snd z == 'u' && y == 'ぶ' = Just (init (fst z) ++ "べば", 'u')
  | snd z == 'u' && y == 'る' = Just (init (fst z) ++ "れば", 'u')
  | snd z == 'i'              = Just (init (fst z) ++ "ければ", 'u')
  | snd z == 'a'              = Just (init (fst z) ++ "であれば", 'u')
  where z = fromJust x
        y = last (fst z)

stem :: Maybe Word -> Maybe Word
stem Nothing = Nothing
stem x
  | snd z == 'r'              = Just (init (fst z), 'r')
  | snd z == 'u' && y == 'う' = Just (init (fst z) ++ "い", 'u')
  | snd z == 'u' && y == 'く' = Just (init (fst z) ++ "き", 'u')
  | snd z == 'u' && y == 'ぐ' = Just (init (fst z) ++ "ぎ", 'u')
  | snd z == 'u' && y == 'す' = Just (init (fst z) ++ "し", 'u')
  | snd z == 'u' && y == 'つ' = Just (init (fst z) ++ "ち", 'u')
  | snd z == 'u' && y == 'ぬ' = Just (init (fst z) ++ "に", 'u')
  | snd z == 'u' && y == 'む' = Just (init (fst z) ++ "み", 'u')
  | snd z == 'u' && y == 'ぶ' = Just (init (fst z) ++ "び", 'u')
  | snd z == 'u' && y == 'る' = Just (init (fst z) ++ "り", 'u')
  | fst z == "する"           = Just ("し", 's')
  | fst z == "くる"           = Just ("き", 'k')
  | otherwise    = Nothing
  where z = fromJust x
        y = last (fst z)

polite :: Word -> Maybe Word
polite x
  | snd x == 'n'              = Just (fst x ++ "します", 'e')
  | snd x == 'r'              = Just (init (fst x) ++ "ます", 'e')
  | snd x == 'u' && y == 'う' = Just (init (fst x) ++ "います", 'e')
  | snd x == 'u' && y == 'く' = Just (init (fst x) ++ "きます", 'e')
  | snd x == 'u' && y == 'ぐ' = Just (init (fst x) ++ "ぎます", 'e')
  | snd x == 'u' && y == 'す' = Just (init (fst x) ++ "します", 'e')
  | snd x == 'u' && y == 'つ' = Just (init (fst x) ++ "ちます", 'e')
  | snd x == 'u' && y == 'ぬ' = Just (init (fst x) ++ "にます", 'e')
  | snd x == 'u' && y == 'む' = Just (init (fst x) ++ "みます", 'e')
  | snd x == 'u' && y == 'ぶ' = Just (init (fst x) ++ "びます", 'e')
  | snd x == 'u' && y == 'る' = Just (init (fst x) ++ "ります", 'e')
  | otherwise                 = Nothing
  where y = last (fst x)

politeNeg :: Word -> Maybe Word
politeNeg x
  | isNothing (polite x) = Nothing 
  | otherwise            = Just (init y ++ "せん", 'e')
  where y                = fst . fromJust . polite $ x

politeNegPast :: Word -> Maybe Word
politeNegPast x
  | isNothing (politeNeg x) = Nothing
  | otherwise               = Just (y ++ "でした", 'e')
  where y                   = fst . fromJust . politeNeg $ x

politePast :: Word -> Maybe Word
politePast x 
  | isNothing (polite x) = Nothing
  | otherwise            = Just (y ++ "した", 'e')
  where y                = fst . fromJust . polite $ x

politeVolitional :: Word -> Maybe Word
politeVolitional x
  | isNothing (polite x) = Nothing
  | otherwise            = Just (init y ++ "しょう", 'e')
  where y                = fst . fromJust . polite $ x

validC :: Conjugation -> Bool
validC [] = False
validC x
  | x == "Te"           = True
  | x == "Imperitive"   = True
  | x == "Past"         = True
  | x == "TeIru"        = True
  | x == "Negative"     = True
  | x == "Tai"          = True
  | x == "NegTai"       = True
  | x == "NegPastTai"   = True
  | x == "Potential"    = True
  | x == "Causitive"    = True
  | x == "Passive"      = True
  | x == "CausPass"     = True
  | x == "Polite"       = True
  | x == "PoliteNeg"    = True
  | x == "PolitePast"   = True
  | x == "PNegPast"     = True
  | x == "PoVo"         = True
  | x == "Stem"         = True
  | otherwise           = False

addType :: String -> Maybe Word
addType x
  | take 2 (reverse x) == "する" = Just (init (init x),'n')
  | last x == 'う' = Just (x,'u')
  | last x == 'く' = Just (x,'u') 
  | last x == 'ぐ' = Just (x,'u')
  | last x == 'す' = Just (x,'u')
  | last x == 'つ' = Just (x,'u')
  | last x == 'づ' = Just (x,'u')
  | last x == 'ぬ' = Just (x,'u')
  | last x == 'ぶ' = Just (x,'u')
  | last x == 'む' = Just (x,'u')
  | last x == 'だ' = Just (x, 'd')
  | otherwise      = dbLookup x
