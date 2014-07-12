module Checker where

type Word = (String, Char)
type Conjugation = String

conjugate :: Word -> Conjugation -> String
conjugate w []         = fst w
conjugate ([],_) _     = []
conjugate w c
  | not (validC c)      = fst w
--  | not (snd w == 'u')  = fst w
  | c == "Te"           = teForm $ fst w
  | c == "TeIru"        = teIru $ fst w
  | c == "Past"         = fst . pastTense $ w
  | c == "Tai"          = fst . taiForm $ w
  | c == "NegTai"       = fst . negative . taiForm $ w
  | c == "NegPastTai"   = fst . pastTense . negative . taiForm $ w
  | otherwise           = "Conjugated U verb"

teForm :: String -> String
teForm x
  | last x == 'く'    = init x ++ "いて"
  | last x == 'ぐ'    = init x ++ "いで"
  | last x == 'う'    = init x ++ "って"
  | last x == 'つ'    = init x ++ "って"
  | last x == 'る'    = init x ++ "って"
  | last x == 'ぬ'    = init x ++ "んで"
  | last x == 'む'    = init x ++ "んで"
  | last x == 'ぶ'    = init x ++ "んで"
  | last x == 'す'    = init x ++ "して"
  -- add いるえる　To　て
  | otherwise           = "Invalid input"

teIru :: String -> String
teIru x = teForm x ++ "いる"

pastTense :: Word -> Word
pastTense x
  | snd x == 'r'                   = (init (fst x) ++ "た", 'r')
  | snd x == 'u' && last y == 'て' = (init y ++ "た", 'u')
  | snd x == 'u' && last y == 'で' = (init y ++ "だ", 'u')
  | snd x == 'i'                   = (init (fst x) ++ "かった", 'i')
  | snd x == 'n'                   = (init (fst x) ++ "だった", 'n')
  where y = teForm (fst x)

taiForm :: Word -> Word
taiForm x
  | snd x == 'r'              = (init (fst x) ++ "たい", 'i')
  | snd x == 'u' && y == 'う' = (init (fst x) ++ "いたい", 'i')
  | snd x == 'u' && y == 'く' = (init (fst x) ++ "きたい", 'i')
  | snd x == 'u' && y == 'ぐ' = (init (fst x) ++ "ぎたい", 'i')
  | snd x == 'u' && y == 'す' = (init (fst x) ++ "したい", 'i')
  | snd x == 'u' && y == 'つ' = (init (fst x) ++ "ちたい", 'i')
  | snd x == 'u' && y == 'ぬ' = (init (fst x) ++ "にたい", 'i')
  | snd x == 'u' && y == 'む' = (init (fst x) ++ "みたい", 'i')
  | snd x == 'u' && y == 'ぶ' = (init (fst x) ++ "びたい", 'i')
  | snd x == 'u' && y == 'る' = (init (fst x) ++ "りたい", 'i')
  | snd x == 's'              = (init (fst x) ++ "したい", 'i')
  where y = last (fst x)

--negativeTai :: Word -> String
--negativeTai x = (init (taiForm x)) ++ "くない"

negative :: Word -> Word
negative x
  | snd x == 'i'              = (init (fst x) ++ "くない", 'i')
  | snd x == 'r'              = (init (fst x) ++ "ない", 'i')
  | snd x == 'n'              = (init (fst x) ++ "じゃない", 'i')
  | snd x == 'u' && y == 'う' = (init (fst x) ++ "わない", 'i')
  | snd x == 'u' && y == 'く' = (init (fst x) ++ "かない", 'i')
  | snd x == 'u' && y == 'ぐ' = (init (fst x) ++ "がない", 'i')
  | snd x == 'u' && y == 'す' = (init (fst x) ++ "さない", 'i')
  | snd x == 'u' && y == 'つ' = (init (fst x) ++ "たない", 'i')
  | snd x == 'u' && y == 'ぬ' = (init (fst x) ++ "なない", 'i')
  | snd x == 'u' && y == 'む' = (init (fst x) ++ "まない", 'i')
  | snd x == 'u' && y == 'ぶ' = (init (fst x) ++ "ばない", 'i')
  | snd x == 'u' && y == 'る' = (init (fst x) ++ "らない", 'i')
  where y = last (fst x)

imperitive :: Word -> Word
imperitive x
  | snd x == 's'              = (fst x ++ "しろ", 'r')
  | snd x == 'r'              = (init (fst x) ++ "れ",'r')
  | snd x == 'u' && y == 'う' = (init (fst x) ++ "え", 'u')
  | snd x == 'u' && y == 'く' = (init (fst x) ++ "け", 'u')
  | snd x == 'u' && y == 'ぐ' = (init (fst x) ++ "げ", 'u')
  | snd x == 'u' && y == 'す' = (init (fst x) ++ "せ", 'u')
  | snd x == 'u' && y == 'つ' = (init (fst x) ++ "て", 'u')
  | snd x == 'u' && y == 'ぬ' = (init (fst x) ++ "ね", 'u')
  | snd x == 'u' && y == 'む' = (init (fst x) ++ "め", 'u')
  | snd x == 'u' && y == 'ぶ' = (init (fst x) ++ "べ", 'u')
  | snd x == 'u' && y == 'る' = (init (fst x) ++ "れ", 'u')
  where y = last (fst x)

negativeImperitive :: Word -> Word
negativeImperitive x
  | snd x == 's'              = (fst x ++ "するな", 'n')
  | otherwise                 = (fst x ++ "な", 'n')

volitional :: Word -> Word
volitional x
  | snd x == 's'              = (fst x ++ "しよう", 'u')
  | snd x == 'r'              = (init (fst x) ++ "よう",'r')
  | snd x == 'u' && y == 'う' = (init (fst x) ++ "おう", 'u')
  | snd x == 'u' && y == 'く' = (init (fst x) ++ "こう", 'u')
  | snd x == 'u' && y == 'ぐ' = (init (fst x) ++ "ごう", 'u')
  | snd x == 'u' && y == 'す' = (init (fst x) ++ "そう", 'u')
  | snd x == 'u' && y == 'つ' = (init (fst x) ++ "とう", 'u')
  | snd x == 'u' && y == 'ぬ' = (init (fst x) ++ "のう", 'u')
  | snd x == 'u' && y == 'む' = (init (fst x) ++ "もう", 'u')
  | snd x == 'u' && y == 'ぶ' = (init (fst x) ++ "ぼう", 'u')
  | snd x == 'u' && y == 'る' = (init (fst x) ++ "ろう", 'u')
  | snd x == 'i'              = (init (fst x) ++ "かろう", 'u')
  | snd x == 'a'              = (init (fst x) ++ "だろう", 'u')
  where y = last (fst x)

potential :: Word -> Word
potential x
  | snd x == 'n'              = (fst x ++ "できる", 'r')
  | snd x == 'r'              = (init (fst x) ++ "られる",'r')
  | snd x == 'u' && y == 'う' = (init (fst x) ++ "える", 'u')
  | snd x == 'u' && y == 'く' = (init (fst x) ++ "ける", 'u')
  | snd x == 'u' && y == 'ぐ' = (init (fst x) ++ "げる", 'u')
  | snd x == 'u' && y == 'す' = (init (fst x) ++ "せる", 'u')
  | snd x == 'u' && y == 'つ' = (init (fst x) ++ "てる", 'u')
  | snd x == 'u' && y == 'ぬ' = (init (fst x) ++ "ねる", 'u')
  | snd x == 'u' && y == 'む' = (init (fst x) ++ "める", 'u')
  | snd x == 'u' && y == 'ぶ' = (init (fst x) ++ "べる", 'u')
  | snd x == 'u' && y == 'る' = (init (fst x) ++ "れる", 'u')
  | snd x == 'i'              = (init (fst x) ++ "あり得る", 'u')
  where y = last (fst x)

conditional :: Word -> Word
conditional x = (fst (pastTense x) ++ "ら", snd x)

passive :: Word -> Word
passive x
  | snd x == 's'              = (fst x ++ "される", 'r')
  | snd x == 'r'              = (init (fst x) ++ "られる",'r')
  | snd x == 'u' && y == 'う' = (init (fst x) ++ "われる", 'u')
  | snd x == 'u' && y == 'く' = (init (fst x) ++ "かれる", 'u')
  | snd x == 'u' && y == 'ぐ' = (init (fst x) ++ "がれる", 'u')
  | snd x == 'u' && y == 'す' = (init (fst x) ++ "される", 'u')
  | snd x == 'u' && y == 'つ' = (init (fst x) ++ "たれる", 'u')
  | snd x == 'u' && y == 'ぬ' = (init (fst x) ++ "なれる", 'u')
  | snd x == 'u' && y == 'む' = (init (fst x) ++ "まれる", 'u')
  | snd x == 'u' && y == 'ぶ' = (init (fst x) ++ "ばれる", 'u')
  | snd x == 'u' && y == 'る' = (init (fst x) ++ "られる", 'u')
  where y = last (fst x)

causitive :: Word -> Word
causitive x
  | snd x == 's'              = (fst x ++ "させる", 'r')
  | snd x == 'r'              = (init (fst x) ++ "させる",'r')
  | snd x == 'u' && y == 'う' = (init (fst x) ++ "わせる", 'u')
  | snd x == 'u' && y == 'く' = (init (fst x) ++ "かせる", 'u')
  | snd x == 'u' && y == 'ぐ' = (init (fst x) ++ "がせる", 'u')
  | snd x == 'u' && y == 'す' = (init (fst x) ++ "させる", 'u')
  | snd x == 'u' && y == 'つ' = (init (fst x) ++ "たせる", 'u')
  | snd x == 'u' && y == 'ぬ' = (init (fst x) ++ "なせる", 'u')
  | snd x == 'u' && y == 'む' = (init (fst x) ++ "ませる", 'u')
  | snd x == 'u' && y == 'ぶ' = (init (fst x) ++ "ばせる", 'u')
  | snd x == 'u' && y == 'る' = (init (fst x) ++ "らせる", 'u')
  where y = last (fst x)

causitivePassive :: Word -> Word
causitivePassive = passive . causitive

provisionalConditional :: Word -> Word
provisionalConditional x
  | snd x == 's'              = (fst x ++ "すれば", 'u')
  | snd x == 'r'              = (init (fst x) ++ "れば",'r')
  | snd x == 'u' && y == 'う' = (init (fst x) ++ "えば", 'u')
  | snd x == 'u' && y == 'く' = (init (fst x) ++ "けば", 'u')
  | snd x == 'u' && y == 'ぐ' = (init (fst x) ++ "げば", 'u')
  | snd x == 'u' && y == 'す' = (init (fst x) ++ "せば", 'u')
  | snd x == 'u' && y == 'つ' = (init (fst x) ++ "てば", 'u')
  | snd x == 'u' && y == 'ぬ' = (init (fst x) ++ "ねば", 'u')
  | snd x == 'u' && y == 'む' = (init (fst x) ++ "めば", 'u')
  | snd x == 'u' && y == 'ぶ' = (init (fst x) ++ "べば", 'u')
  | snd x == 'u' && y == 'る' = (init (fst x) ++ "れば", 'u')
  | snd x == 'i'              = (init (fst x) ++ "ければ", 'u')
  | snd x == 'a'              = (init (fst x) ++ "であれば", 'u')
  where y = last (fst x)

polite :: Word -> String
polite x
  | snd x == 's'              = fst x ++ "します"
  | snd x == 'r'              = init (fst x) ++ "ます"
  | snd x == 'u' && y == 'う' = init (fst x) ++ "います"
  | snd x == 'u' && y == 'く' = init (fst x) ++ "きます"
  | snd x == 'u' && y == 'ぐ' = init (fst x) ++ "ぎます"
  | snd x == 'u' && y == 'す' = init (fst x) ++ "します"
  | snd x == 'u' && y == 'つ' = init (fst x) ++ "ちます"
  | snd x == 'u' && y == 'ぬ' = init (fst x) ++ "にます" 
  | snd x == 'u' && y == 'む' = init (fst x) ++ "みます"
  | snd x == 'u' && y == 'ぶ' = init (fst x) ++ "びます"
  | snd x == 'u' && y == 'る' = init (fst x) ++ "ります"
  where y = last (fst x)

politeNeg :: Word -> String
politeNeg x = init (polite x) ++ "せん"

politeNegPast :: Word -> String
politeNegPast x = politeNeg x ++ "でした"

politePast :: Word -> String
politePast x = polite x ++ "した"

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
  | otherwise           = False

addType :: String -> Word
addType x
  | take 2 (reverse x) == "する" = (init (init x),'s')
  | last x == 'う' = (x,'u')
  | last x == 'く' = (x,'u') 
  | last x == 'ぐ' = (x,'u')
  | last x == 'す' = (x,'u')
  | last x == 'つ' = (x,'u')
  | last x == 'づ' = (x,'u')
  | last x == 'ぬ' = (x,'u')
  | last x == 'ぶ' = (x,'u')
  | last x == 'む' = (x,'u')
  | last x == 'る' = dbLookup x
  | otherwise      = (x,'n')

dbLookup :: String -> Word
dbLookup x = undefined
 -- where y = openFile "edict" ReadMode
