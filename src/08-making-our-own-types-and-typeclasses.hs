data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Eq, Show)

data Digit = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Enum, Eq, Ord, Show)

data Card = Card { digit :: Digit
                 , suit :: Suit
                 }
    deriving (Eq)

instance Show Card where
  show (Card d s) = show "The " ++ show d ++ show " of " ++ show s


class Hand a where
  play :: [a] -> Bool

instance Hand Card where
  play c = any isAceOfSpades c

betterCard :: Card -> Card -> Card
betterCard a@(Card d _) b@(Card d' _) | d > d' = a
                                      | otherwise = b

isAceOfSpades :: Card -> Bool
isAceOfSpades c = c == (Card Ace Spades)

data Coin = Heads | Tails
    deriving (Eq, Show)

