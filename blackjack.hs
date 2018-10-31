import System.Random
import System.Exit (exitSuccess)
import Data.List

data Nummer = Twee | Drie | Vier | Vijf | Zes | Zeven | Acht | Negen | Tien | Boer | Konigin | Koning | Aas deriving (Read, Enum, Eq, Show, Ord)
data Suit = Klaveren | Ruiten | Harten | Schoppen deriving (Read, Enum, Eq, Show, Ord)

data SpeelKaarten = Kaart Nummer Suit deriving (Read, Eq, Show, Ord)

-- function to show Kaart
showKaart :: SpeelKaarten -> String
showKaart (Kaart n s) = show n ++ " -- " ++ show s

-- function to take the top Kaart
pakKaart :: Speler -> SpeelKaarten
pakKaart speler = head speler

type Deck = [SpeelKaarten]
type Speler = [SpeelKaarten]

nieuwDeck :: Deck
nieuwDeck = [Kaart x y|  y <- [Klaveren .. Schoppen], x <- [Twee .. Aas]]


-- shuffle Twee decks
schudDeck :: Deck -> IO Deck
schudDeck deck = do
    if length deck /= 0
        then do 
            let deckLen = (length deck) - 1
            n <- randomRIO(0, deckLen) :: IO Int
            let randomKaart = deck !! (fromIntegral n)
            tailShuffle <- schudDeck (delete randomKaart deck)
            return ([randomKaart] ++ tailShuffle)
        else return deck

deelTweeKaarten :: Monad m => m Deck -> m (Speler, Speler, Deck)
deelTweeKaarten d = 
    do
    deck <- d
    return (take 2 deck, take 2 (drop 2 deck), drop 4 deck)

deelEenKaart :: Speler -> Deck -> (Speler, Deck)
deelEenKaart speler deck = (take 1 deck, drop 1 deck)

hit :: (Speler, Deck) -> (Speler, Deck)
hit (speler, deck) = ((take 1 deck)++speler, drop 1 deck)

pakSpelerKaarten :: (Speler, Speler, Deck) -> Speler
pakSpelerKaarten (speler, _, _) = speler

pakDealerKaarten :: (Speler, Speler, Deck) -> Speler
pakDealerKaarten (_, dealer, _) = dealer

pakDealerEersteKaart :: Speler -> SpeelKaarten
pakDealerEersteKaart dealer = dealer !! 0

krijgDeck :: (Speler, Speler, Deck) -> Deck
krijgDeck (_, _, deck) = deck

speelSpelersBeurt :: (Speler, Deck) -> IO (Speler, Deck)
speelSpelersBeurt (speler, deck) = do
  putStr "\nWat wil je doen? (hit/staan): "
  action <- getLine
  
  if action == "hit"
    then do
      let (speler', deck') = hit (speler, deck)
      putStr "Jouw hand: "
      print speler'
      
      putStr "Jouw score: "
      if (dealerOver21 speler')
          then print (score speler')
          else print (highScore speler')
      
      if (spelerScoreOver21 speler')
          then do 
              putStrLn "\n****************************"
              putStr "Je bent over 21 punten. Je verliest!\n"
              exitSuccess
          else do
              gameStatus <- (speelSpelersBeurt (speler', deck'))
              return gameStatus

    else if action == "staan"
      then  do
          putStrLn "\n--------------------------------"
          putStrLn "Je hebt gekozen te blijven staan..."
          putStrLn "--------------------------------"
          return (speler, deck)
      else do
          putStrLn "*** Verkeerde invoer. Type hit of staan. (hit/staan)"
          gameStatus <- (speelSpelersBeurt (speler, deck))
          return gameStatus
      
speelDealersBeurt :: (Speler, Deck) -> IO (Speler, Deck)
speelDealersBeurt (dealer, deck) = do
    
    if (highScore dealer < 17)
    then do
      putStrLn "\nDealer hits..."
      let (dealer', deck') = hit (dealer, deck)
      
      putStr "Dealer's hand: "
      print dealer'
      putStr "Dealer's Score: "
      print (highScore dealer')
      
      if (dealerOver21 dealer')
          then do 
              putStrLn "\n****************************"
              putStr "Dealer hand kwam over 21. Jij hebt gewonnen!\n"
              exitSuccess
          else do
              gameStatus <- (speelDealersBeurt (dealer', deck'))
              return gameStatus
      
    else return (dealer, deck)

-- Kaart Waardes van dag 2 uit het boek. Aas is hier 1.
kaartWaarde :: SpeelKaarten -> Int
kaartWaarde (Kaart Aas _) = 1
kaartWaarde (Kaart Koning _) = 10
kaartWaarde (Kaart Konigin _) = 10
kaartWaarde (Kaart Boer _) = 10
kaartWaarde (Kaart Twee _) = 2
kaartWaarde (Kaart Drie _) = 3
kaartWaarde (Kaart Vier _) = 4
kaartWaarde (Kaart Vijf _) = 5
kaartWaarde (Kaart Zes _) = 6
kaartWaarde (Kaart Zeven _) = 7
kaartWaarde (Kaart Acht _) = 8
kaartWaarde (Kaart Negen _) = 9
kaartWaarde (Kaart Tien _) = 10


-- Kaart Waardes van dag 2 uit het boek. Aas is 11 hier.
kaartWaarde2 :: SpeelKaarten -> Int
kaartWaarde2 (Kaart Aas _) = 11
kaartWaarde2 (Kaart Koning _) = 10
kaartWaarde2 (Kaart Konigin _) = 10
kaartWaarde2 (Kaart Boer _) = 10
kaartWaarde2 (Kaart Twee _) = 2
kaartWaarde2 (Kaart Drie _) = 3
kaartWaarde2 (Kaart Vier _) = 4
kaartWaarde2 (Kaart Vijf _) = 5
kaartWaarde2 (Kaart Zes _) = 6
kaartWaarde2 (Kaart Zeven _) = 7
kaartWaarde2 (Kaart Acht _) = 8
kaartWaarde2 (Kaart Negen _) = 9
kaartWaarde2 (Kaart Tien _) = 10

score :: Speler -> Int
score speler = sum $ map kaartWaarde speler

highScore :: Speler -> Int
highScore speler = sum $ map kaartWaarde2 speler

spelerScoreOver21 :: Speler -> Bool
spelerScoreOver21 speler =
  score speler > 21

dealerOver21 :: Speler -> Bool
dealerOver21 speler =
  highScore speler > 21
  
winner:: Speler -> Speler -> IO()
winner speler dealer = do
    putStr "\nJouw Kaarten: "
    print speler
    putStr "Jouw score: "
    print (highScore speler)
    
    
    putStr "\nDealer's Kaarten: "
    print dealer
    putStr "Dealer's score: "
    print (highScore dealer)
    
    putStrLn "\n**************************"
    if (highScore speler) > (highScore dealer)
        then putStrLn "Gefeliciteerd! Je hebt gewonnen"
        else if (highScore speler) == (highScore dealer)
            then putStrLn "Het spel is gelijk."
            else putStrLn "Je hebt verloren!"

doTurns:: (Speler, Speler, Deck) -> IO()
doTurns (speler, dealer, deck) = do
    ------------------------------------------------------------------------------------
  -- speler turn
  ------------------------------------------------------------------------------------
  putStrLn "\n--------------------------------"
  putStrLn "Spelers beurt..."
  putStrLn "--------------------------------"
  (speler', deck') <- speelSpelersBeurt (speler, deck)
  
  ------------------------------------------------------------------------------------
  -- Dealer turn
  ------------------------------------------------------------------------------------
  putStrLn "\n--------------------------------"
  putStrLn "Dealer's beurt..."
  putStrLn "--------------------------------\n"
  putStr "Jouw Kaarten: "
  print speler'
  
  putStr "Dealer's hand is: "
  print dealer
  (dealer', deck'') <- speelDealersBeurt (dealer, deck')
  putStrLn "\n--------------------------------"
  putStrLn "Dealer staat op... "
  putStrLn "--------------------------------"
  ------------------------------------------------------------------------------------
  -- Dealer turn Winner
  ------------------------------------------------------------------------------------
  
  winner speler' dealer'
  
blackJack :: Speler -> Bool
blackJack speler = (highScore speler) == 21

main :: IO ()
main = do
  putStrLn "Welkom bij BlackJack"
  putStrLn "---------------------------------------------"
  
  ------------------------------------------------------------------------------------
  -- Shuffle, deal and show Kaarten
  ------------------------------------------------------------------------------------
  putStrLn "\nKaarten schudden..."
  let deck = schudDeck nieuwDeck
  
  putStrLn "\nKaarten delen...\n"
  let gameTuple = deelTweeKaarten deck
  
  game_tuple <- gameTuple
  putStrLn "------------------------------------"
  
  putStr "Jouw Kaarten: "
  let speler = pakSpelerKaarten game_tuple
  let deck = krijgDeck game_tuple
  print (speler)
  putStr "Jouw score: "
  print (highScore speler)

  putStr "Dealer's Kaarten: "
  let dealer = pakDealerKaarten game_tuple
  print (pakDealerEersteKaart(dealer), "Verborgen Kaart")
  putStr "Dealer's score is groter dan: "
  print (kaartWaarde2 (dealer!!0))
  
  if (blackJack speler)
      then do
          putStrLn "\n**************************"
          if (blackJack dealer)
          then putStrLn "Iedereen heeft BlackJack. Het spel is gelijk."
          else putStrLn "BlackJack! je hebt gewonnen."
      else doTurns (speler, dealer, deck)
  putStrLn "\n************************"
  putStrLn "Eind van het spel \n"