import System.Random (randomRIO)
import Data.List (maximumBy)

randomElement :: [a] -> IO a
randomElement list = do
    index <- randomRIO (0, length list - 1)
    return (list !! index)

randomRoom :: IO String
randomRoom = randomElement rooms

randomItem :: IO String
randomItem = randomElement items

randomBossDamage :: IO Int
randomBossDamage = randomRIO (10, 100)

data Game = Game {
    health :: Int,
    inventory :: [String],
    location :: String,
    bossHP :: Int,
    collectedRooms :: [String],
    collectedSwords :: [String]
} deriving (Eq, Show)

newtype GameMonad a = GameMonad { runGameMonad :: Game -> (a, Game)}

instance Functor GameMonad where
    fmap f (GameMonad g) = GameMonad $ \game ->
        let (a, newGame) = g game
        in (f a, newGame)

instance Applicative GameMonad where
    pure x = GameMonad $ \game -> (x, game)
    (<*>) (GameMonad sf) (GameMonad sx) = GameMonad $ \game ->
        let (f, newGame1) = sf game
            (x, newGame2) = sx newGame1
        in (f x, newGame2)

instance Monad GameMonad where
    (GameMonad sx) >>= f = GameMonad $ \game ->
        let (x, newGame) = sx game
            GameMonad sy = f x
        in sy newGame

rooms :: [String]
rooms = ["Entrance", "Vault", "Catacomb", "Shrine", "Boss Chamber"]

items :: [String]
items = ["Health Potion", "Key", "Common Sword", "Rare sword", "Legendary Sword"]

swordDamage :: [(String, Int)]
swordDamage = [("Common Sword", 20), ("Rare Sword", 50), ("Legendary Sword", 100)]

initialGame :: Game
initialGame = Game {
    health = 100,
    inventory = [],
    location = "Entrance",
    bossHP = 1000,
    collectedRooms = [],
    collectedSwords = []
}

moveToRoom :: String -> GameMonad ()
moveToRoom room = GameMonad $ \game ->
    if room == "Boss Chamber" && not ("Key" `elem` inventory game)
    then ((), game)
    else
        let newGame = game {location = room, collectedRooms = []}
        in ((), newGame)

collectItem :: String -> GameMonad ()
collectItem item = GameMonad $ \game ->
    let currentRoom = location game
        swords = ["Common Sword", "Rare Sword", "Legendary Sword"]
    in if currentRoom `elem` collectedRooms game
        then ((), game)
        else if item `elem` swords && item `elem` collectedSwords game
            then ((), game)
            else
                let newInventory = item : inventory game
                    newCollectedRooms = currentRoom : collectedRooms game
                    newCollectedSwords = if item `elem` swords
                                         then item : collectedSwords game
                                         else collectedSwords game
                in ((), game {inventory = newInventory, collectedRooms = newCollectedRooms, collectedSwords = newCollectedSwords})

takeDamage :: Int -> GameMonad ()
takeDamage damage = GameMonad $ \game -> ((), game {health = health game - damage})

attackBoss :: GameMonad ()
attackBoss = GameMonad $ \game ->
    case findBestSword (inventory game) of
        Nothing -> ((), game)
        Just (bestSword, damage) ->
            let newBossHP = max 0 (bossHP game - damage)
            in ((), game {bossHP = newBossHP})

findBestSword :: [String] -> Maybe (String, Int)
findBestSword inventory =
    let swords = filter (`elem` map fst swordDamage) inventory
        swordWithDamage = [(s, d) | (s, d) <- swordDamage, s `elem` swords]
    in if null swordWithDamage
       then Nothing
       else Just (maximumBy (\(_, d1) (_, d2) -> compare d1 d2) swordWithDamage)
    
heal :: Int -> GameMonad ()
heal amount = GameMonad $ \game ->
    if "Health Potion" `elem` inventory game
    then
        let newInventory = removeItem "Health Potion" (inventory game)
        in ((), game {health = health game + amount, inventory = newInventory})
    else
        ((), game)

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys)
    | x == y    = ys
    | otherwise = y : removeItem x ys

displayState :: GameMonad ()
displayState = GameMonad $ \game -> ((), game)

interactiveLogic :: GameMonad ()
interactiveLogic = do
    GameMonad $ \game -> ((), game {location = "Randomized Room!"}) 

askUser :: String -> IO String
askUser prompt = do
    putStrLn prompt
    getLine

main :: IO ()
main = gameLoop initialGame

gameLoop :: Game -> IO ()
gameLoop game = do
    putStrLn $ "\nYou are in " ++ location game ++ "."
    putStrLn $ "Health: " ++ show (health game)
    putStrLn $ "Inventory: " ++ show (inventory game)
    action <- askUser "What do you want to do? (move, collect, heal, status, attack, quit): "
    case action of
        "move" -> do
            room <- randomRoom
            if room == "Boss Chamber" && not ("Key" `elem` inventory game)
            then do
                putStrLn "You need a Key to enter the Boss Chamber!"
                gameLoop game
            else do
                putStrLn $ "Moving to " ++ room ++ "..."
                let(_, newGame) = runGameMonad (moveToRoom room) game
                gameLoop newGame
        "collect" -> do
            let currentRoom = location game
            if currentRoom `elem` collectedRooms game
            then do
                putStrLn "You have already collected an item in this room. Move to another room to collect more!"
                gameLoop game
            else do
                item <- randomItem
                putStrLn $ "You found a " ++ item ++ "!"
                let (_, newGame) = runGameMonad (collectItem item) game
                gameLoop newGame
        "heal" -> do
            let (_, newGame) = runGameMonad (heal 10) game
            if newGame == game
            then putStrLn "You have no Health Potion to heal!"
            else putStrLn "You used a Health Potion to heal 10 points!"
            gameLoop newGame
        "status" -> do
            let (_, _) = runGameMonad displayState game
            gameLoop game
        "attack" -> do
            if location game == "Boss Chamber"
            then do
                let (_, newGame) = runGameMonad attackBoss game
                if bossHP newGame < bossHP game
                then do
                    let damageDealt = bossHP game - bossHP newGame
                    putStrLn $ "You dealed " ++ show damageDealt ++ " damage. Boss HP: " ++ show (bossHP newGame)
                    if bossHP newGame <= 0
                    then putStrLn "You defeated the boss!"
                    else do
                        bossDamage <- randomBossDamage
                        let newHealth = max 0 (health newGame - bossDamage)
                        putStrLn $ "The boss attacked you, dealing " ++ show bossDamage ++ " damage!"
                        if newHealth <= 0
                        then putStrLn "You died!"
                        else gameLoop newGame { health = newHealth }
                else do
                    putStrLn "You have no weapon!"
                    gameLoop newGame
            else do
                putStrLn "You cannot attack in the current room."
                gameLoop game
        "quit" -> putStrLn "Thanks for playing!"
        _ -> do
            putStrLn "Invalid action. Try again."
            gameLoop game