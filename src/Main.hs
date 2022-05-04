module Main(main) where


import Data.Bool
import Data.Set as S
import Data.Text.Internal.Read
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import System.Directory
import System.IO.Unsafe


---- Data Types ----


-- A Game of Halex.
data HalexGame = HalexGame
    {
        player          :: Player,
        exits           :: [Exit],
        staticEnv       :: [StaticEnvironment],
        dynamicEnv      :: [DynamicEnvironment],
        input           :: S.Set Key,
        level           :: Int,    
        exitAll         :: [[Exit]],
        staticAll       :: [[StaticEnvironment]],
        dynamicAll      :: [[DynamicEnvironment]]
    }

-- The Player Character.
data Player = Player 
    {
        playerIdle      :: [Picture],       -- The list of sprite frames for the Player's idle animation.
        playerWalk      :: [[Picture]],     -- The list of sprite frames for the Player's walk animation.
        playerFrame     :: Int,             -- The current frame of the animation.
        playerDelta     :: Int,             -- The counter until the next frame can advance.
        playerX         :: Int,             -- The Player's x-coordinate.
        playerY         :: Int,             -- The Player's y-coordinate.
        playerSpeed     :: Int,             -- The Player's movement speed.
        playerDirection :: Direction,       -- The Player's current direction.
        playerMoving    :: Bool             -- Determines whether the Player is walking.
    }

-- The Eight Cardinal Directions.
data Direction = North | South | East | West | Northeast | Northwest | Southeast | Southwest

-- Exits transport the Player between Levels.
data Exit = Exit
    {
        exitSprites     :: Picture,         -- The sprite for the exit.
        exitX           :: Int,             -- The exit's x-coordinate.
        exitY           :: Int,             -- The exit's y-coordinate
        exitCode        :: Int              -- The destination of the exit.
    }

-- Static Objects that Compose a Level.
data StaticEnvironment = StaticEnvironment
    {
        senvSprite      :: Picture,         -- The sprite for the object.
        senvX           :: Int,             -- The object's x-coordinate.
        senvY           :: Int,             -- The object's y-coordinate.
        senvType        :: EnvType          -- The class of environment to which this object belongs.
    }

-- Animated Objects that Compose a Level.
data DynamicEnvironment = DynamicEnvironment
    {
        denvAnim        :: [Picture],       -- The list of sprite frames for the object's animation.
        denvFrame       :: Int,             -- The current frame of the animation.
        denvDelta       :: Int,             -- The counter until the next frame can advance.
        denvX           :: Int,             -- The object's x-coordinate.
        denvY           :: Int,             -- The object's y-coordinate.
        denvType        :: EnvType          -- The class of environment to which this object belongs.
    }

-- The Types of Environment.
data EnvType = Terrain                      -- Ground that can be walked on but never walked off.
             | Structure                    -- Objects on the Ground that cannot be walked through.
             | Decoration                   -- Objects on the Ground that can be walked on.
             | Setpiece                     -- Special decorations that are very large.


---- Utility ----


-- Returns a specified sprite file loaded into a Picture (if it can be loaded).
loadSprite :: FilePath -> Picture
loadSprite filePath = maybe (text "Error: Could not load file.") id (unsafePerformIO $ loadJuicyPNG filePath)

-- Returns the item at the specified index from a given list (if it can be reached).
getItem :: Int -> [a] -> a 
getItem index list = 
    let get n i (z:zs) = if n == i then z else get n (i + 1) zs
        get n i [] = error ("Error: Item " ++ show n ++ " not found at limit " ++ show i)
    in get index 1 list

-- Returns the list of files in a given directory.
getFilesInDir :: String -> IO [FilePath]
getFilesInDir path = do
    entries <- listDirectory path
    let files = Prelude.map (\x -> path ++ x) entries
    return files

-- Returns a list of sprites corresponding to a list of files.
loadSprites :: [String] -> [Picture]
loadSprites files = reverseList (Prelude.map(\x -> loadSprite x) files)

-- Reverses a list, believe it or not.
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (z:zs) = reverseList zs ++ [z]

-- Returns an integer based on the given Direction.
dirToInt :: Direction -> Int
dirToInt direction = 
    case direction of
        North       -> 1
        South       -> 2
        East        -> 3
        West        -> 4 
        Northeast   -> 5 
        Northwest   -> 6
        Southeast   -> 7
        Southwest   -> 8


---- Functions ----

loadGame :: [Picture] -> [Picture] -> [[Picture]] -> [Picture] -> [[Picture]] -> [Picture] -> [Picture] -> [Picture] -> [[Picture]] -> IO HalexGame
loadGame terrainSprites staticStructureSprites dynamicStructureAnims staticDecorationSprites dynamicDecorationAnims setpieceSprites exitSprites playerIdleSprites playerWalkAnims = do

    exits1              <- loadAllExits "Edge" exitSprites
    exits2              <- loadAllExits "Underworld" exitSprites
    exits3              <- loadAllExits "Cathedral" exitSprites
    exits4              <- loadAllExits "Necropolis" exitSprites
    exits5              <- loadAllExits "Wasteland" exitSprites
    exits6              <- loadAllExits "Ascension" exitSprites

    let exitAll = [exits1, exits2, exits3, exits4, exits5, exits6]

    staticEnvironment1  <- loadAllStatic "Edge" terrainSprites staticStructureSprites staticDecorationSprites setpieceSprites
    staticEnvironment2  <- loadAllStatic "Underworld" terrainSprites staticStructureSprites staticDecorationSprites setpieceSprites
    staticEnvironment3  <- loadAllStatic "Cathedral" terrainSprites staticStructureSprites staticDecorationSprites setpieceSprites
    staticEnvironment4  <- loadAllStatic "Necropolis" terrainSprites staticStructureSprites staticDecorationSprites setpieceSprites
    staticEnvironment5  <- loadAllStatic "Wasteland" terrainSprites staticStructureSprites staticDecorationSprites setpieceSprites
    staticEnvironment6  <- loadAllStatic "Ascension" terrainSprites staticStructureSprites staticDecorationSprites setpieceSprites

    let staticAll = [staticEnvironment1, staticEnvironment2, staticEnvironment3, staticEnvironment4, staticEnvironment5, staticEnvironment6]

    dynamicEnvironment1 <- loadAllDynamic "Edge" dynamicStructureAnims dynamicDecorationAnims
    dynamicEnvironment2 <- loadAllDynamic "Underworld" dynamicStructureAnims dynamicDecorationAnims
    dynamicEnvironment3 <- loadAllDynamic "Cathedral" dynamicStructureAnims dynamicDecorationAnims
    dynamicEnvironment4 <- loadAllDynamic "Necropolis" dynamicStructureAnims dynamicDecorationAnims
    dynamicEnvironment5 <- loadAllDynamic "Wasteland" dynamicStructureAnims dynamicDecorationAnims
    dynamicEnvironment6 <- loadAllDynamic "Ascension" dynamicStructureAnims dynamicDecorationAnims

    let dynamicAll = [dynamicEnvironment1, dynamicEnvironment2, dynamicEnvironment3, dynamicEnvironment4, dynamicEnvironment5, dynamicEnvironment6]
    
    let player = Player playerIdleSprites playerWalkAnims 1 5 0 0 2 South False

    let spawnX = (-1120)
    let spawnY = 1856

    let exitStart = updateExits exits1 spawnX spawnY []
    let staticStart = updateStaticEnvironment staticEnvironment1 spawnX spawnY []
    let dynamicStart = updateDynamicEnvironment dynamicEnvironment1 spawnX spawnY []

    let game = HalexGame player exitStart staticStart dynamicStart S.empty 1 exitAll staticAll dynamicAll
    return game

loadAllExits :: String -> [Picture] -> IO [Exit]
loadAllExits levelName exitSprites = do
    exitFile <- readFile("Assets/Levels/" ++ levelName ++ "/Exits.txt")
    let exitObjects = loadExits exitFile 0 0 exitSprites []
    return exitObjects

loadAllStatic :: String -> [Picture] -> [Picture] -> [Picture] -> [Picture] -> IO [StaticEnvironment]
loadAllStatic levelName terrainSprites staticStructureSprites staticDecorationSprites setpieceSprites = do
    terrainFile <- readFile("Assets/Levels/" ++ levelName ++ "/Terrain.txt")
    let terrainObjects = loadStaticEnvironment terrainFile 0 (-32) terrainSprites Terrain []

    staticStructureFile <- readFile("Assets/Levels/" ++ levelName ++ "/StaticStructures.txt")
    let staticStructureObjects = loadStaticEnvironment staticStructureFile 0 0 staticStructureSprites Structure []

    staticDecorationFile <- readFile("Assets/Levels/" ++ levelName ++ "/StaticDecorations.txt")
    let staticDecorationObjects = loadStaticEnvironment staticDecorationFile 0 0 staticDecorationSprites Decoration []

    setpieceFile <- readFile("Assets/Levels/" ++ levelName ++ "/Setpieces.txt")
    let setpieceObjects = loadStaticEnvironment setpieceFile 0 32 setpieceSprites Setpiece []

    let staticEnvironment = terrainObjects ++ setpieceObjects ++ staticStructureObjects ++ staticDecorationObjects
    return staticEnvironment

loadAllDynamic :: String -> [[Picture]] -> [[Picture]] -> IO [DynamicEnvironment]
loadAllDynamic levelName dynamicStructureAnims dynamicDecorationAnims = do
    dynamicStructureFile <- readFile("Assets/Levels/" ++ levelName ++ "/DynamicStructures.txt")
    let dynamicStructureObjects = loadDynamicEnvironment dynamicStructureFile 0 0 dynamicStructureAnims Structure []

    dynamicDecorationFile <- readFile("Assets/Levels/" ++ levelName ++ "/DynamicDecorations.txt")
    let dynamicDecorationObjects = loadDynamicEnvironment dynamicDecorationFile 0 0 dynamicDecorationAnims Decoration []

    let dynamicEnvironment = dynamicStructureObjects ++ dynamicDecorationObjects
    return dynamicEnvironment

loadExits :: String -> Int -> Int -> [Picture] -> [Exit] -> [Exit]
loadExits [] x y sprites output = output
loadExits (z:zs) x y sprites output = 
    case z of
        '\n'    ->
            loadExits zs 0 (y - 64) sprites output
        '0'     ->
            loadExits zs (x + 64) y sprites output
        'X'     ->
            loadExits zs (x + 64) y sprites output
        _       ->
            let exit = Exit (getItem 1 sprites) x y (hexDigitToInt(z))
            in loadExits zs (x + 64) y sprites (output ++ [exit])

updateExits :: [Exit] -> Int -> Int -> [Exit] -> [Exit]
updateExits [] deltaX deltaY output = output
updateExits (z:zs) deltaX deltaY output =
    case z of
        Exit sprite x y code ->
            updateExits zs deltaX deltaY (output ++ [(Exit sprite (x + deltaX) (y + deltaY) code)])

drawExits :: [Exit] -> [Picture] -> [Picture]
drawExits [] output = output
drawExits (z:zs) output =
    case z of
        Exit sprite x y code -> 
            let translatedSprite = translate (fromIntegral(x)) (fromIntegral(y)) (sprite)
            in drawExits zs (output ++ [translatedSprite])

loadStaticEnvironment :: String -> Int -> Int -> [Picture] -> EnvType -> [StaticEnvironment] -> [StaticEnvironment]
loadStaticEnvironment [] x y sprites envtype output = output
loadStaticEnvironment (z:zs) x y sprites envtype output =
    case z of 
        '\n'    ->
            loadStaticEnvironment zs 0  (y - 64) sprites envtype output
        '0'     ->
            loadStaticEnvironment zs (x + 64) y sprites envtype output
        'X'     ->
            loadStaticEnvironment zs (x + 64) y sprites envtype output
        _       ->
            let tile = StaticEnvironment (getItem (hexDigitToInt(z)) sprites) x y envtype
            in loadStaticEnvironment zs (x + 64) y sprites envtype (output ++ [tile])

loadDynamicEnvironment :: String -> Int -> Int -> [[Picture]] -> EnvType -> [DynamicEnvironment] -> [DynamicEnvironment]
loadDynamicEnvironment [] x y anims envtype output = output
loadDynamicEnvironment (z:zs) x y anims envtype output =
        case z of 
            '\n'    ->
                loadDynamicEnvironment zs 0 (y - 64) anims envtype output
            '0'     -> 
                loadDynamicEnvironment zs (x + 64) y anims envtype output
            'X'     ->
                loadDynamicEnvironment zs (x + 64) y anims envtype output
            _       ->
                let tile = DynamicEnvironment (getItem (hexDigitToInt(z)) anims) 1 10 x y envtype
                in loadDynamicEnvironment zs (x + 64) y anims envtype (output ++ [tile])

updateStaticEnvironment :: [StaticEnvironment] -> Int -> Int -> [StaticEnvironment] -> [StaticEnvironment]
updateStaticEnvironment [] deltaX deltaY output = output
updateStaticEnvironment (z:zs) deltaX deltaY output =
    case z of
        StaticEnvironment sprite x y envtype ->
            updateStaticEnvironment zs deltaX deltaY (output ++ [(StaticEnvironment sprite (x + deltaX) (y + deltaY) envtype)])

updateDynamicEnvironment :: [DynamicEnvironment] -> Int -> Int -> [DynamicEnvironment] -> [DynamicEnvironment]
updateDynamicEnvironment [] deltaX deltaY output = output
updateDynamicEnvironment (z:zs) deltaX deltaY output = 
    case z of
        DynamicEnvironment anim frame delta x y envtype ->
            if (delta > 0) then
                updateDynamicEnvironment zs deltaX deltaY (output ++ [DynamicEnvironment anim frame (delta - 1) (x + deltaX) (y + deltaY) envtype])
            else 
                if (frame >= length(anim)) then updateDynamicEnvironment zs deltaX deltaY (output ++ [DynamicEnvironment anim 1 10 (x + deltaX) (y + deltaY) envtype])
                else updateDynamicEnvironment zs deltaX deltaY (output ++ [DynamicEnvironment anim (frame + 1) 10 (x + deltaX) (y + deltaY) envtype])

drawStaticEnvironment :: [StaticEnvironment] -> [Picture] -> [Picture]
drawStaticEnvironment [] output = output
drawStaticEnvironment (z:zs) output =
    case z of
        StaticEnvironment sprite x y envtype -> 
            let translatedSprite = translate (fromIntegral(x)) (fromIntegral(y)) (sprite)
            in drawStaticEnvironment zs (output ++ [translatedSprite])

drawDynamicEnvironment :: [DynamicEnvironment] -> [Picture] -> [Picture]
drawDynamicEnvironment [] output = output
drawDynamicEnvironment (z:zs) output = 
    case z of
        DynamicEnvironment anim frame delta x y envtype ->
            let currentSprite = getItem frame anim
                translatedSprite = translate (fromIntegral(x)) (fromIntegral(y)) (currentSprite)
            in  drawDynamicEnvironment zs (output ++ [translatedSprite])

updatePlayer :: Player -> Direction -> Bool -> Player
updatePlayer player direction moving = 
    case player of
    Player idleAnims walkAnims frame delta x y speed dr mov ->
        case moving of 
            True    ->
                if (delta > 0) then
                    Player idleAnims walkAnims frame (delta - 1) x y speed direction True 
                else 
                    if (frame > 7) then Player idleAnims walkAnims 1 5 x y speed direction True 
                    else Player idleAnims walkAnims (frame + 1) 5 x y speed direction True
            False   ->      Player idleAnims walkAnims frame delta x y speed dr False 

drawPlayer :: Player -> [Picture]
drawPlayer player = case player of
    Player idleSprites walkAnims frame delta x y speed direction moving ->
        case moving of
            True    ->
                let currentSprite = getItem frame (getItem (dirToInt(direction)) (walkAnims))
                    translatedSprite = translate (fromIntegral(x)) (fromIntegral(y)) (currentSprite)
                in [translatedSprite]
            False   -> 
                let currentSprite = getItem (dirToInt(direction)) (idleSprites)
                    translatedSprite = translate (fromIntegral(x)) (fromIntegral(y)) (currentSprite)
                in [translatedSprite]

-- Transports the Player from one level to the next using the three helper methods below.
teleport :: HalexGame -> HalexGame
teleport game = 
    case game of
        (HalexGame player exits staticEnvironment dynamicEnvironment keys level exitAll staticAll dynamicAll) -> 
            let code = checkExitCollision exits
            in if code == (-1) then game else (changeLevel level code game)

-- Returns the code of any Exit the Player is intersecting.
checkExitCollision :: [Exit] -> Int
checkExitCollision [] = (-1)
checkExitCollision (z:zs) = 
    case z of
        Exit sprite x y code ->
            if (x < 64 && x > (-64) && y < 64 && y > (-64)) then code else (checkExitCollision zs)

-- Changes the level by updating the environment.
changeLevel :: Int -> Int -> HalexGame -> HalexGame
changeLevel oldLevel newLevel game =
    case game of
        (HalexGame player exits staticEnvironment dynamicEnvironment keys level exitAll staticAll dynamicAll) -> 
            let (deltaX, deltaY) = getSpawnPoint oldLevel (getItem newLevel exitAll)
            in HalexGame player (updateExits (getItem newLevel exitAll) deltaX deltaY []) (updateStaticEnvironment (getItem newLevel staticAll) deltaX deltaY []) (updateDynamicEnvironment (getItem newLevel dynamicAll) deltaX deltaY []) S.empty newLevel exitAll staticAll dynamicAll

-- Returns the x and y-coordinates for where the Player will spawn in the new level.
getSpawnPoint :: Int -> [Exit] -> (Int, Int)
getSpawnPoint oldLevel [] = (0, 0)
getSpawnPoint oldLevel (z:zs) =
    case z of
        Exit sprite x y code ->
            if code == oldLevel then ((-x), (-y)) else (getSpawnPoint oldLevel zs)

-- Sets the display settings of the game.
window :: Display
window = FullScreen

-- Sets the background of the game.
background :: Color
background = black

-- Sets the frames-per-second of the game.
fps :: Int
fps = 90

-- Returns a Picture of the current state of the game.
render :: HalexGame -> Picture
render game = 
    case game of
        (HalexGame player exits staticEnvironment dynamicEnvironment keys level exitAll staticAll dynamicAll) -> pictures ((drawStaticEnvironment staticEnvironment []) ++ (drawDynamicEnvironment dynamicEnvironment []) ++ (drawExits exits []) ++ (drawPlayer player))

-- Updates the game by one frame  based on user input and game logic.
update :: Float -> HalexGame -> HalexGame
update seconds game
    | S.member (SpecialKey KeySpace) (input game) = teleport game
    | S.member (Char 'w') (input game) && S.member (Char 'd') (input game) = 
        game 
        {
            exits           = updateExits (exits game) (-1) (-1) [],
            staticEnv       = updateStaticEnvironment (staticEnv game) (-1) (-1) [],
            dynamicEnv      = updateDynamicEnvironment (dynamicEnv game) (-1) (-1) [],
            player          = updatePlayer (player game) Northeast True
        }
    | S.member (Char 'w') (input game) && S.member (Char 'a') (input game) = 
        game 
        {
            exits           = updateExits (exits game) 1 (-1) [],
            staticEnv       = updateStaticEnvironment (staticEnv game) 1 (-1) [],
            dynamicEnv      = updateDynamicEnvironment (dynamicEnv game) 1 (-1) [],
            player          = updatePlayer (player game) Northwest True
        }
    | S.member (Char 's') (input game) && S.member (Char 'd') (input game) = 
        game {
            exits           = updateExits (exits game) (-1) 1 [],
            staticEnv       = updateStaticEnvironment (staticEnv game) (-1) 1 [],
            dynamicEnv      = updateDynamicEnvironment (dynamicEnv game) (-1) 1 [],
            player          = updatePlayer (player game) Southeast True
        }
    | S.member (Char 's') (input game) && S.member (Char 'a') (input game) = 
        game 
        {
            exits           = updateExits (exits game) 1 1 [],
            staticEnv       = updateStaticEnvironment (staticEnv game) 1 1 [],
            dynamicEnv      = updateDynamicEnvironment (dynamicEnv game) 1 1 [],
            player          = updatePlayer (player game) Southwest True
        }
    | S.member (Char 'w') (input game) = 
        game 
        {
            exits           = updateExits (exits game) 0 (-1) [],
            staticEnv       = updateStaticEnvironment (staticEnv game) 0 (-1) [],
            dynamicEnv      = updateDynamicEnvironment (dynamicEnv game) 0 (-1) [],
            player          = updatePlayer (player game) North True
        }
    | S.member (Char 'a') (input game) = 
        game 
        {
            exits           = updateExits (exits game) 1 0 [],
            staticEnv       = updateStaticEnvironment (staticEnv game) 1 0 [],
            dynamicEnv      = updateDynamicEnvironment (dynamicEnv game) 1 0 [],
            player          = updatePlayer (player game) West True
        }
    | S.member (Char 's') (input game) = 
        game
        {
            exits           = updateExits (exits game) 0 1 [],
            staticEnv       = updateStaticEnvironment (staticEnv game) 0 1 [],
            dynamicEnv      = updateDynamicEnvironment (dynamicEnv game) 0 1 [],
            player          = updatePlayer (player game) South True
        }
    | S.member (Char 'd') (input game) =
        game 
        {
            exits           = updateExits (exits game) (-1) 0 [],
            staticEnv       = updateStaticEnvironment (staticEnv game) (-1) 0 [],
            dynamicEnv      = updateDynamicEnvironment (dynamicEnv game) (-1) 0 [],
            player          = updatePlayer (player game) East True
        }
    | otherwise = 
        game 
        {
            exits           = updateExits (exits game) 0 0 [],
            staticEnv       = updateStaticEnvironment (staticEnv game) 0 0 [],
            dynamicEnv      = updateDynamicEnvironment (dynamicEnv game) 0 0 [],
            player          = updatePlayer (player game) North False
        }

-- Adds user input to the input set in the game object.
manager :: Event -> HalexGame -> HalexGame
manager (EventKey key Down _ _) game  = game {input = S.insert key (input game)}
manager (EventKey key Up _ _) game    = game {input = S.delete key (input game)}
manager _ game                        = game

-- Main Function.
main :: IO ()
main = do

    -------- LOAD RESOURCES --------

    ---- Environment ----

    -- Terrain --

    terrainFiles                <- getFilesInDir "Assets/Sprites/Environment/Terrain/"
    let terrainSprites           = loadSprites terrainFiles

    -- Static Structures --

    staticStructureFiles        <- getFilesInDir "Assets/Sprites/Environment/Structures/Static/"
    let staticStructureSprites   = loadSprites staticStructureFiles

    -- Animated Structures --

    bonfireFiles                <- getFilesInDir "Assets/Sprites/Environment/Structures/Animated/Bonfire/"
    pipeFiles                   <- getFilesInDir "Assets/Sprites/Environment/Structures/Animated/Pipe/"
    treeFiles                   <- getFilesInDir "Assets/Sprites/Environment/Structures/Animated/Tree/"
    wellFiles                   <- getFilesInDir "Assets/Sprites/Environment/Structures/Animated/Well/"

    let dynamicStructureAnims = [
                                    loadSprites bonfireFiles,
                                    loadSprites pipeFiles,
                                    loadSprites treeFiles,
                                    loadSprites wellFiles
                                ]

    -- Static Decorations --

    staticDecorationFiles       <- getFilesInDir "Assets/Sprites/Environment/Decorations/Static/"
    let staticDecorationSprites  = loadSprites staticDecorationFiles

    -- Animated Decorations --

    candlesFiles                <- getFilesInDir "Assets/Sprites/Environment/Decorations/Animated/Candles/"
    jackoFiles                  <- getFilesInDir "Assets/Sprites/Environment/Decorations/Animated/JackO/"
    oilFiles                    <- getFilesInDir "Assets/Sprites/Environment/Decorations/Animated/Oil/"
    seanceFiles                 <- getFilesInDir "Assets/Sprites/Environment/Decorations/Animated/Seance/"

    let dynamicDecorationAnims = [
                                    loadSprites candlesFiles,
                                    loadSprites jackoFiles,
                                    loadSprites oilFiles,
                                    loadSprites seanceFiles
                                ]

    -- Setpieces --

    setpieceFiles               <- getFilesInDir "Assets/Sprites/Environment/Setpieces/"
    let setpieceSprites          = loadSprites setpieceFiles

    ---- Exits ----

    exitFiles                   <- getFilesInDir "Assets/Sprites/Exits/"
    let exitSprites              = loadSprites exitFiles

    ---- Player ----

    -- Idle --

    let playerIdleSprites =     [
                                    loadSprite "Assets/Sprites/Player/Idle/Player_Idle_N.png",
                                    loadSprite "Assets/Sprites/Player/Idle/Player_Idle_S.png",
                                    loadSprite "Assets/Sprites/Player/Idle/Player_Idle_E.png",
                                    loadSprite "Assets/Sprites/Player/Idle/Player_Idle_W.png",
                                    loadSprite "Assets/Sprites/Player/Idle/Player_Idle_NE.png",
                                    loadSprite "Assets/Sprites/Player/Idle/Player_Idle_NW.png",
                                    loadSprite "Assets/Sprites/Player/Idle/Player_Idle_SE.png",
                                    loadSprite "Assets/Sprites/Player/Idle/Player_Idle_SW.png"
                                ]

    -- Walk --

    playerWalkNFiles            <- getFilesInDir "Assets/Sprites/Player/Walk/N/"
    playerWalkSFiles            <- getFilesInDir "Assets/Sprites/Player/Walk/S/"
    playerWalkEFiles            <- getFilesInDir "Assets/Sprites/Player/Walk/E/"
    playerWalkWFiles            <- getFilesInDir "Assets/Sprites/Player/Walk/W/"
    playerWalkNEFiles           <- getFilesInDir "Assets/Sprites/Player/Walk/NE/"
    playerWalkNWFiles           <- getFilesInDir "Assets/Sprites/Player/Walk/NW/"
    playerWalkSEFiles           <- getFilesInDir "Assets/Sprites/Player/Walk/SE/"
    playerWalkSWFiles           <- getFilesInDir "Assets/Sprites/Player/Walk/SW/"

    let playerWalkAnims =       [
                                    loadSprites playerWalkNFiles,
                                    loadSprites playerWalkSFiles,
                                    loadSprites playerWalkEFiles,
                                    loadSprites playerWalkWFiles,
                                    loadSprites playerWalkNEFiles,
                                    loadSprites playerWalkNWFiles,
                                    loadSprites playerWalkSEFiles,
                                    loadSprites playerWalkSWFiles
                                ]


    -------- PLAY GAME --------

    game <- loadGame terrainSprites staticStructureSprites dynamicStructureAnims staticDecorationSprites dynamicDecorationAnims setpieceSprites exitSprites playerIdleSprites playerWalkAnims

    play window background fps game render manager update