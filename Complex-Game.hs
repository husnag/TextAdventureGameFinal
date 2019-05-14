import Data.List
import Data.Char

--declaring the variable
--all the variables are string in this case
type Location = String
type Direction = String
type Thing = String
type Response = String

--we create a path for the game
--we use the location and direction variable
type PathMap = [((Location, Direction), Location)]
--using all the path, we use create a map
paths :: PathMap
paths = [

     --direction is used to enter each room
     --the user can go from room1 to room2 by going north (n)
    (("room1", "n"), "room2"),
    --you can go from room1 to room3 by going east (e)
    (("room1", "e"), "room3"),
    (("room2", "e"), "room4"),
    (("room2", "s"), "room1"),
    (("room3", "n"), "room4"),
    (("room3", "w"), "room1"),
    (("room4", "n"), "room5"),
    (("room4", "w"), "room2"),
    (("room4", "s"), "room3"),
    (("room5", "e"), "room6"),
    (("room5", "s"), "room4"),
    (("room6", "w"), "room5")
    ]

--locationMap is used to locate the user
--the chest and key into one of the room
type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations =  [
    --the user will be room1 at the beginning of the game
    ("myself", "room1"),
    --chest is found in room5
    ("chest", "room5"),
    ("key", "room4"),
    -- This is a hack, so I don't have to add more lists to the "World" state
    ("room1", "alive")
    ]
--creating an interface called World
--that connects the PathMap, LocationMap with Response
type World = (PathMap, LocationMap, Response)
world :: IO (PathMap, LocationMap, Response)
world = return (paths, locations, "")

--this is our main interface
--the user will type in 'main' which will display messages
main :: IO (String)
main = do
--the user will be asked to input their name
    putStrLn "Hello, what's your name?"
    name <- getLine
--the map of the game along with instructions will be displayed
    putStrLn "\nWelcome to the my game!\n"
    putStrLn gameRoomsMap
    putStrLn instructions
    putStr room1
    play_game ( return (paths, locations, ""))
--when the user types 'quit', it will display the message of "Goodbye!"
    return "Goodbye!"

--the map will be displayed to give the user an idea
--of how to get to the other rooms
gameRoomsMap =
  "Map\n" ++
  "    _________\n" ++
  "____| 5 ! 6 |\n" ++
  "| 2 ! 4 |----\n" ++
  "| 1 ! 3 |\n" ++
  "---------\n"

--the instructions are for the user to learn how to use the functions
--of the game
instructions =
    "Enter commands using one or two words.\n" ++
    "Available commands are:\n" ++
    "main               -- to start the game.\n" ++
    "n  s  e  w         -- to go in that direction.\n" ++
    "pick               -- to pick up the item.\n" ++
    "drop               -- to drop the item.\n" ++
    "quit               -- to end the game and quit.\n"


--the game will begin afterwards
--it will ask the user to display the commands
play_game :: IO (World) -> IO (World)

--if the user finds chest and the key - it will display
--the message of the user 'win'
play_game world = do
    (paths, locations, response) <- world
    putStrLn response
    putStrLn ""
    if response == win
       then return (paths, locations, "Quitting.")
    else if response == lost
       then return (paths, locations, "Quitting.")
    else do
--displaying the command
      putStr "command> "
      command <- getLine
--enabling the user to quit if the user inputs it
      if command == "quit"
         then return (paths, locations, "Quitting.")
      else play_game ( return (do_command command paths locations))

--enabling the user to move
--with each direction that they input
move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

--the commands are used for user input
--when the user inputs -'e' it will be taken to the east
--which will be a new room, thus a new location
do_command :: String -> PathMap -> LocationMap -> World
do_command "n" paths locations = go "n" paths locations
do_command "e" paths locations = go "e" paths locations
do_command "s" paths locations = go "s" paths locations
do_command "w" paths locations = go "w" paths locations
do_command "u" paths locations = go "u" paths locations
--we also create a command for picking an item which in our key/chest
do_command "pick" paths locations = pickItem paths locations
--the key will be dropped in the room where the chest is
--therefore the user winning the game
do_command "drop" paths locations = dropItem paths locations
--if the user inputs any other command thats not listed above
-- it will display the message above of an 'invalid input'
do_command _ paths locations = (paths, locations, "Invalid Input!")

--creating the path for the user
--getting the user to find its own location
--once the user is found in which it is in
--it will be able to moved to another new location
go :: String -> PathMap -> LocationMap -> World
go direction paths locations = do
            let my_location = get "myself" locations
            let new_location = move my_location direction paths
            let new_locations = put "myself" new_location locations
            let response = describe new_location new_locations
            (paths, new_locations, response)

--commands to pick up the drop
--we will be focusing on the maps
pickItem :: PathMap -> LocationMap -> World
pickItem paths locations = do
--finding the locations of the user, key and chest
  let my_location = get "myself" locations
  let chest_location = get "chest" locations
  let key_location = get "key" locations
  --if the location of the user is the same as the chest
  if my_location == chest_location then do
--then it will delete the location of the chest and replace it with new location
--of the user
    let new_locations = delete ("chest", chest_location) locations
--this will give us a final_locations where the user and chest are both present
    let final_locations = put "chest" "myself" new_locations
--the user will be informed to pick up the chest
    (paths, final_locations, "You pick up the chest!")
--if the user's location is the same as the key location
  else if my_location == key_location then do
--it will replace the location of the key with a new location
    let new_locations = delete ("key", chest_location) locations
--the final location will be a new location with the user and key
    let final_locations = put "key" "myself" new_locations
--inform the user to pick up the key
    (paths, final_locations, "You pick up the key!")
--if there is no chest or key then it will display the message below
  else (paths, locations, "Nothing to pick up!")


--commands for dropping an item
dropItem :: PathMap -> LocationMap -> World
dropItem paths locations = do
--setting the current location of the user to 'myself'
  let my_location = get "myself" locations
--setting the chest location to 'chest'
  let chest_location = get "chest" locations
--setting the location of the key to 'key'
  let key_location = get "key" locations
--if the location of the chest is the same as myself
  if chest_location == "myself" then do
  --it will replace the location of the chest with new location
    let new_locations = delete ("chest", chest_location) locations
--the final location will consist of both the chest and the user
    let final_locations = put "chest" my_location new_locations
--a message will display telling the user to drop the chest
    (paths, final_locations, "You drop the chest!")
--if the key and the user's location are the same
  else if key_location == "myself" then do
--the locations will be replaced with a new location
    let new_locations = delete ("key", key_location) locations
--the final location consists of the key and the user
    let final_locations = put "key" my_location new_locations
--the user will be informed about dropping the key
    (paths, final_locations, "You drop the key!")
--if there is no key then it will display the message below
  else (paths, locations, "You have nothing to drop!")

-- "get" finds the value of a key in a (key, value) list
get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                     Just result -> result
--if no key is found then display the message
                     Nothing -> "Not found."

put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

--describing the users, chest and key
describe :: Location -> LocationMap -> String
describe new_location locations =
    let here = get "myself" locations
        chest_location = get "chest" locations
        key_location = get "key" locations
--describe helper is used to inform the user if they have won or lost
    in describe_helper here chest_location key_location locations

describe_helper :: Location -> String -> String -> LocationMap -> String
--if the user is in room 3 with the chest and key it will win
describe_helper "room3" "myself" "myself" locations = description "win"
--if the user is room2 without the chest and key; it will display a message lose
describe_helper "room2" _ "myself" locations = description "lose"
--any other rooms, it will display the details below
describe_helper here _ _ locations = description here

description :: Location -> String

win = "Congratulations!! You have recovered the chest and won the game."
lost = "You found the chest, but you don't have the key. You Lost!"


room1 = "You are in a dark, huge room1, standing in the mud.\n" ++
    " It smells rusty and  You can go north or east.\n"

description "win" = win

description "lose" = lost

description "room1" = room1

description "room2" =
    "You are in room 2! There is a Spider on the ceiling \n" ++
    " right in front of you!\n" ++
    "Leave without it noticing you. \n" ++
    "You can go south or east."

description "room3" =
    "You are in a room 3. To the north is the bright room 4. "

description "room4" =
    "You are in room 4.  The exit is to\n" ++
    "the south; there is dark tunnel to\n" ++
    "the north. You can the key on the floor."

description "room5" =
    "There is a giant room 5 here! You have found the chest on the floor!"

description "room6" =
    "This is a small room 6 which is full of small ants crawling."

description someplace = someplace ++ ", and you can't see anything."
