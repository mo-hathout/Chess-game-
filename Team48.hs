type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])

setBoard :: Board
setBoard =   (White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1),
             Q ('d',1),B ('c',1),N ('b',1),R ('a',1),
             P ('h',2),P ('g',2),P ('f',2),P ('e',2),
             P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,
             [R ('h',8),N ('g',8),B ('f',8),K ('e',8),
              Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
              P ('h',7),P ('g',7),P ('f',7),P ('e',7),
              P ('d',7),P ('c',7),P ('b',7),P ('a',7)])
-- | Convert a piece to its string representation.
pieceToString :: Piece -> String
pieceToString (P _) = "P"
pieceToString (N _) = "N"
pieceToString (B _) = "B"
pieceToString (R _) = "R"
pieceToString (Q _) = "Q"
pieceToString (K _) = "K"

getlocation (P (a,b)) = (a,b)
getlocation (N (a,b)) = (a,b)
getlocation (B (a,b)) = (a,b)
getlocation (R (a,b)) = (a,b)
getlocation (Q (a,b)) = (a,b)
getlocation (K (a,b)) = (a,b)  

iswhite :: Location -> [Piece] -> Bool
iswhite _ [] = False
iswhite (a,b) (h:t) = if getlocation h == (a,b) then True else iswhite (a,b) t

isblack :: Location -> [Piece] -> Bool
isblack _ [] = False
isblack (a,b) (h:t) = if getlocation h == (a,b) then True else isblack (a,b) t


checkpresW :: [Piece] ->Location -> String
checkpresW [] _ = " "
checkpresW (h:t) (x,y) = if (x,y) == getlocation h  then pieceToString h  else checkpresW t (x,y)

checkpresWhelper::[Piece]->[Piece]->Location->String
checkpresWhelper w b (x,y) = if checkpresW w (x,y) /= " " then checkpresW w (x,y) ++ "W" else if checkpresW b (x,y) /= " " then checkpresW b (x,y)++"B" else " "  

print3 = "a  b  c  d  e  f  g  h\n"


--visualizeChessBoard :: Board -> String
visualizeBoard (player, whitePieces, blackPieces) = 
  print3 ++ "8 | "++ ( checkpresWhelper whitePieces blackPieces ('a',8)) ++" | "
   ++( checkpresWhelper whitePieces blackPieces ('b',8)) ++" | " ++ (checkpresWhelper whitePieces blackPieces ('c',8)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('d',8)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('e',8)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('f',8)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('g',8)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('h',8)) ++"\n" ++ "7 | "++ (checkpresWhelper whitePieces blackPieces  ('a',7))  ++" | "++(checkpresWhelper whitePieces blackPieces ('b',7)) ++" | " ++ (checkpresWhelper whitePieces blackPieces ('c',7)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('d',7)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('e',7)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('f',7)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('g',7)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('h',7)) ++ "\n" ++  "6 | "++ (checkpresWhelper whitePieces blackPieces ('a',6)) ++" | "++(checkpresWhelper whitePieces blackPieces ('b',6)) ++" | " ++ (checkpresWhelper whitePieces blackPieces ('c',6)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('d',6)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('e',6)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('f',6)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('g',6)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('h',6)) ++ "\n" ++"5 | "++ (checkpresWhelper whitePieces blackPieces ('a',5)) ++" | "++(checkpresWhelper whitePieces blackPieces ('b',5)) ++" | " ++ (checkpresWhelper whitePieces blackPieces ('c',5)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('d',5)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('e',5)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('f',5)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('g',5)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('h',5)) ++ "\n" ++"4 | "++ (checkpresWhelper whitePieces blackPieces ('a',4)) ++" | "++(checkpresWhelper whitePieces blackPieces ('b',4)) ++" | " ++ (checkpresWhelper whitePieces blackPieces ('c',4)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('d',4)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('e',4)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('f',4)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('g',4)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('h',4)) ++ "\n" ++"3 | "++ (checkpresWhelper whitePieces blackPieces ('a',3)) ++" | "++(checkpresWhelper whitePieces blackPieces ('b',3)) ++" | " ++ (checkpresWhelper whitePieces blackPieces ('c',3)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('d',3)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('e',3)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('f',3)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('g',3)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('h',3)) ++ "\n" ++"2 | "++ (checkpresWhelper whitePieces blackPieces ('a',2)) ++" | "++(checkpresWhelper whitePieces blackPieces ('b',2)) ++" | " ++ (checkpresWhelper whitePieces blackPieces ('c',2)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('d',2)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('e',2)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('f',2)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('g',2)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('h',2)) ++ "\n" ++"1 | "++ (checkpresWhelper whitePieces blackPieces ('a',1)) ++" | "++(checkpresWhelper whitePieces blackPieces ('b',1)) ++" | " ++ (checkpresWhelper whitePieces blackPieces ('c',1)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('d',1)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('e',1)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('f',1)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('g',1)) ++ " | " ++ (checkpresWhelper whitePieces blackPieces ('h',1)) ++ "\n"
 

isEmpty :: Location->[Piece]->Bool
isEmpty (x1,y1) [] = True
isEmpty (x1,y1) (h:t) = if (x1,y1) == getlocation h then False else isEmpty (x1,y1) t

isEmptyH::Location->[Piece]->[Piece]->Bool
isEmptyH (x,y) w b = if isEmpty (x,y) w == True && isEmpty (x,y) b == True then True else False  
 
pathbetween :: Location -> Location -> [Piece] -> [Piece] -> Bool
pathbetween (c,d) (x,y) w b| (iswhite (c,d) w && isblack(x,y) b) || (isblack (c,d) b && iswhite (x,y) w ) = True
                           | (c,d) == (x,y) = True
                           | c == x && d < y && isEmpty (c,d+1) (w++b) = pathbetween (c,d+1) (x,y) w b
                           | c == x && d > y && isEmpty (c,d-1) (w++b) = pathbetween (c,d-1) (x,y) w b
                           | x > c && d == y && isEmpty (succ c,d) (w++b) = pathbetween (succ c,d) (x,y) w b
                           | x < c && d == y && isEmpty (pred c,d) (w++b) = pathbetween (pred c,d) (x,y) w b
                           | x > c && d < y && isEmptyH (succ c, d+1) w b = pathbetween (succ c,d+1) (x,y) w b
                           | x > c && d > y && isEmptyH (succ c, d-1) w b = pathbetween (succ c,d-1) (x,y) w b
                           | x < c && d < y && isEmptyH (pred c, d+1) w b = pathbetween (pred c,d+1) (x,y) w b
                           | x < c && d > y && isEmptyH (pred c, d-1) w b = pathbetween (pred c,d-1) (x,y) w b
                           | otherwise = False


isdiagonalpath :: (Char, Int) -> (Char, Int) -> Bool
isdiagonalpath (a,b) (x,y)| y > b && x > a = isdiagonalpath (succ a, b+1) (x,y) 
                          | y < b && x < a = isdiagonalpath (pred a, b-1) (x,y)
                          | a > x && y > b = isdiagonalpath (pred a, b+1) (x,y)
                          | b > y && x > a = isdiagonalpath (succ a, b-1) (x,y)
                          | otherwise = if (a,b) == (x,y) then True else False

diagonalpathbetween :: Location -> Location -> [Piece] -> [Piece] -> Bool
diagonalpathbetween (c,d) (x,y) w b | (c,d) == (x,y) = True
                                    | y>d && x>c && isEmptyH (succ c,d+1) w b = diagonalpathbetween (succ c,d+1) (x,y) w b 
                                    | y<d && x<c && isEmptyH (pred c,d-1) w b = diagonalpathbetween (pred c,d-1) (x,y) w b
                                    | c>x && y>d && isEmptyH (pred c,d+1) w b = diagonalpathbetween (pred c,d+1) (x,y) w b
                                    | d>y && x>c && isEmptyH (succ c,d-1) w b = diagonalpathbetween (succ c,d-1) (x,y) w b
                                    | otherwise = False



                                        
isLegal :: Piece -> Board -> Location -> Bool
isLegal (Q (a,b)) (player,whitepieces,blackpieces) (x,y)| isdiagonalpath (a,b) (x,y) && diagonalpathbetween (a,b) (x,y) whitepieces blackpieces  = True
                                                        | pathbetween (a,b) (x,y) whitepieces blackpieces  = True
                                                        | otherwise = False
isLegal (R (a,b)) (player,whitepieces,blackpieces) (x,y) = if pathbetween (a,b) (x,y) whitepieces blackpieces == True then True else False
isLegal (N (a,b)) (player,whitepieces,blackpieces) (x,y) = if (x,y) == (succ a,b+2) then True else if (x,y) == (pred a,b+2) then True else if (x,y) == (succ a,b-2) then True else if (x,y) == (pred a,b-2) then False else False
isLegal (B (a,b)) (player,whitepieces,blackpieces) (x,y) = if isdiagonalpath (a,b) (x,y) && diagonalpathbetween (a,b) (x,y) whitepieces blackpieces then True else False
isLegal (K (a,b)) (player,whitepieces,blackpieces) (x,y) | pathbetween (a,b) (x,y) whitepieces blackpieces && (y-b)<=1 = True 
isLegal (P (a,b)) (player,whitepieces,blackpieces) (x,y) | pathbetween (a,b) (x,y) whitepieces blackpieces && (y-b)<=1 = True
allLocations :: [Location]
allLocations = [(x, y) | x <- ['a'..'h'], y <- [1..8]]

suggestMoveH :: Piece-> Board -> [Location]->[Location]
suggestMoveH piece board [] = []
suggestMoveH piece board (h:t) = if isLegal piece board h then h:suggestMoveH piece board t else suggestMoveH piece board t


suggestMove:: Piece -> Board -> [Location]
suggestMove piece board = suggestMoveH piece board allLocations


bubble [] =(0,0)
bubble (h:t) = if m<h then (m,h:ys) else (x,xs)



