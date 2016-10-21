import FurnitureResources



statsList :: [([Char],[[([Char],[Char],Int)]])]
statsList =  sortZ (slHalp training [])

slHalp [] stat = stat
slHalp (room:xs) stat = slHalp xs (generate room stat)



sortZ [] = []
sortZ ((object,[right,below]):xs) = (object,[sortZZ right, sortZZ below]): (sortZ xs)

sortZZ [] = []
sortZZ (x:xs) = insert x (sortZZ xs)

insert x [] = [x]
insert (object1,pos1,freq1) ((object2,pos2,freq2):ys) =
  if freq2<=freq1 then
  (object1,pos1,freq1):(object2,pos2,freq2):ys else
  (object2,pos2,freq2):(insert (object1,pos1,freq1) ys)




findFurnitureUpdate :: [Char] -> [Char] -> [Char] -> [([Char],[[([Char],[Char],Int)]])] -> [([Char],[[([Char],[Char],Int)]])]
findFurnitureUpdate a b c [] =
  if(c == "right") then
  [(a,[[(b,c,1)],[]])] else
  [(a,[[], [(b,c,1)]])]

findFurnitureUpdate a b c ((object, stats):xs) =
  if(object == a) then
  ((object, (updateZ b c stats)):xs) else
  ((object, stats): findFurnitureUpdate a b c xs)

updateZ b c [right, below] =
  if c == "right" then
  [(updateZZ b c right),below] else
  [right,(updateZZ b c below)]

updateZZ b c [] = [(b,c,1)]
updateZZ b c (((nghbour,p,freq)):xs) =
  if (nghbour == b) then
  ((b,p,(freq+1))):xs else
  ((nghbour,p,freq)):(updateZZ b c xs)



generate :: [[[Char]]] -> [([Char],[[([Char],[Char],Int)]])]->[([Char],[[([Char],[Char],Int)]])]
generate [x] stat = genRight x stat
generate (x:y:xy) stat = generate (y:xy) (gen x y stat)

gen x y stat =  genBelow x y (genRight x stat)

genRight [] stat = stat
genRight [x] stat = stat
genRight (x:y:xy) stat = genRight (y:xy) (findFurnitureUpdate x y "right" stat)

genBelow [] [] stat = stat
genBelow (x:xs) (y:ys) stat = genBelow xs ys (findFurnitureUpdate x y "below" stat)



furnishRoom :: Int -> [Char] -> [[[Char]]]
furnishRoom 1 object = [[object]]
furnishRoom n object = furnishRowz 1 n object []

-- furnish n rows given the upper row
-- case first row call furnishFirstRow
-- else call furnishRow
furnishRowz c n object ups =
  if c == 1 then
  row2:(furnishRowz (c+1) n below row2)
  else if(c<n) then
  row1:(furnishRowz (c+1) n below row1)
  else [row1]
  where (row1, row2, below,[r,b]) = (furnishRow 1 n object ups ,furnishFirstRow 1 n object , getPossibleNeighbour b b, getFurnStat object)

-- only for the first row
furnishFirstRow c n object =
  if c < n then
  object:(furnishFirstRow (c+1) n nb)
  else [nb]
  where (nb,[r,b]) = (getPossibleNeighbour r r, getFurnStat object)

-- furnish a row
-- object is the object to be placed in this cell
-- (up:xs) the upper row
furnishRow c n object (up:xs)=
  if c == 1 then
  object: (furnishRow (c+1) n nb2 xs)
  else if c < n then
  object:(furnishRow (c+1) n nb1 xs)
  else [nb1]
  where (nb1,nb2,[r1,b1],[r2,b2]) = (getPossibleNeighbour r1 b2, getPossibleNeighbour r1 r1, getFurnStat object, getFurnStat up)



getFurnStat :: [Char] -> [[([Char],[Char],Int)]]
getFurnStat x = gfs x statsList

gfs x [] = error("Nooooh")
gfs x ((y,stats):xs) =
  if(x == y) then
  stats else
  gfs x xs



getPossibleNeighbour :: [([Char],[Char],Int)] -> [([Char],[Char],Int)] -> [Char]
getPossibleNeighbour x y =
  if((randomZeroToX 1) == 0) then
  xdups !! (randomZeroToX ((length xdups)-1)) else
  ydups !! (randomZeroToX ((length ydups)-1))
  where(xdups, ydups) = (duplz x, duplz y)

duplz [] = []
duplz ((object,_,freq):xs) = (duplzO object freq) ++ (duplz xs)

duplzO object 1 = [object]
duplzO object freq = object: (duplzO object (freq-1))
