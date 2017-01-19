data Vec3 = Vec3 {
    x :: Float,
    y :: Float,
    z :: Float
} deriving (Show)

add :: Vec3 -> Vec3 -> Vec3
add v1 v2 = Vec3 ((x v1) + (x v2)) ((y v1) + (y v2)) ((z v1) + (z v2))

minus :: Vec3 -> Vec3 -> Vec3
minus v1 v2 = Vec3 ((x v1) - (x v2)) ((y v1) - (y v2)) ((z v1) - (z v2))

data Shape =
    Sphere {
        center :: Vec3,
        radius :: Float
    }
    | Plane {
        point :: Vec3,
        normal :: Vec3
    }

data Ray = Ray {
    startPos :: Vec3,
    direction :: Vec3
} deriving (Show)

data Camera = Camera {
    position :: Vec3,
    front :: Vec3,
    up :: Vec3
}

camera = Camera (Vec3 0 0 (-1)) (Vec3 0 0 1) (Vec3 0 1 0)

viewPlaneCoords = [Vec3 x y 0 | x <- [1 .. 100], y <- [1 .. 80]]

coordToRay :: Vec3 -> Vec3 -> Ray
coordToRay startPos coord = Ray startPos (coord `minus` startPos)

rayToColor :: Ray -> Int
rayToColor _ = 0

main = do
    putStrLn $ show (map (coordToRay (position camera)) viewPlaneCoords)
