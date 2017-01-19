data Vec3 = Vec3 {
    x :: Float,
    y :: Float,
    z :: Float
} deriving (Show)

add :: Vec3 -> Vec3 -> Vec3
add v1 v2 = Vec3 ((x v1) + (x v2)) ((y v1) + (y v2)) ((z v1) + (z v2))

data Shape =
    Sphere {
        position :: Vec3,
        radius :: Float
    } | Plane {
        point :: Vec3,
        normal :: Vec3
    }

main = do
    putStrLn $ show ((Vec3 1 2 3) `add` (Vec3 4 5 6))
