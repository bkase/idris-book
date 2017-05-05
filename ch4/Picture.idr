||| Represents shapes
data Shape = ||| A triangle, with its base length and height
             Triangle Double Double
           | ||| A rectangle, with it's length and height
             Rectangle Double Double
           | ||| A circle, with it's radius
             Circle Double
%name Shape shape, shape1, shape2

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

%name Picture pic, pic1, pic2

rect : Picture
rect = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rect)
              (Combine (Translate 35 5 circle)
               (Translate 15 25 triangle))

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

data Biggest = NoTriangle | Size Double

chooseBigger : (biggest0 : Biggest) -> (biggest1 : Biggest) -> Biggest
chooseBigger NoTriangle NoTriangle = NoTriangle
chooseBigger NoTriangle (Size x) = Size x
chooseBigger (Size x) NoTriangle = Size x
chooseBigger (Size x) (Size y) = if x > y then Size x else Size y

biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive (Triangle x y)) = Size $ area (Triangle x y)
biggestTriangle (Primitive (Rectangle x y)) = NoTriangle
biggestTriangle (Primitive (Circle x)) = NoTriangle
biggestTriangle (Combine pic pic1) = let biggest0 = biggestTriangle pic in
                                         let biggest1 = biggestTriangle pic1 in
                                             chooseBigger biggest0 biggest1
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic


