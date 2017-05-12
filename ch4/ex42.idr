import Data.Vect
import Data.Fin

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
     Unicycle : Vehicle Pedal
     Bicycle : Vehicle Pedal
     Tram : Vehicle Electric
     Motorcycle : (fuel: Nat) -> Vehicle Petrol
     Car : (fuel: Nat) -> Vehicle Petrol
     Bus : (fuel: Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car _) = 4
wheels (Bus _) = 4
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2
wheels Tram = 0

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 50

vectTake : (f: Fin n) -> Vect n a -> Vect (finToNat f) a
vectTake FZ _ = []
vectTake (FS k) (x :: xs) = x :: (vectTake k xs)

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case (integerToFin pos n) of
                                Nothing => Nothing
                                (Just idx) => Just ((index idx xs) + (index idx ys))

