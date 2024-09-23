module Vehicles where

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

data Size = Size Integer Integer deriving (Eq, Show)

data Vehicle2 = Car2 Manufacturer Price | Plane2 Airline Size deriving (Eq, Show)
