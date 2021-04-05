{-# LANGUAGE DeriveGeneric, NoImplicitPrelude, OverloadedStrings, OverloadedLabels #-}

module DB.Selda where

import           Database.Selda
import           Database.Selda.SQLite
import           Relude


data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
instance SqlType Pet

data Person = Person
  { name :: Text
  , age  :: Int
  , pet  :: Maybe Pet
  } deriving Generic
instance SqlRow Person


people :: Table Person
people = table "people" [#name :- primary]

main :: IO ()
main = withSQLite "people.sqlite" $ do
  createTable people
  insert_
    people
    [ Person "Velvet"    19 (Just Dog)
    , Person "Kobayashi" 23 (Just Dragon)
    , Person "Miyu"      10 Nothing
    ]

  adultsAndTheirPets <- query $ do
    person <- select people
    restrict (person ! #age .>= 18)
    return (person ! #name :*: person ! #pet)
  liftIO $ print adultsAndTheirPets
