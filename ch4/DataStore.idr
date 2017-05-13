module Main

import Data.Vect

data DataStore : Type where
     MkData : (size: Nat) ->
              (items: Vect size String) ->
              DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

searchStore : DataStore -> String -> Maybe (Integer, String)
searchStore (MkData size items) str =
  case findIndex (isInfixOf str) items of
       Nothing => Nothing
       (Just idx) => Just (finToInteger idx, index idx items)

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "size" "" = Just Size
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand "search" str = Just (Search str)
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
      case integerToFin pos (size store) of
            Nothing => Just ("Out of range\n", store)
            (Just id) => Just (index id (items store) ++ "\n", store)

performSearch : (str : String) -> (store : DataStore) -> Maybe (String, DataStore)
performSearch str store = case searchStore store str of
                               Nothing => Just (str ++ " not found\n", store)
                               (Just (idx, str)) => Just (show idx ++ ": " ++ str ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid command\n", store)
                              (Just (Add str)) => 
                                  Just ("ID " ++ show (size store) ++ "\n", addToStore store str)
                              (Just Size) => 
                                  Just ("Size is " ++ show (size store) ++ "\n", store)
                              (Just (Get pos)) => getEntry pos store
                              (Just (Search str)) => performSearch str store
                              (Just Quit) => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
