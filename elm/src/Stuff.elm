module Stuff exposing (Id, createId, nextId)


type Id
    = Id Int


createId : Int -> Id
createId n =
    Id n


flatId : Id -> Int
flatId id =
    case id of
        Id n ->
            n


mapId : (Int -> Int) -> Id -> Id
mapId f id =
    Id << f << flatId <| id


nextId : Id -> Id
nextId id =
    mapId (\x -> x + 1) id
