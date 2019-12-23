module Stuff exposing (Id, createId, flatId, nextId)


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


nextId : Id -> Id
nextId (Id id) =
    Id (id + 1)
