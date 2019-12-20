module Todo exposing (Todo, createTodo)

import Stuff exposing (Id)


type alias Todo =
    { text : String
    , id : Id
    , checked : Bool
    }


createTodo : String -> Bool -> Id -> Todo
createTodo text checked id =
    { text = text
    , id = id
    , checked = checked
    }
