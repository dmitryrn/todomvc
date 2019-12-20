module Todo exposing (Todo, createTodo)

import Stuff exposing (Id)


type alias Todo =
    { text : String
    , id : Id
    }


createTodo : String -> Id -> Todo
createTodo text id =
    { text = text
    , id = id
    }
