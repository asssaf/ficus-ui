module CollectionUtil exposing (..)

-- if the given list is empty, replace with a singleton default, otherwise return unchanged


listWithDefault : a -> List a -> List a
listWithDefault default list =
    listHeadOrDefault default list :: List.drop 1 list



-- return the first element of the list or a default if empty


listHeadOrDefault : a -> List a -> a
listHeadOrDefault default list =
    Maybe.withDefault default (List.head list)
