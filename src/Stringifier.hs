module Stringifier
  ( toJSON
  ) where

import Parser (Expr(..), Doc(Doc))

toJSON :: Expr -> String
toJSON (Identifier n)   = n
toJSON f@(Function n e (Doc d)) =
  "{\"signature\": " ++ "\"" ++ toSignature f ++ "\"" ++ ", " ++ "\"doc\": " ++ "\"" ++ d ++ "\"" ++ "}"
toJSON (Module n xs)    =
  "{\"name\": " ++ n ++ ", " ++ "\"funcs\": " ++ "[" ++ concat (map toJSON xs) ++ "]" ++ "}"
toJSON _                = error "$ $ $"

toSignature :: Expr -> String
toSignature (Identifier n)   = n
toSignature (Function n e _) =
  n ++ " : " ++ (toSignature e)
toSignature (Arrow e e')     =
  (toSignature e) ++ " -> " ++ (toSignature e')
toSignature _                = error "Not a function"
