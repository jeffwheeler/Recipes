module Data.Recipe.Structure where

import Data.Tree
import Data.Ratio
import Text.Html hiding (name, action)

data Recipe = Ingredient Int String
            | Result { name   :: Maybe String
                     , action :: String
                     , pieces :: [Recipe]
                     , yield  :: Int
                     }
  deriving (Show)

sumIngredients :: String -> [Recipe] -> Recipe
sumIngredients a rs =
  Result { name   = Nothing
         , action = a
         , pieces = rs
         , yield  = sum $ map yield' rs
         }
    where yield' :: Recipe -> Int
          yield' (Ingredient n _) = n
          yield' (Result _ _ _ y) = y

startingWith :: Recipe -> String -> Recipe
startingWith r action = sumIngredients action [r]

-- Needs verification
forYield :: Recipe -> Ratio Int -> [(Ratio Int, Recipe)] 
forYield r@(Ingredient m _) n =
  [(n*(1%m), r)]
forYield r@(Result _ _ _ _) n =
    concatMap f (pieces r)
  where f i@(Ingredient m _) = [(n*(m%yield r), i)]
        f i@(Result _ _ _ _) = concatMap (g $ yield i) (pieces i)
        g y i = map (\(ratio, ingr) -> ((1%y)*ratio, ingr)) (f i)

-- Tree representation
tree :: Recipe -> Tree Recipe
tree i@(Ingredient _ _) =
  Node i []
tree r@(Result _ _ _ _) =
  Node r $ map tree $ pieces r

treeS :: Recipe -> Tree String
treeS i@(Ingredient _ _) =
  Node (show i) []
treeS r@(Result _ _ _ _) =
  Node (show r) $ map treeS $ pieces r

-- HTML representation
recipeGraph :: Recipe -> String
recipeGraph r = renderHtml $ tStyle +++ (table << f r)
  where f i@(Ingredient _ s) = cell $ td << s
        f r@(Result _ _ _ _) = aboves (map f $ pieces r) `beside` (td << action r)

        tStyle = style ! [thetype "text/css"] << (
          "body { background: #ccc; }" ++
          "table { border-collapse: collapse; border: 2px solid #999; }" ++
          "td { background: white; border: 1px solid #999; " ++
            "border-right: 2px solid #333; padding: 5px; }")

