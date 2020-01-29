-- jack.hs

module Jack where

import Text.Trifecta
import Control.Applicative

parseClass :: Parser String
parseClass = token (string "class")

parseClassAndName :: Parser String
parseClassAndName =
     parseClass
  *> parseIdentifier
  <* symbol "{"
  <* many (noneOf "}")
  <* symbol "}"


parseIdentifier :: Parser String
parseIdentifier =
  let lcharP = oneOf ['a'..'z']
      ucharP = oneOf ['A'..'Z']
      digP = oneOf ['0'..'9']
      uscore = char '_'
      validFirst =
            lcharP
        <|> ucharP
        <|> uscore
      validRest =
            validFirst <|> digP
  in token $ (:)
          <$> validFirst
          <*> many validRest
