import qualified Text.Parsec as P

data Packet = List ![Packet] | Raw !Int

instance Show Packet where
  show (List []) = ""
  show (List ps) = "[" ++ concatMap ((++ ",") . show ) ps ++ "]"
  show (Raw n) = show n

    -- from Reddit
parseInput :: P.SourceName -> String -> Either P.ParseError [(Packet, Packet)]
parseInput =
  P.parse $
    ((,) <$> (parse <* P.newline) <*> (parse <* P.newline))
      `P.sepBy1` P.newline
      <* P.eof
  where
    parse :: P.Parsec String () Packet
    parse =
      Raw . read <$> P.many1 P.digit
        P.<|> List <$> P.between (P.char '[') (P.char ']') (parse `P.sepBy` P.char ',')
