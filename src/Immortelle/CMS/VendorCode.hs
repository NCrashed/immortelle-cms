module Immortelle.CMS.VendorCode(
    encodeVendorCode
  , decodeVendorCode
  ) where

import Data.Bifunctor
import Data.Foldable (toList)
import Data.Monoid
import Data.Text (Text, pack)
import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.Text

import Immortelle.CMS.Types

import qualified Data.Set as S
import qualified Data.Text as T

-- | Encode vendor code to short string
encodeVendorCode :: VendorCode -> Text
encodeVendorCode VendorCode{..} = T.intercalate "-" [
    encodeCategory vcodeCategory
  , encodePatination vcodePatination
  , encodeIncrs vcodeIncrustations
  , encodeAuthors vcodeAuthors
  , encodeId vcodeId
  ]
  where
    encodeCategory c = case c of
      PendantLeaf   -> "PL"
      PendantOther  -> "PO"
      Necklace      -> "N"
      Earings       -> "E"
      Bracelet bt   -> "B" <> case bt of
        BraceletNet   -> "N"
        BraceletLace  -> "LC"
        BraceletLeaf  -> "LF"
      Ring          -> "R"
      Hair ht       -> "H" <> case ht of
        HairPinWood    -> "PW"
        HairPinCopper  -> "PC"
        Crest          -> "C"
        Barrette       -> "B"
      Brooch bt     -> "BR" <> case bt of
        BroochUsual   -> "U"
        HatPin        -> "H"
        Fibula        -> "F"
      Bookmark      -> "BO"
      Grand         -> "G"
    encodeColor c = case c of
      Red       -> "R"
      Orange    -> "O"
      Yellow    -> "Y"
      Green     -> "G"
      LightBlue -> "LB"
      Blue      -> "B"
      Magenta   -> "M"
    encodePatination Nothing = "Z"
    encodePatination (Just p) = case p of
      PatinationRainbow cls   -> "R" <> foldMap encodeColor cls
      PatinationAmmonia       -> "A"
      PatinationAmmoniaBlue   -> "AB"
      PatinationSulfur        -> "S"
      PatinationGreen         -> "G"
      StainedGlassPaint cls   -> "SG" <> foldMap encodeColor cls
    encodeAuthor c = case c of
      AuthorOlga   -> "OLG"
      AuthorSveta  -> "SVE"
      AuthorPolina -> "POL"
      AuthorOther  -> "OTH"
    encodeAuthors xs
      | S.null xs = "Z"
      | otherwise = T.intercalate ":" . fmap encodeAuthor . toList $ xs
    encodeStone s = case s of
      Labrador -> "LABR"
      Amethyst -> "AMET"
    encodeIncr i = case i of
      IncrustationGlass cls  -> "G" <> foldMap encodeColor cls
      IncrustationStore stns -> "S"<> foldMap encodeStone stns
      IncrustationPearl      -> "PER"
      IncrustationOther      -> "OTH"
    encodeIncrs xs
      | S.null xs = "Z"
      | otherwise = T.intercalate ":" . fmap encodeIncr . toList $ xs
    encodeId = T.pack . show

-- | Decode vendor code from string
decodeVendorCode :: Text -> Either Text VendorCode
decodeVendorCode = first (pack . parseErrorPretty) . parse vendorCode "" . T.toUpper . T.strip

-- | Parser for vendor code
vendorCode :: Parser VendorCode
vendorCode = do
  vcodeCategory <- category
  _ <- char '-'
  vcodePatination <- (char 'Z' *> pure Nothing) <|> fmap Just patination
  _ <- char '-'
  vcodeIncrustations <- (char 'Z' *> pure S.empty) <|> (S.fromList <$> sepBy incrustation (char ':'))
  _ <- char '-'
  vcodeAuthors <- (char 'Z' *> pure S.empty) <|> (S.fromList <$> sepBy author (char ':'))
  _ <- char '-'
  vcodeId <- identifier
  pure VendorCode{..}
  where
    -- Hand written L(1) prefix parser
    category = label "category" $
          try (string "PL" *> pure PendantLeaf)
      <|> try (string "PO" *> pure PendantOther)
      <|> try (string "N" *> pure Necklace)
      <|> try (string "E" *> pure Earings)
      <|> fmap Bracelet (
              try (string "BN" *> pure BraceletNet)
          <|> try (string "BLC" *> pure BraceletLace)
          <|> try (string "BLF" *> pure BraceletLeaf) )
      <|> try (string "R" *> pure Ring)
      <|> fmap Hair (
              try (string "HPW" *> pure HairPinWood)
          <|> try (string "HPC" *> pure HairPinCopper)
          <|> try (string "HC" *> pure Crest)
          <|> try (string "HB" *> pure Barrette) )
      <|> fmap Brooch (
            try (string "BRU" *> pure BroochUsual)
        <|> try (string "BRH" *> pure HatPin)
        <|> try (string "BRF" *> pure Fibula) )
      <|> try (string "BO" *> pure Bookmark)
      <|> try (string "G" *> pure Grand)
    color = label "color" $
          (char 'R' *> pure Red)
      <|> (char 'O' *> pure Orange)
      <|> (char 'Y' *> pure Yellow)
      <|> (char 'G' *> pure Green)
      <|> (string "LB" *> pure LightBlue)
      <|> (char 'B' *> pure Blue)
      <|> (char 'M' *> pure Magenta)
    patination = label "patination" $
          try (char 'R' *> fmap (PatinationRainbow . S.fromList) (many color))
      <|> try (string "AB" *> pure PatinationAmmoniaBlue)
      <|> try (char 'A' *> pure PatinationAmmonia)
      <|> try (string "SG" *> fmap (StainedGlassPaint . S.fromList) (many color))
      <|> try (char 'S' *> pure PatinationSulfur)
      <|> try (char 'G' *> pure PatinationGreen)
    stone = label "stone" $
          (string "LABR" *> pure Labrador)
      <|> (string "AMET" *> pure Amethyst)
    incrustation = label "incrustation" $
          (char 'G' *> fmap (IncrustationGlass . S.fromList) (many color))
      <|> (char 'S' *> fmap (IncrustationStore . S.fromList) (many stone))
      <|> (string "PER" *> pure IncrustationPearl)
      <|> (string "OTH" *> pure IncrustationOther)
    author = label "author" $
          try (string "OLG" *> pure AuthorOlga)
      <|> (string "SVE" *> pure AuthorSveta)
      <|> (string "POL" *> pure AuthorPolina)
      <|> (string "OTH" *> pure AuthorOther)
    identifier = label "id" $ fmap fromIntegral integer
