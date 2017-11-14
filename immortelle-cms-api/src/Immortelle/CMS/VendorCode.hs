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
      PendantLeaf   -> "КУЛЛ"
      PendantOther  -> "КУЛД"
      Necklace      -> "КОЛЬ"
      Earings       -> "СЕР"
      Bracelet bt   -> "БРАС" <> case bt of
        BraceletNet   -> "С"
        BraceletLace  -> "КР"
        BraceletLeaf  -> "Л"
      Ring          -> "КОЛЦ"
      Hair ht       -> "ВОЛ" <> case ht of
        HairPinWood    -> "ШПД"
        HairPinCopper  -> "ШПМ"
        Crest          -> "ГР"
        Barrette       -> "ЗАК"
      Brooch bt     -> "БРОШ" <> case bt of
        BroochUsual   -> "ОБ"
        HatPin        -> "БУЛ"
        Fibula        -> "ФИБ"
      Bookmark      -> "ЗАКЛ"
      Grand         -> "ГРАНД"
    encodeColor c = case c of
      Red       -> "К"
      Orange    -> "О"
      Yellow    -> "Ж"
      Green     -> "З"
      LightBlue -> "Г"
      Blue      -> "С"
      Magenta   -> "Ф"
      White     -> "Б"
      Black     -> "Ч"
      Pink      -> "Р"
    encodePatination Nothing = "Н"
    encodePatination (Just p) = case p of
      PatinationRainbow cls   -> "РАД" <> foldMap encodeColor cls
      PatinationAmmonia       -> "А"
      PatinationAmmoniaBlue   -> "АГ"
      PatinationSulfur        -> "С"
      PatinationGreen         -> "З"
      StainedGlassPaint cls   -> "ВКР" <> foldMap encodeColor cls
    encodeAuthor c = case c of
      AuthorOlga   -> "ШЕФ"
      AuthorSveta  -> "СВТ"
      AuthorPolina -> "ПОЛ"
      AuthorOther  -> "ДРУГ"
    encodeAuthors xs
      | S.null xs = "Н"
      | otherwise = T.intercalate ":" . fmap encodeAuthor . toList $ xs
    encodeStone s = case s of
      Labrador -> "ЛАБР"
      Amethyst -> "АМЕТ"
      Quartz -> "КВАР"
      Rauchtopaz -> "ДМКВ"
      Aquamarine -> "АКВА"
      Rhinestone -> "ХРУС"
      Turquoise -> "БИР"
      Peridot -> "ОЛИВ"
    encodeIncr i = case i of
      IncrustationGlass cls         -> "С" <> foldMap encodeColor cls
      IncrustationGlassDichroic cls -> "ДС" <> foldMap encodeColor cls
      IncrustationStone stns        -> "К" <> foldMap encodeStone stns
      IncrustationPearl             -> "ЖЕМ"
      IncrustationPorcelain cls     -> "Ф" <> foldMap encodeColor cls
      IncrustationBone              -> "КОСТ"
      IncrustationOther             -> "ДРУГ"
    encodeIncrs xs
      | S.null xs = "Н"
      | otherwise = T.intercalate ":" . fmap encodeIncr . toList $ xs
    encodeId = T.pack . show . unProductId

-- | Decode vendor code from string
decodeVendorCode :: Text -> Either Text VendorCode
decodeVendorCode = first (pack . parseErrorPretty) . parse vendorCode "" . T.toUpper . T.strip

-- | Parser for vendor code
vendorCode :: Parser VendorCode
vendorCode = do
  vcodeCategory <- category
  _ <- char '-'
  vcodePatination <- (char 'Н' *> pure Nothing) <|> fmap Just patination
  _ <- char '-'
  vcodeIncrustations <- (char 'Н' *> pure S.empty) <|> (S.fromList <$> sepBy incrustation (char ':'))
  _ <- char '-'
  vcodeAuthors <- (char 'Н' *> pure S.empty) <|> (S.fromList <$> sepBy author (char ':'))
  _ <- char '-'
  vcodeId <- identifier
  pure VendorCode{..}
  where
    -- Hand written L(1) prefix parser
    category = label "category" $
          try (string "КУЛЛ" *> pure PendantLeaf)
      <|> try (string "КУЛД" *> pure PendantOther)
      <|> try (string "КОЛЬ" *> pure Necklace)
      <|> try (string "СЕР" *> pure Earings)
      <|> fmap Bracelet (
              try (string "БРАСС" *> pure BraceletNet)
          <|> try (string "БРАСКР" *> pure BraceletLace)
          <|> try (string "БРАСЛ" *> pure BraceletLeaf) )
      <|> try (string "КОЛЦ" *> pure Ring)
      <|> fmap Hair (
              try (string "ВОЛШПД" *> pure HairPinWood)
          <|> try (string "ВОЛШПМ" *> pure HairPinCopper)
          <|> try (string "ВОЛГР" *> pure Crest)
          <|> try (string "ВОЛЗАК" *> pure Barrette) )
      <|> fmap Brooch (
            try (string "БРОШОБ" *> pure BroochUsual)
        <|> try (string "БРОШБУЛ" *> pure HatPin)
        <|> try (string "БРОШФИБ" *> pure Fibula) )
      <|> try (string "ЗАКЛ" *> pure Bookmark)
      <|> try (string "ГРАНД" *> pure Grand)
    color = label "color" $
          (char 'К' *> pure Red)
      <|> (char 'О' *> pure Orange)
      <|> (char 'Ж' *> pure Yellow)
      <|> (char 'З' *> pure Green)
      <|> (char 'Г' *> pure LightBlue)
      <|> (char 'С' *> pure Blue)
      <|> (char 'Ф' *> pure Magenta)
      <|> (char 'Б' *> pure White)
      <|> (char 'Ч' *> pure Black)
      <|> (char 'Р' *> pure Pink)
    patination = label "patination" $
          try (string "РАД" *> fmap (PatinationRainbow . S.fromList) (many color))
      <|> try (string "АГ" *> pure PatinationAmmoniaBlue)
      <|> try (string "А" *> pure PatinationAmmonia)
      <|> try (string "ВКР" *> fmap (StainedGlassPaint . S.fromList) (many color))
      <|> try (char 'С' *> pure PatinationSulfur)
      <|> try (char 'З' *> pure PatinationGreen)
    stone = label "stone" $
          (string "ЛАБР" *> pure Labrador)
      <|> (string "АМЕТ" *> pure Amethyst)
      <|> (string "КВАР" *> pure Quartz)
      <|> (string "ДМКВ" *> pure Rauchtopaz)
      <|> (string "АКВА" *> pure Aquamarine)
      <|> (string "ХРУС" *> pure Rhinestone)
      <|> (string "БИР" *> pure Turquoise)
      <|> (string "ОЛИВ" *> pure Peridot)
    incrustation = label "incrustation" $
          (char 'С' *> fmap (IncrustationGlass . S.fromList) (many color))
      <|> (string "ДС" *> fmap (IncrustationGlassDichroic . S.fromList) (many color))
      <|> (string "КОСТ" *> pure IncrustationBone)
      <|> (char 'К' *> fmap (IncrustationStone . S.fromList) (many stone))
      <|> (char 'Ф' *> fmap (IncrustationPorcelain . S.fromList) (many color))
      <|> (string "ЖЕМ" *> pure IncrustationPearl)
      <|> (string "ДРУГ" *> pure IncrustationOther)
    author = label "author" $
          try (string "ШЕФ" *> pure AuthorOlga)
      <|> (string "СВТ" *> pure AuthorSveta)
      <|> (string "ПОЛ" *> pure AuthorPolina)
      <|> (string "ДРУГ" *> pure AuthorOther)
    identifier = label "id" $ fmap (ProductId . fromIntegral) integer
