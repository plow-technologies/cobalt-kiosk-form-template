{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Rendering.XML where

import           Control.Applicative             ((<$>))
import           Data.ByteString                 (ByteString)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text, pack, unwords)
import           Data.Text.Encoding              (encodeUtf8)
import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Element
import           Text.Blaze.Html                 (toHtml)
import           Text.Blaze.Html.Renderer.String

-- | Render Generator

xmlSanitize :: Text -> Text
xmlSanitize = pack . renderHtml . toHtml
-- Rendering a Attribute
renderAttribute :: Attribute -> Text
renderAttribute attr = name attr <>
                        "=" <>
                        val attr


-- Rendering a list of Attributes
renderAttrList :: (AttributeClass a) =>  [a] -> Text
renderAttrList attrs = Data.Text.unwords $ renderAttribute. toAttribute <$> attrs

-- Rendering Label Tag
renderLabel :: Label -> Text
renderLabel (Label txt attrs) = "<label " <> renderAttrList attrs
                                          <> ">"
                                          <> (xmlSanitize txt)
                                          <> "</label>"
-- Render Radio Tag
renderRadio :: Radio -> Text
renderRadio (Radio labl opts qualifiers) = "<radio>"
                                            <> renderLabel labl
                                            <> renderOptionList opts
                                            <> renderOptionQualifierList qualifiers
                                            <> "</radio>"

-- Render Dropdown Tag
renderDropdown :: Dropdown -> Text
renderDropdown (Dropdown lbl opts maybeIn) = "<dropdown>"
                                                    <> renderLabel lbl
                                                    <> renderOptionList opts
                                                    <> maybe "" renderInput maybeIn
                                                    <> "</dropdown>"
-- Render Option Tag
renderOptionList :: [Option] -> Text
renderOptionList = renderList renderOption

renderOption:: Option -> Text
renderOption(Option txt attrs) = "<option " <> renderAttrList attrs
                                                 <> ">"
                                                 <> (xmlSanitize txt)
                                                 <> "</option>"
-- Render OptionQualifier Tag
renderOptionQualifierList :: [OptionQualifier] -> Text
renderOptionQualifierList = renderList renderOptionQualifier

renderOptionQualifier :: OptionQualifier -> Text
renderOptionQualifier (OptionQualifier qualifierChoices qualifierAttributes) = decodeQualifierParts
  where
   decodeQualifierParts = "<option-qualifier " <> renderAttrList qualifierAttributes <> ">"
                          <> renderQualifierChoicesList qualifierChoices
                          <> "</option-qualifier>"
   renderQualifierChoicesList = renderList renderQualifierChoices
   renderQualifierChoices (QualifierLabel l ) = renderLabel l
   renderQualifierChoices (QualifierInput i) = renderInput i


-- Rendering Input Tag
renderInput :: Input -> Text
renderInput (Input inputType attrs) = "<input " <> renderAttrList attrs
                                                <> ">"
                                                <> renderInputType inputType
                                                <> "</input>"

-- Rendering Automatic Input Tag
renderAutoInput :: AutoInput -> Text
renderAutoInput (AutoInput (Input inputType attrs)) = "<auto-input " <> renderAttrList attrs
                                                            <> ">"
                                                            <> renderInputType inputType
                                                            <> "</auto-input>"



-- Rendering Different Input Type
renderInputType :: InputType -> Text
renderInputType (InputTypeText (InputText t)) = t
renderInputType (InputTypeSignature (Signature s)) = s
renderInputType (InputTypeDate (InputDate d)) = d
renderInputType (InputTypeTime (InputTime t)) = t
renderInputType (InputTypeInt (InputInt i)) = pack . show $ i
renderInputType (InputTypeDouble (InputDouble d)) = pack .show $ d

-- Rendering Button Tag
renderButton :: Button -> Text
renderButton (Button txt attrs) = "<button " <> renderAttrList attrs
                                             <> ">"
                                             <> xmlSanitize txt
                                             <> "</button>"
-- Rendering Empty Block
renderEmptyBlock :: EmptyBlock -> Text
renderEmptyBlock _emptyBlock = pack . show $ (""::String)

-- Rendering TableTopHeader Tag
renderTableTopHeader :: TableTopHeader -> Text
renderTableTopHeader  (TableTopHeader txt) = "<tableTopHeader>"  <> xmlSanitize txt
                                                                 <> "</tableTopHeader>"

-- Rendering TableLeftHeader Tag
renderTableLeftHeader :: TableLeftHeader -> Text
renderTableLeftHeader  (TableLeftHeader txt) =  "<tableLeftHeader>" <> xmlSanitize txt
                                                                    <> "</tableLeftHeader>"

-- Rendering Company Tag
renderCompany :: Company -> Text
renderCompany (Company txt attrs) = "<company " <> renderAttrList attrs
                                                <> ">"
                                                <> xmlSanitize txt
                                                <> "</company>"

-- Render Logo Tag
renderLogo :: Logo  -> Text
renderLogo (Logo txt attrs) = "<logo " <> renderAttrList attrs
                                                        <> ">"
                                                        <> xmlSanitize txt
                                                        <> "</logo>"
-- Render Phone Tag
renderPhone :: Phone  -> Text
renderPhone (Phone txt attrs) = "<phone " <> renderAttrList attrs
                                                        <> ">"
                                                        <> txt
                                                        <> "</phone>"
-- Rendering Address Tag
renderAddress :: Address -> Text
renderAddress (Address txt attrs) = "<address " <> renderAttrList attrs
                                                <> ">"
                                                <> txt
                                                <> "</address>"

-- Rendering Constant Tag

renderConstant :: Constant -> Text
renderConstant (Constant txt attrs)  = "<constant " <> renderAttrList attrs
                                                    <> ">"
                                                    <> (xmlSanitize txt)
                                                    <> "</constant>"

-- Rendering a list of Constants
renderConstantList :: [Constant] -> Text
renderConstantList constants = Data.Text.unwords $ renderConstant <$> constants

-- Rendering ItemType
renderItemType :: ItemType -> Text
renderItemType it =
  case it of
       (ItemLabel l)            -> renderLabel l
       (ItemInput i)            -> renderInput i
       (ItemAutoInput i)        -> renderAutoInput i
       (ItemButton b)           -> renderButton b
       (ItemEmptyBlock e)       -> renderEmptyBlock e
       (ItemRadio r)            -> renderRadio r
       (ItemDropdown d)            -> renderDropdown d
       (ItemTableLeftHeader lh) -> renderTableLeftHeader lh
       (ItemTableTopHeader th)  -> renderTableTopHeader th

-- Rendering a list of ItemTypes
renderItemTypeList :: [ItemType] -> Text
renderItemTypeList its = Data.Text.unwords $ renderItemType <$> its

-- Rendering Item tag
renderItem :: Item -> Text
renderItem (Item its attrs) = "<item " <> renderAttrList attrs
                                       <> ">"
                                       <> renderItemTypeList its
                                       <> "</item>"

-- Rendering a list of Items
renderItemList :: [Item] -> Text
renderItemList items = Data.Text.unwords $ renderItem <$> items

-- Rendering Row Tag
renderRow :: Row -> Text
renderRow (Row items attrs) = "<row " <> renderAttrList attrs
                                      <> ">"
                                      <> renderItemList items
                                      <> "</row>"

-- Rendering a list of Rows
renderRowList :: [Row] -> Text
renderRowList rows = Data.Text.unwords $ renderRow <$> rows

-- Rendering Onping Form
renderOnpingForm :: Form -> Text
renderOnpingForm (Form company address logo phone constants rows) = "<form>" <> renderCompany company
                                                                             <> renderAddress address
                                                                             <> renderLogo logo
                                                                             <> renderPhone phone
                                                                             <> renderConstantList constants
                                                                             <> renderRowList rows
                                                                             <> "</form>"

-- Convert The XML Text into ByteString
renderByteString :: Text -> ByteString
renderByteString = encodeUtf8


-- Utility
-- Render List
renderList :: (a -> Text) ->
              [a] -> Text
renderList renderFunction lst  = Data.Text.unwords $ renderFunction <$> lst
