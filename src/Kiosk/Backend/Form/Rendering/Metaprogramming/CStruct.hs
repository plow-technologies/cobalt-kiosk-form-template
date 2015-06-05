{-# LANGUAGE TemplateHaskell #-}

module Kiosk.Backend.Form.Rendering.Metaprogramming.CStruct
  ( getTypeAndRecords
  , createHeaderAndHSC
  ) where

import Language.Haskell.TH
import Control.Monad
import Data.Char (toLower)

extractFirst :: (a, b, c) -> a
extractFirst (a,_,_) = a

extractThird :: (a, b, c) -> c
extractThird (_,_,c) = c

-- AppT ListT
{-
(ConT c)
(AppT ListT (ConT c)) -> "[" ++ nameBase c ++ "]"
-}
getTypeAndRecords :: Name -> Q Exp
getTypeAndRecords t = do
  TyConI (DataD _ typeName _ constructors _) <- reify t
  let r = flatten $ map getTypeNameAndRecordName constructors
  return $ TupE [LitE $ StringL $ nameBase typeName, r]

  where
    -- '[' means list
    getTypeName t = case t of
      (ConT c) -> nameBase c
      (AppT ListT (ConT c)) -> "[" ++ nameBase c
      _ -> ""
    returnAsTupleList pairs = ListE $ map (\(x,y) -> TupE [LitE $ StringL x, LitE $ StringL y]) pairs
    getTypeNameAndRecordName r = case r of
      (RecC    name fields) -> returnAsTupleList $ map (\x -> ((getTypeName . extractThird) x, (nameBase . extractFirst) x)) fields
      (NormalC name fields) -> ListE $ map (LitE . StringL) $ map (\x -> (getTypeName . snd) x) fields
      _ -> ListE []
    flatten l = case l of 
      (x:xs) -> x
      _ -> ListE []
    -- getRecordNames (RecC name fields) = ListE $ map (\(x,y) -> TupE [LitE $ StringL x, LitE $ StringL y]) $ map (\x -> ((getTypeName . extractThird) x, (nameBase . extractFirst) x)) fields

-- List of items is a pointer
-- List of Ptrs is a double pointer
haskellTypeToC :: (String, String) -> String
haskellTypeToC ("String",i) = "  char *"  ++ (map toLower i) ++ ";\n"
haskellTypeToC ("Text"  ,i) = "  char *"  ++ (map toLower i) ++ ";\n"
haskellTypeToC ("Int"   ,i) = "  int "    ++ (map toLower i) ++ ";\n"
haskellTypeToC ("Double",i) = "  double " ++ (map toLower i) ++ ";\n"
haskellTypeToC ("Bool"  ,i) = "  bool "   ++ (map toLower i) ++ ";\n"
haskellTypeToC ('[':"String",i) = "  char **"  ++ (map toLower i) ++ ";\n"
haskellTypeToC ('[':"Text"  ,i) = "  char **"  ++ (map toLower i) ++ ";\n"
haskellTypeToC ('[':"Int"   ,i) = "  int *"    ++ (map toLower i) ++ ";\n"
haskellTypeToC ('[':"Double",i) = "  double *" ++ (map toLower i) ++ ";\n"
haskellTypeToC ('[':"Bool"  ,i) = "  bool *"   ++ (map toLower i) ++ ";\n"
haskellTypeToC ('[':t       ,i) = "  " ++ (map toLower t) ++ " **" ++ (map toLower i) ++ ";\n"
haskellTypeToC (t       ,i) = "  " ++ (map toLower t) ++ " *" ++ (map toLower i) ++ ";\n"

-- mistake in here, not always ToC
haskellTypeToMarshall :: String -> String
haskellTypeToMarshall "String" = "CString"
haskellTypeToMarshall "Text"   = "CString"
haskellTypeToMarshall "Int"    = "Int"
haskellTypeToMarshall "Double" = "Double"
haskellTypeToMarshall "Bool"   = "Bool"
haskellTypeToMarshall "[String" = "[CString]"
haskellTypeToMarshall "[Text"   = "[CString]"
haskellTypeToMarshall "[Int"    = "[Int]"
haskellTypeToMarshall "[Double" = "[Double]"
haskellTypeToMarshall "[Bool"   = "[Bool]"
haskellTypeToMarshall '[':t     = "[Ptr " ++ t ++ "ToC]"
haskellTypeToMarshall t        = "Ptr " ++ t ++ "ToC"

-- first one needs to be without a comma
defineHeadRecord :: [(String,String)] -> String
defineHeadRecord ((t,r):xs) = "    " ++ r ++ "ToC :: " ++ (haskellTypeToMarshall t) ++ "\n" ++ defineRestRecord xs
defineHeadRecord _     = "" 
-- add comma
defineRestRecord :: [(String,String)] -> String
defineRestRecord ((t,r):xs) = "  , " ++ r ++ "ToC :: " ++ (haskellTypeToMarshall t) ++ "\n" ++ defineRestRecord xs
defineRestRecord _     = ""

produceCStruct :: String -> String -> String
produceCStruct d s = typedef ++ d ++ structName
  where
    typedef = "typedef struct {\n"
    structName = "} " ++ (map toLower s) ++ ";\n\n"

haskellDataToCStruct :: (String, [(String,String)]) -> String
haskellDataToCStruct (x,y) = produceCStruct (cTypesToString y) x
  where
    cTypesToString = concat . map haskellTypeToC

hscFileHeader :: String -> String -> String
hscFileHeader mName cHeader = languageExtensions ++
                                defineModule ++
                                defineImports ++
                                defineInclude ++
                                defineAlignment ++
                                passFreePointerFunction
  where
    languageExtensions = "{-# LANGUAGE ForeignFunctionInterface #-}\n" ++
                         "{-# LANGUAGE CPP                      #-}\n"

    defineModule  = "module " ++ mName ++ " where\n\n"

    defineImports = "import Foreign\n"  ++
                    "import Foreign.C\n" ++
                    "import Control.Applicative\n" ++
                    "import Control.Monad\n" ++
                    "import Kiosk.Backend.Form.Attribute\n" ++
                    "import Kiosk.Backend.Form.Attribute.Action\n" ++
                    "import Kiosk.Backend.Form.Attribute.Indexable\n" ++
                    "import Kiosk.Backend.Form.Attribute.Max\n" ++
                    "import Kiosk.Backend.Form.Attribute.Min\n" ++  
                    "import Kiosk.Backend.Form.Attribute.Path\n" ++
                    "import Kiosk.Backend.Form.Attribute.Width\n" ++
                    "import Kiosk.Backend.Form.Element\n\n"

    defineInclude = "#include \"" ++ cHeader ++ "\"\n"
    defineAlignment = "#let alignment t = \"%lu\", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)\n\n"
    
    passFreePointerFunction = "foreign export ccall \"freePointerSetInHaskell\" free :: Ptr a -> IO ()\n\n"

haskellDataToHSC :: (String, [(String,String)]) -> String
haskellDataToHSC (x,y) = case nmt of
                          True -> defineData ++ 
                                  (defineHeadRecord y) ++ 
                                  derivingShow ++ 
                                  (defineInstance x y) ++ "\n" 
                          False -> (defineInstance x y) ++ "\n"
  where
    
    -- need to check if we are making a new type
    nmt = needsMarshallType $ map (haskellTypeToMarshall . fst) y
    
    -- if it is a new type for marshalling, append "ToC" to the end
    -- otherwise use the original type
    dToC = case nmt of
            True -> x ++ "ToC" 
            False -> x
    
    -- if the original type has String, Text or Ptr
    -- then we need to create a new type that can be marshalled to C
    -- we append "ToC" to the end of the original type name
    defineData = "data " ++ dToC ++ " = " ++ dToC ++ " {\n"
    defineRecord (t,r) = "  , " ++ r ++ "ToC :: " ++ (haskellTypeToMarshall t) ++ "\n" 
    derivingShow = "} deriving Show\n\n"

    -- automatically create a Storable instance of the type that will be tied to C
    defineInstance t ps = "instance Storable " ++ dToC ++ " where\n" ++
                          "    sizeOf    _ = #{size " ++ (map toLower t) ++ "}\n" ++
                          "    alignment _ = #{alignment " ++ (map toLower t) ++ "}\n" ++
                          "    poke p " ++ (map toLower t) ++ " = do\n" ++
                          (concat $ map (\p -> definePoke t (snd p)) ps) ++
                          "    peek p = return " ++ dToC ++ "\n" ++
                          (concat $ map (\p -> definePeek t (snd p)) ps)
    
    definePoke t v = case nmt of
                       True  -> "        #{poke " ++ (map toLower t) ++ ", " ++ (map toLower v) ++ "} p $  " ++ v ++ "ToC " ++ (map toLower t) ++ "\n"
                       False -> "        #{poke " ++ (map toLower t) ++ ", " ++ (map toLower v) ++ "} p $  " ++ v ++ " " ++ (map toLower t) ++ "\n"
    definePeek t v = "        `ap` (#{peek "++ (map toLower t) ++ "," ++ (map toLower v) ++ "} p)\n"
    
    {-
    exportCcall = "foreign export ccall \"set" ++ x ++ "\" set" ++ x ++ " :: Ptr " ++ dToC ++ " -> IO ()\n"
    setterFunction = "set" ++ x ++ " :: Ptr " ++ dToC ++ " -> IO ()\n" ++ 
                       "set" ++ x ++ " p = do\n" ++
                       pointers ++
                       "    poke p $ " ++ dToC ++ (concat $ map (\z -> " " ++ z) newVars) ++ "\n" ++
                       "    return ()\n\n"

    dataTypesAndVars = namePointers y 0
    newVars = map snd dataTypesAndVars
    pointers = setPointers dataTypesAndVars
    -}

-- rename Pointer Vars
-- pointers are just for testing
-- want to hand write functions we pass to C  
setPointers  :: [(String,String)] -> String
setPointers (("CString",p):xs) = "    " ++ p ++ " <- newCString \"Test from Haskell\"\n" ++ setPointers xs
setPointers _ = ""

namePointers :: [(String,String)] -> Int -> [(String,String)]
namePointers (("String",_):xs) counter = [("CString", "newC" ++ (show counter))] ++ namePointers xs (counter + 1) 
namePointers (("Text",_):xs) counter = [("CString", "newC" ++ (show counter))] ++ namePointers xs (counter + 1) 
namePointers ((x,y):xs) counter = [(x,"1")] ++ namePointers xs counter 
namePointers _ _ = []

createHeaderAndHSC :: String -> String -> String -> [(String, [(String,String)])] -> IO ()
createHeaderAndHSC cFileName hsc mName ns = do
  writeFile cFile $ "#include \"" ++ cFileName ++ ".h\"\n"
  writeFile cHeader $ (concat $ map haskellDataToCStruct ns)
  writeFile hscFile $ (hscFileHeader mName (cFileName ++ ".h")) ++ (concat $ map haskellDataToHSC ns)
  return ()
  where
    cFile   = "cbits/"   ++ cFileName ++ ".c"
    cHeader = "include/" ++ cFileName ++ ".h"
    hscFile = "src/Kiosk/Backend/Form/Rendering/" ++ hsc ++ ".hsc"
    

-- There are restrictions on what types can be marshalled directly from C
-- If a type does not contain a Ptr or CString, then we don't need to make
-- a new type to match it. If it does then we need a new Haskell type
needsMarshallType :: [String] -> Bool
needsMarshallType ("CString":xs) = True || needsMarshallType xs
needsMarshallType (('P':'t':'r':rest):xs) = True || needsMarshallType xs
needsMarshallType (x:xs) = False || needsMarshallType xs
needsMarshallType _ = False

-- haven't decided to handle this yet, AddressAttributes is basically a synonym for WidthAttribute
-- should address attributes be replace with WidthAttribute or copy it?
--data AddressAttributes = AddressWidth WidthAttribute deriving (Show, Ord, Eq)


