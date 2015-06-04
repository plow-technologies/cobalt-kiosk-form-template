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

getTypeAndRecords :: Name -> Q Exp
getTypeAndRecords t = do
  TyConI (DataD _ typeName _ constructors _) <- reify t
  let r = flatten $ map getTypeNameAndRecordName constructors
  return $ TupE [LitE $ StringL $ nameBase typeName, r]

  where
    getTypeName (ConT c) = nameBase c
    returnAsTupleList pairs = ListE $ map (\(x,y) -> TupE [LitE $ StringL x, LitE $ StringL y]) pairs
    getTypeNameAndRecordName r = case r of
      (RecC    name fields) -> returnAsTupleList $ map (\x -> ((getTypeName . extractThird) x, (nameBase . extractFirst) x)) fields
      (NormalC name fields) -> ListE $ map (LitE . StringL) $ map (\x -> (getTypeName . snd) x) fields
      _ -> ListE []
    flatten l = case l of 
      (x:xs) -> x
      _ -> ListE []
    -- getRecordNames (RecC name fields) = ListE $ map (\(x,y) -> TupE [LitE $ StringL x, LitE $ StringL y]) $ map (\x -> ((getTypeName . extractThird) x, (nameBase . extractFirst) x)) fields

haskellTypeToC :: (String, String) -> String
haskellTypeToC ("String",i) = "  char *" ++ i ++ ";\n"
haskellTypeToC ("Int"   ,i) = "  int "    ++ i ++ ";\n"
haskellTypeToC ("Double",i) = "  double " ++ i ++ ";\n"
haskellTypeToC (t       ,i) = "  " ++ (map toLower t) ++ " *" ++ i ++ ";\n"

haskellTypeToMarshall :: String -> String
haskellTypeToMarshall "String" = "CString"
haskellTypeToMarshall "Int"    = "Int"
haskellTypeToMarshall "Double" = "Double"
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
    structName = "} " ++ (map toLower s) ++ ";\n"

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
                    "import Control.Monad\n\n"

    defineInclude = "#include \"" ++ cHeader ++ "\"\n"
    defineAlignment = "#let alignment t = \"%lu\", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)\n\n"
    
    passFreePointerFunction = "foreign export ccall \"freePointerSetInHaskell\" free :: Ptr a -> IO ()\n\n"

haskellDataToHSC :: (String, [(String,String)]) -> String
haskellDataToHSC (x,y) = defineData ++ 
                           (defineHeadRecord y) ++ 
                           derivingShow ++ 
                           (defineInstance x y) ++ "\n" 

                           -- ++ exportCcall ++
                           -- setterFunction
  where
    
    dToC = x ++ "ToC" 
    
    defineData = "data " ++ dToC ++ " = " ++ dToC ++ " {\n"
    defineRecord (t,r) = "  , " ++ r ++ "ToC :: " ++ (haskellTypeToMarshall t) ++ "\n" 
    derivingShow = "} deriving Show\n\n"
    defineInstance t ps = "instance Storable " ++ dToC ++ " where\n" ++
                          "    sizeOf    _ = #{size " ++ (map toLower t) ++ "}\n" ++
                          "    alignment _ = #{alignment " ++ (map toLower t) ++ "}\n" ++
                          "    poke p " ++ (map toLower t) ++ " = do\n" ++
                          (concat $ map (\p -> definePoke t (snd p)) ps) ++
                          "    peek p = return " ++ dToC ++ "\n" ++
                          (concat $ map (\p -> definePeek t (snd p)) ps)
    definePoke t v = "        #{poke " ++ (map toLower t) ++ ", " ++ (map toLower v) ++ "} p $  " ++ v ++ "ToC " ++ (map toLower t) ++ "\n"
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
    
{-
alloca $ \f1 ->
  poke f1 AttributesC
-}


