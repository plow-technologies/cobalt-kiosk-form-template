module TestImport ( testTextString
                  , testButtonString
                  , testLabelString
                  , testInputString) where

testTextString :: String
testTextString = "<element attr1='3' attr2='Cat'> Some Text </element>"

testButtonString :: String
testButtonString = "<button action='sendJson'>Send JSON </button>"

testLabelString :: String
testLabelString = "<label>Time Loaded</label>"

testInputString :: String
testInputString = "<input type='text'>Send JSON</input>"
