{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Control.Monad
import           Data.Char (isSpace)
import           Data.Default.Class
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as L
import           System.IO
import qualified Text.XML as X


import           Paths_acsl_parser


ppNode :: (String -> IO ()) -> X.Node -> IO ()
ppNode pp (X.NodeElement e) = do
  pp $ "NODE    " ++ show (X.elementName e)
ppNode pp (X.NodeInstruction i) = do
  pp $ "INSTRUC " ++ show i
ppNode pp (X.NodeContent c) =
  pp $ "CONTENT " ++ show c
ppNode pp (X.NodeComment c) = do
  pp $ "COMMENT " ++ show c

checkElementName :: X.Element -> X.Name -> IO ()
checkElementName e nm = do
  when (X.elementName e /= nm) $ do
    error $ "Element has name " ++ show (X.elementName e) ++ ", expected " ++ show nm

ppElementNodes :: Int -> X.Element -> IO ()
ppElementNodes c e =
  mapM_ (ppNode (\m -> putStrLn $ replicate c ' ' ++ m)) (X.elementNodes e)

$(pure [])

extractCodeNodeContent :: X.Node -> Either String L.Text
extractCodeNodeContent n =
  case n of
    X.NodeElement e ->
      case X.elementName e of
        "a" -> extractCodeContent e
        "anchor" -> extractCodeContent e
        nm -> Left $ "Unknown line tag  " ++ show nm
    X.NodeContent c -> Right $ L.fromStrict c
    _ -> Left $ "Unexpected line node " ++ show n

extractCodeContent :: X.Element -> Either String L.Text
extractCodeContent e = mconcat <$> (mapM extractCodeNodeContent (X.elementNodes e))


$(pure [])

extractPSTextNodeContent :: X.Node -> Either String L.Text
extractPSTextNodeContent n =
  case n of
    X.NodeElement e ->
      case X.elementName e of
        "line" ->
          case extractCodeContent e of
            Left err -> Left $ "Could not parse line: " ++ err
            Right r -> Right $ r <> "\n"
        nm -> Left $ "Unknown pstext tag  " ++ show nm
    X.NodeContent c | Text.all isSpace c -> Right ""
    _ -> Left $ "Unexpected pstext node " ++ show n

parsePSText :: X.Element -> Either String L.Text
parsePSText e = do
  mconcat <$> mapM extractPSTextNodeContent (X.elementNodes e)

$(pure [])

checkPSAttr ::  X.Name -> Text -> Either String ()
checkPSAttr nm v =
  case nm of
    "sections" ->
      case v of
        "1" -> pure ()
        _ -> Left $ "Unknown sections attribute " ++ show v
    "secttype" ->
      case v of
        "Operation" -> pure ()
        "noheading" -> pure ()
        "Library" -> pure ()
        "Shared Decode" -> pure ()
        _ -> Left $ "Unknown section type " ++ show v
    "enclabels" ->
      case v of
        "" -> pure ()
        _ -> Left $ "Unknown enclabels attribute " ++ show v
    "name" -> pure ()
    "mylink" -> pure ()
    _ -> Left $ "Unknown attribute " ++ show nm ++ " " ++ show v

processPSNode :: X.Node -> Either String [L.Text]
processPSNode n =
  case n of
    X.NodeElement e ->
      case X.elementName e of
        "pstext" ->
          case parsePSText e of
            Left e ->
              Left $ "Error with psnode: " ++ e
            Right r -> do
              pure [r]
              --putStrLn "CODE"
              --putStr (L.unpack r)
        nm -> Left $ "Unknown ps tag  " ++ show nm
    X.NodeContent c | Text.all isSpace c -> Right []
    _ -> Left $ "Unexpected ps node " ++ show n

data PS = PS { pstext :: L.Text }

parsePS :: X.Element -> Either String PS
parsePS e = do
  mapM_ (uncurry checkPSAttr) (Map.toList (X.elementAttributes e))
  code <- mconcat <$> mapM processPSNode (X.elementNodes e)
  case code of
    [t] -> pure PS { pstext = t }
    _ -> Left $ "Expected single pstext element"

$(pure [])

checkPSSectionAttr ::  X.Name -> Text -> Either String ()
checkPSSectionAttr nm v =
  case nm of
    "howmany" -> pure ()
    _ -> Left $ "Unknown attribute " ++ show nm ++ " " ++ show v

ppPSSectionNode :: X.Node -> Either String [PS]
ppPSSectionNode n =
  case n of
    X.NodeElement e ->
      case X.elementName e of
        "ps" -> (:[]) <$> parsePS e
        nm -> Left $ "Unknown ps_section tag  " ++ show nm
    X.NodeContent c | Text.all isSpace c -> pure []
    _ -> Left $ "Unexpected ps_section node " ++ show n

parsePSSection :: X.Element -> Either String [PS]
parsePSSection e = do
  mapM_ (uncurry checkPSSectionAttr) (Map.toList (X.elementAttributes e))
  mconcat <$> mapM ppPSSectionNode (X.elementNodes e)

$(pure [])

ppInstructionClassNode :: X.Node -> Either String [PS]
ppInstructionClassNode n =
  case n of
    X.NodeElement e ->
      case X.elementName e of
        "arch_variants" -> pure []
        "docvars" -> pure []
        "encoding" -> pure []
        "iclassintro" -> pure []
        "ps_section" -> do
          r <- parsePSSection e
          Right r
        "regdiagram" -> pure []
        nm -> Left $ "Unknown instruction class tag  " ++ show nm
    X.NodeContent c | Text.all isSpace c -> pure []
    _ -> Left $ "Unexpected instruction class node " ++ show n

parseInstructionClass :: X.Element -> Either String [PS]
parseInstructionClass e = do
  mconcat <$> mapM ppInstructionClassNode (X.elementNodes e)

$(pure [])

ppInstructionClassesNode :: X.Node -> IO ()
ppInstructionClassesNode n =
  case n of
    X.NodeElement e ->
      case X.elementName e of
        "classesintro" -> pure ()
        "iclass" ->
          case parseInstructionClass e of
            Left e -> error e
            Right r -> do
              when (not (length r `elem` [0,1])) $ do
                putStrLn $ "iclass " ++ show (length r)
        nm -> putStrLn $ "Unknown instruction classes tag  " ++ show nm
    X.NodeContent c | Text.all isSpace c -> pure ()
    _ -> putStrLn $ "Unexpected instruction classes node " ++ show n

parseInstructionClasses :: X.Element -> IO ()
parseInstructionClasses e = do
  mapM_ ppInstructionClassesNode (X.elementNodes e)

$(pure [])

ppInstructionSectionAttr ::  X.Name -> Text -> IO ()
ppInstructionSectionAttr nm v =
  case nm of
    "id" -> pure ()
    "title" -> pure ()
    "type" ->
      case v of
        "alias" -> pure ()
        "instruction" -> pure ()
        "pseudocode" -> pure ()
        _ -> putStrLn $ "Unknown instruction type " ++ show v
    _ -> putStrLn $ "Unknown attribute " ++ show nm ++ " " ++ show v

ppInstructionSectionNode ::  X.Node -> IO ()
ppInstructionSectionNode n =
  case n of
    X.NodeElement e ->
      case X.elementName e of
        "alias_list" -> pure ()
        "aliastablehook" -> pure ()
        "aliasto" -> pure ()
        "classes" ->
          parseInstructionClasses e
        "desc" -> pure ()
        "docvars" -> pure ()
        "explanations" -> pure ()
        "heading" -> pure ()
        "ps_section" -> do
          case parsePSSection e of
            Left e -> do
              putStrLn "SECTION"
              error e
            Right r ->
              pure ()
              --when (length r /= 1) $ do
              --  putStrLn $ "SECTION " ++ show (length r)

        nm -> error $ "Unknown instruction section tag  " ++ show nm
    X.NodeContent c | Text.all isSpace c -> pure ()
    X.NodeComment _ -> pure ()
    _ -> error $ "Unsupported node " ++ show n

parseInstructionSection :: X.Element -> IO ()
parseInstructionSection e = do
  mapM_ (uncurry ppInstructionSectionAttr) (Map.toList (X.elementAttributes e))
  mapM_ ppInstructionSectionNode (X.elementNodes e)

$(pure [])

ppHierarchyAttr ::  X.Name -> Text -> IO ()
ppHierarchyAttr nm v =
  case nm of
    _ -> putStrLn $ "Unknown attribute " ++ show nm ++ " " ++ show v

ppHierarchyNode ::  X.Node -> IO ()
ppHierarchyNode n =
  case n of
    X.NodeElement e ->
      case X.elementName e of
        "regdiagram" -> pure ()
        "node" -> pure ()
        nm -> putStrLn $ "Unknown hierarchy tag  " ++ show nm
    X.NodeContent c | Text.all isSpace c -> pure ()
    X.NodeComment _ -> pure ()
    _ -> putStrLn $ "Unsupported node " ++ show n

parseHierarchy :: X.Element -> IO ()
parseHierarchy e = do
  mapM_ (uncurry ppHierarchyAttr) (Map.toList (X.elementAttributes e))
  mapM_ ppHierarchyNode (X.elementNodes e)

$(pure [])

ppEncodingIndexAttr ::  X.Name -> Text -> IO ()
ppEncodingIndexAttr nm v =
  case nm of
    "instructionset" -> pure ()
    _ -> error $ "Unknown attribute " ++ show nm ++ " " ++ show v

ppEncodingIndexNode ::  X.Node -> IO ()
ppEncodingIndexNode n =
  case n of
    X.NodeElement e ->
      case X.elementName e of
        "hierarchy" ->
          parseHierarchy e
        "groups" -> pure ()
        "maintable" -> pure ()
        "iclass_sect" -> pure ()
        "funcgroupheader" -> pure ()
        nm -> error $ "Unknown encoding index tag  " ++ show nm
    X.NodeContent c | Text.all isSpace c -> pure ()
    X.NodeComment _ -> pure ()
    _ -> error $ "Unsupported node " ++ show n

parseEncodingIndex :: X.Element -> IO ()
parseEncodingIndex e = do
  mapM_ (uncurry ppEncodingIndexAttr) (Map.toList (X.elementAttributes e))
  mapM_ ppEncodingIndexNode (X.elementNodes e)

$(pure [])

ppFileNode ::  X.Node -> IO ()
ppFileNode n =
  case n of
    X.NodeElement e ->
      case X.elementName e of
        "alphaindex" -> pure ()
        "encodingindex" ->
          parseEncodingIndex e
        "constraint_text_mappings" -> pure ()
        "instructionsection" -> do
          parseInstructionSection e
        nm -> putStrLn $ "  " ++ show nm
    X.NodeInstruction i ->
      error $ "Unsupported instruction " ++ show i
    X.NodeContent _ -> pure ()
    X.NodeComment _ -> pure ()


parseFile :: X.Element -> IO ()
parseFile e = do
  mapM_ ppFileNode (X.elementNodes e)

$(pure [])

ppSectionNode ::  X.Node -> IO ()
ppSectionNode n =
  case n of
    X.NodeElement e ->
      case X.elementName e of
        "title" -> pure ()
        "para" -> pure ()
        "file" -> parseFile e
        _ -> do
          error $ "Unexpected node " ++ show (X.elementName e)
    X.NodeInstruction i ->
      error $ "Unsupported instruction " ++ show i
    X.NodeContent _ -> pure ()
    X.NodeComment _ -> pure ()

$(pure [])

parseAllinstrsNode :: X.Node -> IO ()
parseAllinstrsNode n =
  case n of
    X.NodeElement e ->
      case X.elementName e of
        "title" -> pure ()
        "para" -> pure ()
        "file" -> parseFile e
        "sect1" -> do
          mapM_ ppSectionNode (X.elementNodes e)
        _ -> do
          error $ "Unexpected node: " ++ show (X.elementName e)
    X.NodeInstruction i ->  error $ "Unexpected node: " ++ show i
    X.NodeContent _ -> pure ()
    X.NodeComment _ -> pure ()

parseAllinstrs :: X.Element -> IO ()
parseAllinstrs e = do
  checkElementName e "allinstrs"
  forM_ (X.elementNodes e) $ \n -> do
    parseAllinstrsNode n

main :: IO ()
main = do
  d <- X.readFile def "ISA_v82A_A64_xml_00bet3.1_OPT/onebigfile.xml"
  putStrLn "Test suite"
  parseAllinstrs (X.documentRoot d)
