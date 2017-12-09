import Data.Char(isSpace)
import System.Environment(getArgs)
import System.FilePath
import System.Directory(createDirectoryIfMissing)


main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn "PLease provide some files by dropping it on th executable"
            xs -> mapM_ renderFile xs

renderFile :: FilePath -> IO ()
renderFile x =
  do let dir = takeDirectory x
         file = takeFileName x
         newFile = dir </> "html" </> replaceExtension file "html"
     putStrLn ("Converting " ++ show (dropExtension file) ++ " to html")
     txt <- readFile x
     createDirectoryIfMissing True (takeDirectory newFile)
     writeFile newFile (doRender txt)

replaceExt :: String -> FilePath -> FilePath
replaceExt ext file = addExtension ext (dropExtension file) 

doRender :: String -> String
doRender = wrapHtml . songToHtml . map (map (lineToSyncs . addDash)) . parse

type Para = [Line]
type Line = [Part]

data Part = Lyrics String | Chord String

parse = parseLines . lines

parseLines xs =
  case break isBlankLine xs of
    ([],[])   -> []
    ([],_:bs) -> parseLines bs
    (as,bs)   -> parsePara as : parseLines bs

isBlankLine = all isSpace

parsePara = map parseLine

parseLine (']' : more) = parseLine more
parseLine xs =
  case break (== '[') xs of
    ([],[])   -> []
    ([],_:bs) -> parseChord bs
    (as,_:bs) -> Lyrics as : parseChord bs
    (as,[])   -> [Lyrics as]

parseChord xs =
  case break (== ']') xs of
    (cs,ds) -> Chord (map cvt cs) : parseLine ds
  where cvt 'b' = '\9837'
        cvt x = x

data Sync = Sync String String


addDash (Lyrics a : Chord b : more)
  | not (null a) && not (isSpace (last a)) = Lyrics (a ++ "-") : Chord b : addDash more
addDash (x : xs) = x : addDash xs
addDash [] = []


lineToSyncs (Chord a : Lyrics b : more) = Sync b a : lineToSyncs more
lineToSyncs (Lyrics a : more) = Sync a "" : lineToSyncs more
lineToSyncs (Chord a : more) = Sync "" a : lineToSyncs more
lineToSyncs []               = []


syncToHtml (Sync a b) = "<div class=\"sync\">" ++
                          "<div class=\"chord\">" ++ content b ++ "</div>"++
                          "<div class=\"lyrics\">" ++ content a ++ "</div>"++
                        "</div\n>"
  where
  content "" = "&nbsp;"
  content xs = concatMap esc xs
  esc c = case c of
            '<' -> "&lt;"
            '>' -> "&gt;"
            '&' -> "&amp;"
            ' ' -> "&nbsp;"
            _  | co > 127 -> "&#" ++ show co ++ ";"
               | otherwise -> [c]
               where co = fromEnum c

lineToHtml xs = concatMap syncToHtml xs ++ "<br>"

paraToHtml xs = "<div class=\"para\">" ++ concatMap lineToHtml xs ++ "</div>\n\n"

songToHtml = concatMap paraToHtml


wrapHtml body = unlines
  [ "<!DOCTYPE HTML>"
  , "<html>"
  , "<head>"
  , "<link rel=\"stylesheet\" href=\"style.css\">"
  , "</head>"
  , "<body>"
  , body
  , "</body>"
  , "</html>"
  ]

