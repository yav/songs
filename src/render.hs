import Data.Char(isSpace)
import System.Environment(getArgs)
import System.FilePath
import System.Directory
import Control.Monad
import Data.List

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn "Please provide some files by dropping it on th executable"
            xs -> do is <- findImages (head xs)
                     cs <- findChords (head xs)
                     mapM_ (renderFile is cs) xs

renderFile :: [String] -> [String] -> FilePath -> IO ()
renderFile is cs x =
  do let dir = takeDirectory x
         file = takeFileName x
         newFile = dir </> "html" </> replaceExtension file "html"
     putStrLn ("Converting " ++ show (dropExtension file) ++ " to html")
     txt <- readFile x
     createDirectoryIfMissing True (takeDirectory newFile)
     writeFile newFile (doRender is cs txt)

replaceExt :: String -> FilePath -> FilePath
replaceExt ext file = addExtension ext (dropExtension file) 


findFileNames :: FilePath -> [String] -> IO [String]
findFileNames base dirs =
  do let path = foldr1 (</>) (takeDirectory base : dirs)
     xs <- filterM (doesFileExist . (path </>))=<< getDirectoryContents path
     return (m3ap takeFileName xs)


findImages :: FilePath -> IO [String]
findImages base = findFileNames base [ "html", "images"]
  
findChords :: FilePath -> IO [String]
findChords base = findFileNames base ["html", "chords", "ukulele"]
     
     
     
doRender :: [String] -> [String] -> String -> String
doRender is cs inp = wrapHtml $ randBg is $ addChords cs chords $ songToHtml $ map (map (lineToSyncs . addDash)) parsed
  where parsed = parse inp
        chords = sort $ nub [ c | a <- parsed, b <- a, Chord c <- b ] 


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

addChords chordImgs cs rest = "Chords: " ++ content (unwords cs) ++ "</br>" ++ rest



randBg imgs xs = xs ++ unlines
  [ "<script>"
  , "var imgs = " ++ show imgs
  , "var pick = Math.floor(Math.random() * imgs.length)"
  , "document.getElementsByTagName('body')[0].style.backgroundImage = 'url(images/' + imgs[pick] + ')'" 
  , "</script>"
  ]


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

