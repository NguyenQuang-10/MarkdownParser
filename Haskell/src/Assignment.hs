{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Use void" #-}
module Assignment (markdownParser, convertADTHTML, saveHtml) where

import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..), ParseError (..), ParseResult (..))
import           Parser ( spaces, string, stringTok, int, isNot, is, noneof, inlineSpace, oneof, eof, failed, char )
import           Control.Applicative (Alternative(..))


-- ============================================================================
-- ================= SUPPORTING CUSTOM PARSERS ================================
-- ============================================================================

-- function notEof
--
-- A parser that will returns an Error if an empty string is the input,
-- Parses nothing otherwise
-- The main purpose is to serve as base case for some parser that recurse
-- without other base cases
-- DOCTEST:
-- >>> parse (notEof) ""
-- Unexpected end of stream

-- >>> parse (notEof) "abc"
-- Result >abc< ()
notEof :: Parser ()
notEof = Parser f
  where
    f "" = Error UnexpectedEof 
    f x  = Result x ()


-- function count
--
-- Parse another parser (p) for a certain number of time (n)
-- Grouping the result into a list
-- Throws an error if parser p cannot be parsed n times 
-- >>> parse (count 5 $ is 'a') "aaaaaa"
-- Result >a< "aaaaa"

-- >>> parse (count 5 $ is 'a') "aaabaa"
-- Unexpected character: "b"

-- >>> parse (count 5 $ is 'a') "aaa"
-- Unexpected end of stream
count :: Int -> Parser a -> Parser [a]
count n p
  | n > 0 = (:) <$> p <*> count (n-1) p
  | otherwise = pure []

-- function indent
--
-- Parse 4 spaces (1 indent)
-- Doctest:
-- >>> parse (indent) "        abc"
-- Result >    abc< "    "
indent :: Parser String
indent = string "    "

-- function notNewline
--
-- Parse 1 character that is not a newline
-- Doctest:
-- >>> parse (notNewline) "abc"
-- Result >bc< 'a'

-- >>> parse (notNewline) "\nabc"
-- Unexpected character: "\n"
notNewline :: Parser Char
notNewline = isNot '\n'

-- function notInlineSpace
--
-- Parse 1 character that is not a inline space (spaces that are not \n)
-- Doctest:
-- >>> parse (notNewline) "abc"
-- Result >bc< 'a'

-- >>> parse (notNewline) "\tabc"
-- Result >abc< '\t'
notInlineSpace :: Parser Char
notInlineSpace = noneof "\t\r\f\v "

-- function notInlineSpace
--
-- Like inlineSpace but there must be at least 1 inline space character
-- Doctest:
-- >>> parse (inlineSpace1) "abc"
-- Unexpected character: "a"

-- >>> parse (notNewline) "\tabc"
-- Result >abc< '\t'
inlineSpace1 :: Parser String
inlineSpace1 = some (oneof "\t\r\f\v ")

-- function lineEnding
--
-- Parse either a EOF or a \n character with preceding inline spaces
-- Doctests:
-- >>> parse ( many notNewline <* lineEnding) "line\n"
-- Result >< "line"

-- >>> parse ( many notNewline <* lineEnding) "line"
-- Result >< "line"
lineEnding :: Parser ()
lineEnding = (inlineSpace *> eof) <|> (() <$ inlineSpace <* is '\n')

-- function constraint
--
-- Apply a parser but only return sucessfully if the result of that parser
-- returns a true when passed into a validation function f
-- Otherwise return a Failed Constraint error
-- Doctest
-- >>> parse (constraint (>3) int) "5"
-- Result >< 5

-- >>> parse (constraint (>3) int) "2"
-- Failed constraint
constraint :: (a -> Bool) -> Parser a -> Parser a
constraint f parser = parser >>= validate
  where
    validate val
     | f val = pure val
     | otherwise = failed FailedConstraint

-- function parseAnyUntil
--
-- Parse a parser zero or more time until the stopString is encountered/successfuly parsed by 
-- the parser 'string', accumulating result in a list
-- DOCTESTS:
-- >>> parse (parseAnyUntil char "|") "|"
-- Result >< ""

-- >>> parse (parseAnyUntil char "|") "abc defg|"
-- Result >< "abc defg"
parseAnyUntil :: Parser a -> String -> Parser [a]
parseAnyUntil parser stopString = f
  where
    f = ([] <$ (inlineSpace <* string stopString))  <|> ((:) <$> parser) <*> f


-- function parseSomeUntil
--
-- Parse a parser once or more time until the stopString is encountered/successfuly parsed by 
-- the parser 'string', accumulating result in a list
-- DOCTESTS:
-- >>> parse (parseSomeUntil char "|") "|"
-- Unexpected end of stream

-- >>> parse (parseSomeUntil char "|") "hello|"
-- Result >< "hello"
parseSomeUntil :: Parser a -> String -> Parser [a]
parseSomeUntil parser stopString = f
  where
    f = (:[]) <$> parser <* inlineSpace <* string stopString  <|> ((:) <$> parser) <*> f


-- function parseBraces
--
-- Parse the open string, then parses all character until the close string is encounterd
-- Accumulating result into a string
-- DOCTESTS:
-- >>> parse (parseBraces "[" "]") "[  string inside braces  ] string outside"
-- Result > string outside< "string inside braces"
parseBraces :: String -> String -> Parser String
parseBraces open close = stringTok open *> parseSomeUntil notNewline close

-- ========================================================================================
-- ============================= PARSER FOR TEXT MODIFIERS ================================
-- ========================================================================================
-- Notes:
--    You might need to read throught the ADT description before any of the
--    following parsers
-- ========================================================================================

-- function parseMod
-- returns a parsers that parse for the string enclosed in a modifier, removing trailing white space inside the modifier
-- Return parse error if the string doesn't start with modifier passed in, there must a string inside modifier
-- See the below functions for relevant doctests
parseMod :: String -> Parser [RichText]
parseMod modifier = stringTok modifier *> parseSomeUntil parseNestedMod modifier

parseFootnoteNumber :: Parser Int
parseFootnoteNumber = string "[^" *> int <* string "]"

-- >>> parse parseFootnote "[^15]"
-- Result >< Footnote 15
parseFootnote :: Parser RichText
parseFootnote = Footnote <$> parseFootnoteNumber

-- >>> parse parseItalic "_ Hello  _ world"
-- Result > world< Italic [Regular 'H',Regular 'e',Regular 'l',Regular 'l',Regular 'o']
parseItalic :: Parser RichText
parseItalic = Italic <$> parseMod "_"

-- >>> parse parseBold "** Hello * ** world"
-- Result > world< Bold [Regular 'H',Regular 'e',Regular 'l',Regular 'l',Regular 'o',Regular ' ',Regular '*']
parseBold :: Parser RichText
parseBold = Bold <$> parseMod "**"

-- >>> parse parseStkthr "~~ Hello ~~ ~~ world"
-- Result > ~~ world< Strikethrough [Regular 'H',Regular 'e',Regular 'l',Regular 'l',Regular 'o']
parseStkthr :: Parser RichText
parseStkthr = Strikethrough <$> parseMod "~~"

parseInlCode :: Parser RichText
parseInlCode = InlineCode <$> parseMod "`"

--- >>> parse parseLink "[link text](url text)"
-- Result >< <a href="url text">link text</a>
parseLink :: Parser RichText
parseLink = Link <$> parseBraces "[" "]" <*> parseBraces "(" ")"

regularChar :: Parser RichText
regularChar = Regular <$> notNewline


parseNestedMod :: Parser RichText
parseNestedMod =  parseItalic <|> parseStkthr <|> parseBold <|> parseLink <|>  parseFootnote <|> regularChar

--- >>> parse parseRichText "["
-- Result >< [

-- >>> parse parseRichText ""
-- Unexpected end of stream
parseRichText :: Parser RichText
parseRichText = parseItalic <|> parseStkthr <|> parseBold <|> parseInlCode <|> parseLink <|> parseFootnote <|> regularChar

-- apply a parser with an applicative effect until string end or the stopper parser parsed a valid output
-- parsing an empty string will return an Error, otherwise it cause infinite recursion

-- recursiveParse :: Parser a -> Parser b -> Parser [a]
-- recursiveParse parser stopper = notEof >> f
--   where
--     f = (stopper *> pure []) <|> (((:) <$> parser) <*> f )  <|> pure []
-- -- recursive call to recursiveParse looks ugly ngl -- ask a tutor for feedback
-- -- recursiveParse with stopper: recursiveParse parser stopper = (((:) <$> parser) <*> recursiveParse parser stopper) <|> (stopper *> pure [])


  -- f
  -- where
  --   f :: Parser [a]
  --   f =  (((:) <$> parser) <*> f) <|> (is '\n' *> pure [])


-- >>> parse parseFreeTextBlock "**Tonight  ** the _music_ _ seems so loud [^1], I wish that we could ~~ ~~lose this~~ `crowd`, [Baby it's better this way](bruh.com)\n :)"
-- Result > :)< <p><strong>Tonight</strong> the <em>music</em> _ seems so loud <sup><a id="fn1ref" href="#fn1">1</a></sup>, I wish that we could <del>~~lose this</del> <code>crowd</code>, <a href="bruh.com">Baby it's better this way</a></p>

-- >>> parse parseFreeTextBlock "Hello"
-- Result >< <p>Hello</p>

manyRichText :: Parser [RichText]
manyRichText = many parseRichText <* inlineSpace <* lineEnding

parseFreeTextBlock :: Parser Block
parseFreeTextBlock = FreeText <$> manyRichText

-- >>> parse parseImage "![  Alt text ](www.google.com  \"Caption Text\"  )    \n"
-- Result >< <img src="www.google.com" alt="Alt text" title="Caption Text">
parseImage :: Parser Block
parseImage = Images <$> (spaces *> is '!' *> parseBraces "[" "]") <*> ( spaces *> is '(' *> some notInlineSpace <* inlineSpace1) <*> (parseBraces "\"" "\"" <* spaces <* is ')' <*  inlineSpace <* is '\n')

parseRef :: Parser Block
parseRef = Reference <$> (inlineSpace *> parseFootnoteNumber <* is ':') <*> (inlineSpace *> many notNewline <* lineEnding)

parseHeadingLevel :: Parser Int
parseHeadingLevel = constraint (<=6) (length <$> (inlineSpace *> some (is '#') <* inlineSpace1))

parseInlHeading :: Parser Block
parseInlHeading = (Heading <$> parseHeadingLevel) <*> manyRichText
-- remember the alternate

parseAltHeading1 :: Parser Block
parseAltHeading1 = Heading 1 <$> (manyRichText <* spaces <* atLeast 2 (is '=') <* lineEnding)


-- >>> parse (parseHeading) "Alt Heading 1\n===============\n"
-- Result >< Heading 1 [Regular 'A',Regular 'l',Regular 't',Regular ' ',Regular 'H',Regular 'e',Regular 'a',Regular 'd',Regular 'i',Regular 'n',Regular 'g',Regular ' ',Regular '1']
parseAltHeading2 :: Parser Block
parseAltHeading2 = Heading 2 <$> (manyRichText <* spaces <* atLeast 2 (is '-') <* lineEnding)

parseHeading :: Parser Block
parseHeading = parseInlHeading <|> parseAltHeading1 <|> parseAltHeading2

-- >>> parse parseBlockquote "> This is a block quote.\n> It can **span** multiple lines."
-- Result >< <blockquote>
--     <p>This is a block quote.</p>
--     <p>It can <strong>span</strong> multiple lines.</p>
-- </blockquote>
parseBlockquote :: Parser Block
parseBlockquote = Blockquote <$> some (inlineSpace *> is '>' *> spaces *> parseFreeTextBlock)

--- >>> parse markdownParser "```\nmain :: IO ()\nmain = do\nputStrLn \"Never gonna give you up\"\nputStrLn \"Never gonna let you down\"\nputStrLn \"Never gonna run around and desert you\"\n```\n"
-- Result >< Markdown [<pre><code>    main :: IO ()
--     main = do
--     putStrLn "Never gonna give you up"
--     putStrLn "Never gonna let you down"
--     putStrLn "Never gonna run around and desert you"
-- <BLANKLINE>
-- </code></pre>
-- ]
parseCode :: Parser Block
parseCode = do
  lang <- inlineSpace *> string "```" *> many notNewline <* is '\n'
  code <- parseAnyUntil char "\n```" <* inlineSpace <* lineEnding
  pure (Code lang code)


-- >>> parse (parseListItem 2) "        2. **Bold** text\n"
-- Result ><                     <li><strong>Bold</strong> text</li>

-- >>> parse (parseListItem 1) "    2. **Bold** text\n        1. Item in sublist\n        2. Second item in sublist\n4. Outside of list item"
-- Result >4. Outside of list item<             <li><strong>Bold</strong> text
--                 <ol>
--                     <li>Item in sublist</li>
--                     <li>Second item in sublist</li>
--                 </ol>
--             </li>

-- >>> parse (parseListItem 0) "2. **Bold** text\n    1. Sublist\n        2. Sub-sublist\n            3. Sub-sub-sublist\n"
-- Result ><     <li><strong>Bold</strong> text
--         <ol>
--             <li>Sublist
--                 <ol>
--                     <li>Sub-sublist
--                         <ol>
--                             <li>Sub-sub-sublist</li>
--                         </ol>
--                     </li>
--                 </ol>
--             </li>
--         </ol>
--     </li>

parseListItem:: Int -> Parser ListItem
parseListItem level = do
  _ <- count level indent
  _ <- int
  _ <- is '.'
  _ <- spaces
  richText <- manyRichText
  ListItem richText <$> many (parseListItem $ level+1);

-- >>> parse parseOrderedList "2. **Bold** text\n    1. Sublist\n        2. Sub-sublist\n            3. Sub-sub-sublist\n    1. Sublist item 2\n4.Main list item 2" 
-- Result >< 
-- <ol>
--     <li><strong>Bold</strong> text
--         <ol>
--             <li>Sublist
--                 <ol>
--                     <li>Sub-sublist
--                         <ol>
--                             <li>Sub-sub-sublist</li>
--                         </ol>
--                     </li>
--                 </ol>
--             </li>
--             <li>Sublist item 2</li>
--         </ol>
--     </li>
--     <li>Main list item 2</li>
-- </ol>

parseOrderedList :: Parser Block
parseOrderedList = OrderedList <$> some (parseListItem 0);

atLeast :: Int -> Parser a -> Parser [a]
atLeast times parser = (++) <$> count times parser <*> many parser

-- >>> parse (parseTableRichText) "Cell | Remaining text |"
-- Result > Remaining text |< [C,e,l,l]

-- >>> parse (parseTableRichText) "\n | Remaining text |"
-- Unexpected character: "\n"
parseTableRichText :: Parser [RichText]
parseTableRichText = inlineSpace *> parseAnyUntil parseRichText "|"

-- >>> parse (parseManyCell) "| I don't know if this is **legal** or not please no deduction :) | Remaining text |"
-- Result >< [[I, ,d,o,n,',t, ,k,n,o,w, ,i,f, ,t,h,i,s, ,i,s, ,<strong>legal</strong>, ,o,r, ,n,o,t, ,p,l,e,a,s,e, ,n,o, ,d,e,d,u,c,t,i,o,n, ,:,)],[R,e,m,a,i,n,i,n,g, ,t,e,x,t]]
parseManyCell :: Parser [[RichText]]
parseManyCell = spaces *> is '|' *> some parseTableRichText <* lineEnding

-- >>> parse (parseBodyRow 3) "| Cell 1 | **Cell 2** | ~~Cell 3~~ |"
-- Result >< Row [[C,e,l,l, ,1],[<strong>Cell 2</strong>],[<del>Cell 3</del>]]

-- >>> parse (parseBodyRow 2) "| Cell 1 | **Cell 2** | ~~Cell 3~~ |\n"
-- Unexpected end of stream

-- >>> parse (parseBodyRow 4) "| Cell 1 | **Cell 2** | ~~Cell 3~~ |\n"
-- Unexpected end of stream
parseBodyRow :: Int -> Parser Row
parseBodyRow len = Row <$> constraint ((==len) . length) parseManyCell;

-- >>> parse (parseHeadRow) " |  | Cell 2 | Cell 3|  \n |----|---|---|\n"
-- Result > |----|---|---|
-- < Row [[],[C,e,l,l, ,2],[C,e,l,l, ,3]]
parseHeadRow :: Parser Row
parseHeadRow = Row <$> parseManyCell;


--- >>> parse (parseHeadLen) "|---|------|-----|---|----|\n"
-- Result >< 5

--- >>> parse (parseHeadLen) "|---|--|---|---|"
-- Unexpected character: "-"
parseHeadLen :: Parser Int
parseHeadLen = do
  _   <- inlineSpace
  _   <- is '|'
  let f = (inlineSpace *> atLeast 3 (is '-') <* inlineSpace <* is '|') *> ((+1) <$> f) <|>  (0 <$ (inlineSpace *> is '\n') )
  f


--- >>> parse (parseTableHead) " |  | Cell 2 | Cell 3|  \n |----|---|---|\n"
-- Result >< Row [[],[C,e,l,l, ,2],[C,e,l,l, ,3]]

parseTableHead :: Parser Row
parseTableHead = do
  (Row cells) <- spaces *> parseHeadRow
  _           <- constraint (==length cells) parseHeadLen
  pure (Row cells)

--- >>> parse (parseTable) "|  | Head 2 | Head 3|  \n |----|---|---|\n | Cell 11| Cell 12 | Cell 13 | \n | Cell 21| Cell 22 | Cell 23 |"
-- Result >< <table>
--     <thead>
--         <tr>
--             <td></td>
--             <td>Head 2</td>
--             <td>Head 3</td>
--         </tr>
--     </thead>
--     <tbody>
--         <tr>
--             <td>Cell 11</td>
--             <td>Cell 12</td>
--             <td>Cell 13</td>
--         </tr>
--         <tr>
--             <td>Cell 21</td>
--             <td>Cell 22</td>
--             <td>Cell 23</td>
--         </tr>
--     </tbody>
-- </table>
parseTable :: Parser Block
parseTable = do
  (Row headers) <- parseTableHead
  rows <- some (parseBodyRow (length headers))
  pure $ Table (Row headers) rows



-- parseTableHead :: Parser Row
-- parseTableHead = do 
--   (Row cells) <- parseRow
--   (Row dividers) <- 
-- >>> parse parseBlock "Dude"
-- Result >< <p>Dude</p>

-- >>> parse (parseBlock <* is '\n') "Hello"
-- Unexpected end of stream
parseBlock :: Parser Block
parseBlock = notEof >> (parseTable <|> parseOrderedList <|> parseCode <|> parseBlockquote <|> parseHeading <|> parseRef <|> parseImage <|> parseFreeTextBlock)

data RichText = Regular Char
              | Italic [RichText]
              | Bold [RichText]
              | Strikethrough [RichText]
              | Link String String
              | InlineCode [RichText]
              | Footnote Int
  deriving (Show, Eq)

data ListItem = ListItem [RichText] [ListItem]
  deriving (Show, Eq)

data Row = Row [[RichText]]
  deriving (Show, Eq)

data Block = FreeText [RichText]
           | Images String String String
           | Reference Int String
           | Heading Int [RichText]
           | Blockquote [Block]
           | Code String String
           | OrderedList [ListItem]
           | Table Row [Row]
  deriving (Show, Eq)

data ADT = Markdown [Block]
  -- Your ADT **must** derive Show.
  deriving (Show, Eq)

--- >>> parse markdownParser "**Tonight  ** the _music_ _ seems so loud [^1], I wish that we could ~~ ~~lose this~~ `crowd`, [Baby it's better this way](bruh.com)"
-- Result >< Markdown [<p><strong>Tonight</strong> the <em>music</em> _ seems so loud <sup><a id="fn1ref" href="#fn1">1</a></sup>, I wish that we could <del>~~lose this</del> <code>crowd</code>, <a href="bruh.com">Baby it's better this way</a></p>
-- ]

-- >>> parse markdownParser "**I**\n_have_\n~~no\nmouth~~\n`and`\nI must \nscream"
-- Result >< Markdown [<p><strong>I</strong></p>
-- ,<p><em>have</em></p>
-- ,<p>~~no</p>
-- ,<p>mouth~~</p>
-- ,<p><code>and</code></p>
-- ,<p>I must </p>
-- ,<p>scream</p>
-- ]

-- >>> parse markdownParser "line[^1] \n  ![ Alt text] (link \"cap tion\")  \n[^1]:   Hello \nk"
-- Result >< Markdown [<p>line<sup><a id="fn1ref" href="#fn1">1</a></sup> </p>
-- ,<img src="link" alt="Alt text" title="cap tion">
-- ,<p id="fn1">Hello </p>
-- ,<p>k</p>
-- ]

-- >>> parse markdownParser "### Heading 3\n ###### **Strong** heading 6 \n ******* Not a heading"
-- Result >< Markdown [<h3><p>Heading 3</p>
-- </h3>
-- ,<h6><p><strong>Strong</strong> heading 6 </p>
-- </h6>
-- ,<p> <strong>*</strong>** Not a heading</p>
-- ]

-- >>> parse markdownParser "> This is a block quote.\n> It can **span** multiple lines."
-- Result >< Markdown [<blockquote>
--     <p>This is a block quote.</p>
--     <p>It can <strong>span</strong> multiple lines.</p>
-- </blockquote>
-- ]


-- >>> parse markdownParser "\n\n\n"
-- Result >< Markdown [<p></p>
-- ,<p></p>
-- ,<p></p>
-- ]
markdownParser :: Parser ADT
markdownParser = Markdown <$> (spaces *> many parseBlock <* spaces)

-- >>> parse (many (notEof >> parseBlock)) "bing\n"
-- Result >< [FreeText [Regular 'b',Regular 'i',Regular 'n',Regular 'g']]
getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H_%M_%S" <$> getCurrentTime

textModHtml :: RichText -> String
textModHtml (Regular c) =  [c]
textModHtml (Italic str) = "<em>" ++ concatMap textModHtml str ++ "</em>"
textModHtml (Bold str) = "<strong>" ++ concatMap textModHtml str ++ "</strong>"
textModHtml (Strikethrough str) = "<del>" ++ concatMap textModHtml str ++ "</del>"
textModHtml (Link text url) = "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"
textModHtml (InlineCode str) = "<code>" ++ concatMap textModHtml str ++ "</code>"
textModHtml (Footnote num) = "<sup><a id=\"fn" ++ show num ++ "ref\" href=\"#fn" ++ show num ++ "\">" ++ show num ++ "</a></sup>"


getProgLang :: String -> String
getProgLang str
  | not (null str) = " class=\"language-" ++ str ++ "\""
  | otherwise = ""

-- >>> repeatStr 2 "    "
-- "        "
repeatStr :: String -> Int -> String
repeatStr str times
  | times > 0 = str ++ repeatStr str (times-1)
  | otherwise = ""

indentLevel :: Int -> String
indentLevel = repeatStr "    "

addIndents :: Int -> String -> String
addIndents lvl = (indentLevel lvl++)


-- >>> indentAll "Line 1\nLine2" 
-- "    Line 1\n    Line2\n"
indentAll :: Int -> String -> String
indentAll lvl str = concatMap (addIndents lvl . (++"\n")) (lines str)


showSublist :: [ListItem] -> String
showSublist list
  | not (null list) = indentAll 1 $ "<ol>\n" ++ indentAll 1 (concatMap listItemHtml list)  ++ "</ol>\n"
  | otherwise = ""

listItemHtml :: ListItem -> String
listItemHtml (ListItem richText sublist) = "<li>" ++ concatMap textModHtml richText ++ "</li>\n" ++ showSublist sublist

cellHtml:: String -> [RichText] -> String
cellHtml tag txts =  "<" ++ tag ++ ">" ++ concatMap textModHtml txts ++ "</" ++ tag ++">\n"


rowHtml :: String -> Row -> String
rowHtml tag (Row txts) =  "<tr>\n" ++ indentAll 1 (concatMap (cellHtml tag) txts) ++ "</tr>\n"

showTableHead :: Row -> String
showTableHead h =  "<thead>\n" ++ indentAll 1 (rowHtml "th" h) ++ "</thead>\n"

showTableBody :: [Row] -> String
showTableBody rows = "<tbody>\n" ++  indentAll 1 (concatMap (rowHtml "td") rows) ++ "</tbody>\n"


blockHtml :: Block -> String
blockHtml (FreeText list) = indentAll 1 $ "<p>" ++ concatMap textModHtml list ++ "</p>\n"
blockHtml (Images alt url cap) = indentAll 1 $ "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ cap ++ "\">\n"
blockHtml (Reference num str) = indentAll 1 $ "<p id=\"fn" ++ show num ++ "\">" ++ str ++ "</p>\n"
blockHtml (Heading lvl list) = indentAll 1 $ "<h" ++ show lvl ++ ">" ++ concatMap textModHtml list ++"</h" ++  show lvl ++ ">\n"
blockHtml (Blockquote list) = indentAll 1 $ "<blockquote>\n" ++ concatMap (addIndents 1 . blockHtml) list ++ "</blockquote>\n"
blockHtml (Code lang code) = addIndents 1 "<pre><code" ++ getProgLang lang ++ ">" ++ code  ++ "\n" ++  addIndents 1 "</code></pre>\n"
blockHtml (OrderedList list) = showSublist list;
blockHtml (Table headRow rows) = indentAll 1 $ "<table>\n" ++  (indentAll 1 . showTableHead) headRow ++ (indentAll 1 . showTableBody) rows ++  "</table>\n"


htmlPrefix :: String
htmlPrefix = "<!DOCTYPE html>\n\
\<html lang=\"en\">\n\n\
\<head>\n\
\    <meta charset=\"UTF-8\">\n\
\    <title>Test</title>\n\
\</head>\n\n\
\<body>\n"

htmlPostfix :: String
htmlPostfix = "</body>\n\n</html>\n"

convertADTHTML :: ADT -> String
convertADTHTML (Markdown blocks) = htmlPrefix ++ concatMap blockHtml blocks ++  htmlPostfix

saveHtml :: String -> IO ()
saveHtml html = do
  time <- getTime
  let fileName = "./" ++ time ++ ".html"
  writeFile fileName html
  putStrLn "HTML Saved"
