module Graphics.Declarative.Util.SyntaxText where

import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Bordered hiding (map)
import Text.Highlighting.Kate
import Data.Maybe (fromMaybe)
import Data.Vec2 as Vec2

highlightedHaskell :: String -> Form
highlightedHaskell = highlightedSource monoStyle tango "haskell"
  where monoStyle = defaultTextStyle { fontFamily = "Monospace", fontSize = 8 }

highlightedSource :: TextStyle -> Style -> String -> String -> Form
highlightedSource tstyle style language
  = (renderSource tstyle style) . (highlightAs language)

renderSource :: TextStyle -> Style -> [SourceLine] -> Form
renderSource tstyle style sourceLines
  = append Vec2.down $ map (renderLine tstyle style) sourceLines

-- info: type SourceLine = [Token] = [(TokenType, String)]
renderLine :: TextStyle -> Style -> SourceLine -> Form
renderLine tstyle style []     = text tstyle " "
renderLine tstyle style tokens = append Vec2.right $ map (renderToken tstyle style) tokens

-- info: type Token = (TokenType, String)
renderToken :: TextStyle -> Style -> Token -> Form
renderToken tstyle style (tokenType, string) = text textStyle string
  where
    tokenStyle = fromMaybe defStyle $ lookup tokenType $ tokenStyles style
    defaultCol = maybe (0, 0, 0) fromColor $ defaultColor style
    textStyle = tstyle {
      textColor = maybe defaultCol fromColor $ tokenColor tokenStyle,
      bold = tokenBold tokenStyle,
      italic = tokenItalic tokenStyle
    }
