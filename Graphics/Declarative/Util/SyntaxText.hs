module Graphics.Declarative.Util.SyntaxText where

import Graphics.Declarative.Enveloped
import Graphics.Declarative.Backend.Cairo
import Text.Highlighting.Kate
import Data.Maybe (fromMaybe)

highlightedHaskell :: String -> CairoEGraphic
highlightedHaskell = highlightedSource monoStyle tango "haskell"
  where monoStyle = defaultTextStyle { fontFamily = "Monospace", fontSize = 8 }

highlightedSource :: TextStyle -> Style -> String -> String -> CairoEGraphic
highlightedSource tstyle style language = (renderSource tstyle style) . (highlightAs language)

renderSource :: TextStyle -> Style -> [SourceLine] -> CairoEGraphic
renderSource tstyle style sourceLines = groupBy toBottom $ map (renderLine tstyle style) sourceLines

-- info: type SourceLine = [Token] = [(TokenType, String)]
renderLine :: TextStyle -> Style -> SourceLine -> CairoEGraphic
renderLine tstyle style []     = text tstyle " "
renderLine tstyle style tokens = groupBy toRight $ map (renderToken tstyle style) tokens

-- info: type Token = (TokenType, String)
renderToken :: TextStyle -> Style -> Token -> CairoEGraphic
renderToken tstyle style (tokenType, string) = text textStyle string
  where
    tokenStyle = fromMaybe defStyle $ lookup tokenType $ tokenStyles style
    defaultCol = maybe (0, 0, 0) fromColor $ defaultColor style
    textStyle = tstyle {
      textColor = maybe defaultCol fromColor $ tokenColor tokenStyle,
      bold = tokenBold tokenStyle,
      italic = tokenItalic tokenStyle
    }
