module Parser.Classic.ReadP.Common ( end, endShow ) where

import Ast ( Ident )
import Parser.Common ( stoken1 )
import Parser.Classic.Words as W ( Keywords( End ) )

import Control.Monad ( void )
import Text.ParserCombinators.ReadP ( ReadP )

end :: Ident -> ReadP ()
end i = void (stoken1 (show W.End) >> stoken1 i)

endShow :: Show a => a -> ReadP ()
endShow = end . show
