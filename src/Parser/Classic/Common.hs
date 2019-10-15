module Parser.Classic.Common ( end, endShow ) where

import Ast ( Ident )
import Parser.ParserM ( ParserM )
import Parser.Common ( BasicError, stoken1 )
import Parser.Classic.Words as W ( Keywords( End ) )

import Control.Monad ( void )

end :: Ident -> ParserM BasicError ()
end i = void (stoken1 (show W.End) >> stoken1 i)

endShow :: Show a => a -> ParserM BasicError ()
endShow = end . show
