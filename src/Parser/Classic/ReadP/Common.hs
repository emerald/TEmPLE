-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Oleks Shturmov, 2020-2021
-- License     :  BSD 3-Clause (see the file LICENSE)
--
-- Maintainer  :  oleks@oleks.info
-----------------------------------------------------------------------------
module Parser.Classic.ReadP.Common ( end, endShow ) where

import Ast ( Ident )
import Parser.Utils.ReadP ( stoken1 )
import Parser.Classic.Words as W ( Keywords( End ) )

import Control.Monad ( void )
import Text.ParserCombinators.ReadP ( ReadP )

end :: Ident -> ReadP ()
end i = void (stoken1 (show W.End) >> stoken1 i)

endShow :: Show a => a -> ReadP ()
endShow = end . show
