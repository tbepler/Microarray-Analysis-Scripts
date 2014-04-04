--test for Table module

import qualified Table as Table
import System.Environment
import Data.Typeable

main = interact (\x-> show ((read x) :: Table.Table))
