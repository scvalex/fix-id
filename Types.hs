module Types (
        Conf(..)
    ) where

import Database ( Database )

data Conf = Conf { getDatabase :: Database }
