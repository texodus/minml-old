
module Unit.AssSpec where

import Test.Hspec

import Unit.Source

spec :: Spec
spec =

    describe "Assumptions (from template)" $
        
        pendingSample "a trivial example"

            "   x : Num   \n\
            \   x = 1     \n\
            \   x + 2     \n"

------------------------------------------------------------------------------
