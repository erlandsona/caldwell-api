{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Validator where

import Data.List (intercalate)
import Language.Haskell.TH
-- import Language.Haskell.TH.Syntax

listFields :: Name -> Q [Dec]
listFields record = do
    TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify record

    let names = map (\(name,_,_) -> name) fields

    let showField :: Name -> Q Exp
        showField name = [| s |]
            where s = nameBase name

    let showFields :: Q Exp
        showFields = listE $ map showField names

    [d|instance Show $(conT record) where
        show x = intercalate ", " (map ($ x) $showFields)|]


