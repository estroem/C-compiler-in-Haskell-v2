module Id ( Id, funcId, getFuncId, getLoopId, addLoopId, addIfId, popId, incId, getIdString ) where

import Data.List

data IdElem = FuncId String | LoopId Integer | IfId Integer

instance Show IdElem where
    show (FuncId s) = s
    show (LoopId i) = "loop" ++ show i
    show (IfId i) = "if" ++ show i

type Id = ([IdElem], Integer)

funcId :: String -> Id
funcId n = ([FuncId n], 0)

getFuncId :: Id -> String
getFuncId id = case (fst id) !! 0 of
    (FuncId n) -> n
    _ -> error "Id doesn't start with func"

getLoopId :: Id -> Maybe String
getLoopId id = if null id' then Nothing else Just $ getIdString (id', 0)
    where id' = reverse $ dropWhile (\ e -> case e of { LoopId i -> False; _ -> True }) $ reverse $ fst id

addLoopId :: Id -> Id
addLoopId id = ((fst id) ++ [LoopId $ snd id], 0)

addIfId :: Id -> Id
addIfId id = ((fst id) ++ [IfId $ snd id], 0)

popId :: Id -> Id
popId (l, n) = (reverse $ tail $ reverse l, n)

incId :: Id -> Id
incId id = (fst id, (snd id) + 1)

getIdString :: Id -> String
getIdString id = intercalate "." $ map show $ fst id