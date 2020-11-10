{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage s = case words s of
    "E" : (severity : stamp : rs) -> 
        LogMessage (Error (read severity::Int)) (read stamp::Int) $ unwords rs
    "I" : (stamp : rs) -> LogMessage Info (read stamp::Int) $ unwords rs
    "W" : (stamp : rs) -> LogMessage Warning (read stamp::Int) $ unwords rs
    ws -> Unknown $ unwords ws

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- not total!
timeStamp :: LogMessage -> TimeStamp
timeStamp (LogMessage _ s _) = s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node l key r)
    | timeStamp msg <= timeStamp key = Node (insert msg l) key r
    | otherwise = Node l key (insert msg r)

build :: [LogMessage] -> MessageTree
build = foldl (\acc x -> insert x acc) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l key r) = inOrder l ++ [key] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms =
    let sortedMs = inOrder (build ms);
        significant (LogMessage (Error severity) _ _) = severity >= 50;
        significant _ = False;
        getMsg (LogMessage _ _ s) = s -- not total!
    in map getMsg $ filter significant sortedMs