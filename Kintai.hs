module Kintai (kintai) where
import Data.List

-- $ last -R -2000 -x -f /var/log/wtmp | grep "shut\|reboot" | awk '{print $6,$7}' > hoge.txt
-- $ tac hoge.txt |  stack exec hoe -- -m Kintai -l Kintai  'kintai'

type Day = String
type Time = String

calcRange :: [(Day, Time)] -> String
calcRange xs = sd ++ ",\t" ++ st
    where (sd,st) = head xs
          (_,et)  = last xs

kintai :: [String] -> [String]
kintai = map calcRange . groupBy p . map f
    where p (d1,_) (d2,_) = d1 == d2
          f r = case (words r) of
                    d:s:[] -> (d,s)

testInput = "\
\  1 09:31\n\
\  1 10:29\n\
\  1 12:22\n\
\  1 12:23\n\
\  1 12:42\n\
\  1 13:08\n\
\  1 13:12\n\
\  1 19:53\n\
\  1 21:34\n\
\  1 21:35\n\
\  2 19:17\n\
\  3 07:51\n\
\  3 07:51\n\
\  3 11:50\n\
\  3 11:50\n\
\  3 11:51\n\
\  3 11:51\n\
\  3 12:02\n\
\  3 21:12\n\
\  3 22:48\n\
\ 23 22:48\n"

--main = do
--    mapM putStrLn $ kintai $ lines testInput
--    return ()
