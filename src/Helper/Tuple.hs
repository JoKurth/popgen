module Helper.Tuple
where

fst3 (x, _, _) = x

snd3 (_, y, _) = y

thd3 (_, _, z) = z


omitLast (a, b, _) = (a, b)
