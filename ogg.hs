{-
 - Ogg
 - Generates
 - Gcode
 -}

data CMD_PRE = LAYER | G | M
data CMD = CMD CMD_PRE Integer Integer | RAW_GCODE String | CMD_EMPTY

chunk :: String -> [String]
chunk str = case (words str) of
                  x:y:z:rest   -> [x, y, z]
                  x:y:rest     -> [x, y, ""]
                  x:rest       -> [x, "", ""]
                  _            -> ["", "", ""]

flatten :: [String] -> String
flatten (x:y:rest) = x ++ y ++ (flatten rest)
flatten [x]        = x
flatten []         = ""

numeric :: String -> Bool
numeric (x:rest) = elem x "0123456789" && (numeric rest)
numeric [] = True

--process_cmd :: String -> [String]
-- takes in a string broken up by spaces
process_cmd :: [String] -> CMD
process_cmd ["l", x, y]  = CMD LAYER (read x :: Integer) (read y :: Integer)
process_cmd ["g", x, y]  = CMD G (read x :: Integer) (read y :: Integer)
process_cmd ["", "", ""] = CMD_EMPTY
process_cmd lst  = RAW_GCODE (flatten lst)

eval_cmd :: CMD -> IO ()
eval_cmd (CMD prefix x y) = case prefix of
                              LAYER -> putStrLn ("layer operation in " ++ show x)
                              G     -> putStrLn ("G" ++ show x)
                              M     -> putStrLn ("M command" ++ show x)

eval_cmd (RAW_GCODE cmdstr) = putStrLn cmdstr
eval_cmd (CMD_EMPTY) = putStrLn "try again"


main = do
      name <- getLine
      --x <- process_cmd (chunk name)
      putStr name
