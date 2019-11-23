{-
 - Ogg
 - Generates
 - Gcode
 -}

data LAYER_OP = PAUSE | TEMP_SET | CLEAR | ENABLE_DUP | DISABLE_DUP
data CMD_PRE = LAYER LAYER_OP | G | M
data CMD     = CMD CMD_PRE Integer Integer | RAW_GCODE String | CMD_EMPTY
{-data MACRO   = CMD CMD-}

chunk :: String -> [String]
chunk str = case (words str) of
                  x:y:z:rest   -> [x, y, z]
                  x:y:rest     -> [x, y, ""]
                  x:rest       -> [x, "", ""]
                  _            -> ["", "", ""]

flatten :: [String] -> String
flatten (x:y:rest) = x ++ " " ++ y ++ " " ++ (flatten rest)
flatten [x]        = x
flatten []         = ""

numeric :: String -> Bool
numeric (x:rest) = elem x "0123456789" && (numeric rest)
numeric [] = True

--process_cmd :: String -> [String]
-- takes in a string broken up by spaces
process_cmd :: [String] -> CMD
{-process_cmd ['l':ch:rest, x, y]  = CMD LAYER (read x :: Integer) (read y :: Integer)-}
process_cmd ['l':ch:rest, x, y]  = case ch of
                                    'p' -> CMD (LAYER PAUSE) (read x :: Integer) (read y :: Integer)
process_cmd ["g", x, y]  = CMD G (read x :: Integer) (read y :: Integer)
process_cmd ["", "", ""] = CMD_EMPTY
process_cmd lst          = RAW_GCODE (flatten lst)

{- generates serial output -}
eval_cmd :: CMD -> String
eval_cmd (CMD (LAYER op) offset arg) = case op of
                                    PAUSE -> "l3 " ++ show offset
                                    TEMP_SET -> "l2 " ++ show offset ++ show arg
                                    CLEAR -> "l1"
                                    ENABLE_DUP -> "l8"
                                    DISABLE_DUP -> "l9"

eval_cmd (CMD prefix x y) = case prefix of
                              (LAYER op) -> "layer operation in " ++ show x
                              G     -> "G" ++ show x
                              M     -> "M command" ++ show x

eval_cmd (RAW_GCODE cmdstr) = cmdstr
eval_cmd (CMD_EMPTY) = "try again"

{-predefined commands-}
{-layer_pause offset = -}

{-parse_str :: String -> CMD-}

main = do
      name <- getLine
      putStrLn (eval_cmd (process_cmd (chunk name)))
