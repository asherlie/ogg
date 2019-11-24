{-
 - Ogg
 - Generates
 - Gcode
 -}

data LAYER_OP = PAUSE | TEMP_SET | CLEAR | ENABLE_DUP | DISABLE_DUP
{- print commands -}
data PRINT_CMD = STARTPRINT | STOPPRINT

data CMD_PRE = LAYER LAYER_OP | PRINT PRINT_CMD
data CMD_ARG = CMD_ARG_STR String
             | CMD_ARG_INT Integer

data CMD     = CMD CMD_PRE CMD_ARG CMD_ARG | RAW_GCODE String | CMD_EMPTY
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
                                    'p' -> case (numeric x) && (numeric y) of
                                          {-True -> CMD (LAYER PAUSE) (read x :: Integer) (read y :: Integer)-}
                                          True -> CMD (LAYER PAUSE) (CMD_ARG_INT (read x :: Integer)) (CMD_ARG_INT (read y :: Integer))
                                          _    -> CMD_EMPTY
                                    {-layer temp-}
                                    {-'t' -> -}
                                    _  -> CMD_EMPTY
{-process_cmd ["g", x, y]  = CMD G (read x :: Integer) (read y :: Integer)-}
process_cmd ["", "", ""] = CMD_EMPTY
process_cmd lst          = RAW_GCODE (flatten lst)

{- generates serial output 
 - there is no guarantee that eval_cmd returns just one gcode command
 - for each CMD
 -}
eval_cmd :: CMD -> [String]
eval_cmd (CMD (LAYER op) (CMD_ARG_INT offset) (CMD_ARG_INT arg)) = case op of
                                    PAUSE       -> ["l3 " ++ show offset]
                                    TEMP_SET    -> ["l2 " ++ show offset ++ show arg]
                                    CLEAR       -> ["l1"]
                                    ENABLE_DUP  -> ["l8"]
                                    DISABLE_DUP -> ["l9"]
{- layer op without integer arguments -}
eval_cmd (CMD (LAYER op) _ _) = [""]

eval_cmd (CMD prefix x y) = case prefix of
                              {-(LAYER op)    -> ["layer operation in " ++ show x]-}
                              (PRINT ptype) -> case ptype of
                                                     STARTPRINT -> ["g23 " ++ ""]
                                                     STOPPRINT  -> ["M25"]
                              _             -> [""]
                              {-
                               -G     -> "G" ++ show x
                               -M     -> "M command" ++ show x
                               -}
                              {-PRINT ->-}
{-
 -eval_cmd (CMD (PRINT ptype)_ _) = case ptype of
 -                                    STARTPRINT -> "asdh"
 -                                    STOPPRINT -> "asd"
 -}
                                    

eval_cmd (RAW_GCODE cmdstr) = [cmdstr]
eval_cmd (CMD_EMPTY)        = ["try again"]

{-predefined commands-}
{-layer_pause offset = -}

{-parse_str :: String -> CMD-}

main = do
      name <- getLine
      putStrLn (head (eval_cmd (process_cmd (chunk name))))
