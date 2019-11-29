{-
 - Ogg
 - Generates
 - Gcode
 -}
import System.Environment

import Control.Concurrent
import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport


data LAYER_OP = PAUSE | TEMP_SET | CLEAR | ENABLE_DUP | DISABLE_DUP
{- print commands -}
data PRINT_CMD = STARTPRINT | STOPPRINT | GET_POS

data CMD_PRE = LAYER LAYER_OP | PRINT PRINT_CMD
data CMD_ARG = CMD_ARG_STR String
             | CMD_ARG_INT Integer
             | CMD_ARG_EMPTY

data CMD     = CMD CMD_PRE CMD_ARG CMD_ARG | RAW_GCODE String | CMD_EMPTY

data GCODE_ATTR = NONE | AWAIT_RESPONSE
data GCODE_CMD = GCODE_CMD GCODE_ATTR [String]
{-data MACRO   = CMD CMD-}

chunk :: String -> [String]
chunk str = case (words str) of
                  x:y:z:rest   -> [x, y, z]
                  x:y:rest     -> [x, y, ""]
                  x:rest       -> [x, "", ""]
                  _            -> ["", "", ""]

flatten :: [String] -> String
flatten lst = case (filter (\x -> x /= "") lst) of
                    x:[]  -> x
                    x:y:r -> case r of
                                  [] -> x ++ " " ++ y
                                  _  -> x ++ " " ++ flatten r
                    _     -> []

numeric :: String -> Bool
{- empty string is not numeric -}
numeric_float [] = False
numeric_float str = case (sum (map (\c -> case c of '.' -> 1; _ -> 0) str)) of
                        1 -> case reverse str of
                                    ('.':_) -> False
                                    _       -> sum (map (\x -> case x of True -> 0; _ -> 1) (map (\x -> elem x "0123456789") str)) <= 1
                        _    -> False
                  
numeric [] = False
numeric str = foldr1 (&&) (map (\x -> elem x "0123456789") str)

-- takes in a string broken up by spaces
process_cmd :: [String] -> CMD

{- no command starting with the empty string is valid -}
process_cmd ("":_) = CMD_EMPTY
{-process_cmd ['l':ch:_, "", ""] = CMD_EMPTY-}
process_cmd ['l':ch:rest, x, y]  = case ch of
                                    'p' -> case (numeric x) of
                                          {-True -> CMD (LAYER PAUSE) (read x :: Integer) (read y :: Integer)-}
                                          True -> CMD (LAYER PAUSE) (CMD_ARG_INT (read x :: Integer)) CMD_ARG_EMPTY
                                          _    -> RAW_GCODE (flatten ['l':ch:rest, x, y])
                                    't' -> case (numeric x && numeric y) of
                                          True -> CMD (LAYER TEMP_SET) (CMD_ARG_INT (read x :: Integer)) (CMD_ARG_INT (read y :: Integer))
                                          _    -> RAW_GCODE (flatten ['l':ch:rest, x, y])
                                    {-layer temp-}
                                    {-'t' -> -}
                                    _    -> RAW_GCODE (flatten ['l':ch:rest, x, y])

process_cmd ("getpos":_:_) = CMD (PRINT GET_POS) CMD_ARG_EMPTY CMD_ARG_EMPTY

process_cmd ("startprint":"":_) = CMD_EMPTY
process_cmd ["startprint", fname, _] = CMD (PRINT STARTPRINT) (CMD_ARG_STR fname) CMD_ARG_EMPTY
process_cmd ("startprint":_) = CMD_EMPTY
                        
process_cmd lst          = RAW_GCODE (flatten lst)

{- generates serial output 
 - there is no guarantee that eval_cmd returns just one gcode command
 - for each CMD
 -}
{-eval_cmd :: CMD -> [String]-}
eval_cmd :: CMD -> GCODE_CMD
eval_cmd (CMD (LAYER op) (CMD_ARG_INT offset) arg) = case op of
                                    PAUSE       -> GCODE_CMD NONE ["l3 " ++ show offset]
                                    CLEAR       -> GCODE_CMD NONE ["l1"]
                                    ENABLE_DUP  -> GCODE_CMD NONE ["l8"]
                                    DISABLE_DUP -> GCODE_CMD NONE ["l9"]
                                    {-TEMP_SET    -> ["l2 " ++ show offset ++ show arg]-}
                                    TEMP_SET    -> case arg of
                                                      (CMD_ARG_INT temp) -> GCODE_CMD NONE ["l2 " ++ show offset ++ " " ++ show temp]
{- layer op without integer arguments -}
eval_cmd (CMD (LAYER op) _ _) = GCODE_CMD NONE [""]

{- we'll need concurrency to wait for G117 to report back -}
eval_cmd (CMD (PRINT STARTPRINT) (CMD_ARG_STR fname) _) = GCODE_CMD NONE ["M21", "M23 " ++ fname, "M24"]
eval_cmd (CMD (PRINT GET_POS) _ _) = GCODE_CMD AWAIT_RESPONSE ["M114"]
eval_cmd (CMD prefix x y) = case prefix of
                              {-(LAYER op)    -> ["layer operation in " ++ show x]-}
                              (PRINT ptype) -> case ptype of
                                                     STARTPRINT -> GCODE_CMD NONE ["g23 " ++ ""]
                                                     STOPPRINT  -> GCODE_CMD NONE ["M25"]
                              _             -> GCODE_CMD NONE [""]


eval_cmd (RAW_GCODE cmdstr) = GCODE_CMD NONE [cmdstr]
eval_cmd (CMD_EMPTY)        = GCODE_CMD NONE ["try again"]


{-predefined commands-}
{-layer_pause offset = -}

{-parse_str :: String -> CMD-}
gen_gcode :: String -> GCODE_CMD
gen_gcode str = eval_cmd (process_cmd (chunk str))

send_cmds :: SerialPort -> GCODE_CMD -> [IO Int]
send_cmds p (GCODE_CMD attr lst) = case lst of
                       [] -> [return 0]
                       _  -> map (send p) (map B.pack lst)

open_serial :: FilePath -> IO SerialPort
open_serial port = do
                     s <- openSerial port defaultSerialSettings { commSpeed = CS2400 }
                     return s


{-TODO: use maybe-}
await_serial port 0 = return ""
await_serial port timeout  = do
                               putStrLn (show timeout)
                               bytes <- recv port 100
                               let str = B.unpack bytes
                               case str of
                                    "" -> await_serial port (timeout-1)
                                    _  -> return str

{- TODO: 
 - add a concurrent section of code that reads and prints
 - serial data
 -}
repl :: SerialPort -> IO ()
repl port = do
             ln <- getLine
             case ln of
                  "quit" -> do
                              closeSerial port
                              return ()
                  _      -> do
                              let cmds = gen_gcode ln
                              let y = send_cmds port cmds
                              case cmds of 
                                   (GCODE_CMD AWAIT_RESPONSE _) -> do
                                                                     ser_str <- (await_serial port 100)
                                                                     putStrLn ser_str
                                   (GCODE_CMD _ lst) -> putStrLn ("sent command: " ++ (show lst))
                              repl port

main = do
      port <- getArgs
      case port of
           []  -> putStrLn "usage: <serial port>"
           p:r ->  do
                  {-s <- openSerial p defaultSerialSettings { commSpeed = CS2400 }-}
                  conn <- open_serial p
                  repl conn
                  {-
                   -let y = send_cmds conn (gen_gcode "lo")
                   -putStrLn "sent"
                   -}
