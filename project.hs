import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Except
import System.IO
import System.Environment
import Data.List

data AST = Assign String String
          | Assignfloat String String
          | Assignarray String String
          | Assignchar String String
          | If AST AST AST AST
          | Iflog AST String AST AST AST AST
          | Sequence AST AST
          | Floatnum Int Int
          | Relatnl String String Int
          | Main String AST
          | Return String AST
          | Print String
          | Printval String String
          | While AST AST AST AST
          | Whilelog AST String AST AST AST AST
          | Openbraces Char
          | Closebraces Char
          | Ifnot AST AST AST AST
          | Whilenot AST AST AST AST
          | Incdec String String
          | Fileopen String String String String
          deriving(Show, Read)


incldStmnts = do
  string "#include"
  spaces
  char '<'
  many1 letter
  char '.'
  many1 letter
  char '>'
  spaces


fileP :: GenParser Char st AST
fileP = do
  many incldStmnts
  prog <- exprP
  eof
  return prog

exprP = do
  e <- exprP'
  rest <- optionMaybe exprP
  return (case rest of
    Nothing   -> e
    Just e' -> Sequence e e')

exprP' = do
  spaces
  t <- termP
  spaces
  return t

--restSeqP = do
--  char ';'
--  exprP


termP = try fileopenP
     <|> try assignarrayP
     <|> try assignfloatP
     <|> try assignP
     <|> try assign1P
     <|> try assign2P
     <|> try assign3P
     <|> try assigncharP
     <|> try mainP
     <|> try ifP
     <|> try iflogP
     <|> try returnP
     <|> try relatnlP
     <|> try printP
     <|> try printvalP
     <|> try whileP
     <|> try whilelogP
     <|> try ifnotP
     <|> try whilenotP
     <|> try incdecP
     <?> error "Failed here" 


assignP = do
  many $ string "int"
  many $ char ' '
  x<- many1 letter
  many1 $ char ' '
  char '='
  many1 $ char ' '
  many $ char '\''
  rtvalue<- many1 alphaNum `sepBy` (char '+')
  many $ char '\''
  char ';'
  return $ Assign x (intercalate "+" rtvalue)

assign1P = do
  many $ string "int"
  many $ char ' '
  x<- many1 letter
  many1 $ char ' '
  char '='
  many1 $ char ' '
  many $ char '\''
  rtvalue<- many1 alphaNum `sepBy` (char '-')
  many $ char '\''
  char ';'
  return $ Assign x (intercalate "-" rtvalue)

assign2P = do
  many $ string "int"
  many $ char ' '
  x<- many1 letter
  many1 $ char ' '
  char '='
  many1 $ char ' '
  many $ char '\''
  rtvalue<- many1 alphaNum `sepBy` (char '*')
  many $ char '\''
  char ';'
  return $ Assign x (intercalate "*" rtvalue)

assign3P = do
  many $ string "int"
  many $ char ' '
  x<- many1 letter
  many1 $ char ' '
  char '='
  many1 $ char ' '
  many $ char '\''
  rtvalue<- many1 alphaNum `sepBy` (char '/')
  many $ char '\''
  char ';'
  return $ Assign x (intercalate "*" rtvalue)

assigncharP = do
  string "char"
  many1 $ char ' '
  x<- many1 letter
  many1 $ char ' '
  char '='
  many1 $ char ' '
  many $ char '\''
  rtvalue<- many1 alphaNum `sepBy` (char '/')
  many $ char '\''
  char ';'
  return $ Assignchar x (intercalate "*" rtvalue)


assignfloatP = do
  string "float"
  many1 $ char ' '
  x <- many1 letter
  many $ char ' '
  char '='
  many $ char ' '
  v1 <- many1 digit `sepBy` (char '.')
  --char '.'
  --v2 <- many1 digit `sepBy` (char ';')
  char ';'
  return $ Assignfloat x (intercalate "." v1)

assignarrayP = do
  typ <- many1 letter
  many1 $ char ' '
  x <- many1 letter
  many $ char ' '
  char '['
  len <- many1 digit
  char ']'
  many $ char ' '
  char '='
  many $ char ' '
  char '{'
  element <- many1 digit `sepBy` (char ',')
  char '}'
  char ';'
  return $ Assignarray x (intercalate "," element)

ifP = do
  --spaces
  string "if"
  spaces
  string "("
  cond <- exprP
  char ')'
  spaces
  strt <- bracesopenP
  e1 <- exprP
  spaces
  end <- bracescloseP
  return $ If cond strt e1 end

iflogP = do
  --spaces
  string "if"
  spaces
  string "("
  string "("
  cond <- exprP
  string ")"
  many $ char ' '
  logical <- try (string "&&") <|> try (string "||")
  many $ char ' ' 
  string "("
  cond1 <- exprP
  string ")"
  string ")"
  spaces
  strt <- bracesopenP
  e1 <- exprP
  spaces
  end <- bracescloseP
  return $ Iflog cond logical cond1 strt e1 end

ifnotP = do
  string "if"
  many1 $ char ' '
  string "(!"
  string "("
  cond <- exprP
  string "))"
  spaces
  strt <- bracesopenP
  e1 <- exprP
  end <- bracescloseP
  return $ Ifnot cond strt e1 end

bracesopenP = do
  char '{'
  return $ Openbraces '{'

bracescloseP = do
  char '}'
  return $ Closebraces '}'

mainP = do
  string "int main()"
  spaces
  strt <- bracesopenP
  return $ Main "def main():" strt

returnP = do
  string "return 0;"
  spaces
  end <- bracescloseP
  return $ Return "if __name__ == '__main__': main()" end

relatnlP = do
  many $ char ' '
  a <- many1 letter
  many $ char ' '
  opr <- relatnlOpr
  many $ char ' '
  b <- many1 digit
  many $ char ' '
  return $ Relatnl a opr (read b)

relatnlOpr = do
  ch <- try (string "<=") <|> try (string ">=") <|> try (string ">") <|> try (string "<") <|> try (string "==") <|> try (string "!=")
  return $ ch


printP = do
  many $ char ' '
  string "printf("
  char '"'
  msg <- many letter `sepBy` (char ' ')
  char '"'
  char ')'
  char ';'
  return $ Print (intercalate " " msg)

printvalP = do
  many $ char ' '
  string "printf("
  char '"'
  msg <- many letter `sepBy` (char ' ') 
  char '%'
  many1 $ letter
  char '"'
  char ','
  var <- many1 letter
  char ')'
  char ';'
  return $ Printval (intercalate " " msg) var  

whileP = do
  string "while"
  many $ char ' '
  char '('
  cond <- exprP
  char ')'
  spaces
  strt <- bracesopenP
  stmnt <- exprP
  end <- bracescloseP
  return $ While cond strt stmnt end

whilelogP = do
  --spaces
  string "while"
  spaces
  string "("
  string "("
  cond <- exprP
  string ")"
  many $ char ' '
  logical <- try (string "&&") <|> try (string "||")
  many $ char ' ' 
  string "("
  cond1 <- exprP
  string ")"
  string ")"
  spaces
  strt <- bracesopenP
  e1 <- exprP
  spaces
  end <- bracescloseP
  return $ Whilelog cond logical cond1 strt e1 end

whilenotP = do
  string "while"
  many1 $ char ' '
  string "(!"
  string "("
  cond <- exprP
  string "))"
  spaces
  strt <- bracesopenP
  e1 <- exprP
  end <- bracescloseP
  return $ Whilenot cond strt e1 end

incdecP = do
  var <- many1 $ letter
  oprtn <- try (string "++") <|> try (string "--")
  char ';'
  return $ Incdec var oprtn

fileopenP = do
  string "FILE *"
  pntr <- many1 letter
  string ";"
  spaces
  string pntr
  many $ char ' '
  char '='
  many $ char ' '
  string "fopen("
  string "\""
  name <- many1 letter
  char '.'
  typ <- many1 letter
  char '\"'
  many $ char ' '
  char ','
  many $ char ' '
  char '\"'
  mode <- many1 $ letter
  char '\"'
  string ");"
  return $ Fileopen pntr name typ mode



evaluate (Assignfloat x y) = do
  putStrLn (x ++ " = " ++y)
evaluate (Assign var val) = do
  putStrLn(var ++ " = "++val)
evaluate (Assignarray var element) = putStrLn(var ++ " = [" ++ element ++ "]")
evaluate (Assignchar var val) = do
  putStrLn(var ++ " = "++"'"++val++"'")
evaluate (Floatnum v1 v2) = putStrLn (show(v1)++"."++show(v2))
evaluate (Sequence e1 e2) = do
  evaluate e1 
  evaluate e2
evaluate (If e1 strt e2 end) = do
  putStr ("if ")
  evaluate e1 
  putStrLn (": ")
  evaluate strt
  evaluate e2
  evaluate end
evaluate (Iflog e1 logical e2 strt e3 end) = do
  putStr ("if ((")
  evaluate e1
  putStr (") ")
  case logical of
    "&&" -> putStr("and")
    "||" -> putStr ("or")
  putStr (" (")
  evaluate e2
  putStrLn (")): ")
  evaluate strt
  evaluate e3
  evaluate end

evaluate (Main e strt) = do
  putStrLn (e)
  evaluate strt 
evaluate (Return e end) = do
  evaluate end
  putStrLn (e)
evaluate (Relatnl a opr b) = 
  putStr (a ++" " ++opr ++ " " ++show(b))
evaluate (Print msg) = putStrLn ("print \"" ++ msg ++"\"")
evaluate (Printval msg var) = putStrLn ("print \"" ++ msg ++" \""++" + str("++var++")")
evaluate (While cond strt stmnt end) = do
  putStr ("while ")
  evaluate cond 
  putStrLn (": ")
  evaluate strt
  evaluate stmnt
  evaluate end
evaluate (Whilelog e1 logical e2 strt e3 end) = do
  putStr ("while ((")
  evaluate e1
  putStr (") ")
  case logical of
    "&&" -> putStr("and")
    "||" -> putStr ("or")
  putStr (" (")
  evaluate e2
  putStrLn (")): ")
  evaluate strt
  evaluate e3
  evaluate end   
evaluate (Openbraces strt) = putStrLn(strt:[])
evaluate (Closebraces end) = putStrLn(end:[])
evaluate (Ifnot e1 strt e2 end) = do
  putStr("if not(")
  evaluate e1
  putStrLn("): ")
  evaluate strt
  evaluate e2
  evaluate end
evaluate (Whilenot e1 strt e2 end) = do
  putStr("while not(")
  evaluate e1
  putStrLn("): ")
  evaluate strt
  evaluate e2
  evaluate end
evaluate (Incdec var oprtn) = do
  case oprtn of
    "++" -> putStrLn(var++" = "++var++" + 1")
    "--" -> putStrLn(var++" = "++var++" - 1")
evaluate (Fileopen pntr name typ mode) = putStrLn(pntr++" = open(\""++name++"."++typ++"\", "++"'"++mode++"')")

runFile fileName = do
    p <- parseFromFile fileP fileName
    case p of 
        Left parseErr -> print parseErr
        Right expp -> evaluate expp



--main:: IO()
main = do
    --showParsedExp "test.txt"
    args <- getArgs
    runFile $ head args
