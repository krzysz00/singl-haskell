module Evaluator (evaluate) where
import Data.Map as M
import Data.Char (ord,chr)
import Control.Monad.Trans (liftIO)
import Control.Monad.State
import Types

data Value = Number !Integer | Function [Token] deriving Show

type World = ([Token], [[Token]], M.Map Integer Value)
type Interpreter = StateT World IO Integer

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

switchFst :: d -> (a,b,c) -> (d,b,c)
switchFst d (a,b,c) = (d,b,c)

switchSnd :: d -> (a,b,c) -> (a,d,c)
switchSnd d (a,b,c) = (a,d,c)

switchTrd :: d -> (a,b,c) -> (a,b,d)
switchTrd d (a,b,c) = (a,b,d)

getInteger :: IO Integer
getInteger = do
  c <- getChar
  return $ fromIntegral . ord $ c
  
addToken :: Token -> World -> World
addToken t w = switchFst (t:(fst3 w)) w

evalArithmetic :: (Integer -> Integer -> Integer) -> Token -> Token -> Interpreter
evalArithmetic f t1 t2 = do
  modify $ addToken t1 . addToken t2
  r1 <- eval
  r2 <- eval
  return $ f r1 r2

evalIf :: (Integer -> Integer -> Bool) -> Token -> Token -> Token -> Interpreter
evalIf cmp t1 t2 t3 = do
  modify $ addToken t1 . addToken t2 . addToken t3
  result <- eval
  if cmp result 0 
    then modify (\(x:y:ts,b,c) -> (x:ts,b,c))
    else modify (\(x:y:ts,b,c) -> (y:ts,b,c))
  eval

lookupVariable :: Integer -> Interpreter
lookupVariable v = do
  (_,_,m) <- get
  let r = M.lookup v m
  case r of
    Nothing -> return 0
    Just (Number n) -> return n 
    Just (Function l) -> do
      modify $ addToken (Group l)
      eval

eval :: Interpreter
eval = do
  (t:ts,parents,m) <- get
  liftIO $ print (t,m)
  modify $ switchFst ts
  ret <- case t of
    Comment t1 -> (modify $ switchFst (t1:ts)) >> eval
    Variable v -> lookupVariable v
    Dereference v -> lookupVariable v >>= lookupVariable
    Address v -> return v
    Output v -> do
      val <- lookupVariable v
      let o = chr . fromIntegral $ val
      liftIO $ print ("Printing variable,char", v, o)
      liftIO $ putChar o
      return val
    Input v -> do
      i <- liftIO getInteger
      modify $ switchTrd $ M.insert v (Number i) m
      return i
    Add t1 t2 -> evalArithmetic (+) t1 t2
    Subtract t1 t2 -> evalArithmetic (-) t1 t2
    Multiply t1 t2 -> evalArithmetic (*) t1 t2
    Divide t1 t2 -> evalArithmetic div t1 t2
    Modulo t1 t2 -> evalArithmetic mod t1 t2
    EqZero test t1 t2 -> evalIf (==) test t1 t2
    LeqZero test t1 t2 -> evalIf (<=) test t1 t2
    Loop test body -> do
      modify $ addToken test . addToken body
      value <- eval
      if value == 0
        then return value
        else do 
          eval
          modify $ addToken (Loop test body)
          eval
    Group tokens -> do
      put (tokens,ts:parents,m)
      r <- evalToEnd
      (_,ts':parents',m') <- get
      put (ts',parents',m')
      return r
    Assign v tokens -> do
      v' <- case v of 
            Dereference loc -> lookupVariable loc
            Variable loc -> return loc
      let final:others = reverse tokens
      case final of 
        Group l -> do
          modify $ switchTrd $ M.insert v' (Function l) m 
          modify $ addToken (Group others) 
          eval
        otherwise -> do
          modify $ addToken (Group tokens)
          (_,currentFn:ps,_) <- get
          modify $ switchSnd ps
          r <- eval
          modify $ switchTrd $ M.insert v' (Number r) m
          return r
    Argument -> do
      (current,p:ps,map) <- get
      put (p,ps,map)
      r <- eval
      (p',ps',map') <- get
      put (current,p':ps',map')
      return r
  return ret

evalToEnd :: Interpreter
evalToEnd = do
  (current,p,m) <- get
--  liftIO $ print ("Group eval", m)
  evalToEnd' current
  where
    evalToEnd' [] = return 0
    evalToEnd' (t:[]) = eval
    evalToEnd' (t:ts) = eval >> evalToEnd

evaluate :: [Token] -> IO ()
evaluate program = do
  execStateT evalToEnd (program,[[]],M.empty)
  return ()

  