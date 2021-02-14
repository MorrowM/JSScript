module Language.JSScript.Interpreter where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Foldable
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.JSScript.AST
import Language.JSScript.Parser
import Text.Parsec

showt :: Show a => a -> Text
showt = pack . show

annotate :: Monad m => e -> Maybe a -> ExceptT e m a
annotate e Nothing = throwE e
annotate _ (Just a) = pure a

anyToBool :: Any -> Bool
anyToBool = (== ABool True) -- TODO expand this

eval :: Expr -> EvalM Any
eval (ExprLit l) = case l of
  LitInt x -> pure $ AInt x
  LitDouble x -> pure $ ADouble x
  LitText x -> pure $ AText x
  LitBool x -> pure $ ABool x
  LitVec x -> AVec <$> traverse eval x
eval (ExprVar x) =
  annotate ("variable " <> x <> " not in scope")
    . Map.lookup x
    =<< lift get
eval (ExprFuncCall f exprs) = do
  vals <- traverse eval exprs
  AFunc arglist stmts ret <-
    annotate ("function " <> f <> " was called before it was defined")
      . Map.lookup f
      =<< lift get
  let la = length arglist
      le = length vals
  when (la /= le) $
    throwE ("function " <> f <> " takes " <> showt la <> " parameters, but " <> showt le <> " were passed")
  vtable <- lift get
  let funcScope = (`Map.union` Map.fromList (zip arglist vals))
  lift $ modify funcScope
  traverse_ exec stmts
  res <- eval ret
  lift $ put vtable
  pure res
eval (ExprSum x y) =
  (,) <$> eval x <*> eval y >>= \case
    (AInt a, AInt b) -> pure . AInt $ a + b
    (AInt a, ABool b) -> pure . AInt $ a + fromEnum b
    (AInt a, ADouble b) -> pure . ADouble $ fromIntegral a + b
    (AInt a, AText b) -> pure . AText $ showt a <> b
    (ABool a, AInt b) -> pure . AInt $ fromEnum a + b
    (ABool a, ABool b) -> pure . ABool $ a || b
    (ABool a, ADouble b) -> pure . ADouble $ fromIntegral (fromEnum a) + b
    (ABool a, AText b) -> pure . AText $ (if a then "true" else "false") <> b
    (ADouble a, AInt b) -> pure . ADouble $ a + fromIntegral b
    (ADouble a, ABool b) -> pure . ADouble $ a + fromIntegral (fromEnum b)
    (ADouble a, ADouble b) -> pure . ADouble $ a + b
    (ADouble a, AText b) -> pure . AText $ showt a <> b
    (AText a, AInt b) -> pure . AText $ a <> showt b
    (AText a, ABool b) -> pure . AText $ a <> (if b then "true" else "false")
    (AText a, ADouble b) -> pure . AText $ a <> showt b
    (AText a, AText b) -> pure . AText $ a <> b
    (AVec a, b) -> pure . AVec $ V.snoc a b
    (a, AVec b) -> pure . AVec $ V.cons a b
    (a, b) -> throwE $ "cannot add " <> anyToName a <> " to " <> anyToName b
eval (ExprDiv x y) =
  (,) <$> eval x <*> eval y >>= \case
    (AInt a, AInt b) -> pure . ADouble $ (fromIntegral a / fromIntegral b)
    (AInt a, ABool b) -> pure . ADouble $ fromIntegral a / fromIntegral (fromEnum b)
    (AInt a, ADouble b) -> pure . ADouble $ fromIntegral a / b
    (AInt a, AText b) ->
      let (whole, frac) = properFraction (fromIntegral a)
       in pure . AText $ T.concat (replicate whole (T.reverse b)) <> T.take (floor $ fromIntegral @_ @Double (T.length $ T.reverse b) * frac) (T.reverse b)
    (ABool a, AInt b) -> pure . ADouble $ fromIntegral (fromEnum a) / fromIntegral @_ @Double b
    (ABool a, ABool b) -> pure . ABool $ a /= b
    (ABool a, ADouble b) -> pure . ADouble $ fromIntegral (fromEnum a) / b
    (ABool a, AText b) -> pure . AText $ if a then "" else b
    (ADouble a, AInt b) -> pure . ADouble $ a / fromIntegral b
    (ADouble a, ABool b) -> pure . ADouble $ a / fromIntegral (fromEnum b)
    (ADouble a, ADouble b) -> pure . ADouble $ a / b
    (ADouble a, AText b) ->
      let (whole, frac) = properFraction a
       in pure . AText $ T.concat (replicate whole (T.reverse b)) <> T.take (floor $ fromIntegral (T.length $ T.reverse b) * frac) (T.reverse b)
    (AText a, AInt b) ->
      let c = fromIntegral b
          (whole, frac) = properFraction (1 / c)
       in pure . AText $ T.concat (replicate whole a) <> T.take (floor $ fromIntegral @_ @Double (T.length a) * frac) a
    (AText a, ABool b) -> pure . AText $ (if b then "true" else "false") <> a
    (AText a, ADouble b) ->
      let (whole, frac) = properFraction (1 / b)
       in pure . AText $ T.concat (replicate whole a) <> T.take (floor $ fromIntegral (T.length a) * frac) a
    (AText a, AText b) -> pure . AText $ T.reverse $ T.concatMap (\c -> T.singleton c <> a) b
    (a, b) -> throwE $ "cannot divide " <> anyToName a <> " with " <> anyToName b
eval (ExprProd x y) =
  (,) <$> eval x <*> eval y >>= \case
    (AInt a, AInt b) -> pure . AInt $ a * b
    (AInt a, ABool b) -> pure . AInt $ a * fromEnum b
    (AInt a, ADouble b) -> pure . ADouble $ fromIntegral a * b
    (AInt a, AText b) -> pure . AText . T.concat $ replicate a b
    (ABool a, AInt b) -> pure . AInt $ fromEnum a * b
    (ABool a, ABool b) -> pure . ABool $ a && b
    (ABool a, ADouble b) -> pure . ADouble $ fromIntegral (fromEnum a) * b
    (ABool a, AText b) -> pure . AText $ if a then b else ""
    (ADouble a, AInt b) -> pure . ADouble $ a * fromIntegral b
    (ADouble a, ABool b) -> pure . ADouble $ a * fromIntegral (fromEnum b)
    (ADouble a, ADouble b) -> pure . ADouble $ a * b
    (ADouble a, AText b) ->
      let (whole, frac) = properFraction a
       in pure . AText $ T.concat (replicate whole b) <> T.take (floor $ fromIntegral (T.length b) * frac) b
    (AText a, AInt b) -> pure . AText . T.concat $ replicate b a
    (AText a, ABool b) -> pure . AText $ a <> (if b then "true" else "false")
    (AText a, ADouble b) ->
      let (whole, frac) = properFraction b
       in pure . AText $ T.concat (replicate whole a) <> T.take (floor $ fromIntegral (T.length a) * frac) a
    (AText a, AText b) -> pure . AText $ T.concatMap (\c -> T.singleton c <> a) b
    (AVec a, b) -> fmap AVec $ traverse eval $ flip ExprSum (anyToExpr b) . anyToExpr <$> a
    (a, AVec b) -> fmap AVec $ traverse eval $ ExprSum (anyToExpr a) . anyToExpr <$> b
    (a, b) -> throwE $ "cannot multiply " <> anyToName a <> " with " <> anyToName b
eval (ExprEqual x y) =
  (,) <$> eval x <*> eval y >>= \case
    (AInt a, AInt b) -> pure . ABool $ a == b
    (AInt a, ABool b) -> pure . ABool $ a == fromEnum b
    (AInt a, ADouble b) -> pure . ABool $ fromIntegral a == b
    (AInt a, AText b) -> pure . ABool $ showt a == b
    (ABool a, AInt b) -> pure . ABool $ fromEnum a == b
    (ABool a, ABool b) -> pure . ABool $ a == b
    (ABool a, ADouble b) -> pure . ABool $ fromIntegral (fromEnum a) == b
    (ABool a, AText b) -> pure . ABool $ (if a then "true" else "false") == b
    (ADouble a, AInt b) -> pure . ABool $ a == fromIntegral b
    (ADouble a, ABool b) -> pure . ABool $ a == fromIntegral (fromEnum b)
    (ADouble a, ADouble b) -> pure . ABool $ a == b
    (ADouble a, AText b) -> pure . ABool $ showt a == b
    (AText a, AInt b) -> pure . ABool $ a == showt b
    (AText a, ABool b) -> pure . ABool $ a == (if b then "true" else "false")
    (AText a, ADouble b) -> pure . ABool $ a == showt b
    (AText a, AText b) -> pure . ABool $ a == b
    (AVec a, AVec b) -> pure . ABool $ a == b
    (a, b) -> throwE $ "cannot compare " <> anyToName a <> " with " <> anyToName b
eval (ExprNEqual x y) =
  eval (ExprEqual x y) >>= \case
    ABool b -> pure . ABool $ not b
    _ -> throwE "the impossible happened!"
eval (ExprIndex x i) =
  (,) <$> eval x <*> eval i >>= \case
    (AText str, AInt idx)
      | idx >= T.length str || idx < 0 -> throwE $ "index " <> str <> "[" <> showt idx <> "] is out of range"
      | otherwise -> pure . AText . T.singleton $ T.index str idx
    (AVec v, AInt idx) -> case v V.!? idx of
      Nothing -> throwE $ "index " <> showt v <> "[" <> showt idx <> "] is out of range"
      Just a -> pure a
    _ -> throwE "invalid index operation"

exec :: Stmt -> EvalM ()
exec (StmtBlock x) = traverse_ exec x
exec (StmtDeclare x val) = do
  v <- eval val
  lift $ modify $ Map.insert x v
exec (StmtAssign x val) = do
  void $
    annotate ("variable " <> x <> " was asigned to before it was defined")
      . Map.lookup x
      =<< lift get
  v <- eval val
  lift $ modify $ Map.insert x v
exec (StmtIf cond thenB melseB) = do
  res <- eval cond
  if anyToBool res
    then exec thenB
    else maybe (pure ()) exec melseB
exec (StmtWhile cond b) =
  let loop = do
        res <- eval cond
        when (anyToBool res) $ exec b *> loop
   in loop
exec (StmtFunc f args b) = do
  presence <- Map.lookup f <$> lift get
  when (isJust presence) $
    throwE ("function " <> f <> " cannot be defined twice")
  let (bl, ret) = case last b of
        StmtReturn e -> (init b, e)
        _ -> (b, ExprLit (LitBool True)) -- TODO add undefined maybe?
  lift $ modify $ Map.insert f (AFunc args bl ret)
exec (StmtReturn _) = pure () -- TODO functions are fucked
exec (StmtFuncCall f exprs) = do
  vals <- traverse eval exprs
  AFunc arglist stmts _ <-
    annotate ("function " <> f <> " was called before it was defined")
      . Map.lookup f
      =<< lift get
  let la = length arglist
      le = length vals
  when (la /= le) $
    throwE ("function " <> f <> " takes " <> showt la <> " parameters, but " <> showt le <> " were passed")
  vtable <- lift get
  let funcScope = vtable `Map.union` Map.fromList (zip arglist vals)
  lift $ put funcScope
  traverse_ exec stmts
exec StmtBreak = pure () -- TODO breaks are fucked
exec (StmtForeign exprs (Foreign f)) = traverse eval exprs >>= f
exec (StmtImport modu) = do
  let filename = modu <> ".jss"
  contents <- liftIO $ readFile filename
  case parse (many stmt <* eof) filename contents of
    Left err -> throwE $ "parse error while importing " <> pack modu <> ":\n" <> showt err
    Right stmts -> traverse_ exec stmts
