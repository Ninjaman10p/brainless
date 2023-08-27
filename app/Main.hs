{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Control.Arrow
import           Control.Concurrent        (threadDelay)
import           Control.Lens              hiding (uncons)
import           Control.Monad.State.Class
import           Control.Monad.State.Lazy
import           Data.Char
import           Data.List                 (uncons)
import qualified Data.Map.Strict           as M
import           Data.Maybe
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq
import qualified Data.Set                  as S
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           PrettyPrinter
import           Python
import           Safe                      (readMay)
import           System.Environment

data VType = VString | VInt
  deriving (Show, Eq, Ord)

data ProgState = ProgState
  { _bfOutput   :: Seq Char
  , _astInput   :: Program
  , _pointerLoc :: Int
  , _vars       :: M.Map Variable (VType, Int)
  , _allocPtr   :: Int
  , _tempVarPtr :: Int
  , _strLength  :: Int
  , _freed      :: S.Set (Int, Int)
  }
$(makeLenses ''ProgState)

tmpVar :: Int -> Variable
tmpVar n = "__tmp__" <> T.pack (show n)

main :: IO ()
main = do
  args <- getArgs
  progname <- getProgName
  case getFileTarget args of
    Nothing -> putStrLn $ helpMessage progname
    Just fp -> do
      let blockStyle = BlockStyle $ getNumOpt "block-width" 40 args
      src <- T.readFile fp
      templateStyle <- mapM T.readFile $ getStrOpt "template" args
      let style = case getStrOpt "style" args of
                    Nothing -> NoStyle
                    Just "none" -> NoStyle
                    Just "block" -> blockStyle
                    Just "circles" -> CircleStyle $ getNumOpt "radius" 10 args
                    Just "template" -> TemplateStyle $ fromMaybe defTemplate templateStyle
                    Just "discs" -> DiscStyle $ getNumOpt "radius" 10 args
                    Just "dna" -> TemplateStyle defTemplate
                    Just "dna-curtains" -> TemplateStyle dnaCurtains
                    Just _ -> error "Unknown Style"
      let delay = getNumOpt "delay" 0 args
      let strLen = getNumOpt "string-length" 64 args
      let compiled = prettyPrint style $ compileBf strLen src
      unless (getBoolOpt "silent" False args) $
        forM_ (T.lines compiled) $ \line -> do
          threadDelay delay
          T.putStrLn line
      case getStrOpt "outfile" args of
        Just ofp -> T.writeFile ofp compiled
        Nothing  -> return ()

defTemplate :: Text
defTemplate = T.unlines
  [ "....        ...."
  , "....        ...."
  , " ....      .... "
  , "  ....    ....  "
  , "   ....  ....   "
  , "     ......     "
  , "      ....      "
  , "     ......     "
  , "   ....  ....   "
  , "  ....    ....  "
  , " ....      .... "
  , "....        ...."
  , "....        ...."
  ]

dnaCurtains :: Text
dnaCurtains = T.unlines
  [ "....        ....    ....        ....      ....  ....   "
  , "....        ....    ....        ....        ......     "
  , " ....      ....     ....        ....         ....      "
  , "  ....    ....      ....        ....        ......     "
  , "   ....  ....        ....      ....       ....  ....   "
  , "     ......           ....    ....       ....    ....  "
  , "      ....             ....  ....       ....      .... "
  , "     ......              ......        ....        ...."
  , "   ....  ....             ....         ....        ...."
  , "  ....    ....           ......        ....        ...."
  , " ....      ....        ....  ....      ....        ...."
  , "....        ....      ....    ....      ....      .... "
  , "....        ....     ....      ....      ....    ....  "
  ]

helpMessage :: String -> String
helpMessage progname = unlines
  [ "USAGE: " <> progname <> " [OPTIONS] SOURCEFILE [> TARGETFILE]"
  , ""
  , "OPTIONS:"
  , "  --style=STYLE: pretty print the code in the chosen style (default: block)"
  , "  --block-width: width of the block when printing with block style"
  , "  --radius: the radius of each circle when using circles or discs style"
  , "  --silent: supress output (default: off)"
  , ""
  , "STYLES:"
  , "  block (default): just print it as a chunk of text"
  , "  circles: print the code in circles"
  , "  discs: like circles, but filled in"
  , "  dna[-curtains]: DNA strands"
  ]

getNumOpt :: String -> Int -> [String] -> Int
getNumOpt str n args = fromMaybe n $ readMay =<< getStrOpt str args

getStrOpt :: String -> [String] -> Maybe String
getStrOpt str (arg:args)
  | take 2 arg == "--" && str == var = Just . fromMaybe e $ getStrOpt str args
    where (var, e) = second tail $ break (=='=') $ drop 2 arg
getStrOpt str (_:args) = getStrOpt str args
getStrOpt _ [] = Nothing

getBoolOpt :: String -> Bool -> [String] -> Bool
getBoolOpt cs _ (arg:args) | arg == "--" <> cs = getBoolOpt cs True args
getBoolOpt cs _ (arg:args) | arg == "--no-" <> cs = getBoolOpt cs False args
getBoolOpt cs b (_:args) = getBoolOpt cs b args
getBoolOpt _ b [] = b

getFileTarget :: [String] -> Maybe String
getFileTarget (('-':'-':_):css) = getFileTarget css
getFileTarget (cs:_)            = Just cs
getFileTarget []                = Nothing

{----------------------
 - Pretty print bf
 ---------------------}

compileBf :: Int -> Text -> Text
compileBf strLen = compileAST strLen . either (error . show) id . parsePython file "source file"

{----------------------
 - Parse AST to brainf
 ---------------------}

compileAST :: Int -> Program -> Text
compileAST strLen p = T.pack
                    <<< foldr (:) []
                    <<< view bfOutput
                    <<< execState compileASTM $ ProgState
  { _bfOutput = ""
  , _astInput = p
  , _pointerLoc = 0
  , _vars = M.empty
  , _allocPtr = 0
  , _tempVarPtr = 1
  , _strLength = strLen
  , _freed = S.empty
  }

compileASTM :: MonadState ProgState m => m ()
compileASTM = do
  -- errorTmp <- view astInput <$> get
  -- error $ show errorTmp
  popCmd >>= \case
    Just p' -> do
      case p' of
        Print e -> do
          var <- calculateExpr e
          printVar var
        While e prog -> do
          cmds <- view astInput <$> get
          modify $ set astInput prog
          res <- calculateExpr e
          shiftToVar res
          bfLoop $ do
            compileASTM
            setVar res e
            shiftToVar res
          modify $ set astInput cmds
        If e prog -> do
          cmds <- view astInput <$> get
          modify $ set astInput prog
          res <- calculateExpr e
          ifVar res $ do
            compileASTM
          modify $ set astInput cmds
        Set var e -> do
          setVar var e
          -- handle <- calculateExpr expr
          -- typ <- getVarType handle
          -- alloc typ var
          -- move handle var
          -- free handle
      compileASTM
    Nothing -> return ()

printVar :: MonadState ProgState m => Variable -> m ()
printVar var = do
    typ <- getVarType var
    case typ of
      VString -> do
        shiftToVar var
        writeBf ">[.>]"
        newline '+'
        writeBf ".[-]"
        writeBf "<[<]"
      VInt -> do
        svar <- calculateExpr $ EStr (EVar var)
        printVar svar
  where newline = writeBf . T.pack . replicate (ord '\n')

-- calculate the value of an expession, and return a pointer to the result
calculateExpr :: MonadState ProgState m => Expression -> m Variable
calculateExpr (EVar v) = makeCopy v

calculateExpr (EBool v) = calculateExpr (ENum (if v then 1 else 0))

calculateExpr (ENot a) = do
  a' <- calculateExpr a
  typ <- getVarType a'
  case typ of
    VInt -> do
      inv <- calculateExpr $ ENum 1
      ifVar a' $ do
        decr inv
      return inv
    _ -> error $ show typ <> " is not boolean!"

calculateExpr (EAnd a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  typ <- getVarType a'
  typ' <- getVarType a'
  case (typ, typ') of
    (VInt, VInt) -> do
      tgt <- allocTmp VInt
      ifVar a' $
        ifVar b' $ do
          shiftToVar tgt
          writeBf "+"
      return tgt
    (_, _) -> error $ show typ <> " cannot be boolean anded with " <> show typ'

calculateExpr (EOr a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  typ <- getVarType a'
  typ' <- getVarType a'
  case (typ, typ') of
    (VInt, VInt) -> do
      calculateExpr $ ENot $ EAnd (ENot (EVar a')) (ENot (EVar b'))
    (_, _) -> error $ show typ <> " cannot be boolean anded with " <> show typ'

calculateExpr (ELt a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  typ <- getVarType a'
  typ' <- getVarType b'
  if typ == typ'
  then
    case typ of
      VInt -> do
        tmp <- makeCopy b'
        repeatVar a' $ do
          decr tmp
        calculateExpr $ ENot (ENot (EVar tmp))
      VString -> do
        acpy <- makeCopy a'
        bcpy <- makeCopy b'
        aptr <- getVarPointer acpy
        bptr <- getVarPointer bcpy

        tgt <- calculateExpr $ ENum 1
        running <- calculateExpr $ ENum 1
        eq <- calculateExpr $ ENum 1

        shiftToVar running

        -- in loop
        bfLoop $ do
          tmpa <- allocTmp VInt
          tmpb <- allocTmp VInt

          shiftTo $ aptr + 1
          bfLoop $ do
            shiftToVar tmpa
            writeBf "+"
            shiftTo $ aptr + 1
            writeBf "-"
          shiftToVar acpy
          writeBf ">>[[-<+>]>]<<[<]" -- shift all of `a` left

          shiftTo $ bptr + 1
          bfLoop $ do
            shiftToVar tmpb
            writeBf "+"
            shiftTo $ bptr + 1
            writeBf "-"
          shiftToVar bcpy
          writeBf ">>[[-<+>]>]<<[<]"

          bgeqa <- calculateExpr $ EGt (EVar tmpa) (EVar tmpb)
          ifVar bgeqa $ do
            decr tgt
            setVar running $ ENum 0

          beqa <- calculateExpr $ ENot (EEq (EVar tmpa) (EVar tmpb))
          ifVar beqa $ do
            decr eq

          bDone <- calculateExpr $ ENot (EVar tmpb)
          ifVar bDone $ do
            ifVar eq $ do
              decr tgt
            setVar running $ ENum 0

          free bgeqa
          free bDone

          free tmpa
          free tmpb
          shiftToVar running
        -- end loop

        free eq
        free bcpy
        free acpy
        return tgt
      -- _ -> error $ "Ordering hasn't been implemented for " <> show typ <> " yet"
  else error "Can only compare values of the same type"

calculateExpr (EGt a b) = calculateExpr $ ELt b a

calculateExpr (ELeq a b) = calculateExpr (EGeq b a)

calculateExpr (EGeq a b) = calculateExpr $ ENot (ELt a b)

calculateExpr (EEq a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  typ <- getVarType a'
  typ' <- getVarType b'
  printVar a'
  printVar b'
  if typ == typ'
  then calculateExpr $ EAnd (EGeq (EVar a') (EVar b')) (EGeq (EVar b') (EVar a'))
  else calculateExpr (ENum 0)

calculateExpr (EAdd a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  tgt <- makeCopy a'
  addToVar tgt b'
  return tgt

calculateExpr (ESub a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  tgt <- makeCopy a'
  subFromVar tgt b'
  return tgt

calculateExpr (EMul a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  tgt <- makeCopy a'
  mulByVar tgt b'
  return tgt

calculateExpr (EDiv a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  typ <- getVarType a'
  typ' <- getVarType b'
  case (typ, typ') of
    (VInt, VInt) -> do
      acpy <- makeCopy a'
      tgt <- allocTmp VInt
      shiftToVar acpy
      writeBf "+"
      bfLoop $ do -- loop on a
        repeatVar b' $ do
          decr acpy
        ifVar acpy $ do
          shiftToVar tgt
          writeBf "+"
        shiftToVar acpy
      return tgt
    (_, _) -> error $ "Cannot divide " <> show typ <> " by " <> show typ'

calculateExpr (EMod a b) = do
  a' <- calculateExpr a
  b' <- calculateExpr b
  typ <- getVarType a'
  typ' <- getVarType b'
  case (typ, typ') of
    (VInt, VInt) -> do
      tgt <- makeCopy a'
      modByVar tgt b'
      calculateExpr $ ESub (EVar a') (EMul (EVar b') (EDiv (EVar a') (EVar b')))
    (_, _) -> error $ "Cannot modulo " <> show typ <> " by " <> show typ'

calculateExpr (EChr e) = do
  var <- calculateExpr e
  typ <- getVarType var
  case typ of
    VString -> error "Cannot cast string to character"
    VInt -> do
      tgt <- allocTmp VString
      tgtptr <- getVarPointer tgt
      repeatVar var $ do
        shiftTo $ tgtptr + 1
        writeBf "+"
      return tgt

calculateExpr (EOrd e) = do
  var <- calculateExpr e
  typ <- getVarType var
  case typ of
    VInt -> error "Can only cast character to character code"
    VString -> do
      tgt <- allocTmp VInt
      varcpy <- makeCopy var
      varptr <- getVarPointer varcpy
      shiftTo $ varptr + 1
      bfLoop $ do
        shiftToVar tgt
        writeBf "+"
        shiftTo $ varptr + 1
        writeBf "-"
      return tgt

calculateExpr (EStr e) = do
  var <- calculateExpr e
  typ <- getVarType var
  case typ of
    VString -> return var
    VInt -> do
      tgt <- allocTmp VString
      isZero <- calculateExpr $ ENot e
      ifVar isZero $
        setVar tgt $ EString "0"
      ifVar var $ do
        counter <- makeCopy var
        tgtptr <- getVarPointer tgt
        ten <- calculateExpr $ ENum 10
        shiftToVar counter
        bfLoop $ do
          nextChar <- calculateExpr $ EAdd (ENum $ ord '0') (EMod (EVar counter) (EVar ten))
          --- shift string
          shiftToVar tgt
          writeBf ">[>]<" -- move to end of string
          writeBf "[[->+<]<]"
          --- end shift string
          repeatVar nextChar $ do
            shiftTo $ tgtptr + 1
            writeBf "+"
          setVar counter $ EDiv (EVar counter) (EVar ten)
          free nextChar
          shiftToVar counter
        free counter
        free ten
      return tgt

calculateExpr (ENum num) = do
  -- if num <= 12
    -- then do
      tgt <- allocTmp VInt
      shiftToVar tgt
      replicateM_ num $ writeBf "+"
      return tgt
    -- else
      -- if isPrime num
        -- then calculateExpr $ EAdd (ENum 3) (ENum (num - 3))
        -- else fromMaybe (calculateExpr $ EAdd (ENum 1) (ENum (num - 1))) $
          -- fmap (\x -> calculateExpr $ EMul (ENum x) (ENum $ quot num x)) $
            -- firstJust (\x -> if mod num x == 0 && x >= 2 then Just x else Nothing) $
            -- [2..num]

calculateExpr (EInput Nothing) = do
  let newline = writeBf . T.pack . replicate (ord '\n')
  var <- allocTmp VString
  shiftToVar var
  newline '-'
  bfLoop $ do
    newline '+'
    writeBf ">,"
    newline '-'
  writeBf "<[<]"
  return var
calculateExpr (EInput (Just e)) = do
  v0 <- calculateExpr e
  printVar v0
  calculateExpr (EInput Nothing)
calculateExpr (EString cs) = do
  tgt <- allocTmp VString
  tgtPtr <- getVarPointer tgt
  forM_ (zip [1..] $ T.unpack cs) $ \(n, c) -> do
    shiftTo $ tgtPtr + n
    writeBf "[-]"
    replicateM_ (ord c) $ writeBf "+"
  return tgt

{----------------------------
 - In place opertions
 ----------------------------}
-- methods in haskell, i cri
-- only valid on natural numbers

  {-
expByVar :: MonadState ProgState m => Variable -> Variable -> m ()
expByVar tgt src = do
  typ <- getVarType src
  typ' <- getVarType tgt
  case (typ, typ') of
    (VInt, VInt) -> do
      tmp <- allocTmp typ
      move tgt tmp
      shiftToVar tgt
      writeBf "+"
      repeatVar src $ do
        shiftToVar tgt
        mulByVar tgt tmp
      free tmp
    (_, _) -> error $ "cannot exponentiate " <> show typ <> " by " <> show typ'
  -}

modByVar :: MonadState ProgState m => Variable -> Variable -> m ()
modByVar tgt src = do
  typ <- getVarType src
  typ' <- getVarType tgt
  case (typ, typ') of
    (VInt, VInt) -> do
      running <- calculateExpr $ EGeq (EVar tgt) (EVar src)
      shiftToVar running
      bfLoop $ do
        subFromVar tgt src
        setVar running $ EGeq (EVar tgt) (EVar src)
        shiftToVar running
      free running
    (_, _) -> error $ "cannot mod " <> show typ <> " by " <> show typ'

mulByVar :: MonadState ProgState m => Variable -> Variable -> m ()
mulByVar tgt src = do
  typ <- getVarType src
  typ' <- getVarType tgt
  case (typ, typ') of
    (VInt, VInt) -> do
      tmp <- allocTmp typ
      move tgt tmp
      repeatVar src $ do
        shiftToVar tgt
        addToVar tgt tmp
      free tmp
    (_, _) -> error $ "cannot multiply " <> show typ <> " by " <> show typ'

subFromVar :: MonadState ProgState m => Variable -> Variable -> m ()
subFromVar tgt src = do
  typ <- getVarType src
  typ' <- getVarType tgt
  case (typ, typ') of
    (VInt, VInt) -> do
      repeatVar src $ do
        shiftToVar tgt
        writeBf "-"
    (_, _) -> error $ "cannot subtract " <> show typ' <> " from " <> show typ

addToVar :: MonadState ProgState m => Variable -> Variable -> m ()
addToVar tgt src = do
  typ <- getVarType src
  typ' <- getVarType tgt
  case (typ, typ') of
    (VInt, VInt) -> do
      repeatVar src $ do
        shiftToVar tgt
        writeBf "+"
    (VString, VString) -> do
      bcpy <- makeCopy src
      bptr <- getVarPointer bcpy

      shiftTo $ bptr + 1
      bfLoop $ do
        shiftToVar tgt
        writeBf ">[>]+[<]"
        shiftTo $ bptr + 1
        bfLoop $ do
          shiftToVar tgt
          writeBf ">[>]<+[<]"
          shiftTo $ bptr + 1
          writeBf "-"
        shiftToVar tgt
        writeBf ">[>]<-<[<]"

        shiftToVar bcpy
        writeBf ">>[[-<+>]>]<<[<]"
        shiftTo $ bptr + 1

      free bcpy
    (_, _) -> error $ "cannot add " <> show typ' <> " to " <> show typ

{----------------------------
 - Other operations
 ---------------------------}

-- int only
ifVar :: MonadState ProgState m => Variable -> m () -> m ()
ifVar var m = do
  tmp <- allocTmp VInt
  shiftToVar var
  bfLoop $ do
    m
    move var tmp
  move tmp var
  free tmp

makeCopy :: MonadState ProgState m => Variable -> m Variable
makeCopy var = do
  typ <- getVarType var
  tgt <- allocTmp typ
  nullify tgt
  copy var tgt
  return tgt

setVar :: MonadState ProgState m => Variable -> Expression -> m ()
setVar v e = do
  pExpr <- calculateExpr e
  typ <- getVarType pExpr
  -- tmp <- move pExpr
  alloc typ v
  nullify v
  move pExpr v

nullify :: MonadState ProgState m => Variable -> m ()
nullify var = do
  shiftToVar var
  typ <- getVarType var
  case typ of
    VInt    -> writeBf "[-]"
    VString -> writeBf ">[>]<[[-]<]"

repeatVar :: MonadState ProgState m => Variable -> m () -> m ()
repeatVar var m = do
  typ <- getVarType var
  case typ of
    VInt -> do
      varcpy <- makeCopy var
      shiftToVar varcpy
      bfLoop $ do
        m
        shiftToVar varcpy
        writeBf "-"
    _ -> error $ show typ <> " is not currently iterable"

decr :: MonadState ProgState m => Variable -> m ()
decr var = do
  ifVar var $ do
    shiftToVar var
    writeBf "-"

shiftToVar :: MonadState ProgState m => Variable -> m ()
shiftToVar = getVarPointer >=> shiftTo

getVarInfo :: MonadState ProgState m => Variable -> m (VType, Int)
getVarInfo var = do
  st <- get
  case st^.vars.at var of
    Just i  -> return i
    Nothing -> error $ "Undefined variable: " <> T.unpack var

getVarSize :: MonadState ProgState m => Variable -> m Int
getVarSize var = do
  typ <- getVarType var
  sizeOf typ

getVarPointer :: MonadState ProgState m => Variable -> m Int
getVarPointer = fmap snd . getVarInfo

getVarType :: MonadState ProgState m => Variable -> m VType
getVarType = fmap fst . getVarInfo

shiftTo :: MonadState ProgState m => Int -> m ()
shiftTo n = do
  currentLoc <- view pointerLoc <$> get
  modify $ set pointerLoc n
  let dist = abs $ currentLoc - n
      c = if currentLoc < n then '>' else '<'
  writeBf (T.pack $ replicate dist c)

popCmd :: MonadState ProgState m => m (Maybe Instruction)
popCmd = do
  (p, ps) <- maybe (Nothing, []) (first Just) . uncons . view astInput <$> get
  modify $ set astInput ps
  return p

alloc :: MonadState ProgState m => VType -> Variable -> m ()
alloc typ var = do
  st <- get
  case st^.vars.at var of
    Nothing -> do
      siz <- sizeOf typ
      let position = S.lookupGE (siz, 0) (st^.freed)
      case position of
        Nothing -> do
          modify $ set (vars.at var) $ Just (typ, st^.allocPtr)
          modify $ over allocPtr (+siz)
        Just (siz', loc) -> do
          modify $ over freed $ S.delete (siz', loc)
          modify $ over freed $ S.insert (siz' - siz, loc + siz)
          modify $ set (vars.at var) $ Just (typ, loc)
    Just (oldtyp, _) ->
      when (oldtyp /= typ) $ do
        free var
        alloc typ var
  -- nullify var

copy :: MonadState ProgState m => Variable -> Variable -> m ()
copy src tgt = do
  typ <- getVarType src
  case typ of
    VInt -> do
      tmp <- allocTmp VInt
      ps <- getVarPointer src
      pt <- getVarPointer tgt
      p0 <- getVarPointer tmp
      size <- sizeOf VInt
      forM_ [0..size - 1] $ \n -> do
        shiftTo (ps + n)
        bfLoop $ do
          shiftTo (p0 + n)
          writeBf "+"
          shiftTo (pt + n)
          writeBf "+"
          shiftTo (ps + n)
          writeBf "-"
      move tmp src
      free tmp
    VString -> do
      tmp <- allocTmp VString
      p0 <- getVarPointer tmp

      move src tmp

      shiftTo $ p0 + 1
      bfLoop $ do
        shiftToVar tgt
        writeBf ">[>]+[<]"
        shiftToVar src
        writeBf ">[>]+[<]"

        shiftTo $ p0 + 1
        bfLoop $ do
          shiftToVar tgt
          writeBf ">[>]<+[<]"
          shiftToVar src
          writeBf ">[>]<+[<]"
          shiftTo $ p0 + 1
          writeBf "-"
        shiftToVar tgt
        writeBf ">[>]<-<[<]"
        shiftToVar src
        writeBf ">[>]<-<[<]"

        shiftToVar tmp
        writeBf ">>[[-<+>]>]<<[<]" -- shift all of a left

        shiftTo $ p0 + 1
      free tmp

move :: MonadState ProgState m => Variable -> Variable -> m ()
move src tgt = do
  typ <- getVarType src
  case typ of
    VInt -> do
      ps <- getVarPointer src
      pt <- getVarPointer tgt
      size <- sizeOf typ
      forM_ [0..size - 1] $ \n -> do
        shiftTo (ps + n)
        bfLoop $ do
          shiftTo (pt + n)
          writeBf "+"
          shiftTo (ps + n)
          writeBf "-"
    VString -> do
      ps <- getVarPointer src

      shiftTo $ ps + 1
      bfLoop $ do
        shiftToVar tgt
        writeBf ">[>]+[<]"
        shiftTo $ ps + 1
        bfLoop $ do
          shiftToVar tgt
          writeBf ">[>]<+[<]"
          shiftTo $ ps + 1
          writeBf "-"
        shiftToVar tgt
        writeBf ">[>]<-<[<]"

        shiftToVar src
        writeBf ">>[[-<+>]>]<<[<]" -- shift all of a left

        shiftTo $ ps + 1


-- Precondition: m does not shift pointerLoc
bfLoop :: MonadState ProgState m => m () -> m ()
bfLoop m = do
  writeBf "["
  m
  writeBf "]"

  {-
renameVar :: MonadState ProgState m => Variable -> Variable -> m ()
renameVar src tgt = do
  v <- getVarInfo src
  modify $ set (vars.at tgt) $ Just v
  modify $ set (vars.at src) Nothing
  -}

sizeOf :: MonadState ProgState m => VType -> m Int
sizeOf VString = (+2) . view strLength <$> get -- account for null bytes
sizeOf VInt    = return 1

allocTmp :: MonadState ProgState m => VType -> m Variable
allocTmp typ = do
  st <- get
  let var = tmpVar (st^.tempVarPtr)
  alloc typ var
  modify $ over tempVarPtr (+1)
  nullify var
  return var

-- does nothing for now
-- later on can optimise
free :: MonadState ProgState m => Variable -> m ()
free var = do
  siz <- getVarSize var
  loc <- getVarPointer var
  modify $ over freed $ S.insert (siz, loc)
  modify $ set (vars.at var) Nothing
  -- nullify var

writeBf :: MonadState ProgState m => Text -> m ()
writeBf cs = modify $ over bfOutput (<> Seq.fromList (T.unpack cs))

{--------------------
 - Generic functions
 --------------------}

  {-
isPrime :: Int -> Bool
isPrime n = not $ foldr ((||) . (== 0) . quot n) False [2..n-1]
-}
