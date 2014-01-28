{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module ModeInference.Constraint.Inference
  (ModeInstanceNames, inference)
where

import           Control.Exception (assert)
import           Control.Monad.RWS.Strict
import           Data.Generics
import           Data.Maybe (fromJust)
import           Data.List (find)
import           Data.Either (lefts,rights)
import qualified Data.Map as M
import           ModeInference.Language
import           ModeInference.Syntax (unmode)
import           ModeInference.Type hiding (mtypeOf)
import           ModeInference.Util
import           ModeInference.Constraint
import           ModeInference.Inference (inferPattern)

type ModeInstances     = M.Map Identifier          [([Mode],Mode)]
type ModeInstanceNames = M.Map (Identifier,[Mode]) Identifier

inference :: Program Type -> [MType] -> (Program MType, [ModeConstraint], ModeInstanceNames)
inference program mainArgTypes = (program', constraints, modeInstanceNames)
  where
    Program main decls = program
    bindings           = lefts eitherDecls

    (_, Output modeInstances modeInstanceNames bindings' supAndCaseConstraints) = 
      execRWS (fromInfer $ do inferBinding main mainArgTypes
                              mapM inferAllInstances bindings 
              )
              (emptyEnvironment program) emptyState

    modeInConstraints = concatMap (makeModeInConstraints modeInstances) bindings'

    constraints       = supAndCaseConstraints ++ modeInConstraints

    isMain (Binding (TypedIdentifier "main" _) _ _) = True
    isMain _                                        = False
    eitherDecl (DeclBind b)                         = Left b
    eitherDecl (DeclAdt  a)                         = Right a
    eitherDecls                                     = map eitherDecl decls

    program' = Program (fromJust $ find isMain bindings')
             $ (map DeclAdt $ rights eitherDecls) 
            ++ (map DeclBind (filter (not . isMain) bindings'))

data State = State { counter :: Integer }
emptyState = State 0

data Environment = Environment {
    envVarBindings :: M.Map Identifier MType
  , envProgram     :: Program Type
}
emptyEnvironment = Environment M.empty

data Output = Output {
    modeInstances     :: ModeInstances
  , modeInstanceNames :: ModeInstanceNames
  , instanceBindings  :: [Binding MType]
  , constraints       :: [ModeConstraint]
}

instance Monoid Output where
  mempty      = Output mempty mempty mempty mempty
  mappend a b = assert (M.null $ M.intersection (modeInstanceNames a) (modeInstanceNames b)) $
    Output (M.unionWith (++) (modeInstances a)     (modeInstances b))
           (mappend          (modeInstanceNames a) (modeInstanceNames b))
           (mappend          (instanceBindings a)  (instanceBindings b))
           (mappend          (constraints a)       (constraints b))

newtype Infer a = Infer { fromInfer :: RWS Environment Output State a }
  deriving (Monad, MonadReader Environment, MonadWriter Output, MonadState State)

newInt :: Infer Integer
newInt = do 
  c <- gets counter
  modify $ \s -> s { counter = counter s + 1 }
  return c

inferMType :: Type -> Infer MType
inferMType t = do
  program <- asks envProgram
  makeMType makeVar program t
  where
    makeVar = newInt >>= \int -> return $ ModeVar $ '_' : (show int)

inferConstantMType :: ModeAtom -> Type -> Infer MType
inferConstantMType atom t = do
  program <- asks envProgram
  return $ makeConstantMType atom program t

inferAllInstances :: Binding Type -> Infer ()
inferAllInstances binding = do
  program <- asks envProgram
  let paramTypes    = map idType $ bindParameters binding
      instanceTypes = sequence $ map (makeMonotoneMTypes program) paramTypes
  
  forM_ instanceTypes $ inferBinding binding

inferBinding :: Binding Type -> [MType] -> Infer ()
inferBinding binding argMTypes = assert (length argMTypes == length paramNames) $ do

  instanceName <- if bindingName == "main"
                  then return "main"
                  else newInt >>= \n -> return $ bindingName ++ "_" ++ (show n)

  (expM, expMType) <- local updateEnv $ inferExpression 
                                      $ bindExpression binding

  let instanceMType = if isConstant then expMType
                      else FunctionType argMTypes expMType

  tell $ Output 
       ( M.singleton bindingName [(argModes, typeAnnotation expMType)] )
       ( M.singleton (bindingName,argModes) instanceName )
       [ Binding (TypedIdentifier instanceName instanceMType) instanceParams expM ]
       [ ]
  
  where
    isConstant     = null argMTypes 
    bindingName    = identifier $ bindName binding
    paramNames     = map identifier $ bindParameters binding
    instanceParams = zipWith TypedIdentifier paramNames argMTypes
    argModes       = map typeAnnotation argMTypes

    updateEnv env = 
      env { envVarBindings = M.union (M.fromList $ zip paramNames argMTypes)
                                     (envVarBindings env)
          }

inferExpression :: Expression Type -> Infer (Expression MType, MType)
inferExpression expression = case expression of
  ExpVar v -> do
    asks (M.lookup (identifier v) . envVarBindings) >>= \case
      Nothing    -> error "Constraint.Inference.inferExpression"
      Just mtype -> return (ExpVar $ TypedIdentifier (identifier v) mtype, mtype)

  ExpCon (TypedIdentifier c t) -> do
    mtype <- inferConstantMType Known t
    return (ExpCon $ TypedIdentifier c mtype, mtype)

  ExpApp (ExpVar v) args -> do
    (args', args'Types) <- forM args inferExpression >>= return . unzip

    resultType' <- inferMType $ resultType $ idType v

    let vType = FunctionType args'Types resultType'

    return (ExpApp (ExpVar $ v {idType = vType}) args', resultType')

  ExpApp (ExpCon c) args -> do
    (args', args'MTypes) <- forM args inferExpression >>= return . unzip

    cMType <- inferConstructorApp c args'MTypes 
    return ( ExpApp (ExpCon $ c { idType = FunctionType args'MTypes cMType}) args'
           , cMType)

  ExpCase d branches -> do
    (d',dMType)               <- inferExpression d
    (branches', branchMTypes) <- forM branches (inferBranch dMType) >>= return . unzip
    caseMType                 <- inferMType $ unmode $ head branchMTypes

    tell $ Output mempty mempty [] [modeCase caseMType dMType branchMTypes]

    return (ExpCase d' branches', caseMType)

  ExpLet (Binding name [] value) exp -> do
    (value', valueType) <- inferExpression value
    (exp',expType)      <- local (updateEnv valueType) $ inferExpression exp

    return ( ExpLet (Binding (name { idType = valueType }) [] value') exp'
           , expType
           )
    where
      updateEnv valueType env =
        env { envVarBindings = M.insert (identifier name) valueType $ envVarBindings env }

inferConstructorApp :: TypedIdentifier Type -> [MType] -> Infer MType
inferConstructorApp cId cArgMTypes = do
  adt          <- asks $ adtFromConstructorName cId . envProgram
  constructor  <- asks $ constructorFromName    cId . envProgram
  resultMType  <- inferMType $ resultType $ idType cId

  makeConstraints adt constructor $ typeAnnotation resultMType

  return resultMType

  where 
    cArgModes = map typeAnnotation cArgMTypes

    makeConstraints adt constructor resultMode = 
      tell $ Output mempty mempty [] 
           $ map (uncurry ModeSup) 
           $ M.toList suprema
      where
        suprema :: M.Map Mode [Mode]
        suprema = M.insertWith   (++) resultMode [toMaxKnown resultMode]
                $ M.fromListWith (++) 
                $ zipWith supremum [0..] cArgModes

        supremum i argMode = (submode adt constructor i resultMode, [argMode])

inferBranch :: MType -> Branch Type -> Infer (Branch MType, MType)
inferBranch dMType (Branch pat exp) = do
  pat'             <- asks $ (\p -> inferPattern p dMType pat) . envProgram
  (exp', expMType) <- local (updateEnv pat') $ inferExpression exp
  return (Branch pat' exp', expMType)
  where
    updateEnv pat' env = 
      env { envVarBindings = M.union (M.fromList newVarBindings) $ envVarBindings env }
      where
        newVarBindings = case pat' of
          PatVar v    -> [(identifier v, idType v)]
          PatCon _ vs -> map (\v -> (identifier v, idType v)) vs

makeModeInConstraints :: ModeInstances -> Binding MType -> [ModeConstraint]
makeModeInConstraints modeInstances = everything (++) $ mkQ [] goExp
  where
    goExp (ExpApp (ExpVar v) _) = map (goInstance vArgModes vResultMode) instances
      where
        vArgModes   = map typeAnnotation $ argumentTypes $ idType v
        vResultMode =     typeAnnotation $ resultType    $ idType v
        instances   = case M.lookup (identifier v) modeInstances of
          Nothing -> error "Constraint.Inference.makeModeInConstraints"
          Just is -> is
    goExp _ = []

    goInstance vArgModes vResultMode (instArgModes, instResultMode) =
      assert (length vArgModes == length instArgModes) $
      ModeImpl (zip vArgModes instArgModes) (vResultMode, instResultMode)
