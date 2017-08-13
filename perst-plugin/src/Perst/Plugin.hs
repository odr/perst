module Perst.Plugin
       ( plugin -- :: Plugin
       ) where

import           Data.Time
-- import           GHC.TcPluginM.Extra (lookupModule, lookupName)
import           Control.Arrow ((&&&))
import           Data.Data     (cast, dataTypeOf, gmapQ, toConstr)
import           GhcPlugins
import           Panic
import           TcPluginM
import           TcRnTypes


plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = installCore
  , tcPlugin = const (Just tcPerstPlugin)
  }

installCore :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installCore _ todo = do
 putMsgS $ "Hello CoreToDo! ToDo length: " ++ show (length todo)
 return todo

tcPerstPlugin :: TcPlugin
tcPerstPlugin = TcPlugin { tcPluginInit  = tcPerstInit
                         , tcPluginSolve = tcPerstSolve
                         , tcPluginStop  = tcPerstStop
                         }

 -- | Find a module
lookupModule :: ModuleName -- ^ Name of the module
             -> FastString -- ^ Name of the package containing the module
             -> TcPluginM Module
lookupModule mod_nm pkg = do
  found_module <- findImportedModule mod_nm $ Just pkg
  case found_module of
    Found _ h -> return h
    _          -> do
      found_module' <- findImportedModule mod_nm $ Just $ fsLit "this"
      case found_module' of
        Found _ h -> return h
        _          -> panicDoc "Unable to resolve module looked up by plugin: "
                               (ppr mod_nm)

-- | Find a 'Name' in a 'Module' given an 'OccName'
lookupName :: Module -> OccName -> TcPluginM Name
lookupName md occ = lookupOrig md occ

tcPerstInit :: TcPluginM TyCon
tcPerstInit = do
  tcPluginIO $ do
    ct <- formatTime defaultTimeLocale "%T" <$> getCurrentTime
    print $ "Init TypeChecker ! " ++ ct
  return ()
  md <- lookupModule apkModule apkPackage
  apkTcNm <- lookupName md (mkTcOcc "AllParentKeys")
  tcLookupTyCon apkTcNm
 where
  apkModule  = mkModuleName "Perst.Database.Tree.Def"
  apkPackage = fsLit "perst"

tcPerstSolve :: TyCon -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
tcPerstSolve _ given derived [] = return (TcPluginOk [] [])
tcPerstSolve _ given derived wanted = do
  tcPluginIO $ do
    putStrLn "Run TypeChecker solver!"
    -- mapM_ (putStrLn . showSDocUnsafe . ppr . ctev_pred . ctEvidence) wanted
    mapM_
      (\x -> do
        let pred = ctev_pred $ ctEvidence x
        print
          $ ((,,) <$> toConstr <*> dataTypeOf <*> gmapQ ((,) <$> toConstr <*> dataTypeOf))
          pred
        putStrLn ""
        putStrLn "-----------"
        putStrLn ""
        showTyConApp pred
      ) wanted

    -- ++ " given: "    ++ show (length given)   ++ showSDocUnsafe (ppr given)
    -- ++ "; derived: " ++ show (length derived) ++ showSDocUnsafe (ppr derived)
    -- ++ "; wanted: "  ++ show (length wanted)  ++ showSDocUnsafe (ppr wanted)
  return $ TcPluginOk [] []
 where
  showTyConApp x = case cast x of
    Nothing -> return ()
    Just y -> case tcRepSplitTyConApp_maybe y of
      Nothing -> return ()
      Just (tc,ts) -> do
        putStrLn $ showSDocUnsafe $ ppr tc
        mapM_ showTyConApp ts

tcPerstStop :: TyCon -> TcPluginM ()
tcPerstStop _ = do
  tcPluginIO $ do
    ct <- formatTime defaultTimeLocale "%T" <$> getCurrentTime
    print $ "Stop TypeChecker! " ++ ct
  return ()

-- solve _     _ _ []      = return (TcPluginOk [] [])
-- solve tc _ _ wanteds = return $! case failed of
--     [] -> TcPluginOk (mapMaybe (\c -> (,c) <$> evMagic c) solved) []
--     f  -> TcPluginContradiction f
--   where
--     gcdWanteds :: [(Ct,(Integer,Integer))]
--     gcdWanteds = mapMaybe (toGCDEquality gcdTc) wanteds
--
--     solved, failed :: [Ct]
--     (solved,failed) = (map fst *** map fst)
--                     $ partition (uncurry (==) . snd) gcdWanteds
