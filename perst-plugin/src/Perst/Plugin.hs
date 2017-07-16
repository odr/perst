module Perst.Plugin
       ( plugin -- :: Plugin
       ) where

import           Data.Time
import           GhcPlugins
import           TcPluginM
import           TcRnTypes

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = installCore
  , tcPlugin = const (Just tcPerstPlugin)
  }

installCore :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installCore _ todo = do
 putMsgS "Hello CoreToDo!"
 return todo

tcPerstPlugin :: TcPlugin
tcPerstPlugin = TcPlugin { tcPluginInit  = tcPerstInit
                         , tcPluginSolve = tcPerstSolve
                         , tcPluginStop  = tcPerstStop
                         }

tcPerstInit :: TcPluginM ()
tcPerstInit = do
  tcPluginIO $ do
    ct <- formatTime defaultTimeLocale "%T" <$> getCurrentTime
    print $ "Init TypeChecker! " ++ ct
  return ()

tcPerstSolve :: () -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
tcPerstSolve _ given derived wanted = do
  tcPluginIO $ putStrLn
    $ "Run TypeChecker solver!"
    ++ " given: "    ++ show (length given)   ++ showSDocUnsafe (ppr given)
    ++ "; derived: " ++ show (length derived) ++ showSDocUnsafe (ppr derived)
    ++ "; wanted: "  ++ show (length wanted)  ++ showSDocUnsafe (ppr wanted)
  return $ TcPluginOk [] []

tcPerstStop :: () -> TcPluginM ()
tcPerstStop _ = do
  tcPluginIO $ do
    ct <- formatTime defaultTimeLocale "%T" <$> getCurrentTime
    print $ "Stop TypeChecker! " ++ ct
  return ()
