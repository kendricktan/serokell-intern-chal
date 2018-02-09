{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Serokell.Node () where


import           Serokell.Communication.IPC (Conversation (..), NodeId (..),
                                             connectToUnixSocket, listenUnixSocket)
