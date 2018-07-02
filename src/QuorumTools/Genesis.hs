{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}

module QuorumTools.Genesis where

import           Control.Lens      (view)
import           Data.Aeson
import           Data.Default      (def)
import qualified Data.Map.Strict   as Map
import           Data.Map.Strict   (Map)
import qualified Data.Text         as T
import           Turtle            hiding (view)
import           Prelude           hiding (FilePath)

import           QuorumTools.Types
import           QuorumTools.Util

createGenesisJson :: (MonadIO m, HasEnv m) => m FilePath
createGenesisJson = do
    consensusCfg <- view clusterConsensusConfig
    jsonPath <- view clusterGenesisJson
    balances <- view clusterInitialBalances
    mode <- view clusterMode
    output jsonPath (contents balances consensusCfg mode)
    return jsonPath

  where
    contents :: Map AccountId Integer
             -> ConsensusConfig
             -> ClusterMode
             -> Shell Line
    contents bals consenCfg mode = select $ textToLines $ textEncode $ object
      [ "alloc"      .= (object $
        map (\(ai, bal) ->
              accountIdToText ai .= object ["balance" .= T.pack (show bal)])
            (Map.toList bals) :: Value)
      , "coinbase"   .= addrToText def
      , "config"     .= object
        ([ "homesteadBlock" .= i 1
         , "chainId"        .= i 1
         , "eip155Block"    .= i 3
         , "eip158Block"    .= i 3
         , "isQuorum"       .= (mode == QuorumMode)
         ] <> case consenCfg of
                RaftConfig _ -> []
                CliqueConfig _ -> [ "clique" .= object
                                    [ "period" .= i 1
                                    , "epoch"  .= i 30000
                                    ]
                                  ]
                PowConfig -> []
                IstanbulConfig _epoch _policy -> [ "istanbul" .= object
                                                   [ "epoch"  .= getEpoch _epoch
                                                   , "policy" .= getPolicy _policy
                                                   ]
                                                 ])
      , "difficulty" .=
        case consenCfg of
          RaftConfig _       -> t "0x0"
          CliqueConfig _     -> t "0x0"
          PowConfig          -> t "0x0"
          IstanbulConfig _ _ -> t "0x01"
      , "extraData"  .=
        case consenCfg of
          RaftConfig _ -> empty32
          CliqueConfig addrs ->
            t $ "0x48616c6c6f2077656c7400000000000000000000000000000000000000000000"
              <> foldMap (printHex WithoutPrefix . unAddr . accountId) addrs
              <> T.replicate (65 * 2) "0"
          PowConfig -> empty32
          IstanbulConfig _ _ -> "0x0000000000000000000000000000000000000000000000000000000000000000f897f893946571d97f340c8495b661a823f2c2145ca47d63c2948157d4437104e3b8df4451a85f7b2438ef6699ff94b131288f355bc27090e542ae0be213c20350b76794b912de287f9b047b4228436e94b5b78e3ee1617194d8dba507e85f116b1f7e231ca8525fc9008a696694e36cbeb565b061217930767886474e3cde903ac594f512a992f3fb749857d758ffda1330e590fa915e80c0"
      , "gasLimit"   .= t "0xE0000000"
      , "mixhash"    .=
        case consenCfg of
          RaftConfig _       -> empty32
          CliqueConfig _     -> empty32
          PowConfig          -> empty32
          IstanbulConfig _ _ -> "0x63746963616c2062797a616e74696e65206661756c7420746f6c6572616e6365"
      , "nonce"      .= t "0x0"
      , "parentHash" .= empty32
      , "timestamp"  .= t "0x00"
      ]

    t = id :: Text -> Text
    i = id :: Int -> Int

    empty32 :: Text
    empty32 = hexPrefixed (def :: Bytes32)
