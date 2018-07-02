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

import Blockchain.Data.RLP
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Numeric
import Data.Text.Encoding (encodeUtf8)

prettyPrint :: B.ByteString -> Text
prettyPrint = T.pack . concat . map (flip showHex "") . B.unpack

accountIdToHex :: AccountId -> Text
accountIdToHex = printHex WithoutPrefix . unAddr . accountId

calcExtraData :: [Text] -> Text
calcExtraData addrs = prettyPrint . rlpSerialize $ extraData
  where validators_   = RLPArray $ map (rlpEncode . fst . B16.decode . encodeUtf8) addrs
        seal          = rlpDeserialize . fst . B16.decode $ "b8410000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        committedSeal = RLPArray []
        extraData     = RLPArray [validators_, seal, committedSeal]

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
                IstanbulConfig _epoch _policy _addrs -> [ "istanbul" .= object
                                                          [ "epoch"  .= getEpoch _epoch
                                                          , "policy" .= getPolicy _policy
                                                          ]
                                                        ])
      , "difficulty" .=
        case consenCfg of
          RaftConfig _         -> t "0x0"
          CliqueConfig _       -> t "0x0"
          PowConfig            -> t "0x0"
          IstanbulConfig _ _ _ -> t "0x01"
      , "extraData"  .=
        case consenCfg of
          RaftConfig _ -> empty32
          CliqueConfig addrs ->
            t $ "0x48616c6c6f2077656c7400000000000000000000000000000000000000000000"
              <> foldMap accountIdToHex addrs
              <> T.replicate (65 * 2) "0"
          PowConfig -> empty32
          IstanbulConfig _ _ addrs ->
            t $ "0x"
              <> T.replicate (32 * 2) "0"
              <> (calcExtraData $ map accountIdToHex addrs)
      , "gasLimit"   .= t "0xE0000000"
      , "mixhash"    .=
        case consenCfg of
          RaftConfig _         -> empty32
          CliqueConfig _       -> empty32
          PowConfig            -> empty32
          IstanbulConfig _ _ _ -> "0x63746963616c2062797a616e74696e65206661756c7420746f6c6572616e6365"
      , "nonce"      .= t "0x0"
      , "parentHash" .= empty32
      , "timestamp"  .= t "0x00"
      ]

    t = id :: Text -> Text
    i = id :: Int -> Int

    empty32 :: Text
    empty32 = hexPrefixed (def :: Bytes32)
