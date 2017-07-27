To setup geth - etc-client private network:

Create a file named custom.json  with blockchain definition:
```
{
  "id": "custom",
  "name": "Morden Testnet",
  "genesis": {
    "nonce": "0x0000000000000042",
    "timestamp": "0x00",
    "parentHash": "",
    "extraData": "0x00",
    "gasLimit": "0x2fefd8",
    "difficulty": "0x0400",
    "mixhash": "0x0000000000000000000000000000000000000000000000000000000000000000",
    "coinbase": "0x0000000000000000000000000000000000000000",
    "alloc": {
      "d7a681378321f472adffb9fdded2712f677e0ba9": {"balance": "1606938044258990275541962092341162602522202993782792835301376"}
    }
  },
  "chainConfig": {
    "forks": [
      {
        "name": "Homestead",
        "block": 494000,
        "requiredHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
        "features": [
          {
            "id": "difficulty",
            "options": {
              "type": "homestead"
            }
          },
          {
            "id": "gastable",
            "options": {
              "type": "homestead"
            }
          }
        ]
      },
      {
        "name": "GasReprice",
        "block": 1783000,
        "requiredHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
        "features": [
          {
            "id": "gastable",
            "options": {
              "type": "eip150"
            }
          }
        ]
      },
      {
        "name": "The DAO Hard Fork",
        "block": 1885000,
        "requiredHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
        "features": []
      },
      {
        "name": "Diehard",
        "block": 1915000,
        "requiredHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
        "features": [
          {
            "id": "eip155",
            "options": {
              "chainID": 62
            }
          },
          {
            "id": "gastable",
            "options": {
              "type": "eip160"
            }
          },
          {
            "id": "difficulty",
            "options": {
              "length": 2000000,
              "type": "ecip1010"
            }
          }
        ]
      }
    ],
    "badHashes": [
    ]
  },
  "bootstrap": [
    "enode://fb28713820e718066a2f5df6250ae9d07cff22f672dbf26be6c75d088f821a9ad230138ba492c533a80407d054b1436ef18e951bb65e6901553516c8dffe8ff0@104.155.176.151:30304",
    "enode://afdc6076b9bf3e7d3d01442d6841071e84c76c73a7016cb4f35c0437df219db38565766234448f1592a07ba5295a867f0ce87b359bf50311ed0b830a2361392d@104.154.136.117:30403",
    "enode://21101a9597b79e933e17bc94ef3506fe99a137808907aa8fefa67eea4b789792ad11fb391f38b00087f8800a2d3dff011572b62a31232133dd1591ac2d1502c8@104.198.71.200:30403",
    "enode://fd008499e9c4662f384b3cff23438879d31ced24e2d19504c6389bc6da6c882f9c2f8dbed972f7058d7650337f54e4ba17bb49c7d11882dd1731d26a6e62e3cb@35.187.57.94:30304"
  ]
}
```
place it in `[geth_data_dir]/custom`  directory

run geth 3.5 with
```
./geth --chain=custom --cache=1024 --maxpeers 1 --rpc --rpcapi eth,web3,personal,admin --datadir geth_data_dir --rpcport 8545 --port 30309 --nodiscover --identity "TestnetMainNode" --networkid 1
```
run geth 3.4 with
```
./geth --chain-config="[PATH_TO_CUSTOM_JSON]" --cache=1024 --maxpeers 1 --rpc --rpcapi eth,web3,personal,admin --datadir geth_data_dir --rpcport 8545 --port 30309 --nodiscover --identity "TestnetMainNode" --networkid 1
```
Add file with custom etc-client configuration `private-genesis.json` :

```
{
  "extraData": "0x00",
  "nonce": "0x0000000000000042",
  "gasLimit": "0x2fefd8",
  "difficulty": "0x400",
  "ommersHash": "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
  "timestamp": "0x00",
  "coinbase": "0x0000000000000000000000000000000000000000",
  "mixHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
  "alloc": {
    "d7a681378321f472adffb9fdded2712f677e0ba9": {"balance": "1606938044258990275541962092341162602522202993782792835301376"}
  }
}
```
update `application.conf`

set `bootstrap-nodes` to geth node address
set `coinbase`  to some address
set `custom-genesis-file`  to point to `private-genesis.json` 
set `do-fast-sync = false`

You can get geth node address by adding `console` 
And running geth with

`geth --chain=custom --cache=1024 --maxpeers 1 --rpc --rpcapi eth,web3,personal,admin --datadir geth_data_dir --rpcport 8545 --port 30309 --nodiscover --identity "TestnetMainNode" --networkid 1 console`

then in console you can execute `admin.nodeInfo`  command which prints node address

after that you can run etc-client with `sbt run` 

to mine with ours client use ethminer `ethminer -C -F 127.0.0.1:8545` 
to mine with geth use 
```
./geth --chain=custom --cache=1024 --maxpeers 1 --rpc --rpcapi eth,web3,personal,admin --datadir geth_data_dir --rpcport 8545 --port 30309 --nodiscover --identity "TestnetMainNode" --networkid 1 --mine --etherbase '0x4963d1B82050A41af54a8018Da9f04341b16910b'
```