## Setup

For tests we set up 10 EC2 instances on a private network, we used genesis as bellow:
```
{
  "difficulty": "0x400",
  "extraData": "0x00",
  "gasLimit": "0x2fefd8",
  "nonce": "0x0000000000000042",
  "ommersHash": "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
  "timestamp": "0x00",
  "coinbase": "0x0000000000000000000000000000000000000000",
  "mixHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
  "alloc": {
    "d7a681378321f472adffb9fdded2712f677e0ba9": {"balance": "1606938044258990275541962092341162602522202993782792835301376"}
  }
}
```
for mining we used ethminer from `https://github.com/Genoil/cpp-ethereum.git` from branch `108` which supports CPU mining

we were testing on ubuntu 16.04, to compile ethminer you need to instal some packages:
```
sudo apt-get -y install software-properties-common
sudo add-apt-repository -y ppa:ethereum/ethereum
sudo apt-get update
sudo apt-get install git cmake libcryptopp-dev libleveldb-dev libjsoncpp-dev libboost-all-dev libgmp-dev libreadline-dev libcurl4-gnutls-dev ocl-icd-libopencl1 opencl-headers mesa-common-dev libmicrohttpd-dev build-essential -y
sudo apt-get install libjsonrpccpp-dev -y
```

We started ethminer with `ethminer -C -F 127.0.0.1:8546` and ours client with `sbt -Dconfig.file=/home/ubuntu/node.conf run`



## Testing
Firstly we run nodes not connected to each other, this way nodes got their own versions of blockchain after chains were ~60 blocks long we connected them together and let them mine and resolve conflicting branches at the same time.

We let them run for 3 days and after checking did not notice any error in logs, nodes are roughly on the same block and they are mining without any issue.

We also checked that we produced blocks with correct ommer.

After that we connected geth classic node and it was able to join network and sync chain from genesis to current block (it was configured for same private network) and it continues ti accepting new blocks after sync was finished

### Issue with geth
Also while testing we found out that if geth node is connected only to one node it will not sync if best blocks are too far away in terms of height
```
eth/fetcher/fetcher.go:631] Peer 010d7622a3242a48: discarded block #16598 [3861d9e4…], distance 3043
```
what fixed the issue was adding more connections, you can do this with geth console
```
admin.addPeer("node_address")
```

## etc-client configuration
node.conf file should contains
```
etc-client {

  client-id = "etc-client"

  client-version = "etc-client/v0.1"

  datadir = "/home/ubuntu/chain_data"

  //File format of the keys (in plain text): publicKey ++ CR/LF ++ privateKey
  keys-file = ${etc-client.datadir}"nodeId.keys"

  keystore-dir = ${etc-client.datadir}"keystore"

  // time the system will wait to shutdown the ActorSystem.
  shutdown-timeout = "15.seconds"

  network {

    protocol-version = "1"

    server-address {
      interface = "public_ip"
      port = 9076
    }

    discovery {
      bootstrap-nodes = [
        //list of nodes in network
      ]

      bootstrap-nodes-scan-interval = 2 minutes
    }

    peer {
      connect-retry-delay = 20 seconds
      connect-max-retries = 30
      disconnect-poison-pill-timeout = 5 seconds
      wait-for-hello-timeout = 3 seconds
      wait-for-status-timeout = 30 seconds
      wait-for-chain-check-timeout = 15 seconds

      max-blocks-headers-per-message = 200
      max-blocks-bodies-per-message = 200
      max-receipts-per-message = 200
      max-mpt-components-per-message = 400

      max-peers = 200
      network-id = 1
    }

    rpc {
      enabled = true
      interface = "127.0.0.1"
      port = 8546
      apis = "eth,web3,net"
    }
  }

  mining {
    tx-pool-size = 1000
    ommers-pool-size = 30
    block-cashe-size = 30
    coinbase = "aa758c0b47afcafb9751a516e56a7c3332571933"
    pooling-services-timeout = 5.seconds
  }

  blockchain {
    frontier-block-number = "0"
    homestead-block-number = "1150000"
    eip150-block-number = "2500000"
    eip160-block-number = "3000000"
    difficulty-bomb-pause-block-number = "3000000"
    difficulty-bomb-continue-block-number = "5000000"

    // Doc: https://blog.ethereum.org/2016/07/20/hard-fork-completed/
    dao-fork-block-number = "1920000"
    dao-fork-block-total-difficulty = "39490964433395682584"
    dao-fork-block-hash = "94365e3a8c0b35089c1d1195081fe7489b528a84b22199c916180db8b28ade7f"

    chain-id = "3d"

    custom-genesis-file = "/home/ubuntu/custom-genesis.json"

    // YP eq 150
    block-reward = "5000000000000000000"
  }

  fast-sync {
    do-fast-sync = false
    peers-scan-interval = 3.seconds
    blacklist-duration = 30.seconds
    start-retry-interval = 5.seconds
    sync-retry-interval = 5.seconds
    peer-response-timeout = 10.seconds
    print-status-interval = 2.seconds
    persist-state-snapshot-interval = 1.minute

    max-concurrent-requests = 50
    block-headers-per-request = 2048
    block-bodies-per-request = 128
    receipts-per-request = 60
    nodes-per-request = 1000
    min-peers-to-choose-target-block = 2
    target-block-offset = 500

    check-for-new-block-interval = 1.seconds
    block-resolving-depth = 20
  }

  db {
    iodb {
      path = ${etc-client.datadir}"iodb/"
    }
    leveldb {
      path = ${etc-client.datadir}"leveldb/"
      create-if-missing = true
      paranoid-checks = true // raise an error as soon as it detects an internal corruption
      verify-checksums = true // force checksum verification of all data that is read from the file system on behalf of a particular read
      cache-size = 0
    }
  }

  filter {
    filter-timeout = 10.minutes
    filter-manager-query-timeout = 3.seconds
    pending-transactions-manager-query-timeout = 5.seconds
  }

}

akka {
  loggers = ["akka.event.slf4j.Slf4jLogger"]
  loglevel = "DEBUG"
  logging-filter = "akka.event.slf4j.Slf4jLoggingFilter"
  logger-startup-timeout = 30s
}
```

## geth classic configuration
chain.json
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
        //nodes to connect
    ]
}
```