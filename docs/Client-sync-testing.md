## Testing environment (Small EC2 instance)
```
Hardware: HVM domU

CPU: Intel(R) Xeon(R) CPU E5-2676 v3 @ 2.40GHz

Mem: 2GiB

Disk: 60GB

OS: Ubuntu 16.04.2 LTS

openjdk version "1.8.0_131"
```

## Version
```
commit 615ae67dfca31a8a0dbdf2fbbcfe7ddd37bfbd29
Merge: 751db36 023b628
Author: Alan Verbner <alan.verbner@iohk.io>
Date:   Wed Jul 26 13:14:35 2017 -0300

    Merge branch 'fix/couldNotInitialiseKeystoreDirectory' into phase/beta1
```

## Scenarios tested with results

### Regular sync (with discovery enabled):

#### Setup

* Config changes:
```
sync.do-fast-sync = false
```

#### Results

Two times regular sync was ran having the following results:

* The remote EC2 instance was left doing regular sync from 2017/07/26 till 12:16 2017/07/27 were the instance was detected to not be syncing. Upon restart the client resumed syncing but got the exception: `java.lang.RuntimeException: Could not open table 5353`. At that moment the EC2 instance had:

```
Discovered 152 nodes. Incomming connections: 0. Outgoing connections: 11 (6 of them fully handshaked)
Block height: 152135. Tx hash which failed due to exception: aa6b8c5b787ff2982911cd9daae22c97f34303f9f31f789328c583f595df6979
```

* The remote EC2 instance was left doing regular sync from 18:33-2017/7/26 to 13:40 2017/7/27, during which half a million blocks where downloaded and executed. No problem (at least no fatal one) was detected and the node ended up connected to 6 peers (with 1 frequently blacklisted). However, although connected to that number of peers, there were `no peers to download from` (probably due to those peers being synced till before the fork block). Upon restart the `no peers to download from` continued.

### Syncing from *etc-client* node: 

#### Setup

* Config changes:
```
sync.min-peers-to-choose-target-block = 1
server-address.interface = "0.0.0.0"
```
* Minor changes were required on the codebase for enabling using a peer that hadn't reached the fork block for syncing. On [RegularSync](https://github.com/input-output-hk/etc-client/blob/master/src/main/scala/io/iohk/ethereum/blockchain/sync/RegularSync.scala#L269) the `forkAccepted` required was set to `_`. On [FastSync](https://github.com/input-output-hk/etc-client/blob/master/src/main/scala/io/iohk/ethereum/blockchain/sync/FastSync.scala#L40) the following change was made `val peersUsedToChooseTarget = peersToDownloadFrom`.
* Set EC2 instance as the only bootstrap node of a local instance (with discovery disabled), having the EC2 instance only ~5000 blocks.

#### Results

Both regular sync and fast sync were tested, being able in both cases our local node to connect and sync from the remote one. Fast sync was also successfully completed (as the target block was ~4000) and regular sync was started after that.