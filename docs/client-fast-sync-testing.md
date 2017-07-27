* I started fast sync on EC2 on 14:10 CEST 26th of July
* On 16:05 it is on 733184 block, no dead loop, no stuck
* Around block 1389840 10:00 CEST 27th of July we blacklisted all peers and we are receiving lot of transactions not targeted at etereum classic chain
* 10:05 CEST we unbloacklisted 1 peer and we are syncing again (Block: 1392592/4163902)
* 11:32 CEST restart with new configuration lowering the blacklist duration (Block: 1434256/4163902)
* 12:16 CEST restart with latest version of application (Block: 1488656/4163902)
* 12:25 CEST restart with removed trace from message decoder error (Block: 1491216/4163902)
* 12:29 CEST restart with pruning set to `archive` because fast sync was started without pruning
* 12:51 CEST restart update update `nodes-per-request` 1000 -> 500 (Block: 1507984/4163902)
* 13:35 CEST peers are responding with `11:35:07.329 i.i.e.b.s.SyncBlockHeadersRequestHandler - Received 0 block headers in 1556 ms` (Block: 1512464/4163902)
* 13:37 CEST restart to get better peers, updated `connect-max-retries` 1 -> 5
* 15:03 CEST restart with configuration changes from https://github.com/input-output-hk/etc-client/pull/261/commits/363742a9d358a2c631af61bc40d428258116c1f8 (Block: 1518416/4163902)
* 16:39 CEST restart with update `nodes-per-request` 500 -> 200 (Block: 1547472/4163902)
* 17:04 CEST restart with update block-headers-per-request 2048 -> 200 (Block: 1555600/4163902)