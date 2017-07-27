## Tools

The following testing tools where used:

- Etc-client in regular sync mode running a private chain
- Parity nodes
- CPU only mining with EthMiner (https://github.com/Genoil/cpp-ethereum/tree/108)

## Mist Features tested
 
**Feature**: Number of peers connected

**Scenario**: Connected and disconnected multiple peers.

**Known methods involved**: net_peerCount.

 
***

**Feature**: Account creation

**Scenario**: Created accounts and checked that they were properly listed on Mist.

**Known methods involved**: personal_newAccount

***
 
**Feature**: Send value tx

**Scenario**: Sent a tx and checked that tx confirmation progressed (till it got 12 confirmations). Also used Postman to check that the correct extra data was added to the tx (as selected on Mist).

**Known methods involved**: eth_estimateGas, eth_syncing (for tx confirmation)

***
 
**Feature**: Deploy a contract

**Scenario**: Using an existing account, sent a contract creation tx. Also used Postman to check that the correct extra data was added to the tx (as selected on Mist).

**Known methods involved**: eth_estimateGas, eth_syncing (for tx confirmation)

***
 
**Feature**: Transaction confirmation progressbar

**Scenario**: Sent a transactions and seen that tx confirmation progress was increasing and tx information was correct. Also, if it’s a contract creation tx, we have seen address was correct and it was added to watchlist.

***
 
**Feature**: Send tx for calling contract methods and watch contract events

**Scenario**: With contract created, Mist interface was used for calling several of it’s methods, which were checked that they were getting confirmed. This methods involved events, so “Watch contract methods” was turned on to check that this events were properly detected.

**Known methods involved**: Same as when sending txs.
 
***
 
**Feature**: Call constant contract methods

**Scenario**: With contract created, it was tested that the values shown for the constant functions were correct, which required calling contract methods that changed them.

**Known methods involved**: eth_call.
 
***

**Feature**: Create txs but starting miner only after the txs were created

**Scenario**: Without the miner yet started, several transactions were created (testing specially including transactions from the same sender), after which the miner was connected to our node and it was checked that the transactions were included in blocks and getting confirmed.

**Known methods involved**: Same as when sending txs.
 
***

**Feature**: Sync to peer (with only regular sync)

**Scenario**: Connected the node to a parity node (and on the mainnet) and checked that Mist notified correctly that our client was syncing (and showing a correct syncing state).

**Known methods involved**: eth_syncing.
 
***

**Feature**: Connect to miner

**Scenario**: Connected miner to the node and checked that the blocks mined were being detected by Mist.

**Known methods involved**: eth_syncing.
 
***

**Feature**: Balance change when mining

**Scenario**: Configured an existing account as coinbase and started the miner. Account balance was constantly increasing. 
 
***

**Issues detected**
 
- Currently Mist only works with our client if regular sync is enabled, when fast sync is enabled, methods like eth_getBalance fail.
- After creating an account and starting mining, we need to restart Mist to see increasing balance. It seems it’s a Mist related issue as it happened with Parity too
- Mist keeps returning errors when trying to send a transaction related to gas estimation. This seems to be a mist problem as have seen it using Parity.


 
