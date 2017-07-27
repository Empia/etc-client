## A note on how transactions are executed 

The first thing to note is that transactions (txs) are executed in two places. The first and only time a transaction should fail is when a miner attempts to create a block by executing all the transactions in the block. The second is when a block of transactions from the past is downloaded as part of chain synchronization. In this case the state needs to be updated with all historical transactions up to the present in order to acquire an up to date state. Even though in the second case a transaction *must* be valid, as it's already in a block, we still validate the transactions according to the rules so that we *know* our state has been legally arrived at. 

Transactions execute sequentially. Given a block of transactions and a state trie, the state trie is passed to the first transaction in the block, that transaction executes altering the state and the new state is passed to the next transaction. When all transactions have executed the new state trie is now the state of the ledger and will be used in subsequent block processing.

When a transaction executes (after the account is debited and the sig checked etc) contract code may call other contracts. These message calls are blocking and may be re-entrant. 
If a contract changes a value available via it's public interface and calls a second contract which access the original contract's public interface, which value does it see? ##TODO Check!

## Gas and Errors ##
A transaction must provide a contract with 'gas' to pay for it's execution. If the gas is insufficient to pay for the contracts execution the transaction is abandoned and any state changes attempted by the transaction are abandoned. The state passed to the next transaction is the same as the state passed to the abandoned transaction. Similarly if a contract `throws`, the gas is consumed and the state changes abandoned.

If a message call (ie when a contract calls another contract) `throws` or runs out of gas a '0' or false is returned on the stack for the calling contract to deal with. 






 