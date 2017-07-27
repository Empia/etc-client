_The purpose of this page is to keep track of all the blocks that failed to execute while we were working on TX execution. This is just a history of problems and fixes. It should help us deal with possible regressions and provide more insight into interpretations of YP._

**All transactions to present (June 2017) successfully executed ...!**

#### 3003036
TxsExecutionError(Left(TxsExecutionError(Account of tx sender 0xdfce88c609d54021d4ca78a550fc6ffa621ece1d not found))), in block 3003036

The issue was that we required sender account of a transaction to be present in MPT before transaction execution, but if we are not transferring ether it is possible to issue transaction which costs 0 ether by setting the gas price to 0.
In this case, we should create sender account with nonce 0, balance 0 and save it in MPT before transaction execution. PR [#173](https://github.com/input-output-hk/etc-client/pull/173)

#### 2420342

We did not pass addresses already market to delete to CALLs, it allowed for multiple refunds for deleting the same address. PR [#172](https://github.com/input-output-hk/etc-client/pull/172)

#### 1149150
It was related to not checking if `v` from `ECDSARECOVER` has correct value (27 or 28) and precompiled contract was not failing when it should. PR [#170](https://github.com/input-output-hk/etc-client/pull/170)

#### 549413
The stack arguments to instructions CALLDATALOAD, CALLDATACOPY, CODECOPY, EXTCODECOPY were optimistically/naively converted to `Int` resulting in potentially deadly overflows. PR [#169](https://github.com/input-output-hk/etc-client/pull/169)

#### 505404
We used a wrong hash function for the precompiled contract @ 0x02. It should be SHA-256, not KEC-256 (SHA3). PR: [#168](https://github.com/input-output-hk/etc-client/pull/168).

#### 299804

When calculating the gas cost of a CALL opcode, the use of UInt256 for it caused an overflow that resulted in the contract being executed instead of an OutOfGas exception. PR: [#167](https://github.com/input-output-hk/etc-client/pull/167).


#### 244793

It's another manifestation of the problem that occurred in [block 68460](#68460). [#164](https://github.com/input-output-hk/etc-client/pull/164)

#### 243826

When executing a contract init, the destination address might have already received funds. See YP EIP-150 REVISION eq (86).PR: [#162](https://github.com/input-output-hk/etc-client/pull/162)

#### 196647

A small bug in gas calculation for CALL prior to EIP-150 adoption. PR: [#160](https://github.com/input-output-hk/etc-client/pull/160)

#### 179332

MSIZE was returning memory size to the exact byte, rather than rounded up to the next 32-byte word. PR: [#154](https://github.com/input-output-hk/etc-client/pull/154)

#### 153259

The contract creating TX the payload was used as input data to the VM execution (it should be empty). PR: [#159]( https://github.com/input-output-hk/etc-client/pull/159)

#### 81383

Gas refunds were not propagated from recursive VM calls (CREATE, *CALL*). PR: [#158](https://github.com/input-output-hk/etc-client/pull/158)

#### 73276

Wrong handling of deleted accounts. PR: [#150](https://github.com/input-output-hk/etc-client/pull/150)

#### 68460

~~This was an example of YP being a little vague. It was not clear what was supposed to happen when the last stack argument (`outSize`) is greater than data returned from called contract. Now we know `outSize` should determine the memory size. See the the memory related equations in definition of `CALL`. PR: [#154](https://github.com/input-output-hk/etc-client/pull/154)~~

The above conclusion was incorrect (while the fix in PR #154 helped in this particular case). YP is actually clear about it, though somewhat inconsistent about updating memory contents vs expanding the memory. See equations for **_μ′<sub>m</sub>_** and **_μ′<sub>i</sub>_** in the CALL definition (this may be required to know the memory related gas cost a priori, although another way would be to refund it). PR: [#164](https://github.com/input-output-hk/etc-client/pull/164)

#### 62102
`SMOD` opCode was wrong

#### 53145
Logs were not discarded in the event of VM error

#### 50111
Gas refunds were not included in block's cumulative gas. This may be a bug in YP, see: https://github.com/ethereum/yellowpaper/issues/299
PR: [#147](https://github.com/input-output-hk/etc-client/pull/147)

#### 49157
????

#### 49018
Code deposit gas not handled correctly for contract creating TX

#### 48462
Incorrect handling of value transfer from A to A (same sender and recipient)

#### 47205
Nonce increased twice for contract creating TX

#### 46000
State not rolled back after OOG error