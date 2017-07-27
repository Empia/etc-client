# Testing environment
Hardware: Clevo P751ZM

CPU: Intel(R) Xeon(R) CPU E3-1231 v3 @ 3.40GHz,

Mem: 16GB

Disk: Crucial 275GB SATA SSD MX300 M.2 2280

OS: Fedora 26, kernel 4.11

openjdk version "1.8.0_131"

# Config changes
```

# Maximum block headers in a single response message (as a blockchain host)
max-blocks-headers-per-message = 100

# Maximum block bodies in a single response message (as a blockchain host)
max-blocks-bodies-per-message = 100

# Maximum transactions receipts in a single response message (as a blockchain host)
max-receipts-per-message = 100

# Maximum MPT components in a single response message (as a blockchain host)
max-mpt-components-per-message = 200



# Response time-out from peer during sync. If a peer fails to respond within this limit, it will be blacklisted
peer-response-timeout = 30.seconds

```

# Version
```
commit 0aaef18b8675ab5176e7a48b8d810e247450f3b6 (HEAD -> phase/beta1, origin/phase/beta1)
Merge: 454931d5 5b2e6a08
Author: Alan Verbner <alan.verbner@iohk.io>
Date:   Tue Jul 25 14:19:38 2017 -0300

Merge branch 'feature/referenceCountPrunning' into phase/beta1
```

# Result
Fast sync finished in 16-17 hours (with breaks, this is the total time app was running).

Started at 2017-07-26 9:30am, finished at 2017-07-27 1pm

Datadir size at block 4166064 is ~15G.

Syncing continues in "regular" mode.