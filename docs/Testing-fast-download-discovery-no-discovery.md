
### Hardware - Small EC2 instance 

Run from dist created from branch "beta1" on Wed Jul 26 10:30:44 IST 2017
 
### Overrides 
```
fast-sync = false
server-address = <result of ifconfig -a>
```

Created second small instance with following extra override (12.35)
```
discovery = false
```

Both stopped for (no discovery for 1 hour) at 3:43 WEd Jul 26 

Override `peer-response-timeout` to 3 minutes