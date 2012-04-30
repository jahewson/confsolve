array[1..12] of var 1..8: Service_host;
array[1..4] of set of 1..8: Datacenter_machines = [{1,2},{3,4},{5,6},{7,8}];
1..4: cloud = 1;
1..4: enterprise = 2;
set of 1..2: dhcp = {1,2};
set of 1..2: web = {1,2};
var set of {1,4,7,10}: services;
constraint card(services) = 4;

constraint
  forall (s1 in 1..12) (
    s1 in services -> (forall (s2 in 1..12) (
    s2 in services -> (s1 != s2 -> Service_host[s1] != Service_host[s2])
  ))
  );
var int: utilization;

constraint
  utilization = sum (s in 1..12) (
    bool2int(s in services /\ Service_host[s] in Datacenter_machines[enterprise]) * (1)
  );

solve satisfy;

output [
  show(utilization),
  show(services),
  show(web),
  show(dhcp),
  show(enterprise),
  show(cloud),
  show(Datacenter_machines),
  show(Service_host)];
