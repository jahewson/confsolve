array[1..12] of var 1..4: Service_host;

constraint
  forall (i in 1..12) (
    Machine_test[Service_host[i]] < 50
  );

constraint
  forall (i in 1..2) (
    Machine_test[Service_host[i]] > 20
  );
array[1..4] of var 0..100: Machine_test;
array[1..2] of set of 1..4: Datacenter_machines = [{1,2},{3,4}];
array[1..2] of var 0..100: Datacenter_b;
var 1..2: cloud;
1..2: enterprise = 1;
set of 1..2: dhcp = {1,2};
set of 1..2: web = {1,2};

constraint
  Datacenter_b[cloud] > 60;
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
  show(Datacenter_b),
  show(Datacenter_machines),
  show(Machine_test),
  show(Service_host)];
