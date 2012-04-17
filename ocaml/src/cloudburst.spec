class Service {
  var host as ref Machine;
}

class Machine {}

class Datacenter {
  var machines as Machine[2];
}

var cloud as Datacenter;
var enterprise as Datacenter;

var web as Service[4];

var services as ref Service[4];

forall s1 in services {
  forall s2 in services {
    s1 != s2 -> s1.host != s2.host;
  };
};

var utilization as int;
utilization = count (s in services where s.host in enterprise.machines);