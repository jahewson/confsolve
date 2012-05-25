class Service {
  var host as ref Machine;
}

class Machine {}

class Datacenter {
  var machines as Machine[300];
}

var cloud as Datacenter;
var enterprise as Datacenter;

var services as Service[400];

// only one service per machine
forall s1 in services {
  forall s2 in services {
    s1 != s2 -> s1.host != s2.host;
  };
};

// maximise utilisation of the Enterprise
var utilization as int;
utilization = count (s in services where s.host in enterprise.machines);
maximize utilization;