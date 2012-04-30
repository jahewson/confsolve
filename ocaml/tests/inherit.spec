/*
  test that inheritance of fields and constraints is working
*/

abstract class Service {
  var host as ref Machine;
  host.test < 50;
}

class Web_Service extends Service {
  // this is a refinement of the constraint in Service
  host.test > 20;
}

class DHCP_Service extends Service {
}

class Machine {
  var test as 0..100;
}

class Datacenter {
   var machines as Machine[2];
   var b as 0..100;
}

var cloud as ref Datacenter;
var enterprise as Datacenter;

var dhcp as DHCP_Service[2];
var web as Web_Service[2];

cloud.b > 60;

// no two services on same machine 
var services as ref Service[4];
forall s1 in services {
   forall s2 in services {
      s1 != s2 -> s1.host != s2.host;
   };
};

// favour placement of machines in the enterprise datacenter:
var utilization as int;
utilization = sum s in services where s.host in enterprise.machines { 
   1;
};