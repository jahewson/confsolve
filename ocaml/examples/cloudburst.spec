abstract class Service {
   var host as ref Machine;
}

class Web_Service extends Service {
}

class DHCP_Service extends Service {
}

class Machine {
}

class Datacenter {
   var machines as Machine[2];
}

var cloud as Datacenter;
var enterprise as Datacenter;

var dhcp as DHCP_Service[2];
var web as Web_Service[2];

// no two services on same machine 
var services as ref Service[4];
forall s1 in services {
   forall s2 in services {
      s1 != s2 -> s1.host != s2.host;
   };
};

// favour placement of machines in the enterprise datacenter:
var utilization as int;
utilization = count (s in services where s.host in enterprise.machines);
