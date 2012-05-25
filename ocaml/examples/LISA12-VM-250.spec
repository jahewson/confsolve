// LISA '12
// VM Benchmark

class Machine {
  var cpu as int;         // 1/2 cores
  var memory as int;      // MB
  var disk as int ;       // GB
	
  cpu = 16;               // 2x Quad Core (1/2 core units)
  memory = 16384;         // 16 GB = 4x4GB DIMM
  disk = 2048;            // 2 TB
}

abstract class VM {
  var host as ref Machine;
  var disk as int;
  var cpu as int;
  var memory as int;
}

class SmallVM extends VM {
  cpu = 1;
  memory = 768;
  disk = 20;
}

class LargeVM extends VM {
  cpu = 4;
  memory = 3584;
  disk <= 500;
}

// physical machines
var rack1 as Machine[48];

// VMs (virtual machines)
var smallVMs as SmallVM[200];
var largeVMs as SmallVM[50];

var machines as ref Machine[48];
var vms as ref VM[250];

forall m in machines {
  sum r in vms where r.host = m {
    r.cpu;
  } <= m.cpu
  &&
  sum r in vms where r.host = m {
    r.memory;
  } <= m.memory
  &&
  sum r in vms where r.host = m {
    r.disk;
  } <= m.disk;
};