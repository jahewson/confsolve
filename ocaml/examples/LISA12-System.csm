// LISA '12
// introduction example

// ---------------------------------
// SAN disks
  
class LogicalDisk {
  var capacityGB as int;
}

var db_disk as LogicalDisk;
db_disk.capacityGB = 2048;  // 2TB

var web_disk as LogicalDisk;
web_disk.capacityGB = 10;    // 10GB

// ---------------------------------
// classes

enum OperatingSystem { Windows, UNIX, OSX }

class NetworkInterface {
  var isEthernet as bool;
  var subnet as int;
}

class Machine {
  var bootDisk as ref LogicalDisk; // LUN
  
  var en0 as NetworkInterface;
   
  var os as OperatingSystem;
  var cpus as 1..4;
  var memory as int;
}

abstract class Role {
  var machine as ref Machine;
}

class WebServer extends Role {
  var port as int;
  
  // SAN disk
  machine.bootDisk = web_disk;
}

enum DatabaseRole { Master, Slave }

class DatabaseServer extends Role {
  var role as DatabaseRole;
  var peer as ref DatabaseServer; // <-- forces every master to have a slave, and vice-versa.
  
  // the peer cannot be itself
  peer != this;  
  
  // a master's peer must be a slave, and a slave's peer must be a master
  role != peer.role;
  
  // SAN disk
  machine.bootDisk = db_disk;
}

// ---------------------------------
// rack (for impact - can we loose a rack?)
// (then, have the answer be no, and ask ConfSolve how we could achieve this?) [TRICKY]
// standard sizes are 42U and 24U
class Rack {
  var machines as Machine[24];  // <-- a set :)
}

// ---------------------------------
// machines

var r1 as Rack; // 24 machines
var r2 as Rack; // 24 machines

var m0 as ref Machine;
// ...

// ---------------------------------
// roles

var masterDB as DatabaseServer;
masterDB.role = DatabaseRole.Master;    // DB
masterDB.peer = slaveDB;                // DB

var slaveDB as DatabaseServer;
slaveDB.role = DatabaseRole.Slave;      // DB
slaveDB.peer = masterDB;                // DB

var webServers as WebServer[2];

forall ws in webServers {
  ws.port = 80;
};

// ---------------------------------
// role assignments

//masterDB.machine = m1;
//slaveDB.machine = m2;

// ---------------------------------

// prefer databases to be placed in different racks:
var databases as ref DatabaseServer[2];
var racks as ref Rack[2];

maximize sum r in racks {
  count (db in databases where db.machine in r.machines != db.peer.machine in r.machines);
};
