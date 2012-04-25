///////////////////////////////////////
//   THIS FILE WAS AUTO-GENERATED    //
///////////////////////////////////////

///////////////////////////////////////////////////////
// ModRef 2011 Example
// Author: John Hewson, University of Edinburgh
///////////////////////////////////////////////////////

// types //
class Machine {
  var cpu as int;         // 1/2 cores
  var memory as int;      // MB
  var disk as int ;       // GB
	
  cpu = 16;               // 2x Quad Core (1/2 core units)
  memory = 16384;         // 16 GB = 4x4GB DIMM
  disk = 2048;            // 2 TB
}

abstract class Role {
  var host as ref Machine;
  var disk as int;
  var cpu as int;
  var memory as int;
}

class SmallRole extends Role {
  cpu = 1;
  memory = 768;
  disk <= 20;
}

class LargeRole extends Role {
   cpu = 4;
	 memory = 3584;
	 disk <= 490;
}

// globals //

// physical machines
var machines as Machine[10];

// roles (virtual machines)
	var role_1 as SmallRole;
	role_1.disk = 1;

	var role_2 as SmallRole;
	role_2.disk = 1;

	var role_3 as SmallRole;
	role_3.disk = 1;

	var role_4 as SmallRole;
	role_4.disk = 1;

	var role_5 as SmallRole;
	role_5.disk = 1;

	var role_6 as SmallRole;
	role_6.disk = 1;

	var role_7 as SmallRole;
	role_7.disk = 1;

	var role_8 as SmallRole;
	role_8.disk = 1;

	var role_9 as SmallRole;
	role_9.disk = 1;

	var role_10 as SmallRole;
	role_10.disk = 1;

	var role_11 as SmallRole;
	role_11.disk = 1;

	var role_12 as SmallRole;
	role_12.disk = 1;

	var role_13 as SmallRole;
	role_13.disk = 1;

	var role_14 as SmallRole;
	role_14.disk = 1;

	var role_15 as SmallRole;
	role_15.disk = 1;

	var role_16 as SmallRole;
	role_16.disk = 1;

	var role_17 as SmallRole;
	role_17.disk = 1;

	var role_18 as SmallRole;
	role_18.disk = 1;

	var role_19 as SmallRole;
	role_19.disk = 1;

	var role_20 as SmallRole;
	role_20.disk = 1;

var roles as ref Role[20];

forall m in machines {
  sum r in roles where r.host = m {
    r.cpu;
  } <= m.cpu
  &&
  sum r in roles where r.host = m {
    r.memory;
  } <= m.memory
  &&
  sum r in roles where r.host = m {
    r.disk;
  } <= m.disk;
};