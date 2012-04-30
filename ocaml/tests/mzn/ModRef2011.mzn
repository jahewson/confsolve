array[1..20] of var int: Machine_cpu;
array[1..20] of var int: Machine_memory;
array[1..20] of var int: Machine_disk;

constraint
  forall (i in 1..20) (
    Machine_cpu[i] = 16
  );

constraint
  forall (i in 1..20) (
    Machine_memory[i] = 16384
  );

constraint
  forall (i in 1..20) (
    Machine_disk[i] = 2048
  );
array[1..60] of var 1..20: Role_host;
array[1..60] of var int: Role_disk;
array[1..60] of var int: Role_cpu;
array[1..60] of var int: Role_memory;

constraint
  forall (i in 1..20) (
    Role_cpu[i] = 1
  );

constraint
  forall (i in 1..20) (
    Role_memory[i] = 768
  );

constraint
  forall (i in 1..20) (
    Role_disk[i] <= 20
  );

constraint
  forall (i in 1..0) (
    Role_cpu[i] = 4
  );

constraint
  forall (i in 1..0) (
    Role_memory[i] = 3584
  );

constraint
  forall (i in 1..0) (
    Role_disk[i] <= 490
  );
set of 1..20: machines = {1,2,3,4,5,6,7,8,9,10};
1..20: role_1 = 1;

constraint
  Role_disk[role_1] = 1;
1..20: role_2 = 2;

constraint
  Role_disk[role_2] = 1;
1..20: role_3 = 3;

constraint
  Role_disk[role_3] = 1;
1..20: role_4 = 4;

constraint
  Role_disk[role_4] = 1;
1..20: role_5 = 5;

constraint
  Role_disk[role_5] = 1;
1..20: role_6 = 6;

constraint
  Role_disk[role_6] = 1;
1..20: role_7 = 7;

constraint
  Role_disk[role_7] = 1;
1..20: role_8 = 8;

constraint
  Role_disk[role_8] = 1;
1..20: role_9 = 9;

constraint
  Role_disk[role_9] = 1;
1..20: role_10 = 10;

constraint
  Role_disk[role_10] = 1;
1..20: role_11 = 11;

constraint
  Role_disk[role_11] = 1;
1..20: role_12 = 12;

constraint
  Role_disk[role_12] = 1;
1..20: role_13 = 13;

constraint
  Role_disk[role_13] = 1;
1..20: role_14 = 14;

constraint
  Role_disk[role_14] = 1;
1..20: role_15 = 15;

constraint
  Role_disk[role_15] = 1;
1..20: role_16 = 16;

constraint
  Role_disk[role_16] = 1;
1..20: role_17 = 17;

constraint
  Role_disk[role_17] = 1;
1..20: role_18 = 18;

constraint
  Role_disk[role_18] = 1;
1..20: role_19 = 19;

constraint
  Role_disk[role_19] = 1;
1..20: role_20 = 20;

constraint
  Role_disk[role_20] = 1;
var set of {1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58}: roles;
constraint card(roles) = 20;

constraint
  forall (m in machines) (
    (sum (r in 1..60) (
    bool2int(r in roles /\ Role_host[r] = m) * (Role_cpu[r])
  ) <= Machine_cpu[m] /\ sum (r in 1..60) (
    bool2int(r in roles /\ Role_host[r] = m) * (Role_memory[r])
  ) <= Machine_memory[m] /\ sum (r in 1..60) (
    bool2int(r in roles /\ Role_host[r] = m) * (Role_disk[r])
  ) <= Machine_disk[m])
  );

solve satisfy;

output [
  show(roles),
  show(role_20),
  show(role_19),
  show(role_18),
  show(role_17),
  show(role_16),
  show(role_15),
  show(role_14),
  show(role_13),
  show(role_12),
  show(role_11),
  show(role_10),
  show(role_9),
  show(role_8),
  show(role_7),
  show(role_6),
  show(role_5),
  show(role_4),
  show(role_3),
  show(role_2),
  show(role_1),
  show(machines),
  show(Role_memory),
  show(Role_cpu),
  show(Role_disk),
  show(Role_host),
  show(Machine_disk),
  show(Machine_memory),
  show(Machine_cpu)];
