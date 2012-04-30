array[1..8] of var bool: Firewall_httpIn;
array[1..8] of var bool: Firewall_telnetIn;
array[1..8] of var bool: Firewall_blueAccess;
array[1..8] of var bool: Firewall_redAccess;
array[1..8] of 1..8: FirewallWithPolicy_firewall = [1,2,3,4,5,6,7,8];

constraint
  forall (i in 1..8) (
    not ((Firewall_telnetIn[FirewallWithPolicy_firewall[i]] /\ Firewall_blueAccess[FirewallWithPolicy_firewall[i]]))
  );
array[1..2] of 1..8: SystemWithPolicy_f0 = [1,2];
array[1..2] of 1..8: SystemWithPolicy_f1 = [3,4];
array[1..2] of 1..8: SystemWithPolicy_f2 = [5,6];
array[1..2] of 1..8: SystemWithPolicy_f3 = [7,8];
array[1..2] of var bool: SystemWithPolicy_f0_to_f2;
array[1..2] of var bool: SystemWithPolicy_f0_to_f3;
array[1..2] of var bool: SystemWithPolicy_f1_to_f2;
array[1..2] of var bool: SystemWithPolicy_f1_to_f3;
array[1..2] of var bool: SystemWithPolicy_f2_to_red;
array[1..2] of var bool: SystemWithPolicy_f2_to_blue;
array[1..2] of var bool: SystemWithPolicy_f3_to_red;
array[1..2] of var bool: SystemWithPolicy_f3_to_blue;

constraint
  forall (i in 1..2) (
    SystemWithPolicy_f2_to_blue[i] <-> Firewall_blueAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f2[i]]]
  );

constraint
  forall (i in 1..2) (
    SystemWithPolicy_f3_to_blue[i] <-> Firewall_blueAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f3[i]]]
  );

constraint
  forall (i in 1..2) (
    Firewall_blueAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f2[i]]] /\ SystemWithPolicy_f0_to_f2[i] -> Firewall_blueAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f0[i]]]
  );

constraint
  forall (i in 1..2) (
    Firewall_blueAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f3[i]]] /\ SystemWithPolicy_f0_to_f3[i] -> Firewall_blueAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f0[i]]]
  );

constraint
  forall (i in 1..2) (
    Firewall_blueAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f2[i]]] /\ SystemWithPolicy_f1_to_f2[i] -> Firewall_blueAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f1[i]]]
  );

constraint
  forall (i in 1..2) (
    Firewall_blueAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f3[i]]] /\ SystemWithPolicy_f1_to_f3[i] -> Firewall_blueAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f1[i]]]
  );

constraint
  forall (i in 1..2) (
    SystemWithPolicy_f2_to_red[i] <-> Firewall_redAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f2[i]]]
  );

constraint
  forall (i in 1..2) (
    SystemWithPolicy_f3_to_red[i] <-> Firewall_redAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f3[i]]]
  );

constraint
  forall (i in 1..2) (
    Firewall_redAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f2[i]]] /\ SystemWithPolicy_f0_to_f2[i] -> Firewall_redAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f0[i]]]
  );

constraint
  forall (i in 1..2) (
    Firewall_redAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f3[i]]] /\ SystemWithPolicy_f0_to_f3[i] -> Firewall_redAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f0[i]]]
  );

constraint
  forall (i in 1..2) (
    Firewall_redAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f2[i]]] /\ SystemWithPolicy_f1_to_f2[i] -> Firewall_redAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f1[i]]]
  );

constraint
  forall (i in 1..2) (
    Firewall_redAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f3[i]]] /\ SystemWithPolicy_f1_to_f3[i] -> Firewall_redAccess[FirewallWithPolicy_firewall[SystemWithPolicy_f1[i]]]
  );

constraint
  forall (i in 1..2) (
    (SystemWithPolicy_f0_to_f2[i] /\ SystemWithPolicy_f2_to_blue[i] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f0[i]]] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f2[i]]]) \/ (SystemWithPolicy_f0_to_f3[i] /\ SystemWithPolicy_f3_to_blue[i] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f0[i]]] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f3[i]]]) \/ (SystemWithPolicy_f1_to_f2[i] /\ SystemWithPolicy_f2_to_blue[i] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f1[i]]] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f2[i]]]) \/ (SystemWithPolicy_f1_to_f3[i] /\ SystemWithPolicy_f3_to_blue[i] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f1[i]]] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f3[i]]])
  );

constraint
  forall (i in 1..2) (
    (SystemWithPolicy_f0_to_f2[i] /\ SystemWithPolicy_f2_to_red[i] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f0[i]]] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f2[i]]]) \/ (SystemWithPolicy_f0_to_f3[i] /\ SystemWithPolicy_f3_to_red[i] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f0[i]]] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f3[i]]]) \/ (SystemWithPolicy_f1_to_f2[i] /\ SystemWithPolicy_f2_to_red[i] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f1[i]]] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f2[i]]]) \/ (SystemWithPolicy_f1_to_f3[i] /\ SystemWithPolicy_f3_to_red[i] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f1[i]]] /\ Firewall_httpIn[FirewallWithPolicy_firewall[SystemWithPolicy_f3[i]]])
  );
1..2: s = 1;

solve satisfy;

output [
  show(s),
  show(SystemWithPolicy_f3_to_blue),
  show(SystemWithPolicy_f3_to_red),
  show(SystemWithPolicy_f2_to_blue),
  show(SystemWithPolicy_f2_to_red),
  show(SystemWithPolicy_f1_to_f3),
  show(SystemWithPolicy_f1_to_f2),
  show(SystemWithPolicy_f0_to_f3),
  show(SystemWithPolicy_f0_to_f2),
  show(SystemWithPolicy_f3),
  show(SystemWithPolicy_f2),
  show(SystemWithPolicy_f1),
  show(SystemWithPolicy_f0),
  show(FirewallWithPolicy_firewall),
  show(Firewall_redAccess),
  show(Firewall_blueAccess),
  show(Firewall_telnetIn),
  show(Firewall_httpIn)];
