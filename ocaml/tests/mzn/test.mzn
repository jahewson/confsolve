1..12: yy = 1;
var 1..12: ry;
var set of 1..12: sry;
constraint card(sry) = 2;
var bool: x;
set of 1..12: y = {2,3,4};
var set of 1..5: s;
constraint card(s) = 2;
array[1..48] of var 1..10: B_x;

constraint
  forall (i in 1..48) (
    B_x[i] > 3
  );
array[1..12] of var int: C_mv;
array[1..12] of 1..48: C_mc = [1,2,3,4,5,6,7,8,9,10,11,12];
array[1..12] of 1..48: C_md = [13,14,15,16,17,18,19,20,21,22,23,24];
array[1..12] of set of 1..48: C_mb = [{25,26},{27,28},{29,30},{31,32},{33,34},{35,36},{37,38},{39,40},{41,42},{43,44},{45,46},{47,48}];
array[1..12] of var set of 1..48: C_mr;
constraint forall (i in 1..12) (card(C_mr[i]) = 2);
1..12: p = 5;
1..12: q = 6;
var int: r;

constraint
  666 > 4 * 5 /\ true != false \/ 2 > 3;

constraint
  forall (c in y) (
    (C_mv[c] != 4)
  );

constraint
  forall (b in C_mb[yy]) (
    ((B_x[b] > 4) /\ (B_x[b] > 5))
  );

constraint
  exists (c in 1..12) (
    c in sry -> (C_mv[c] < 9)
  );

solve satisfy;

output [
  show(r),
  show(q),
  show(p),
  show(C_mr),
  show(C_mb),
  show(C_md),
  show(C_mc),
  show(C_mv),
  show(B_x),
  show(s),
  show(y),
  show(x),
  show(sry),
  show(ry),
  show(yy)];
