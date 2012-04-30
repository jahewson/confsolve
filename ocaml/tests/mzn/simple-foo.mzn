array[1..50] of var int: Bar_z;

constraint
  forall (i in 1..50) (
    Bar_z[i] > 3
  );
array[1..10] of var int: Foo_x;
array[1..10] of 1..50: Foo_y1 = [1,2,3,4,5,6,7,8,9,10];
array[1..10] of 1..50: Foo_y2 = [11,12,13,14,15,16,17,18,19,20];
array[1..10] of var 1..50: Foo_b;
array[1..10] of var set of 1..3: Foo_s;
constraint forall (i in 1..10) (card(Foo_s[i]) = 2);
array[1..10] of var set of 1..10: Foo_rsn;
constraint forall (i in 1..10) (card(Foo_rsn[i]) = 2);
array[1..10] of set of 1..50: Foo_SN = [{21,22,23},{24,25,26},{27,28,29},{30,31,32},{33,34,35},{36,37,38},{39,40,41},{42,43,44},{45,46,47},{48,49,50}];
array[1..10] of var set of 1..50: Foo_rrn;
constraint forall (i in 1..10) (card(Foo_rrn[i]) >= 1 /\ card(Foo_rrn[i]) <= 3);
1..10: V = 1;
1..10: W = 2;

constraint
  Bar_z[Foo_y1[V]] > 5;
var 1..10: R;

constraint
  Bar_z[Foo_y1[R]] > 8;
var set of 1..3: P;
constraint card(P) = 3;
var set of 1..10: RS;
constraint card(RS) = 2;
set of 1..10: S = {3,4,5};
var set of 1..50: b;
constraint card(b) = 3;

constraint
  b = Foo_SN[V];
var set of 1..50: rr;
constraint card(rr) >= 1 /\ card(rr) <= 3;

constraint
  Foo_x[V] + 2 = 4 -> Foo_x[V] < 100;

constraint
  forall (rs in 1..10) (
    rs in RS -> (Foo_x[rs] > 0)
  );

constraint
  forall (p in 3..3) (
    p in P -> (p > 0)
  );

constraint
  forall (Q in 1..50) (
    Q in b -> (Bar_z[Q] > 2)
  );

solve satisfy;

output [
  show(rr),
  show(b),
  show(S),
  show(RS),
  show(P),
  show(R),
  show(W),
  show(V),
  show(Foo_rrn),
  show(Foo_SN),
  show(Foo_rsn),
  show(Foo_s),
  show(Foo_b),
  show(Foo_y2),
  show(Foo_y1),
  show(Foo_x),
  show(Bar_z)];
