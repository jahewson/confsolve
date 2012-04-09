var yy as C;
var ry as ref C;
var sry as ref C [2];
var x as bool;
var y as C[3];
var s as 1..5[2];

class B extends C2
  var x as 1..10;
  x > 3
end;
  
class C : C2
  var mv as int; 
  var mc as B;
  var md as B;
  var mb as B[2] ;
  var mr as ref B[2]
end;

var p as C;
var q as C;
var r as int;

666 > 4 * 5 && true != false or 2 > 3;

forall c in y {
  c.mv is not 4
};

for b in yy.mb {
  b.x > 4
};

exists c in sry {
  c.mv < 9
}