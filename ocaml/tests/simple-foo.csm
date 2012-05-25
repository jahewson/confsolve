class Bar {
  var z as int;
  z > 3;
}

class Foo {
  var x as int;
  var y1 as Bar;
  var y2 as Bar;
  var b as ref Bar;
  var s as 1..3[2];
  var rsn as ref Foo[2];
  var SN as Bar[3];
  var rrn as ref Bar[1..3];
}

// objects
var V as Foo;
var W as Foo;
V.y1.z > 5;

// references
var R as ref Foo;
R.y1.z > 8;

// set of range
var P as 1..3[3];

// sets of reference (fixed card)
var RS as ref Foo[2];

// set of object
var S as Foo[3];

// object/reference equality
var b as ref Bar[3];
b = V.SN;

// set of reference (var card)
var rr as ref Bar[1..3];

// complex expression
V.x + 2 = 4 -> V.x < 100;

// fold over set of reference
forall rs in RS {
  rs.x > 0;
};

// fold over set of scalar
forall p in P {
  p > 0;
};

// fold over set of object (nested)
/*forall (b in V.SN) {  // not-supported (non-constant MZ)
  b.z > 2;
};*/

// this achieves the same as the not-supported fold above - todo: AST re-write to allow this.
forall Q in b {
  Q.z > 2;
};