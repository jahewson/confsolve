/*
  test that member references are encoded correctly
*/

class Server {
  var client as ref Client;
  var w as Widget[4];
}

class Client {
  var id as int;
  var module as Module;
}

class Module {
  var width as int;
  var module as ref Module;
  var widget as ref Widget;
  var multi as ref Widget[2];
}

class Widget {
  var phase as int;
}

var c as Client;
var s as Server;

// obj.int
c.id = 3;

// obj.ref.int
s.client.id = 3;

// obj.obj.int
c.module.width = 4;

// obj.obj.ref
c.module.module = c.module;

// obj.ref.obj.int
s.client.module.width = 4;

// obj.ref.obj.ref.int
s.client.module.widget.phase = 5;

// TODO - it seems that this creates unnessesary object IDs for each set member
//        because I don't allow array access such as cset[3].id so it has no use.
var cset as Client[4];

// set of objects
forall cli in cset {
  cli != c;
};

// a fold over only a subset of the available targets
var refs as ref Client[2];
forall cr in refs {
  cr != c;
};
