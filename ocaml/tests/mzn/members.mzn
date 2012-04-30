array[1..2] of var 1..10: Server_client;
array[1..2] of set of 1..8: Server_w = [{1,2,3,4},{5,6,7,8}];
array[1..10] of var int: Client_id;
array[1..10] of 1..10: Client_moduleule = [1,2,3,4,5,6,7,8,9,10];
array[1..10] of var int: moduleule_width;
array[1..10] of var 1..10: moduleule_module;
array[1..10] of var 1..8: moduleule_widget;
array[1..10] of var set of 1..8: moduleule_multi;
constraint forall (i in 1..10) (card(moduleule_multi[i]) = 2);
array[1..8] of var int: Widget_phase;
1..10: c = 1;
1..2: s = 1;

constraint
  Client_id[c] = 3;

constraint
  Client_id[Server_client[s]] = 3;

constraint
  moduleule_width[Client_moduleule[c]] = 4;

constraint
  moduleule_module[Client_moduleule[c]] = Client_moduleule[c];

constraint
  moduleule_width[Client_moduleule[Server_client[s]]] = 4;

constraint
  Widget_phase[moduleule_widget[Client_moduleule[Server_client[s]]]] = 5;
set of 1..10: cset = {2,3,4,5};

constraint
  forall (cli in cset) (
    (cli != c)
  );
var set of 1..10: refs;
constraint card(refs) = 2;

constraint
  forall (cr in 1..10) (
    cr in refs -> (cr != c)
  );

solve satisfy;

output [
  show(refs),
  show(cset),
  show(s),
  show(c),
  show(Widget_phase),
  show(moduleule_multi),
  show(moduleule_widget),
  show(moduleule_module),
  show(moduleule_width),
  show(Client_moduleule),
  show(Client_id),
  show(Server_w),
  show(Server_client)];
