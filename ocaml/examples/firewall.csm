// test3.spec

/****************************
** Simple firewall example **
****************************/

/** Consider a system with 4 firewalls, configured thus:
**                   ---> f0 -=-> f2 -=== redNet
**   <internet> --><        ><       ><
**                   ---> f1 -=-> f3 -=== blueNet
**
**  That is, f0, f1 could be connected to any of f2, f3,
**  and f2,f3 could both be connected to any of redNet and blueNet.
**
**  We want to ensure that
**    (a) incoming http connections are allowed, and
**    (b) incoming telnet connections are allowed, and
**    (c) any firewall with incoming telnet connections does not
**          have a connection to the blueNet.
*/

class Firewall {
    var httpIn as bool ;   // True if incoming http is allowed
    var telnetIn as bool ;  // True if incoming telnet is allowed

    var blueAccess as bool ; // True if access is allowed to the local blue net.
    var redAccess as bool ; // True if access is allowed to the local red net.
}

class FirewallWithPolicy {
    var firewall as Firewall ;

    // We disallow firewalls with blue net access from having incoming telnet enabled.
    !(firewall.telnetIn && firewall.blueAccess) ;
}

class SystemWithPolicy {
    var f0 as FirewallWithPolicy ;
    var f1 as FirewallWithPolicy ;
    var f2 as FirewallWithPolicy ;
    var f3 as FirewallWithPolicy ;

    var f0_to_f2 as bool ; // Whether f0 is connected to f2 and f3
    var f0_to_f3 as bool ; // Whether f0 is connected to f2 and f3

    var f1_to_f2 as bool ; // Whether f1 is connected to f2 and f3
    var f1_to_f3 as bool ; // Whether f1 is connected to f2 and f3

    var f2_to_red as bool ; // Whether f2 is connected to red, blue nets
    var f2_to_blue as bool ; // Whether f2 is connected to red, blue nets
    
    var f3_to_red as bool ; // Whether f3 is connected to red, blue nets
    var f3_to_blue as bool ; // Whether f3 is connected to red, blue nets

    /*** Blue net rules ***/

    // if a firewall is directly connected to the blue net, it has "blueAccess"
    f2_to_blue <-> f2.firewall.blueAccess ;
    f3_to_blue <-> f3.firewall.blueAccess ;

    // if a firewall is indirectly conncted to blue net, it still has "blueAccess"
    f2.firewall.blueAccess && f0_to_f2 -> f0.firewall.blueAccess ;
    f3.firewall.blueAccess && f0_to_f3 -> f0.firewall.blueAccess ;

    f2.firewall.blueAccess && f1_to_f2 -> f1.firewall.blueAccess ;
    f3.firewall.blueAccess && f1_to_f3 -> f1.firewall.blueAccess ;

    /*** Red net rules ***/

    // if a firewall is directly connected to the red net, it has "redAccess"
    f2_to_red <-> f2.firewall.redAccess ;
    f3_to_red <-> f3.firewall.redAccess ;

    // if a firewall is indirectly conncted to red net, it still has "redAccess"
    f2.firewall.redAccess && f0_to_f2 -> f0.firewall.redAccess ;
    f3.firewall.redAccess && f0_to_f3 -> f0.firewall.redAccess ;

    f2.firewall.redAccess && f1_to_f2 -> f1.firewall.redAccess ;
    f3.firewall.redAccess && f1_to_f3 -> f1.firewall.redAccess ;

    // both blue and red nets should have http access.
    
    /** Rule for http access to blue net **/
    
                         (f0_to_f2 && f2_to_blue && f0.firewall.httpIn && f2.firewall.httpIn)
                      || (f0_to_f3 && f3_to_blue && f0.firewall.httpIn && f3.firewall.httpIn)
                      || (f1_to_f2 && f2_to_blue && f1.firewall.httpIn && f2.firewall.httpIn)
                      || (f1_to_f3 && f3_to_blue && f1.firewall.httpIn && f3.firewall.httpIn) ;

    /** Rule for http access to blue net **/
    
                         (f0_to_f2 && f2_to_red && f0.firewall.httpIn && f2.firewall.httpIn)
                      || (f0_to_f3 && f3_to_red && f0.firewall.httpIn && f3.firewall.httpIn)
                      || (f1_to_f2 && f2_to_red && f1.firewall.httpIn && f2.firewall.httpIn)
                      || (f1_to_f3 && f3_to_red && f1.firewall.httpIn && f3.firewall.httpIn) ;
}

// globals
var s as SystemWithPolicy ;
