package Packet;
typeclass Packet#(
    type pkt,
    type h,
    type p,
    type t) 
    dependencies (pkt determines (h, p, t));
endtypeclass

endpackage
