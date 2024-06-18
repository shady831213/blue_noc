package NocConnectionTest;
import NocPacket::*;
import Packet::*;
import NocConnetion::*;
import Vector::*;
import GetPut::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs :: * ;
import Connectable::*;
import NocTestUtils::*;
`include "NocPacket.defines"
typedef 4 MyWLen;
typedef 16 MyWSeq;
typedef 8 MyWId;
typedef NocHeader#(3, MyWId, MyWId, MyWSeq, MyWLen, 32, 0, 1) MyTestHeader;

typedef NocPacket#(3, MyWId, MyWId, MyWSeq, MyWLen, 32, 0, 1, 1) MyTestPacket;

module mkNocConnectionTestCore#(Integer in_bubble, Integer out_bp)(GetPut#(`NocFlitG(nBytes)))
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr)
   );
   match {.ports, .ctrl} <- mkNocTransSimpleTestCore(defaultHeaderCb, defaultPayloadCb, defaultHeaderCb, defaultPayloadCb, in_bubble, out_bp, 100);
   rule done (ctrl.done);
      $display("Simulation Pass!");
      $finish(0);
   endrule
   return ports;
endmodule

module [`ModuleWithNocPacketG] mkFlit2FlitWTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   NumEq#(nBytes, 4),
   NumEq#(mBytes, 8)
   );

   FIFOF#(`NocFlitG(mBytes)) midstream <- mkSizedBypassFIFOF(8);

   GetPut#(`NocFlitG(nBytes)) ifc <- mkNocConnectionTestCore(0, 1);

   match {.upstream, .downstream} = ifc;
   Put#(`NocFlitWG(TDiv#(mBytes, nBytes), nBytes)) mid_put = toPut(toPut(midstream));
   Get#(`NocFlitWG(TDiv#(mBytes, nBytes), nBytes)) mid_get = toGet(toGet(midstream));

   mkConnection(upstream, mid_put);
   mkConnection(mid_get, downstream);
endmodule

(* synthesize *)
module mkFlit2FlitWTest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkFlit2FlitWTestCore);
endmodule

module [`ModuleWithNocPacketG] mkFlit2FlitPTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   NumEq#(nBytes, 4)
   );

   FIFOF#(`NocTransHeaderG) header <- mkSizedBypassFIFOF(4);
   FIFOF#(`NocTransPayloadG(nBytes)) payload <- mkSizedBypassFIFOF(8);

   GetPut#(`NocFlitG(nBytes)) ifc <- mkNocConnectionTestCore(0, 1);
   match {.upstream, .downstream} = ifc;

   mkConnection(upstream, tuple2(toPut(header), toPut(payload)));

   mkConnection(tuple2(toGet(header), toGet(payload)), downstream);
endmodule

(* synthesize *)
module mkFlit2FlitPTest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkFlit2FlitPTestCore);
endmodule


module [`ModuleWithNocPacketG] mkFlit2FlitPWTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   NumEq#(nBytes, 4),
   NumEq#(mBytes, 16)
   );

   FIFOF#(`NocTransHeaderG) header <- mkSizedBypassFIFOF(4);
   FIFOF#(`NocTransPayloadG(mBytes)) payload <- mkSizedBypassFIFOF(4);


   GetPut#(`NocFlitG(nBytes)) ifc <- mkNocConnectionTestCore(0, 1);
   match {.upstream, .downstream} = ifc;

   Put#(`NocTransPayloadWG(TDiv#(mBytes, nBytes), nBytes)) payload_put = toPut(toPut(payload));
   Get#(`NocTransPayloadWG(TDiv#(mBytes, nBytes), nBytes)) payload_get = toGet(toGet(payload));

   mkConnection(upstream, tuple2(toPut(header), payload_put));

   mkConnection(tuple2(toGet(header), payload_get), downstream);
endmodule

(* synthesize *)
module mkFlit2FlitPWTest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkFlit2FlitPWTestCore);
endmodule

module [`ModuleWithNocPacketG] mkFlitW2FlitPTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   NumEq#(nBytes, 4),
   NumEq#(mBytes, 32),
   NumEq#(lBytes, 8)
   );

   FIFOF#(`NocFlitG(mBytes)) flitw_up <- mkSizedBypassFIFOF(3);

   FIFOF#(`NocTransHeaderG) header <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(nBytes)) payload <- mkSizedBypassFIFOF(3);

   FIFOF#(`NocFlitG(lBytes)) flitw_down <- mkSizedBypassFIFOF(3);


   GetPut#(`NocFlitG(nBytes)) ifc <- mkNocConnectionTestCore(0, 1);
   match {.upstream, .downstream} = ifc;

   Put#(`NocFlitWG(TDiv#(mBytes, nBytes), nBytes)) flitw_up_put = toPut(toPut(flitw_up));
   Get#(`NocFlitWG(TDiv#(mBytes, nBytes), nBytes)) flitw_up_get = toGet(toGet(flitw_up));

   mkConnection(upstream, flitw_up_put);

   mkConnection(flitw_up_get, tuple2(toPut(header), toPut(payload)));

   // mkConnection(upstream, tuple2(toPut(header), toPut(payload)));
   Put#(`NocFlitWG(TDiv#(lBytes, nBytes), nBytes)) flitw_down_put = toPut(toPut(flitw_down));
   Get#(`NocFlitWG(TDiv#(lBytes, nBytes), nBytes)) flitw_down_get = toGet(toGet(flitw_down));

   mkConnection(tuple2(toGet(header), toGet(payload)), flitw_down_put);

   mkConnection(flitw_down_get, downstream);
   // mkConnection(tuple2(toGet(header), toGet(payload)), downstream);

endmodule

(* synthesize *)
module mkFlitW2FlitPTest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkFlitW2FlitPTestCore);
endmodule


module [`ModuleWithNocPacketG] mkFlitW2FlitPWTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   NumEq#(nBytes, 4),
   NumEq#(mBytes, 8),
   NumEq#(lBytes, 16)
   );

   FIFOF#(`NocFlitG(mBytes)) flitw_up <- mkSizedBypassFIFOF(3);

   FIFOF#(`NocTransHeaderG) header <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(lBytes)) payload <- mkSizedBypassFIFOF(3);

   FIFOF#(`NocFlitG(mBytes)) flitw_down <- mkSizedBypassFIFOF(3);


   GetPut#(`NocFlitG(nBytes)) ifc <- mkNocConnectionTestCore(0, 1);
   match {.upstream, .downstream} = ifc;

   Put#(`NocFlitWG(TDiv#(mBytes, nBytes), nBytes)) flitw_up_put = toPut(toPut(flitw_up));

   Put#(`NocTransPayloadWG(TDiv#(lBytes, mBytes), mBytes)) payload_put = toPut(toPut(payload));

   mkConnection(upstream, flitw_up_put);

   mkConnection(toGet(flitw_up), tuple2(toPut(header), payload_put));

   // mkConnection(upstream, tuple2(toPut(header), toPut(payload)));

   Get#(`NocTransPayloadWG(TDiv#(lBytes, mBytes), mBytes)) payload_get = toGet(toGet(payload));

   Get#(`NocFlitWG(TDiv#(mBytes, nBytes), nBytes)) flitw_down_get = toGet(toGet(flitw_down));

   mkConnection(tuple2(toGet(header), payload_get), toPut(flitw_down));

   mkConnection(flitw_down_get, downstream);
   // mkConnection(tuple2(toGet(header), toGet(payload)), downstream);

endmodule

(* synthesize *)
module mkFlitW2FlitPWTest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkFlitW2FlitPWTestCore);
endmodule

module [`ModuleWithNocPacketG] mkFlitP2FlitPWTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   NumEq#(nBytes, 4),
   NumEq#(mBytes, 16),
   NumEq#(lBytes, 8)
   );

   FIFOF#(`NocTransHeaderG) header_up <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(nBytes)) payload_up <- mkSizedBypassFIFOF(3);

   FIFOF#(`NocTransHeaderG) header <- mkSizedBypassFIFOF(2);
   FIFOF#(`NocTransPayloadG(mBytes)) payload <- mkSizedBypassFIFOF(3);

   FIFOF#(`NocTransHeaderG) header_down <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(lBytes)) payload_down <- mkSizedBypassFIFOF(3);


   GetPut#(`NocFlitG(nBytes)) ifc <- mkNocConnectionTestCore(0, 1);
   match {.upstream, .downstream} = ifc;

   mkConnection(upstream, tuple2(toPut(header_up), toPut(payload_up)));

   Put#(`NocTransPayloadWG(TDiv#(mBytes, nBytes), nBytes)) payload_put = toPut(toPut(payload));

   mkConnection(tuple2(toGet(header_up), toGet(payload_up)), tuple2(toPut(header), payload_put));

   // mkConnection(upstream, tuple2(toPut(header), toPut(payload)));

   Get#(`NocTransPayloadWG(TDiv#(mBytes, lBytes), lBytes)) payload_get = toGet(toGet(payload));

   mkConnection(tuple2(toGet(header), payload_get), tuple2(toPut(header_down), toPut(payload_down)));


   Get#(`NocTransPayloadWG(TDiv#(lBytes, nBytes), nBytes)) payload_down_get = toGet(toGet(payload_down));

   mkConnection(tuple2(toGet(header_down), payload_down_get), downstream);
   // mkConnection(tuple2(toGet(header), toGet(payload)), downstream);

endmodule

(* synthesize *)
module mkFlitP2FlitPWTest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkFlitP2FlitPWTestCore);
endmodule

module [`ModuleWithNocPacketG] mkFlit2FlitATestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   NumEq#(nBytes, 4),
   NumEq#(mBytes, 2)
   );

   FIFOF#(`NocFlitAG(mBytes)) midstream <- mkSizedBypassFIFOF(8);

   GetPut#(`NocFlitG(nBytes)) ifc <- mkNocConnectionTestCore(0, 1);

   match {.upstream, .downstream} = ifc;
   Get#(`NocFlitWG(TDiv#(nBytes, mBytes), mBytes)) up_get = toGet(upstream);
   Put#(`NocFlitWG(TDiv#(nBytes, mBytes), mBytes)) down_put = toPut(downstream);

   mkConnection(up_get, toPut(midstream));
   mkConnection(toGet(midstream), down_put);
endmodule

(* synthesize *)
module mkFlit2FlitATest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkFlit2FlitATestCore);
endmodule

module [`ModuleWithNocPacketG] mkFlitP2FlitATestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   NumEq#(nBytes, 4),
   NumEq#(mBytes, 2),
   NumEq#(lBytes, 8)
   );

   FIFOF#(`NocFlitAG(mBytes)) aotu_up <- mkSizedBypassFIFOF(8);

   FIFOF#(`NocTransHeaderG) header_up <- mkSizedBypassFIFOF(2);
   FIFOF#(`NocTransPayloadG(lBytes)) payload_up <- mkSizedBypassFIFOF(3);

   FIFOF#(`NocFlitAG(nBytes)) aotu_down <- mkSizedBypassFIFOF(8);

   FIFOF#(`NocTransHeaderG) header_down <- mkSizedBypassFIFOF(2);
   FIFOF#(`NocTransPayloadG(nBytes)) payload_down <- mkSizedBypassFIFOF(3);

   FIFOF#(`NocFlitAG(nBytes)) aotu_down2 <- mkSizedBypassFIFOF(8);

   GetPut#(`NocFlitG(nBytes)) ifc <- mkNocConnectionTestCore(0, 1);

   match {.upstream, .downstream} = ifc;


   Get#(`NocFlitWG(TDiv#(nBytes, mBytes), mBytes)) up_get = toGet(upstream);

   mkConnection(up_get, toPut(aotu_up));

   Put#(`NocTransPayloadWG(TDiv#(lBytes, mBytes), mBytes)) payload_up_put = toPut(toPut(payload_up));

   mkConnection(toGet(aotu_up), tuple2(toPut(header_up), payload_up_put));

   Get#(`NocTransPayloadWG(TDiv#(lBytes, nBytes), nBytes)) payload_up_get = toGet(toGet(payload_up));

   mkConnection(tuple2(toGet(header_up), payload_up_get), toPut(aotu_down));

   mkConnection(toGet(aotu_down), tuple2(toPut(header_down), toPut(payload_down)));

   mkConnection(tuple2(toGet(header_down), toGet(payload_down)), toPut(aotu_down2));

   mkConnection(toGet(aotu_down2), downstream);

endmodule

(* synthesize *)
module mkFlitP2FlitATest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkFlitP2FlitATestCore);
endmodule



endpackage