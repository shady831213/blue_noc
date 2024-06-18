package NocAlignTest;
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
import NocAlign::*;
`include "NocPacket.defines"
typedef 4 MyWLen;
typedef 16 MyWSeq;
typedef 8 MyWId;
typedef NocHeader#(3, MyWId, MyWId, MyWSeq, MyWLen, 32, 0, 1) MyTestHeader;

typedef NocPacket#(3, MyWId, MyWId, MyWSeq, MyWLen, 32, 0, 1, 1) MyTestPacket;

module [`ModuleWithNocPacketG] mkNocSimpleAlignerTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   Add#(wLen, zz, wAddr),
   NumEq#(nBytes, 4),
   NumEq#(mBytes, 8)
   );

   Tuple2#(GetPut#(`NocFlitG(nBytes)), NocTestCtrl) test <- mkNocTransSimpleTestCore(defaultHeaderCb, defaultPayloadCb, defaultHeaderCb, defaultPayloadCb, 0, 1, 100);
   match {.ports, .ctrl} = test;
   rule done (ctrl.done);
      $display("[%0t] Simulation Pass!", $time);
      $finish(0);
   endrule

   match {.upstream, .downstream} = ports;

   FIFOF#(`NocTransHeaderG) w_header_in <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(mBytes)) w_payload_in <- mkSizedBypassFIFOF(1);
   Put#(`NocTransPayloadWG(TDiv#(mBytes, nBytes), nBytes)) w_payload_in_put = toPut(toPut(w_payload_in));

   FIFOF#(`NocTransHeaderG) w_header_out <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(mBytes)) w_payload_out <- mkSizedBypassFIFOF(1);
   NocPipePut#(`NocPacketParams, mBytes) w_pipe_out <- fromTransPut(tuple2(toPut(w_header_out), toPut(w_payload_out)));

   mkNocAligner(tuple2(toGet(w_header_in), toGet(w_payload_in)),w_pipe_out);

   mkConnection(upstream, tuple2(toPut(w_header_in), w_payload_in_put));

   FIFOF#(`NocFlitG(nBytes)) n_in <- mkSizedBypassFIFOF(2);

   FIFOF#(`NocFlitG(nBytes)) n_out <- mkSizedBypassFIFOF(2);
   NocPipePut#(`NocPacketParams, nBytes) n_pipe_out <- fromTransPut(toPut(n_out));

   mkNocAligner(toGet(n_in),n_pipe_out);

   Get#(`NocTransPayloadWG(TDiv#(mBytes, nBytes), nBytes)) w_payload_out_get = toGet(toGet(w_payload_out));
   mkConnection(tuple2(toGet(w_header_out), w_payload_out_get), toPut(n_in));


   mkConnection(toGet(n_out), downstream);
endmodule


(* synthesize *)
module mkNocSimpleAlignerTest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkNocSimpleAlignerTestCore);
endmodule


module [`ModuleWithNocPacketG] mkNoc1to4AlignerTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   Add#(wLen, zz, wAddr),
   NumEq#(nBytes, 2),
   NumEq#(mBytes, 8)
   );

   Tuple2#(GetPut#(`NocFlitG(nBytes)), NocTestCtrl) test <- mkNocTransSimpleTestCore(defaultHeaderCb, defaultPayloadCb, defaultHeaderCb, defaultPayloadCb, 0, 1, 100);
   match {.ports, .ctrl} = test;
   rule done (ctrl.done);
      $display("[%0t] Simulation Pass!", $time);
      $finish(0);
   endrule

   match {.upstream, .downstream} = ports;

   FIFOF#(`NocTransHeaderG) w_header_in <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(mBytes)) w_payload_in <- mkSizedBypassFIFOF(1);
   Put#(`NocTransPayloadWG(TDiv#(mBytes, nBytes), nBytes)) w_payload_in_put = toPut(toPut(w_payload_in));

   FIFOF#(`NocTransHeaderG) w_header_out <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(mBytes)) w_payload_out <- mkSizedBypassFIFOF(1);
   NocPipePut#(`NocPacketParams, mBytes) w_pipe_out <- fromTransPut(tuple2(toPut(w_header_out), toPut(w_payload_out)));

   mkNocAligner(tuple2(toGet(w_header_in), toGet(w_payload_in)),w_pipe_out);

   mkConnection(upstream, tuple2(toPut(w_header_in), w_payload_in_put));

   FIFOF#(`NocFlitG(nBytes)) n_in <- mkSizedBypassFIFOF(2);

   FIFOF#(`NocFlitG(nBytes)) n_out <- mkSizedBypassFIFOF(2);
   NocPipePut#(`NocPacketParams, nBytes) n_pipe_out <- fromTransPut(toPut(n_out));

   mkNocAligner(toGet(n_in),n_pipe_out);

   Get#(`NocTransPayloadWG(TDiv#(mBytes, nBytes), nBytes)) w_payload_out_get = toGet(toGet(w_payload_out));
   mkConnection(tuple2(toGet(w_header_out), w_payload_out_get), toPut(n_in));


   mkConnection(toGet(n_out), downstream);
endmodule

(* synthesize *)
module mkNoc1to4AlignerTest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkNoc1to4AlignerTestCore);
endmodule

module [`ModuleWithNocPacketG] mkNocSimpleFixedAlignerTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   Add#(wLen, zz, wAddr),
   NumEq#(nBytes, 4),
   NumEq#(mBytes, 8)
   );

   function ActionValue#(`NocHeaderG) fixedGenHeaderCb(`NocHeaderG h);
      return actionvalue
         let header = h;
         header.ty = FIXED;
         header.len = ((h.len + (fromInteger(valueOf(nBytes)) - 1)) & ~(fromInteger(valueOf(nBytes)) - 1)) >> fromInteger(valueOf(TLog#(nBytes)));
         return header;
      endactionvalue;
   endfunction

   function  ActionValue#(`NocHeaderG) fixedChkHeaderCb(`NocHeaderG h);
      return actionvalue
         let header = h;
         header.status = REQ;
         header.op = WR;
         return header;
      endactionvalue;
   endfunction
   
   Tuple2#(GetPut#(`NocFlitG(nBytes)), NocTestCtrl) test <- mkNocTransSimpleTestCore(fixedGenHeaderCb, defaultPayloadCb, fixedChkHeaderCb, defaultPayloadCb, 0, 1, 100);
   match {.ports, .ctrl} = test;
   rule done (ctrl.done);
      $display("[%0t] Simulation Pass!", $time);
      $finish(0);
   endrule

   match {.upstream, .downstream} = ports;

   FIFOF#(`NocTransHeaderG) w_header_in <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(mBytes)) w_payload_in <- mkSizedBypassFIFOF(1);
   Put#(`NocTransPayloadWG(TDiv#(mBytes, nBytes), nBytes)) w_payload_in_put = toPut(toPut(w_payload_in));

   FIFOF#(`NocTransHeaderG) w_header_out <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(mBytes)) w_payload_out <- mkSizedBypassFIFOF(1);
   NocPipePut#(`NocPacketParams, mBytes) w_pipe_out <- fromTransPut(tuple2(toPut(w_header_out), toPut(w_payload_out)));

   mkNocAligner(tuple2(toGet(w_header_in), toGet(w_payload_in)),w_pipe_out);

   mkConnection(upstream, tuple2(toPut(w_header_in), w_payload_in_put));

   FIFOF#(`NocFlitG(nBytes)) n_in <- mkSizedBypassFIFOF(2);

   FIFOF#(`NocFlitG(nBytes)) n_out <- mkSizedBypassFIFOF(2);
   NocPipePut#(`NocPacketParams, nBytes) n_pipe_out <- fromTransPut(toPut(n_out));

   mkNocAligner(toGet(n_in),n_pipe_out);

   Get#(`NocTransPayloadWG(TDiv#(mBytes, nBytes), nBytes)) w_payload_out_get = toGet(toGet(w_payload_out));

   Get#(`NocTransHeaderG) to_read = interface Get;
      method ActionValue#(`NocTransHeaderG) get;
         let h <- toGet(w_header_out).get;
         h.h.op = RD;
         h.h.status = RESP;
         return h;
      endmethod
   endinterface;
   mkConnection(tuple2(to_read, w_payload_out_get), toPut(n_in));

   mkConnection(toGet(n_out), downstream);
endmodule


(* synthesize *)
module mkNocSimpleFixedAlignerTest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkNocSimpleFixedAlignerTestCore);
endmodule


module [`ModuleWithNocPacketG] mkNoc1to4FixedAlignerTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   Add#(wLen, zz, wAddr),
   NumEq#(nBytes, 2),
   NumEq#(mBytes, 8)
   );

   function ActionValue#(`NocHeaderG) fixedGenHeaderCb(`NocHeaderG h);
      return actionvalue
         let header = h;
         header.ty = FIXED;
         header.len = ((h.len + (fromInteger(valueOf(nBytes)) - 1)) & ~(fromInteger(valueOf(nBytes)) - 1)) >> fromInteger(valueOf(TLog#(nBytes)));
         return header;
      endactionvalue;
   endfunction

   function  ActionValue#(`NocHeaderG) fixedChkHeaderCb(`NocHeaderG h);
      return actionvalue
         let header = h;
         header.status = REQ;
         header.op = WR;
         return header;
      endactionvalue;
   endfunction
   
   Tuple2#(GetPut#(`NocFlitG(nBytes)), NocTestCtrl) test <- mkNocTransSimpleTestCore(fixedGenHeaderCb, defaultPayloadCb, fixedChkHeaderCb, defaultPayloadCb, 0, 1, 100);
   match {.ports, .ctrl} = test;
   rule done (ctrl.done);
      $display("[%0t] Simulation Pass!", $time);
      $finish(0);
   endrule

   match {.upstream, .downstream} = ports;

   FIFOF#(`NocTransHeaderG) w_header_in <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(mBytes)) w_payload_in <- mkSizedBypassFIFOF(1);
   Put#(`NocTransPayloadWG(TDiv#(mBytes, nBytes), nBytes)) w_payload_in_put = toPut(toPut(w_payload_in));

   FIFOF#(`NocTransHeaderG) w_header_out <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(mBytes)) w_payload_out <- mkSizedBypassFIFOF(1);
   NocPipePut#(`NocPacketParams, mBytes) w_pipe_out <- fromTransPut(tuple2(toPut(w_header_out), toPut(w_payload_out)));

   mkNocAligner(tuple2(toGet(w_header_in), toGet(w_payload_in)),w_pipe_out);

   mkConnection(upstream, tuple2(toPut(w_header_in), w_payload_in_put));

   FIFOF#(`NocFlitG(nBytes)) n_in <- mkSizedBypassFIFOF(2);

   FIFOF#(`NocFlitG(nBytes)) n_out <- mkSizedBypassFIFOF(2);
   NocPipePut#(`NocPacketParams, nBytes) n_pipe_out <- fromTransPut(toPut(n_out));

   mkNocAligner(toGet(n_in),n_pipe_out);

   Get#(`NocTransPayloadWG(TDiv#(mBytes, nBytes), nBytes)) w_payload_out_get = toGet(toGet(w_payload_out));

   Get#(`NocTransHeaderG) to_read = interface Get;
      method ActionValue#(`NocTransHeaderG) get;
         let h <- toGet(w_header_out).get;
         h.h.op = RD;
         h.h.status = RESP;
         return h;
      endmethod
   endinterface;
   mkConnection(tuple2(to_read, w_payload_out_get), toPut(n_in));

   mkConnection(toGet(n_out), downstream);
endmodule


(* synthesize *)
module mkNoc1to4FixedAlignerTest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkNoc1to4FixedAlignerTestCore);
endmodule



module [`ModuleWithNocPacketG] mkNocFixedW2NAlignerTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   Add#(wLen, zz, wAddr),
   NumEq#(nBytes, 4),
   NumEq#(mBytes, 1)
   );

   function ActionValue#(`NocHeaderG) fixedGenHeaderCb(`NocHeaderG h);
      return actionvalue
         let header = h;
         header.ty = FIXED;
         header.len = ((h.len + (fromInteger(valueOf(nBytes)) - 1)) & ~(fromInteger(valueOf(nBytes)) - 1)) >> fromInteger(valueOf(TLog#(nBytes)));
         return header;
      endactionvalue;
   endfunction

   function  ActionValue#(`NocHeaderG) fixedChkHeaderCb(`NocHeaderG h);
      return actionvalue
         let header = h;
         header.status = REQ;
         header.op = WR;
         return header;
      endactionvalue;
   endfunction
   
   Tuple2#(GetPut#(`NocFlitG(nBytes)), NocTestCtrl) test <- mkNocTransSimpleTestCore(fixedGenHeaderCb, defaultPayloadCb, fixedChkHeaderCb, defaultPayloadCb, 0, 1, 100);
   match {.ports, .ctrl} = test;
   rule done (ctrl.done);
      $display("[%0t] Simulation Pass!", $time);
      $finish(0);
   endrule

   match {.upstream, .downstream} = ports;

   FIFOF#(`NocTransHeaderG) w_header_in <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(mBytes)) w_payload_in <- mkSizedBypassFIFOF(1);
   NocFlitWGet#(`NocPacketParams, TDiv#(nBytes, mBytes), mBytes) upstream_get = toGet(upstream);

   FIFOF#(`NocTransHeaderG) w_header_out <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocTransPayloadG(mBytes)) w_payload_out <- mkSizedBypassFIFOF(1);
   NocPipePut#(`NocPacketParams, mBytes) w_pipe_out <- fromTransPut(tuple2(toPut(w_header_out), toPut(w_payload_out)));

   mkNocW2NAligner(tuple2(toGet(w_header_in), toGet(w_payload_in)),w_pipe_out);

   mkConnection(upstream_get, tuple2(toPut(w_header_in),toPut(w_payload_in)));

   FIFOF#(`NocFlitG(nBytes)) n_in <- mkSizedBypassFIFOF(2);

   FIFOF#(`NocFlitG(nBytes)) n_out <- mkSizedBypassFIFOF(2);
   NocPipePut#(`NocPacketParams, nBytes) n_pipe_out <- fromTransPut(toPut(n_out));

   mkNocN2WAligner(toGet(n_in),n_pipe_out);

   NocFlitWPut#(`NocPacketParams, TDiv#(nBytes, mBytes), mBytes) n_in_put = toPut(toPut(n_in));

   Get#(`NocTransHeaderG) to_read = interface Get;
      method ActionValue#(`NocTransHeaderG) get;
         let h <- toGet(w_header_out).get;
         h.h.op = RD;
         h.h.status = RESP;
         return h;
      endmethod
   endinterface;
   mkConnection(tuple2(to_read, toGet(w_payload_out)), n_in_put);

   mkConnection(toGet(n_out), downstream);
endmodule


(* synthesize *)
module mkNocFixedW2NAlignerTest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkNocFixedW2NAlignerTestCore);
endmodule

endpackage