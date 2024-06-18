package NocMuxDemuxTest;
import NocMuxDemux::*;
import NocPacket::*;
import Packet::*;
import NocConnetion::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs :: * ;
import Vector::*;
import GetPut::*;
import NocTestUtils::*;
import Connectable::*;
import Arbiter::*;

`include "NocPacket.defines"
typedef 4 MyWLen;
typedef 16 MyWSeq;
typedef 8 MyWId;
typedef NocHeader#(3, MyWId, MyWId, MyWSeq, MyWLen, 32, 0, 1) MyTestHeader;

typedef NocPacket#(3, MyWId, MyWId, MyWSeq, MyWLen, 32, 0, 1, 1) MyTestPacket;

module [`ModuleWithNocPacketG] mkDemuxSimpleTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   NumEq#(nOut, 3),
   NumEq#(nBytes, 4),
   NumEq#(mBytes, 8)
   );

   Reg#(Bit#(TLog#(nOut))) route_cnt <- mkReg(0);

   Tuple2#(GetPut#(`NocFlitG(nBytes)), NocTestCtrl) tb <- mkNocTransSimpleTestCore(defaultHeaderCb, defaultPayloadCb, defaultHeaderCb, defaultPayloadCb, 0, 1, 100);

   match {.upstream, .downstream} = tpl_1(tb);

   Get#(`NocFlitWG(TDiv#(mBytes, nBytes), nBytes)) up_get <- toGetM(upstream);
   Get#(`NocFlitG(mBytes)) dmux_in = toGet(up_get);

   Put#(`NocFlitWG(TDiv#(mBytes, nBytes), nBytes)) down_put <- toPutM(downstream);
   Put#(`NocFlitG(mBytes)) demux_out = toPut(down_put);

   Vector#(nOut, NocFlitPPut#(`NocPacketParams, mBytes)) outs;
   outs = replicate(nocSplit(demux_out));

   let simple_route_fn = interface RouteFunc;
      method Action route(Integer id, Bool dir, Bit#(wInit) init, Bit#(wTarg) targ);
         if (route_cnt == fromInteger(valueOf(nOut) - 1)) begin
            route_cnt <= 0;
         end
         else begin
            route_cnt <= route_cnt + 1;
         end
      endmethod
      method Bit#(TLog#(nOut)) out = route_cnt;
   endinterface;


   mkNocDemux(1, True, nocSplit(dmux_in), outs, simple_route_fn);

   rule done (tpl_2(tb).done);
      $display("Simulation Pass!");
      $finish(0);
   endrule
endmodule


(* synthesize *)
module mkDemuxSimpleTest();  
   MyTestPacket p = unpack(0);
   getNocModule(p, mkDemuxSimpleTestCore);     
endmodule


module [`ModuleWithNocPacketG] mkMuxSimpleTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   Add#(TLog#(nIn), zz, wInit),
   NumEq#(nIn, 3),
   NumEq#(nBytes, 4),
   NumEq#(mBytes, 8)
   );

   Vector#(nIn, Tuple2#(GetPut#(`NocFlitG(nBytes)), NocTestCtrl)) tbs;
   for (Integer i = 0; i < valueOf(nIn); i = i+ 1) begin
      function ActionValue#(`NocHeaderG) set_header_id(`NocHeaderG h);
         return actionvalue
            let _h = h;
            _h.routeId.initId = fromInteger(i);
            _h.routeId.targId = fromInteger(i);
            return _h;
         endactionvalue;
      endfunction
      tbs[i] <- mkNocTransSimpleTestCore(set_header_id, defaultPayloadCb, defaultHeaderCb, defaultPayloadCb, 0, 1, 100);
   end

   FIFOF#(`NocFlitG(nBytes)) link <- mkSizedBypassFIFOF(1);

   match {.ports, .ctrls} = unzip(tbs);

   match {.upstreams, .downstreams} = unzip(ports);

   Vector#(nIn, Get#(`NocFlitWG(TDiv#(mBytes, nBytes), nBytes))) upstream_outs <- mapM(toGetM, upstreams);
   Vector#(nIn, Get#(`NocFlitG(mBytes))) mux_ins = map(toGet, upstream_outs);

   Put#(`NocFlitWG(TDiv#(mBytes, nBytes), nBytes)) link_in <- toPutM(toPut(link));
   Put#(`NocFlitG(mBytes)) mux_out = toPut(link_in);


   Arbiter_IFC#(nIn) arbiter <- mkRRArbiter;
   mkNocMux(1, True, map(nocSplit, mux_ins), nocSplit(mux_out), arbiter);


   Wire#(Bit#(TLog#(nIn))) init_id <- mkDWire(?);

   let route_fn = interface RouteFunc;
      method Action route(Integer id, Bool dir, Bit#(wInit) init, Bit#(wTarg) targ);
         init_id <= truncate(init);
      endmethod
      method Bit#(TLog#(nIn)) out = init_id;
   endinterface;

   let demux_outs = map(nocSplit, downstreams);

   mkNocDemux(1, True, nocSplit(toGet(link)), demux_outs, route_fn);


   function Bool sim_done(Bool pre, NocTestCtrl ctrl);
      return pre && ctrl.done;
   endfunction

   rule done (foldl(sim_done, True, ctrls));
      $display("Simulation Pass!");
      $finish(0);
   endrule
endmodule


(* synthesize *)
module mkMuxSimpleTest();  
   MyTestPacket p = unpack(0);
   getNocModule(p, mkMuxSimpleTestCore);     
endmodule


module [`ModuleWithNocPacketG] mkMuxFifoArbiterTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   Add#(TLog#(nIn), zz, wInit),
   NumEq#(nIn, 3),
   NumEq#(nBytes, 4),
   NumEq#(mBytes, 8)
   );

   Vector#(nIn, Tuple2#(GetPut#(`NocFlitG(nBytes)), NocTestCtrl)) tbs;
   for (Integer i = 0; i < valueOf(nIn); i = i+ 1) begin
      function ActionValue#(`NocHeaderG) set_header_id(`NocHeaderG h);
         return actionvalue
            let _h = h;
            _h.routeId.initId = fromInteger(i);
            _h.routeId.targId = fromInteger(i);
            return _h;
         endactionvalue;
      endfunction
      tbs[i] <- mkNocTransSimpleTestCore(set_header_id, defaultPayloadCb, defaultHeaderCb, defaultPayloadCb, 0, 1, 100);
   end

   FIFOF#(`NocFlitG(nBytes)) link <- mkSizedBypassFIFOF(1);

   match {.ports, .ctrls} = unzip(tbs);

   match {.upstreams, .downstreams} = unzip(ports);

   Vector#(nIn, Get#(`NocFlitWG(TDiv#(mBytes, nBytes), nBytes))) upstream_outs <- mapM(toGetM, upstreams);
   Vector#(nIn, Get#(`NocFlitG(mBytes))) mux_ins = map(toGet, upstream_outs);

   Put#(`NocFlitWG(TDiv#(mBytes, nBytes), nBytes)) link_in <- toPutM(toPut(link));
   Put#(`NocFlitG(mBytes)) mux_out = toPut(link_in);


   Arbiter_IFC#(nIn) arbiter <- mkFifoArbiter;
   mkNocMux(1, True, map(nocSplit, mux_ins), nocSplit(mux_out), arbiter);


   Wire#(Bit#(TLog#(nIn))) init_id <- mkDWire(?);

   let route_fn = interface RouteFunc;
      method Action route(Integer id, Bool dir, Bit#(wInit) init, Bit#(wTarg) targ);
         init_id <= truncate(init);
      endmethod
      method Bit#(TLog#(nIn)) out = init_id;
   endinterface;

   let demux_outs = map(nocSplit, downstreams);

   mkNocDemux(1, True, nocSplit(toGet(link)), demux_outs, route_fn);


   function Bool sim_done(Bool pre, NocTestCtrl ctrl);
      return pre && ctrl.done;
   endfunction

   rule done (foldl(sim_done, True, ctrls));
      $display("Simulation Pass!");
      $finish(0);
   endrule
endmodule


(* synthesize *)
module mkMuxFifoArbiterTest();  
   MyTestPacket p = unpack(0);
   getNocModule(p, mkMuxFifoArbiterTestCore);     
endmodule

endpackage