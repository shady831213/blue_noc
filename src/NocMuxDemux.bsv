package NocMuxDemux;
import NocPacket::*;
import Packet::*;
import GetPut::*;
import Vector::*;
import NocConnetion::*;
import Probe::*;
import Arbiter::*;
import Assert :: *;

import FIFOF::*;
import SpecialFIFOs :: * ;
`include "NocPacket.defines"

interface RouteFunc#(numeric type wInit, numeric type wTarg, numeric type wOut);
    method Action route(Integer id, Bool dir, Bit#(wInit) init, Bit#(wTarg) targ);
    method Bit#(wOut) out;
endinterface

module mkNocDemux#(
    Integer id, 
    Bool dir,
    NocFlitPGet#(`NocPacketParams, nBytes) in,
    Vector#(nOut, NocFlitPPut#(`NocPacketParams, nBytes)) outs,
    RouteFunc#(wInit, wTarg, TLog#(nOut)) route_fn
    )();

    FIFOF#(Bit#(TLog#(nOut))) out_id <- mkSizedBypassFIFOF(1);

    // Probe#(UInt#(2)) probe <- mkProbe();

    match {.header_in, .payload_in} = in;

    rule route_req(peekGet(header_in) matches .h);
        route_fn.route(id, dir, h.h.routeId.initId, h.h.routeId.targId);
    endrule

    for(Integer i = 0; i < valueOf(nOut); i = i + 1) begin
        rule route_header(route_fn.out == fromInteger(i));
            let h <- header_in.get;
            let out_port = tpl_1(outs[fromInteger(i)]);
            out_port.put(h);
            if (nocHeaderLen(h.h) != 0) begin
                out_id.enq(fromInteger(i));
            end
            // probe <= 2;
        endrule


        rule route_payload(out_id.first == fromInteger(i));
            let p <- payload_in.get;
            let out_port = tpl_2(outs[fromInteger(i)]);
            out_port.put(p);
            if (p.p.last) begin
                out_id.deq;
            end
            // probe <= 3;
        endrule
    end

endmodule

module mkNocMux#(
    Integer id, 
    Bool dir,
    Vector#(nIn, NocFlitPGet#(`NocPacketParams, nBytes)) ins,
    NocFlitPPut#(`NocPacketParams, nBytes) out,
    Arbiter_IFC#(nIn) arbiter
    )();

    FIFOF#(Bit#(TLog#(nIn))) owner <- mkSizedBypassFIFOF(1);
    Reg#(Bool) req_mask <- mkReg(False);
    Vector#(nIn, Wire#(`NocTransHeaderG)) dummy_headers;
    dummy_headers <- replicateM(mkUnsafeDWire(?));

    // Vector#(nIn, Probe#(UInt#(2))) probes;
    // probes <- replicateM(mkProbe());

    function Rules arbitration(Bit#(TLog#(nIn)) idx);
        match {.header_in, .payload_in} = ins[idx];
        return rules
            //dummy dwires to keep peekGet(header_in) condition take effect
            rule request(peekGet(header_in) matches .h &&& !req_mask);
                dummy_headers[idx] <= h;
                arbiter.clients[idx].request();
                // probes[idx] <= 1;
            endrule
            rule granted(arbiter.grant_id() == idx &&& arbiter.clients[idx].grant());
                let h <- header_in.get;
                let out_port = tpl_1(out);
                out_port.put(h);
                if (nocHeaderLen(h.h)!= 0) begin
                    owner.enq(idx);
                    req_mask <= True;
                end
                // probes[idx] <= 2;
            endrule
            rule granted_payload(owner.first == idx);
                let p <- payload_in.get;
                let out_port = tpl_2(out);
                out_port.put(p);
                if (p.p.last) begin
                    owner.deq;
                    req_mask <= False;
                end
                // probes[idx] <= 3;
            endrule
        endrules;
    endfunction

    for (Bit#(TLog#(nIn)) i = 0; i < fromInteger(valueOf(nIn)); i = i+1) begin
        addRules(arbitration(i));
    end
endmodule

module mkRRArbiter(Arbiter_IFC#(count));
    (* hide *)
    let _ifc <- mkArbiter(False);
    return _ifc;
endmodule

// The req has been waiting the longest has te highest priority
// put the waiting reqs into the fifo.
// waiting means, has not been granted and not be in the pending list(the fifo).
// for each request bit, if it has been asserted in one of entry of the fifo,
// it will no be enqueued. Thus, the worst case is every entry of the fifo is
// onehot vector, which means the max length of fifo is length of request vector.
module mkFifoArbiter(Arbiter_IFC#(count));
    let icount = valueOf(count);
    Vector#(count, PulseWire) request_vector <- replicateM(mkPulseWire);
    FIFOF#(Bit#(count)) reqs_seq <- mkSizedBypassFIFOF(icount);
    Arbiter_IFC#(count) arbiter <- mkArbiter(False);

    Reg#(Bit#(count)) cur_reqs[2] <- mkCReg(2, 0);
    Reg#(Bit#(count)) pendings[2] <- mkCReg(2, 0);

    
    function Bit#(count) get_grants();
        function Bool f(ArbiterClient_IFC c);
            return c.grant;
        endfunction
        return pack(map(f, arbiter.clients));
    endfunction

     function Bit#(count) new_reqs();
        function Bool f(PulseWire ifc);
            return ifc._read;
         endfunction
        return pack(map(f, request_vector));
     endfunction

    function Bool enable();
        return new_reqs() != 0;
    endfunction

    rule arbit(enable());
        for (Integer x = 0; x < icount; x = x + 1) begin
            if (cur_reqs[1][x] == 1) begin
                arbiter.clients[x].request();
            end
        end
    endrule

    (* fire_when_enabled *)
    rule update_req(enable() &&& cur_reqs[1] != 0);
        let req_next = cur_reqs[1] & ~get_grants();
        pendings[1] <= pendings[1] & ~get_grants();
        cur_reqs[1] <= req_next;
    endrule

    (* fire_when_enabled *)
    rule next_reqs(enable() &&& cur_reqs[0] == 0);
        let deq_reqs = reqs_seq.first;
        reqs_seq.deq;
        cur_reqs[0] <= deq_reqs;
    endrule

    rule record_reqs;
        let next_mask = pendings[0];
        let enq_reqs = new_reqs() & ~next_mask;
        pendings[0] <= next_mask | enq_reqs;
        if (enq_reqs != 0) begin
            reqs_seq.enq(enq_reqs);
        end
    endrule

    rule enq_check;
        let next_mask = pendings[0];
        let enq_reqs = new_reqs() & ~next_mask;
        if (enq_reqs != 0) begin
            dynamicAssert(reqs_seq.notFull, "reqs_seq fifo should not stall!");
        end
    endrule

    Vector#(count, ArbiterClient_IFC) client_vector = newVector;
    for (Integer x = 0; x < icount; x = x + 1)
      client_vector[x] = (
        interface ArbiterClient_IFC
			method Action request();
				request_vector[x].send();
			endmethod

			method lock = arbiter.clients[x].lock;

			method grant = arbiter.clients[x].grant;
		endinterface);

   interface clients = client_vector;
   method    grant_id = arbiter.grant_id;
endmodule

endpackage