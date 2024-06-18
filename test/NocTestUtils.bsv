package NocTestUtils;
import NocPacket::*;
import Packet::*;
import StmtFSM::*;
import Assert :: *;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs :: * ;
import Vector::*;
import GetPut::*;
import NocConnetion::*;
import ClientServer::*;
import Connectable::*;
import Randomizable::*;
import BUtils::*;

`include "NocPacket.defines"

module mkNocSimpleTestHeaderGen#(
   Integer nBytes,
   Integer in_bubble, 
   Integer pkt_num
   )(Get#(`NocHeaderG))
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32)
   );
   Randomize#(Bit#(wLen)) addr_rand <- mkGenericRandomizer;

   rule init_rand;
      addr_rand.cntrl.init;
  endrule

   FIFOF#(`NocHeaderG) header_fifo <- mkSizedBypassFIFOF(1);

   Reg#(UInt#(32)) pkt_cnt <- mkReg(0);

   let gen <- mkFSM(seq
      while(pkt_cnt != fromInteger(pkt_num)) seq
         delay(fromInteger(in_bubble));
         action
            `NocHeaderG header = unpack(0);
            header.status = REQ;
            header.op = WR;
            header.routeId.seqId = truncate(pack(pkt_cnt));
            let h_addr <- addr_rand.next;
            header.addr = cExtend(h_addr);
            header.len = truncate(pkt_cnt);
            header.ctx = truncate(pack(pkt_cnt));
            Bit#(wLen) mask = fromInteger(nBytes) - 1;
            Bit#(wLen) addr_low = h_addr & mask;
            Bit#(TAdd#(wLen, 1)) aligned_len = {1'b0, addr_low} + {1'b0, pack(header.len)};
            if (unpack(msb(aligned_len))) begin
               header.len = unpack('1 - addr_low);
            end
      
            pkt_cnt <= pkt_cnt + 1;
            header_fifo.enq(header);
         endaction
      endseq
   endseq);

   rule start if (pkt_cnt < fromInteger(pkt_num));
      gen.start;
   endrule

   return toGet(header_fifo);

endmodule

typedef enum {
    SEQ_ID,
    BYTE_IDX
} NocTestPayloadPattern deriving (Eq, Bits, FShow);

module mkNocSimpleTestPayloadGenCore#(
   NocTestPayloadPattern pattern,
   Integer in_bubble,
   Get#(`NocHeaderG) header_in
   )(Get#(Tuple2#(`NocHeaderG, Maybe#(Tuple2#(UInt#(TLog#(TDiv#(TExp#(wLen), nBytes))), `NocPayloadG(nBytes))))))
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr)
   );
   Reg#(UInt#(TLog#(TDiv#(TExp#(wLen), nBytes)))) idx_gen <- mkReg(0);

   Reg#(UInt#(wLen)) len <- mkReg(0);
   Reg#(UInt#(TLog#(nBytes))) addr <- mkReg(0);
   Reg#(NocType) ty <- mkReg(unpack(0));
   Reg#(Bit#(32)) seq_id <- mkReg(unpack(0));


   FIFOF#(Tuple2#(`NocHeaderG, Maybe#(Tuple2#(UInt#(TLog#(TDiv#(TExp#(wLen), nBytes))), `NocPayloadG(nBytes))))) payload_fifo <- mkSizedBypassFIFOF(1);

   let gen <- mkFSM(seq
      while(True) seq
         delay(fromInteger(in_bubble));
         action
            let header = peekGet(header_in);
            if (nocHeaderLen(header) == 0) begin
               let h <- header_in.get;
               payload_fifo.enq(tuple2(h, tagged Invalid));
               len <= 0;
            end
            else begin
               Bit#(TLog#(nBytes)) addr_low = truncate(header.addr);
               ty <= header.ty;
               len <= header.ty == FIXED ? header.len << fromInteger(valueOf(TLog#(nBytes))) : cExtend(header.len);
               addr <= header.ty == FIXED ? 0 : unpack(addr_low);
               seq_id <= cExtend(header.routeId.seqId);
            end
            idx_gen <= 0;
         endaction
         while(len != 0) seq
            action
               let header = peekGet(header_in);
               UInt#(wLen) batch =  fromInteger(valueOf(nBytes)) - cExtend(addr);
               UInt#(wLen) next_len = len > batch  ? len - batch : 0;
               len <= next_len;
               addr <= 0;
               `NocPayloadG(nBytes) payload = unpack(0);
               payload.last = next_len == 0;
               for (int i = 0; i < fromInteger(valueOf(nBytes)); i = i + 1) begin
                  if (pack(i) >= cExtend(addr) &&& pack(i) < cExtend(pack(len)) + cExtend(addr)) begin
                     if (pattern == BYTE_IDX) begin
                        payload.bytes[i].data = truncate(pack(i));
                     end
                     else begin
                        payload.bytes[i].data = truncate(seq_id);
                     end
                     payload.bytes[i].be = True;
                  end
               end
               if (payload.last) begin
                  let h <- header_in.get;
                  idx_gen <= 0;
               end
               else begin
                  idx_gen <= idx_gen + 1;
               end

               payload_fifo.enq(tuple2(header, tagged Valid tuple2(idx_gen, payload)));
            endaction
         endseq
      endseq
   endseq);

   rule start;
      gen.start;
   endrule

   return toGet(payload_fifo);
endmodule

module mkNocSimpleTestGenPktCore#(
   function ActionValue#(`NocPayloadG(nBytes)) gen_payload_cb(UInt#(TLog#(TDiv#(TExp#(wLen), nBytes))) idx, `NocPayloadG(nBytes) p),
   NocTestPayloadPattern pattern,
   Integer in_bubble,
   Get#(`NocHeaderG) header_in
   )(Get#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes)))
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr)
   );
   Vector#(TDiv#(TExp#(wLen), nBytes), Reg#(`NocPayloadG(nBytes))) payloads_gen <- replicateM( mkReg( unpack(0) ) );
   FIFOF#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes)) exp <- mkSizedBypassFIFOF(32);

   Get#(Tuple2#(`NocHeaderG, Maybe#(Tuple2#(UInt#(TLog#(TDiv#(TExp#(wLen), nBytes))), `NocPayloadG(nBytes))))) payload_gen <- mkNocSimpleTestPayloadGenCore(pattern, in_bubble, header_in);

   rule paylaod;
      match {.h, .mpp} <- payload_gen.get;
      if (mpp matches tagged Valid .pp) begin
         match {.idx, .p} = pp;
         p <- gen_payload_cb(idx, p);
         if (p.last) begin
            let payloads = readVReg(payloads_gen);
            payloads[idx] = p;
            writeVReg(payloads_gen, replicate(unpack(0)));
            exp.enq( NocPacketItem {header: h, payload: tagged Valid payloads});
         end
         else begin
            payloads_gen[idx] <= p;
         end
      end
      else begin
         exp.enq(NocPacketItem {header: h, payload: tagged Invalid});
      end
   endrule

   return toGet(exp);
endmodule


module mkNocSimpleTestGenCore#(
   function ActionValue#(`NocHeaderG) gen_header_cb(`NocHeaderG h),
   function `NocPayloadG(nBytes) gen_payload_cb(UInt#(TLog#(TDiv#(TExp#(wLen), nBytes))) idx, `NocPayloadG(nBytes) p),
   NocTestPayloadPattern pattern,
   Integer in_bubble,
   Get#(`NocHeaderG) header_in
   )(Tuple2#(NocPipeGet#(`NocPacketParams, nBytes), Get#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes))))
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr)
   );
   FIFOF#(`NocHeaderG) header_gen_fifo <- mkSizedBypassFIFOF(1);

   FIFOF#(`NocHeaderG) header_fifo <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocPayloadG(nBytes)) payload_fifo <- mkSizedBypassFIFOF(1);

   function ActionValue#(`NocPayloadG(nBytes)) payloadCb(UInt#(TLog#(TDiv#(TExp#(wLen), nBytes))) idx, `NocPayloadG(nBytes) p);
      return actionvalue
         let p1 = gen_payload_cb(idx, p);
         payload_fifo.enq(p1);
         return p1;
      endactionvalue;
   endfunction

   let pkt_gen <- mkNocSimpleTestGenPktCore(payloadCb, pattern, in_bubble, toGet(header_gen_fifo));
   rule header;
      let h <- header_in.get;
      h <- gen_header_cb(h);
      header_fifo.enq(h);
      header_gen_fifo.enq(h);
   endrule

   return tuple2(
      tuple2(toGet(header_fifo), toGet(payload_fifo)),
      pkt_gen
   );
endmodule

module mkNocSimpleTestRecvCore#(
   function ActionValue#(`NocHeaderG) recv_header_cb(`NocHeaderG h),
   function `NocPayloadG(nBytes) recv_payload_cb(UInt#(TLog#(TDiv#(TExp#(wLen), nBytes))) idx, `NocPayloadG(nBytes) p),
   Integer out_bp
   )(Tuple2#(NocPipePut#(`NocPacketParams, nBytes), Get#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes))))
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr)
   );

   Reg#(UInt#(32)) cnt <- mkReg(0);
   rule incr;
      cnt <= cnt + 1;
   endrule

   FIFOF#(`NocHeaderG) header_fifo <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocPayloadG(nBytes)) payload_fifo <- mkSizedBypassFIFOF(1);

   FIFOF#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes)) result <- mkSizedBypassFIFOF(32);
   Vector#(TDiv#(TExp#(wLen), nBytes), Reg#(`NocPayloadG(nBytes))) payloads_check <- replicateM( mkReg( unpack(0) ) );
   Reg#(UInt#(TLog#(TDiv#(TExp#(wLen), nBytes)))) idx_check <- mkReg(0);

   FIFOF#(`NocHeaderG) downstream_payload_en <- mkSizedBypassFIFOF(1);

   rule recv_header(cnt % fromInteger(out_bp) == 0);
      let _h <- toGet(header_fifo).get;
      let h <- recv_header_cb(_h);
      if (nocHeaderLen(h) == 0) begin
         idx_check <= 0;
         writeVReg(payloads_check, replicate(unpack(0)));
         result.enq(NocPacketItem {header: h, payload: tagged Invalid});
      end
      else begin
         downstream_payload_en.enq(h);
      end
   endrule


   rule recv_payload(cnt % fromInteger(out_bp) == 0);
      let h = downstream_payload_en.first;
      let _p <- toGet(payload_fifo).get;
      let p = recv_payload_cb(idx_check, _p);

      let payloads = readVReg(payloads_check);
      payloads[idx_check] = p;
      if (p.last) begin
         idx_check <= 0;
         writeVReg(payloads_check, replicate(unpack(0)));
         result.enq(NocPacketItem {header: h, payload: tagged Valid payloads});
         downstream_payload_en.deq;
      end
      else begin
         idx_check <= idx_check + 1;
         writeVReg(payloads_check, payloads);
      end
   endrule


   return tuple2(
      tuple2(toPut(header_fifo), toPut(payload_fifo)),
      toGet(result)
   );
endmodule

interface NocTestCtrl;
   method Bool done;
endinterface

module mkNocSimpleTestCompareCore#(
   function ActionValue#(Bool) check(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes) exp, `NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes) actual),
   Integer pkt_num,
   Get#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes)) exp,
   Get#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes)) actual
   )(NocTestCtrl);

   Reg#(UInt#(32)) pkt_cnt <- mkReg(0);

   Reg#(Bool) cmp_done <- mkReg(False);

   rule cmp(!cmp_done);
      let e <- exp.get;
      let a <- actual.get;
      let result <- check(e, a);
      dynamicAssert(result, "");
      let next_cnt = pkt_cnt + 1;
      pkt_cnt <= next_cnt;
      cmp_done <= next_cnt >= fromInteger(pkt_num);
   endrule

   method done = cmp_done;

endmodule

module mkNocSimpleTestCore#(
   function ActionValue#(`NocHeaderG) gen_header_cb(`NocHeaderG h),
   function `NocPayloadG(nBytes) gen_payload_cb(UInt#(TLog#(TDiv#(TExp#(wLen), nBytes))) idx, `NocPayloadG(nBytes) p),
   function ActionValue#(`NocHeaderG) chk_header_cb(`NocHeaderG h),
   function `NocPayloadG(nBytes) chk_payload_cb(UInt#(TLog#(TDiv#(TExp#(wLen), nBytes))) idx, `NocPayloadG(nBytes) p),
   Integer in_bubble, 
   Integer out_bp, 
   Integer pkt_num
   )(Tuple2#(NocPipeC#(`NocPacketParams, nBytes), NocTestCtrl))
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr)
   );

   let header_gen <- mkNocSimpleTestHeaderGen(valueOf(nBytes), in_bubble, pkt_num);
   match {.send, .exp} <- mkNocSimpleTestGenCore(gen_header_cb, gen_payload_cb, SEQ_ID, in_bubble, header_gen);
   match {.recv, .actual} <- mkNocSimpleTestRecvCore(chk_header_cb, chk_payload_cb, out_bp);
   let ctrl <- mkNocSimpleTestCompareCore(
      defaultCheckFn,
      pkt_num,
      exp,
      actual
   );

   return tuple2(
      tuple2(
         interface Client;
            interface Get request = tpl_1(send);
            interface Put response = tpl_1(recv);
         endinterface,
         interface Client;
            interface Get request = tpl_2(send);
            interface Put response = tpl_2(recv);
         endinterface
      ),
      ctrl);
endmodule

module mkNocTransSimpleTestCore#(
   function ActionValue#(`NocHeaderG) gen_header_cb(`NocHeaderG h),
   function `NocPayloadG(nBytes) gen_payload_cb(UInt#(TLog#(TDiv#(TExp#(wLen), nBytes))) idx, `NocPayloadG(nBytes) p),
   function ActionValue#(`NocHeaderG) chk_header_cb(`NocHeaderG h),
   function `NocPayloadG(nBytes) chk_payload_cb(UInt#(TLog#(TDiv#(TExp#(wLen), nBytes))) idx, `NocPayloadG(nBytes) p),
   Integer in_bubble, 
   Integer out_bp, 
   Integer pkt_num
   )(Tuple2#(GetPut#(`NocFlitG(nBytes)), NocTestCtrl))
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr)
   );
   match {.ports, .ctrl} <- mkNocSimpleTestCore(gen_header_cb, gen_payload_cb, chk_header_cb, chk_payload_cb, in_bubble, out_bp, pkt_num);
   match {.get, .put} <- mkTransClient(ports);
   NocFlitPGet#(`NocHeaderParams, nBytes) ports_get = get;
   NocFlitPPut#(`NocHeaderParams, nBytes) ports_put = put;
   FIFOF#(`NocFlitG(nBytes)) upstream <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocFlitG(nBytes)) downstream <- mkSizedBypassFIFOF(1);
   mkConnection(ports_get, toPut(upstream));
   mkConnection(toGet(downstream), ports_put);
   return tuple2(tuple2(toGet(upstream), toPut(downstream)), ctrl);
endmodule

function ActionValue#(Bool) defaultCheckFn(`NocPacketItemG(nChunks, nBytes) e, `NocPacketItemG(nChunks, nBytes) a);
   actionvalue
      $display("exp: ", fshow(e));
      $display("actual: ", fshow(a));
      let result = e == a;
      if (result) begin
         $display("[%0t] pkt checked", $time);
      end
      return result;
   endactionvalue
endfunction

function ActionValue#(`NocHeaderG) defaultHeaderCb(`NocHeaderG h);
   return actionvalue
      return h;
   endactionvalue;
endfunction

function `NocPayloadG(nBytes) defaultPayloadCb(UInt#(w) idx, `NocPayloadG(nBytes) p);
   return p;
endfunction

function ActionValue#(`NocPayloadG(nBytes)) defaultPayloadActionCb(UInt#(w) idx, `NocPayloadG(nBytes) p);
   return actionvalue
      return p;
   endactionvalue;
endfunction

endpackage