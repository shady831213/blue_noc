package NocReassembleTest;
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
import NocReassemble::*;
import NocFlowCtrl::*;
import NocCtx::*;
import ClientServer::*;


`include "NocPacket.defines"
typedef 4 MyWLen;
typedef 16 MyWSeq;
typedef 8 MyWId;
typedef 3 MyWCtx;

typedef NocHeader#(3, MyWId, MyWId, MyWSeq, MyWLen, 32, 0, MyWCtx) MyTestHeader;

typedef NocPacket#(3, MyWId, MyWId, MyWSeq, MyWLen, 32, 0, MyWCtx, 1) MyTestPacket;

module [`ModuleWithNocPacketG] mkNocWrapTestCore()
provisos(
   Add#(x, wSeq, 32),
   Add#(y, 8, wSeq),
   Add#(z, wLen, 32),
   Add#(xx, wCtx, 32),
   Add#(yy, TLog#(nBytes), wAddr),
   Add#(wLen, zz, wAddr),
   Add#(xxx, TLog#(outStanding), wCtx),
   NumEq#(nBytes, 4),
   NumEq#(outStanding, 8)
   );

   Reg#(Bool) wnr <- mkReg(False);

   function ActionValue#(`NocHeaderG) wrapGenHeaderCb(`NocHeaderG h);
      return actionvalue
         let header = h;
         if (wnr) begin
            header.op = WR;
         end
         else begin
            header.op = RD;
         end
         wnr <= !wnr;
         header.ty = WRAP;
         Bit#(TLog#(wLen)) len_shift = header.routeId.seqId[valueOf(TSub#(TLog#(wLen), 1)):0];
         header.len = 1 << len_shift;
         if (header.len < fromInteger(valueOf(nBytes))) begin
            header.len = fromInteger(valueOf(nBytes));
         end
         return header;
      endactionvalue;
   endfunction


   let req_header_gen <- mkNocSimpleTestHeaderGen(valueOf(nBytes), 0, 200);
   match {.initiator_send, .initiator_send_exp} <- mkNocSimpleTestGenCore(wrapGenHeaderCb, defaultPayloadCb, SEQ_ID, 0, req_header_gen);
   match {.initiator_header_send, .initiator_payload_send} = initiator_send;
   match {.initiator_recv, .initiator_recv_actual} <- mkNocSimpleTestRecvCore(defaultHeaderCb, defaultPayloadCb, 1);
   match {.initiator_header_recv, .initiator_payload_recv} = initiator_recv;


   match {.target_recv, .target_recv_actual} <- mkNocSimpleTestRecvCore(defaultHeaderCb, defaultPayloadCb, 1);
   match {.target_header_recv, .target_payload_recv} = target_recv;

   FIFOF#(`NocHeaderG) target_gen_header_fifo <- mkFIFOF();

   match {.target_send, .target_send_exp} <- mkNocSimpleTestGenCore(defaultHeaderCb, defaultPayloadCb, SEQ_ID, 0, toGet(target_gen_header_fifo));
   match {.target_header_send, .target_payload_send} = target_send;

   rule target_send_exp_drain;
      let p <- target_send_exp.get;
      // $display("[%0t] target pkt send:", $time, fshow(p));
   endrule

   FIFOF#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes)) wr_resp_exp_fifo <- mkSizedBypassFIFOF(32);
   FIFOF#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes)) wr_resp_actual_fifo <- mkSizedBypassFIFOF(32);
   
   FIFOF#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes)) wr_req_exp_process_fifo <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes)) wr_req_exp_fifo <- mkSizedBypassFIFOF(32);
   FIFOF#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes)) wr_req_actual_fifo <- mkSizedBypassFIFOF(32);

   FIFOF#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes)) rd_exp_fifo <- mkSizedBypassFIFOF(32);
   FIFOF#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes)) rd_actual_fifo <- mkSizedBypassFIFOF(32);

   rule target_read(peekGet(target_recv_actual).header.op == RD);
      let i <- target_recv_actual.get;
      let h = i.header;
      h.status = RESP;
      target_gen_header_fifo.enq(h);
      // $display("[%0t] target pkt recv:", $time, fshow(i));
   endrule

   rule target_write(peekGet(target_recv_actual).header.op == WR);
      let i <- target_recv_actual.get;
      let h = i.header;
      h.status = RESP;
      target_gen_header_fifo.enq(h);
      let p = i;
      p.header.ctx = 0;
      wr_req_actual_fifo.enq(p);
      // $display("[%0t] target pkt recv:", $time, fshow(i));
   endrule

   FIFOF#(`NocHeaderG) rd_exp_header_fifo <- mkSizedBypassFIFOF(1);
   Get#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes)) rd_recv_exp <- mkNocSimpleTestGenPktCore(defaultPayloadActionCb, SEQ_ID, 0, toGet(rd_exp_header_fifo));

   rule wr_exp(peekGet(initiator_send_exp).header.op == WR);
      let p <- initiator_send_exp.get;
      let h = p.header;
      h.status = RESP;
      wr_resp_exp_fifo.enq(NocPacketItem {header: h, payload: tagged Invalid});
      wr_req_exp_process_fifo.enq(p);
   endrule

   rule wr_resp_actual(peekGet(initiator_recv_actual).header.op == WR);
      let p <- initiator_recv_actual.get;
      wr_resp_actual_fifo.enq(p);
   endrule

   let wr_resp_check <- mkNocSimpleTestCompareCore(
      defaultCheckFn,
      100,
      toGet(wr_resp_exp_fifo),
      toGet(wr_resp_actual_fifo)
   );

   Reg#(Maybe#(`NocPacketItemG(TDiv#(TExp#(wLen), nBytes), nBytes))) wr_splitted <- mkReg(tagged Invalid);

   rule wr_req_split_exp(!isValid(wr_splitted));
      let p = wr_req_exp_process_fifo.first;
      p.header.ctx = 0;
      match {.p1, .p2} = wrapPktSplit(p);
      if (!isValid(p2)) begin
         wr_req_exp_process_fifo.deq;
      end
      wr_splitted <= p2;
      wr_req_exp_fifo.enq(p1);
   endrule

   rule wr_req_split2_exp(wr_splitted matches tagged Valid .p2);
      wr_req_exp_process_fifo.deq;
      wr_splitted <= tagged Invalid;
      wr_req_exp_fifo.enq(p2);
   endrule

   Reg#(Bool) wr_req_done <- mkReg(False);
   function ActionValue#(Bool) wrReqCheck(`NocPacketItemG(nChunks, nBytes) e, `NocPacketItemG(nChunks, nBytes) a);
      actionvalue
         let result <- defaultCheckFn(e, a);
         if (a.header.routeId.seqId == 199) begin
            wr_req_done <= True;
         end
         return result;
      endactionvalue
   endfunction

   let wr_req_check <- mkNocSimpleTestCompareCore(
      wrReqCheck,
      200,
      toGet(wr_req_exp_fifo),
      toGet(wr_req_actual_fifo)
   );

   Reg#(Maybe#(`NocHeaderG)) rd_splitted <- mkReg(tagged Invalid);

   rule rd_split_exp(!isValid(rd_splitted) &&& peekGet(initiator_send_exp).header.op == RD);
      let p = peekGet(initiator_send_exp);
      let h = p.header;
      h.status = RESP;
      match {.h1, .h2} = wrapSplit(h, valueOf(nBytes));
      if (!isValid(h2)) begin
         let _p <- initiator_send_exp.get;
      end
      else begin
         h1.status = CONT;
      end
      h1.ty = WRAP;
      rd_splitted <= h2;
      // $display("[%0t] rd exp header:", $time, fshow(h));
      rd_exp_header_fifo.enq(h1);
   endrule

   rule rd_split2_exp(rd_splitted matches tagged Valid .h2 &&& peekGet(initiator_send_exp).header.op == RD);
      let _p <- initiator_send_exp.get;
      rd_splitted <= tagged Invalid;
      let h = h2;
      h.ty = WRAP;
      // $display("[%0t] rd exp header:", $time, fshow(h));
      rd_exp_header_fifo.enq(h);
   endrule

   mkConnection(rd_recv_exp, toPut(rd_exp_fifo));

   rule rd_actual(peekGet(initiator_recv_actual).header.op == RD);
      let p <- initiator_recv_actual.get;
      // $display("[%0t] initiator pkt recv:", $time, fshow(p));
      rd_actual_fifo.enq(p);
   endrule

   Reg#(Bool) rd_done <- mkReg(False);
   function ActionValue#(Bool) rdCheck(`NocPacketItemG(nChunks, nBytes) e, `NocPacketItemG(nChunks, nBytes) a);
      actionvalue
         let result <- defaultCheckFn(e, a);
         if (a.header.routeId.seqId == 198 &&& a.header.status == RESP) begin
            rd_done <= True;
         end
         return result;
      endactionvalue
   endfunction

   let rd_check <- mkNocSimpleTestCompareCore(
      rdCheck,
      200,
      toGet(rd_exp_fifo),
      toGet(rd_actual_fifo)
   );

   rule done (wr_resp_check.done &&& rd_done &&& wr_req_done);
      $display("[%0t] Simulation Pass!", $time);
      $finish(0);
   endrule
   
   FIFOF#(`NocHeaderG) issue_req_header_fifo <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocPayloadG(nBytes)) issue_req_data_fifo <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocHeaderG) issue_resp_header_fifo <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocPayloadG(nBytes)) issue_resp_data_fifo <- mkSizedBypassFIFOF(1);

   NocIssueCtxBufffer#(outStanding, wCtx) issue_ctx <- mkRFCtxBuffer;

   NocCtxPipe#(outStanding, `NocPacketParams, nBytes) issue_pipe = interface NocCtxPipe;
      interface NocPipeC upstream = tuple2(
         interface Client;
            interface Get request = initiator_header_send;
            interface Put response = initiator_header_recv;
         endinterface,
         interface Client;
            interface Get request = initiator_payload_send;
            interface Put response = initiator_payload_recv;
         endinterface
      );
      interface NocPipeS downstream = tuple2(
         interface Server;
            interface Put request = toPut(issue_req_header_fifo);
            interface Get response = toGet(issue_resp_header_fifo);
         endinterface,
         interface Server;
            interface Put request = toPut(issue_req_data_fifo);
            interface Get response = toGet(issue_resp_data_fifo);
         endinterface
      );
   endinterface;

   mkNocIssue(issue_ctx, issue_pipe);

   NocCtxPipe#(outStanding, `NocPacketParams, nBytes) wrap_split_pipe = interface NocCtxPipe;
      interface NocPipeC upstream = tuple2(
         interface Client;
            interface Get request = toGet(issue_req_header_fifo);
            interface Put response = toPut(issue_resp_header_fifo);
         endinterface,
         interface Client;
            interface Get request = toGet(issue_req_data_fifo);
            interface Put response = toPut(issue_resp_data_fifo);
         endinterface
      );
      interface NocPipeS downstream = tuple2(
         interface Server;
            interface Put request = target_header_recv;
            interface Get response = target_header_send;
         endinterface,
         interface Server;
            interface Put request = target_payload_recv;
            interface Get response = target_payload_send;
         endinterface
      );
   endinterface;


   FIFOF#(`NocHeaderG) wrap_splitter_header_in_fifo <- mkSizedBypassFIFOF(1);
   FIFOF#(`NocHeaderG) wrap_splitter_header_out_fifo <- mkSizedBypassFIFOF(1);
   let wrap_splitter <- mkNocWrap2IncrSplitter(valueOf(nBytes), fifofToFifo(wrap_splitter_header_in_fifo), fifofToFifo(wrap_splitter_header_out_fifo));

   mkNocSplitter(wrap_splitter, wrap_split_pipe);

endmodule


(* synthesize *)
module mkNocWrapTest();
   MyTestPacket p = unpack(0);
   getNocModule(p, mkNocWrapTestCore);
endmodule


endpackage