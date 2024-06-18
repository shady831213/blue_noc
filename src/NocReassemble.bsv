package NocReassemble;
import GetPut::*;
import NocPacket::*;
import Packet::*;
import Vector::*;
import Connectable::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs :: * ;
import ClientServer::*;
import NocConnetion::*;
import BUtils::*;
`include "NocPacket.defines"


module mkNocRespErrFilter#(NocCtxPipe#(outStanding, `NocPacketParams, nBytes) pipe)();

    match {.up_header, .up_payload} = pipe.upstream;
    match {.down_header, .down_payload} = pipe.downstream;
    
    mkConnection(up_header.request, down_header.request);
    mkConnection(up_payload.request, down_payload.request);

    FIFOF#(`NocHeaderG) payload_req <- mkSizedBypassFIFOF(1);


    rule resp_header;
        let h <- down_header.response.get;
        up_header.response.put(h);
        payload_req.enq(h);
    endrule

    mkNocPayloadDiscardErrTaker(toGet(payload_req), tuple2(down_payload.response, up_payload.response));


endmodule

module mkNocReqErrFilter#(NocCtxPipe#(outStanding, `NocPacketParams, nBytes) pipe)();

    match {.up_header, .up_payload} = pipe.upstream;
    match {.down_header, .down_payload} = pipe.downstream;
    
    FIFOF#(Bit#(0)) pendings <- mkSizedBypassFIFOF(valueOf(outStanding));
    FIFOF#(`NocHeaderG) err_resp <- mkFIFOF;

    FIFOF#(`NocHeaderG) payload_req <- mkSizedBypassFIFOF(1);
    mkNocPayloadDiscardErrTaker(toGet(payload_req), tuple2(up_payload.request, down_payload.request));

    rule req_header(!isValid(peekGet(up_header.request).err));
        let h <- up_header.request.get;
        down_header.request.put(h);
        payload_req.enq(h);
        pendings.enq(0);
    endrule

    rule req_header_err(isValid(peekGet(up_header.request).err));
        let h <- up_header.request.get;
        payload_req.enq(h);
        h.status = RESP;
        err_resp.enq(h);
    endrule

    rule err_resp_forward(!pendings.notEmpty);
        let h <- toGet(err_resp).get;
        up_header.response.put(h);
    endrule

    rule resp_forward(pendings.notEmpty);
        let h <- down_header.response.get;
        up_header.response.put(h);
        pendings.deq;
    endrule

    mkConnection(up_payload.response, down_payload.response);

endmodule

// taked payloads must be in granularity of nBytes, align to ceiling automatically
module mkNocPayloadTakerCore#(
    function Bool discard(`NocHeaderG h, `NocPayloadG(nBytes) p),
    Get#(`NocHeaderG) header,
    GetPut#(`NocPayloadG(nBytes)) payload
)();
    match {.payload_in, .payload_out} = payload;

    Reg#(UInt#(TSub#(wLen, TLog#(nBytes)))) cnt <- mkReg(0);

    rule every;
        let h = peekGet(header);
        let p <- payload_in.get;
        if (cnt == beatLen(h, p) - 1) begin
            cnt <= 0;
            p.last = True;
            let _h <- header.get;
        end
        else begin
            cnt <= cnt + 1;
        end
        if (!discard(h, p)) begin
            payload_out.put(p);
        end
    endrule
endmodule


// taked payloads must be in granularity of nBytes, align to ceiling automatically
module mkNocPayloadDiscardErrTaker#(
    Get#(`NocHeaderG) header,
    GetPut#(`NocPayloadG(nBytes)) payload
)();
    function Bool discard_err(`NocHeaderG h, `NocPayloadG(nBytes) p);
        return isValid(h.err);
    endfunction
    mkNocPayloadTakerCore(discard_err, header, payload);
endmodule

// taked payloads must be in granularity of nBytes, align to ceiling automatically
module mkNocPayloadTaker#(
    Get#(`NocHeaderG) header,
    GetPut#(`NocPayloadG(nBytes)) payload
)();

    function Bool discard_nothing(`NocHeaderG h, `NocPayloadG(nBytes) p);
        return False;
    endfunction
    mkNocPayloadTakerCore(discard_nothing, header, payload);
endmodule

typedef struct {
    `NocHeaderG header;
    UInt#(wLen) cnt;
} NocSplitterCtx#(`NocPacketParamsD) deriving (Eq, Bits, FShow);


//Ctx is expected unique.
// original header msgs always be recovered in upstream
module mkNocSplitter#(
    GetPut#(`NocHeaderG) split_pipe,
    NocCtxPipe#(outStanding, `NocPacketParams, nBytes) pipe
)()
provisos(Add#(x, TLog#(outStanding), wCtx));
    match {.up_header, .up_payload} = pipe.upstream;
    match {.down_header, .down_payload} = pipe.downstream;

    match {.split_out, .split_in} = split_pipe;

    Vector#(outStanding, Reg#(NocSplitterCtx#(`NocPacketParams))) split_table <- replicateM(mkReg(unpack(0)));

    FIFOF#(`NocHeaderG) payload_split_req <- mkSizedBypassFIFOF(1);

    rule req_no_payload(nocHeaderLen(peekGet(split_out)) == 0);
        let h <- split_out.get;
        down_header.request.put(h);
    endrule

    rule req_with_payload(nocHeaderLen(peekGet(split_out)) != 0);
        let h <- split_out.get;
        down_header.request.put(h);
        payload_split_req.enq(h);
    endrule

    mkNocPayloadTaker(toGet(payload_split_req), tuple2(up_payload.request, down_payload.request));
    
    for(Integer i = 0; i < valueOf(outStanding); i = i + 1) begin
        UInt#(TLog#(outStanding)) idx = fromInteger(i);

        function UInt#(wLen) left(`NocHeaderG h);
            return split_table[idx].cnt > h.len ? split_table[idx].cnt - h.len : 0;
        endfunction

        (* mutually_exclusive = "split, wresp_merge, wresp_forward, rresp" *)
        rule split(ctxAsIdx(peekGet(up_header.request)) == idx);
            let h <- up_header.request.get;
            let ctx = split_table[idx];
            ctx.header = h;
            ctx.cnt = h.len;
            split_table[idx] <= ctx;
            split_in.put(h);
        endrule

        rule wresp_merge(
            peekGet(down_header.response).op == WR &&& 
            ctxAsIdx(peekGet(down_header.response)) == idx &&&
            left(peekGet(down_header.response)) != 0);
            let h <- down_header.response.get;
            let ctx = split_table[idx];
            ctx.cnt = left(h);
            if (!isValid(ctx.header.err)) begin
                ctx.header.err = h.err;
            end
            split_table[idx] <= ctx;
        endrule

        rule wresp_forward(
            peekGet(down_header.response).op == WR &&& 
            ctxAsIdx(peekGet(down_header.response)) == idx &&&
            left(peekGet(down_header.response)) == 0);
            let h <- down_header.response.get;
            let ctx = split_table[idx];
            if (!isValid(ctx.header.err)) begin
                ctx.header.err = h.err;
            end
            ctx.header.status = h.status;
            split_table[idx] <= unpack(0);
            up_header.response.put(ctx.header);
        endrule

        rule rresp(
            peekGet(down_header.response).op == RD &&& 
            ctxAsIdx(peekGet(down_header.response)) == idx);
            let h <- down_header.response.get;
            let ctx = split_table[idx];
            if (left(h) == 0) begin
                ctx.header.status = RESP;
                split_table[idx] <= unpack(0);
            end
            else begin
                ctx.header.status = CONT;
                split_table[idx].cnt <= left(h);
            end
            ctx.header.err = h.err;
            ctx.header.len = h.len;
            ctx.header.addr = h.addr;
            up_header.response.put(ctx.header);
        endrule
    end

    mkConnection(up_payload.response, down_payload.response);
endmodule

module mkNocWrap2IncrSplitter#(
    Integer nBytes,
    FIFO#(`NocHeaderG) header_in,
    FIFO#(`NocHeaderG) header_out
)(GetPut#(`NocHeaderG))
provisos(Add#(wLen, x, wAddr));
    Reg#(Maybe#(`NocHeaderG)) splitted <- mkReg(tagged Invalid);

    rule req_header_forward(header_in.first.ty != WRAP);
        let h <- toGet(header_in).get;
        header_out.enq(h);
    endrule

    rule req_header_split(!isValid(splitted) &&& header_in.first.ty == WRAP);
        let h = header_in.first;
        match {.h1, .h2} = wrapSplit(h, nBytes);
        if (isValid(h2)) begin
            splitted <= h2;
        end
        else begin
            header_in.deq;
        end
        header_out.enq(h1);
    endrule 

    rule req_header_split2(splitted matches tagged Valid .h &&& header_in.first.ty == WRAP);
        header_in.deq;
        header_out.enq(h);
        splitted <= tagged Invalid;
    endrule 

    return tuple2(toGet(header_out), toPut(header_in));
endmodule

// typedef struct {
//     Bit#(TLog#(nBytes)) offset;
//     UInt#(TAdd#(TLog#(nBytes), 1)) len;
// } NocWrapRotatorReq#(numeric type nBytes) deriving (Eq, Bits, FShow);

// module mkNocWrapPayloadRotator#(
//     Bool reverse,
//     Get#(Maybe#(NocWrapRotatorReq#(nBytes))) req,
//     GetPut#(`NocPayloadG(nBytes)) payload
// )();
//     match {.payload_in, .payload_out} = payload;
//     rule forward(peekGet(req) matches tagged Invalid);
//         let p <- payload_in.get;
//         payload_out.put(p);
//         if (p.last) begin
//             let _r <- req.get;
//         end
//     endrule

//     rule rotate(peekGet(req) matches tagged Valid .r);
//         let _r <- req.get;
//         let p <- payload_in.get;
//         p = wrapRotate(reverse, r.offset, r.len, p);
//         payload_out.put(p);
//     endrule
// endmodule

// // wrap packet that needs rotate will never be splitted, and is always single beat.
// module mkNocWrapRotator#(
//     NocCtxPipe#(outStanding, `NocPacketParams, nBytes) pipe
// )()
// provisos(Add#(x, TLog#(outStanding), wCtx));
//     match {.up_header, .up_payload} = pipe.upstream;
//     match {.down_header, .down_payload} = pipe.downstream;

//     FIFOF#(Maybe#(NocWrapRotatorReq#(nBytes))) req_rotate_req <- mkSizedBypassFIFOF(1);

//     FIFOF#(Maybe#(NocWrapRotatorReq#(nBytes))) resp_rotate_req <- mkSizedBypassFIFOF(1);

//     Vector#(outStanding, Reg#(Maybe#(NocWrapRotatorReq#(nBytes)))) rotate_ctx <- replicateM(mkReg(unpack(0)));
    
//     rule req_header_wr(peekGet(up_header.request).op == WR);
//         let h <- up_header.request.get;
//         UInt#(TLog#(outStanding)) idx = ctxAsIdx(h);
//         if (needRotate(h, valueOf(nBytes))) begin
//             h.ty = INCR;
//             let req = tagged Valid NocWrapRotatorReq {offset: h.addr[valueOf(TSub#(TLog#(nBytes), 1)):0], len: unpack(pack(h.len)[valueOf(TLog#(nBytes)):0])};
//             rotate_ctx[idx] <= req;
//             req_rotate_req.enq(req);
//         end
//         else begin
//             let req = tagged Invalid;
//             rotate_ctx[idx] <= req;
//             req_rotate_req.enq(tagged Invalid);
//         end
//         down_header.request.put(h);
//     endrule

//     rule req_header_rd(peekGet(up_header.request).op == RD);
//         let h <- up_header.request.get;
//         UInt#(TLog#(outStanding)) idx = ctxAsIdx(h);
//         if (needRotate(h, valueOf(nBytes))) begin
//             h.ty = INCR;
//             rotate_ctx[idx] <= tagged Valid NocWrapRotatorReq {offset: h.addr[valueOf(TSub#(TLog#(nBytes), 1)):0], len: unpack(pack(h.len)[valueOf(TLog#(nBytes)):0])};
//         end
//         else begin
//             rotate_ctx[idx] <= tagged Invalid;
//         end
//         down_header.request.put(h);
//     endrule

//     mkNocWrapPayloadRotator(False, toGet(req_rotate_req), tuple2(up_payload.request, down_payload.request));

//     rule resp_header_wr(peekGet(down_header.response).op == WR);
//         let h <- down_header.response.get;
//         UInt#(TLog#(outStanding)) idx = ctxAsIdx(h);
//         if (isValid(rotate_ctx[idx])) begin
//             h.ty = WRAP;
//         end
//         up_header.response.put(h);
//     endrule

//     rule resp_header_rd(peekGet(down_header.response).op == RD);
//         let h <- down_header.response.get;
//         UInt#(TLog#(outStanding)) idx = ctxAsIdx(h);
//         if (isValid(rotate_ctx[idx])) begin
//             h.ty = WRAP;
//         end
//         up_header.response.put(h);
//         resp_rotate_req.enq(rotate_ctx[idx]);
//     endrule

//     mkNocWrapPayloadRotator(True, toGet(resp_rotate_req), tuple2(down_payload.response, up_payload.response));

// endmodule

endpackage