package NocAlign;
import GetPut::*;
import NocPacket::*;
import Packet::*;
import Vector::*;
import Connectable::*;
import FIFOF::*;
import SpecialFIFOs :: * ;
import ClientServer::*;
import NocConnetion::*;
import BUtils::*;
import Assert::*;
`include "NocPacket.defines"

module mkNocNormalN2WAligner#(Get#(`NocTransHeaderG) header, GetPut#(`NocPayloadG(nBytes)) payload)()
provisos(
    Add#(wLen, x, wAddr)
);
    match {.payload_in, .payload_out} = payload;

    Vector#(TSub#(nBytes, 1), Reg#(NocPayloadByte)) bytes <- replicateM( mkReg( unpack(0) ));
    Reg#(Bool) err <- mkReg(False);

    Reg#(UInt#(TAdd#(wLen, 1))) left <- mkReg(0);
    Wire#(UInt#(TAdd#(wLen, 1))) left_next <- mkWire;

    Wire#(Maybe#(UInt#(TAdd#(wLen, 1)))) left_w <- mkDWire(tagged Invalid);

    Reg#(Bool) new_burst <- mkReg(True);


    function UInt#(TAdd#(wLen, 1)) curLeft();
        return fromMaybe(left, left_w);
    endfunction

    function Bit#(TLog#(nBytes)) offset(`NocTransHeaderG h);
        Bit#(wAddr) mask = cExtend(1) << h.size ;
        mask = mask - 1;
        Bit#(wAddr) tail_aligned_addr = h.h.addr & (~mask);
        return pack(tail_aligned_addr)[valueOf(TSub#(TLog#(nBytes), 1)):0];
    endfunction

    function UInt#(TAdd#(wLen, 1)) leftInit(`NocTransHeaderG h);
        Bit#(wLen) mask = fromInteger(valueOf(nBytes)) - 1;
        Bit#(wLen) head_len = truncate(h.h.addr) & mask;
        let aligned_len = {1'b0, pack(h.h.len)} + {1'b0, head_len};
        return unpack(aligned_len);
    endfunction

    function UInt#(TAdd#(wLen, 1)) leftNext(UInt#(TAdd#(wLen, 1)) cur);
        return cur > fromInteger(valueOf(nBytes)) ? cur - fromInteger(valueOf(nBytes)) : 0;
    endfunction

    rule next_states;
        let h = peekGet(header);
        if (new_burst) begin
            let left_init = leftInit(h);
            left_w <= tagged Valid left_init;
            left_next <= leftNext(left_init);
            // $display("[%0t] n2w init left_init = %x offset = %x: ", $time, left_init, offset(h), fshow(peekGet(header)));
        end
        else begin
            left_next <= leftNext(left);
        end
    endrule

    rule forward(offset(peekGet(header)) == 0);
        let p <- payload_in.get;
        payload_out.put(p);
        left <= left_next;
        if (left_next == 0) begin
            let h <- header.get;
        end
        // $display("[%0t] n2w forward left_next = %x: ", $time, left_next,fshow(peekGet(header)), fshow(p));
        new_burst <= left_next == 0;
    endrule

    rule align(offset(peekGet(header)) != 0 &&& curLeft() > cExtend(offset(peekGet(header))) &&& left_next != 0);
        let h = peekGet(header);
        new_burst <= False;
        left <= left_next;
        let p <- payload_in.get;

        Vector#(nBytes, NocPayloadByte) pre_bytes = rotate(cons(unpack(0), readVReg(bytes)));
        Vector#(nBytes, NocPayloadByte) cur_bytes = shiftOutFromN(unpack(0), p.bytes, {1'b0, offset(h)});
        Vector#(nBytes, NocPayloadByte) cur_rest_bytes = shiftOutFrom0(unpack(0), p.bytes, fromInteger(valueOf(nBytes)) - {1'b0, offset(h)});
        Vector#(TSub#(nBytes, 1), NocPayloadByte) rest_bytes = take(cur_rest_bytes);
        writeVReg(bytes, rest_bytes);
        err <= p.err;

        payload_out.put(NocPayload {last: False, err: p.err || err, bytes: unpack(pack(pre_bytes) | pack(cur_bytes))});
        // $display("[%0t] n2w align curLeft = %x, left_next = %x offset = %x: ", $time, curLeft(), left_next, offset(h), fshow(p), fshow(NocPayload {last: False, err: p.err || err, bytes: unpack(pack(pre_bytes) | pack(cur_bytes))}));
    endrule

    rule align_last(offset(peekGet(header)) != 0 &&& curLeft() > cExtend(offset(peekGet(header))) &&& left_next == 0);
        let h <- header.get;
        
        writeVReg(bytes, unpack(0));
        err <= False;
        new_burst <= True;
        left <= left_next;
        let p <- payload_in.get;

        Vector#(nBytes, NocPayloadByte) pre_bytes = rotate(cons(unpack(0), readVReg(bytes)));
        Vector#(nBytes, NocPayloadByte) cur_bytes = shiftOutFromN(unpack(0), p.bytes, {1'b0, offset(h)});
        payload_out.put(NocPayload {last: True, err: p.err || err, bytes: unpack(pack(pre_bytes) | pack(cur_bytes))});

        // $display("[%0t] n2w align last curLeft = %x, left_next = %x offset = %x: ", $time, curLeft(), left_next, offset(h), fshow(p), fshow(NocPayload {last: True, err: p.err || err, bytes: unpack(pack(pre_bytes) | pack(cur_bytes))}));
    endrule

    rule align_flush(offset(peekGet(header)) != 0  &&& curLeft() <= cExtend(offset(peekGet(header))));
        let h <- header.get;
        
        writeVReg(bytes, unpack(0));
        err <= False;
        new_burst <= True;
        left <= left_next;

        Vector#(nBytes, NocPayloadByte) pre_bytes = rotate(cons(unpack(0), readVReg(bytes)));
        payload_out.put(NocPayload {last: True, err: err, bytes: pre_bytes});
        // $display("[%0t] n2w align flush curLeft = %x, left_next = %x offset = %x: ", $time, curLeft(), left_next, offset(h), fshow(peekGet(header)), fshow(NocPayload {last: True, err: err, bytes: pre_bytes}));
    endrule


endmodule

module mkNocNormalW2NAligner#(Get#(`NocTransHeaderG) header, GetPut#(`NocPayloadG(nBytes)) payload)()
provisos(
    Add#(wLen, x, wAddr)
);
    match {.payload_in, .payload_out} = payload;

    Reg#(UInt#(wLen)) head_align_cnt <- mkReg(0);
    Wire#(Maybe#(UInt#(wLen))) head_align_cnt_w <- mkDWire(tagged Invalid);
    Wire#(UInt#(wLen)) head_align_cnt_next <- mkWire;

    Reg#(UInt#(wLen)) valid_cnt <- mkReg(0);
    Wire#(Maybe#(UInt#(wLen))) valid_cnt_w <- mkDWire(tagged Invalid);
    Wire#(UInt#(wLen)) valid_cnt_next <- mkWire;

    Reg#(UInt#(wLen)) tail_align_cnt <- mkReg(0);
    Wire#(Maybe#(UInt#(wLen))) tail_align_cnt_w <- mkDWire(tagged Invalid);
    Wire#(UInt#(wLen)) tail_align_cnt_next <- mkWire;

    Reg#(Bool) new_burst <- mkReg(True);


    function UInt#(wLen) curHeadAlignCnt();
        return fromMaybe(head_align_cnt, head_align_cnt_w);
    endfunction

    function UInt#(wLen) curValidCnt();
        return fromMaybe(valid_cnt, valid_cnt_w);
    endfunction

    function UInt#(wLen) curTailAlignCnt();
        return fromMaybe(tail_align_cnt, tail_align_cnt_w);
    endfunction

    function UInt#(wLen) nextCnt(UInt#(wLen) cur);
        return cur == 0 ? 0 : cur - 1;
    endfunction

    function UInt#(wLen) headAlignCntInit(`NocTransHeaderG h);
        Bit#(wAddr) mask = cExtend(1) << h.size;
        mask = mask - 1;
        Bit#(wLen) addr_low = truncate(h.h.addr & mask);
        return unpack(addr_low >> fromInteger(valueOf(TLog#(nBytes))));
    endfunction


    function UInt#(wLen) validCntInit(`NocTransHeaderG h);
        Bit#(wLen) mask = fromInteger(valueOf(nBytes)) - 1;
        Bit#(wLen) head_len = truncate(h.h.addr) & mask;
        let aligned_len = pack(h.h.len) + head_len;
        let cnt_base = aligned_len >> fromInteger(valueOf(TLog#(nBytes)));
        return unpack(cnt_base + cExtend(pack((aligned_len & mask) != 0)));
    endfunction

    function UInt#(wLen) tailAlignCntInit(`NocTransHeaderG h);
        Bit#(wLen) mask = cExtend(1) << h.size;
        mask = mask - 1;
        Bit#(wLen) head_len = truncate(h.h.addr) & mask;
        let aligned_len = pack(h.h.len) + head_len;
        let tail_len = aligned_len & mask;
        let tail_len_aligned = tail_len == 0 ? 0 : cExtend(1) << (h.size - fromInteger(valueOf(TLog#(nBytes))));
        Bit#(wLen) mask_local = fromInteger(valueOf(nBytes)) - 1;
        let tail_valid_len = aligned_len & mask_local;
        let tail_valid_len_aligned = tail_valid_len == 0 ? (tail_len >> fromInteger(valueOf(TLog#(nBytes)))) : (tail_len >> fromInteger(valueOf(TLog#(nBytes)))) + 1;
        return unpack(tail_len_aligned - tail_valid_len_aligned);
    endfunction


    rule update_stage;
        let h = peekGet(header);
        if (new_burst) begin
            let haed_init = headAlignCntInit(h);
            head_align_cnt_w <= tagged Valid haed_init;
            head_align_cnt_next <= nextCnt(haed_init);
            let valid_init = validCntInit(h);
            valid_cnt_w <= tagged Valid valid_init;
            valid_cnt_next <= nextCnt(valid_init);
            let tail_init = tailAlignCntInit(h);
            tail_align_cnt_w <= tagged Valid tail_init;
            tail_align_cnt_next <= nextCnt(tail_init);
            // $display("[%0t] w2n init head_align_cnt = %x, valid_cnt = %x, tail_align_cnt = %x: ", $time, haed_init, valid_init, tail_init);
        end
        else begin
            head_align_cnt_next <= nextCnt(head_align_cnt);
            valid_cnt_next <= nextCnt(valid_cnt);
            tail_align_cnt_next <= nextCnt(tail_align_cnt);
        end
    endrule

    rule head_align(curHeadAlignCnt() != 0);
        let _p <- payload_in.get;
        head_align_cnt <= head_align_cnt_next;
        valid_cnt <= curValidCnt();
        tail_align_cnt <= curTailAlignCnt();
        new_burst <= False;
        // $display("[%0t] w2n head head_align_cnt = %x: ", $time, curHeadAlignCnt(), fshow(peekGet(header)), fshow(_p));
    endrule

    rule valid(curValidCnt() != 0 &&& curHeadAlignCnt() == 0);
        let p <- payload_in.get;
        if (valid_cnt_next == 0) begin
            p.last = True;
            if (curTailAlignCnt() == 0) begin
                let h <- header.get;
            end
        end
        new_burst <= valid_cnt_next == 0 &&& curTailAlignCnt() == 0;
        payload_out.put(p);
        valid_cnt <= valid_cnt_next;
        tail_align_cnt <= curTailAlignCnt();
        // $display("[%0t] w2n valid cur = %x, next = %x, tail = %x next new_bust = %x: ", $time, curValidCnt(), valid_cnt_next, curTailAlignCnt(), pack( valid_cnt_next == 0 &&& curTailAlignCnt() == 0), fshow(peekGet(header)), fshow(p));
    endrule

    rule tail_align(curTailAlignCnt() != 0 &&& curValidCnt() == 0 &&& curHeadAlignCnt() == 0);
        let _p <- payload_in.get;
        if (tail_align_cnt_next == 0) begin
            let h <- header.get;
        end
        new_burst <= tail_align_cnt_next == 0;
        tail_align_cnt <= tail_align_cnt_next;
        // $display("[%0t] w2n tail: ", $time, fshow(peekGet(header)), fshow(_p));
    endrule

endmodule


module mkNocFixedN2WAligner#(Get#(`NocTransHeaderG) header, GetPut#(`NocPayloadG(nBytes)) payload)()
provisos(
    Add#(wLen, x, wAddr)
);
    match {.payload_in, .payload_out} = payload;

    Reg#(UInt#(wLen)) left <- mkReg(0);
    Wire#(UInt#(wLen)) left_next <- mkWire;

    Reg#(UInt#(TLog#(nBytes))) cnt <- mkReg(0);
    Wire#(UInt#(TAdd#(TLog#(nBytes), 1))) cnt_next <- mkWire;
    Wire#(Maybe#(UInt#(TLog#(nBytes)))) cnt_w <- mkDWire(tagged Invalid);

    Reg#(Bool) new_burst <- mkReg(True);


    function UInt#(TLog#(nBytes)) curCnt();
        return fromMaybe(cnt, cnt_w);
    endfunction

    function Bit#(TLog#(nBytes)) offset(`NocTransHeaderG h);
        Bit#(wAddr) mask = cExtend(1) << h.size ;
        mask = mask - 1;
        Bit#(wAddr) tail_aligned_addr = h.h.addr & (~mask);
        return pack(tail_aligned_addr)[valueOf(TSub#(TLog#(nBytes), 1)):0];
    endfunction

    // fixed len is beat len
    function UInt#(wLen) leftInit(`NocTransHeaderG h);
        return h.h.len;
    endfunction

    function UInt#(wLen) leftNext(UInt#(wLen) cur);
        return cur - 1;
    endfunction

    function UInt#(TAdd#(TLog#(nBytes), 1)) cntNext(`NocTransHeaderG h, UInt#(TLog#(nBytes)) cur);
        Bit#(wLen) size = cExtend(1) << h.size ;
        return unpack({1'b0, pack(cur)} + {1'b0, size[valueOf(TSub#(TLog#(nBytes), 1)):0]});
    endfunction

    function Bool cntFlag(UInt#(TAdd#(TLog#(nBytes), 1)) cur);
        return unpack(msb(cur));
    endfunction

    rule next_states;
        let h = peekGet(header);
        if (new_burst) begin
            let left_init = leftInit(h);
            left_next <= leftNext(left_init);
            cnt_w <= tagged Valid 0;
            cnt_next <= cntNext(h, 0);
            // $display("[%0t] fixed n2w init left_init = %x offset = %x: ", $time, left_init, offset(h), fshow(peekGet(header)));
        end
        else begin
            left_next <= leftNext(left);
            cnt_next <= cntNext(h, cnt);
        end
    endrule

    rule align;
        let h = peekGet(header);
        if (left_next == 0) begin
            let _h <- header.get;
        end
        let p = peekGet(payload_in);
        if (left_next == 0 || cntFlag(cnt_next)) begin
            let _p <- payload_in.get;
        end
        left <= left_next;
        new_burst <= left_next == 0;
        cnt <= unpack(pack(cnt_next)[valueOf(TSub#(TLog#(nBytes), 1)):0]);

        Vector#(nBytes, NocPayloadByte) cur_bytes = shiftOutFrom0(unpack(0), p.bytes, {1'b0, pack(curCnt())});
        cur_bytes = shiftOutFromN(unpack(0), cur_bytes, {1'b0, offset(h)});

        payload_out.put(NocPayload {last: left_next == 0, err: p.err, bytes: cur_bytes});
        // $display("[%0t] fixed n2w align left_next = %x offset = %x: ", $time, left_next, offset(h), fshow(p), fshow(NocPayload {last: left_next == 0, err: p.err, bytes: cur_bytes}));
    endrule
endmodule


module mkNocFixedW2NAligner#(Get#(`NocTransHeaderG) header, GetPut#(`NocPayloadG(nBytes)) payload)()
provisos(
    Add#(wLen, x, wAddr)
);
    match {.payload_in, .payload_out} = payload;

    Reg#(UInt#(wLen)) left <- mkReg(0);
    Wire#(UInt#(wLen)) left_next <- mkWire;
    Wire#(Maybe#(UInt#(wLen))) left_w <- mkDWire(tagged Invalid);

    Reg#(UInt#(wLen)) cnt <- mkReg(0);
    Wire#(UInt#(wLen)) cnt_next <- mkWire;
    Wire#(Maybe#(UInt#(wLen))) cnt_w <- mkDWire(tagged Invalid);

    Reg#(Bool) new_burst <- mkReg(True);

    function UInt#(wLen) curLeft();
        return fromMaybe(left, left_w);
    endfunction

    function UInt#(wLen) curCnt();
        return fromMaybe(cnt, cnt_w);
    endfunction

    function Bit#(wLen) offset(`NocTransHeaderG h);
        Bit#(wLen) mask = fromInteger(valueOf(nBytes)) - 1;
        Bit#(wLen) size_mask = cExtend(1) << h.size;
        size_mask = size_mask - 1;
        Bit#(wLen) len = truncate(h.h.addr) & (~mask) & size_mask;
        return len;
    endfunction

    // fixed len is beat len
    function UInt#(wLen) leftInit(`NocTransHeaderG h);
        return h.h.len;
    endfunction

    function UInt#(wLen) leftNext(UInt#(wLen) cur);
        return cur - 1;
    endfunction

    function UInt#(wLen) cntNext(`NocTransHeaderG h, UInt#(wLen) cur);
        Bit#(wLen) size = fromInteger(valueOf(nBytes));
        Bit#(wLen) boundary = cExtend(1) << h.size;
        Bit#(wLen) size_mask = boundary - 1;
        Bit#(wLen) next = pack(cur) + size;
        return unpack(next & size_mask);
    endfunction

    function Bool cntFlag(`NocTransHeaderG h, UInt#(wLen) cur);
        Bit#(wLen) size = fromInteger(valueOf(nBytes));
        Bit#(wLen) boundary = cExtend(1) << h.size;
        Bit#(wLen) mask = boundary - 1;
        return pack(cur) == boundary - size;
    endfunction

    rule next_cnt;
        let h = peekGet(header);
        if (new_burst) begin
            cnt_w <= tagged Valid 0;
            cnt_next <= cntNext(h, 0);
        end
        else begin
            cnt_next <= cntNext(h, cnt);
        end
    endrule

    rule next_left;
        let h = peekGet(header);
        if (new_burst) begin
            let left_init = leftInit(h);
            left_w <= tagged Valid left_init;
            left_next <= leftNext(left_init);
        end
        else if (pack(curCnt()) == offset(h)) begin
            left_next <= leftNext(left);
        end
        else begin
            left_next <= left;
        end
    endrule

    rule forward (pack(curCnt()) == offset(peekGet(header)));
        let h = peekGet(header);
        if (cntFlag(h, curCnt()) &&& left_next == 0) begin
            let _h <- header.get;
        end
        let p <- payload_in.get;
        new_burst <= cntFlag(h, curCnt()) &&& left_next == 0;
        cnt <= cnt_next;
        left <= left_next;
        payload_out.put(NocPayload {last: left_next == 0, err: p.err, bytes: p.bytes});
        // $display("[%0t] fixed w2n align left_next = %x, curCnt = %x offset = %x: ", $time, left_next, curCnt(), offset(h));
    endrule

    rule discard (pack(curCnt()) != offset(peekGet(header)));
        let h = peekGet(header);
        if (cntFlag(h, curCnt()) &&& left_next == 0) begin
            let _h <- header.get;
            
        end
        let p <- payload_in.get;
        new_burst <= cntFlag(h, curCnt()) &&& left_next == 0;
        cnt <= cnt_next;
        left <= curLeft();
        // $display("[%0t] fixed w2n align left_next = %x, curCnt = %x offset = %x: ", $time, left_next, curCnt(), offset(h));
    endrule
endmodule


module mkNocBypassAligner#(Get#(`NocTransHeaderG) header, GetPut#(`NocPayloadG(nBytes)) payload)();
    match {.payload_in, .payload_out} = payload;

    rule header(nocHeaderLen(peekGet(header).h) == 0);
        let h <- header.get;
    endrule

    rule payload(nocHeaderLen(peekGet(header).h) != 0);
        let h = peekGet(header);
        let p <- payload_in.get;
        payload_out.put(p);
        if (p.last) begin
            let _h <- header.get;
        end
    endrule
endmodule

typedef enum {
    BYPASS,
    N2W,
    W2N,
    FIXED_N2W,
    FIXED_W2N
} NocAlignTy deriving (Eq, Bits, FShow);

module mkNocAligner#(transLayer in, NocPipePut#(`NocPacketParams, nBytes) out)()
provisos(
    NocSplitable#(transLayer, Get#(`NocTransHeaderG), Get#(`NocTransPayloadG(nBytes))),
    Add#(wLen, x, wAddr)
);

    match {.header_in, .payload_in} = nocSplit(in);
    match {.header_out, .payload_out} = out;

    FIFOF#(`NocTransHeaderG) header_fifo <- mkSizedBypassFIFOF(1);

    Wire#(Maybe#(NocAlignTy)) ty <- mkDWire(tagged Invalid);

    rule forward_header;
        let h <- header_in.get;
        header_out.put(h.h);
        header_fifo.enq(h);
    endrule

    rule route;
        let h = header_fifo.first;
        (* split *)
        if (nocHeaderLen(h.h) != 0) begin
            (* split *)
            if (h.h.ty == FIXED) begin
                (* split *)
                if (h.size < fromInteger(valueOf(TLog#(nBytes))) &&& h.h.op == WR) begin
                    ty <= tagged Valid FIXED_N2W;
                end
                else if (h.size > fromInteger(valueOf(TLog#(nBytes))) &&& h.h.op == RD) begin
                    ty <= tagged Valid FIXED_W2N;
                end
                else begin
                    ty <= tagged Valid BYPASS;
                end
            end
            else begin
                (* split *)
                if (h.size < fromInteger(valueOf(TLog#(nBytes)))) begin
                    ty <= tagged Valid N2W;
                end
                else if (h.size > fromInteger(valueOf(TLog#(nBytes)))) begin
                    ty <= tagged Valid W2N;
                end
                else begin
                    ty <= tagged Valid BYPASS;
                end
            end
        end
        else begin
            ty <= tagged Valid BYPASS;
        end
    endrule

    let n2w_header = interface Get;
        method ActionValue#(`NocTransHeaderG) get if (ty matches tagged Valid N2W);
            let h <- toGet(header_fifo).get;
            // $display("[%0t] n2w get header: ", $time, fshow(h));
            return h;
        endmethod
    endinterface;

    let n2w_payload =  tuple2(
        interface Get;
            method ActionValue#(`NocPayloadG(nBytes)) get if (ty matches tagged Valid N2W);
                let p <- payload_in.get;
                // $display("[%0t] n2w get payload: ", $time, fshow(p));
                return p.p;
            endmethod
        endinterface,
        interface Put;
            method Action put(`NocPayloadG(nBytes) p) if (ty matches tagged Valid N2W);
                payload_out.put(p);
                // $display("[%0t] n2w put payload: ", $time, fshow(p));
            endmethod
        endinterface
    );


    mkNocNormalN2WAligner(n2w_header, n2w_payload);

    let w2n_header = interface Get;
        method ActionValue#(`NocTransHeaderG) get if (ty matches tagged Valid W2N);
            let h <- toGet(header_fifo).get;
            // $display("[%0t] w2n get header: ", $time, fshow(h));
            return h;
        endmethod
    endinterface;

    let w2n_payload =  tuple2(
        interface Get;
            method ActionValue#(`NocPayloadG(nBytes)) get if (ty matches tagged Valid W2N);
                let p <- payload_in.get;
                // $display("[%0t] w2n get payload: ", $time, fshow(p));
                return p.p;
            endmethod
        endinterface,
        interface Put;
            method Action put(`NocPayloadG(nBytes) p) if (ty matches tagged Valid W2N);
                payload_out.put(p);
                // $display("[%0t] w2n put payload: ", $time, fshow(p));
            endmethod
        endinterface
    );

    mkNocNormalW2NAligner(w2n_header, w2n_payload);

    let fixed_n2w_header = interface Get;
        method ActionValue#(`NocTransHeaderG) get if (ty matches tagged Valid FIXED_N2W);
            let h <- toGet(header_fifo).get;
            // $display("[%0t] fixed n2w get header: ", $time, fshow(h));
            return h;
        endmethod
    endinterface;

    let fixed_n2w_payload =  tuple2(
        interface Get;
            method ActionValue#(`NocPayloadG(nBytes)) get if (ty matches tagged Valid FIXED_N2W);
                let p <- payload_in.get;
                // $display("[%0t] fixed n2w get payload: ", $time, fshow(p));
                return p.p;
            endmethod
        endinterface,
        interface Put;
            method Action put(`NocPayloadG(nBytes) p) if (ty matches tagged Valid FIXED_N2W);
                payload_out.put(p);
                // $display("[%0t] fixed n2w put payload: ", $time, fshow(p));
            endmethod
        endinterface
    );

    mkNocFixedN2WAligner(fixed_n2w_header, fixed_n2w_payload);


    let fixed_w2n_header = interface Get;
        method ActionValue#(`NocTransHeaderG) get if (ty matches tagged Valid FIXED_W2N);
            let h <- toGet(header_fifo).get;
            // $display("[%0t] fixed w2n get header: ", $time, fshow(h));
            return h;
        endmethod
    endinterface;

    let fixed_w2n_payload =  tuple2(
        interface Get;
            method ActionValue#(`NocPayloadG(nBytes)) get if (ty matches tagged Valid FIXED_W2N);
                let p <- payload_in.get;
                // $display("[%0t] fixed w2n get payload: ", $time, fshow(p));
                return p.p;
            endmethod
        endinterface,
        interface Put;
            method Action put(`NocPayloadG(nBytes) p) if (ty matches tagged Valid FIXED_W2N);
                payload_out.put(p);
                // $display("[%0t] fixed w2n put payload: ", $time, fshow(p));
            endmethod
        endinterface
    );

    mkNocFixedW2NAligner(fixed_w2n_header, fixed_w2n_payload);

    let bypass_header = interface Get;
        method ActionValue#(`NocTransHeaderG) get if (ty matches tagged Valid BYPASS);
            let h <- toGet(header_fifo).get;
            // // $display("[%0t] bypass get header: ", $time, fshow(h));
            return h;
        endmethod
    endinterface;

    let bypass_payload =  tuple2(
        interface Get;
            method ActionValue#(`NocPayloadG(nBytes)) get if (ty matches tagged Valid BYPASS);
                let p <- payload_in.get;
                // // $display("[%0t] bypass get payload: ", $time, fshow(p));
                return p.p;
            endmethod
        endinterface,
        interface Put;
            method Action put(`NocPayloadG(nBytes) p) if (ty matches tagged Valid BYPASS);
                payload_out.put(p);
                // // $display("[%0t] bypass put payload: ", $time, fshow(p));
            endmethod
        endinterface
    );

    mkNocBypassAligner(bypass_header, bypass_payload);

endmodule

module mkNocN2WAligner#(transLayer in, NocPipePut#(`NocPacketParams, nBytes) out)()
provisos(
    NocSplitable#(transLayer, Get#(`NocTransHeaderG), Get#(`NocTransPayloadG(nBytes))),
    Add#(wLen, x, wAddr)
);

    match {.header_in, .payload_in} = nocSplit(in);
    match {.header_out, .payload_out} = out;

    FIFOF#(`NocTransHeaderG) header_fifo <- mkSizedBypassFIFOF(1);

    Wire#(Maybe#(NocAlignTy)) ty <- mkDWire(tagged Invalid);

    rule forward_header;
        let h <- header_in.get;
        header_out.put(h.h);
        header_fifo.enq(h);
    endrule

    rule route;
        let h = header_fifo.first;
        (* split *)
        if (nocHeaderLen(h.h) != 0) begin
            (* split *)
            if (h.h.ty == FIXED) begin
                (* split *)
                if (h.size < fromInteger(valueOf(TLog#(nBytes))) &&& h.h.op == WR) begin
                    ty <= tagged Valid FIXED_N2W;
                end
                else if (h.size > fromInteger(valueOf(TLog#(nBytes))) &&& h.h.op == RD) begin
                    dynamicAssert(False, "Unexpected fixed w2n packet in NocN2WAligner!");
                end
                else begin
                    ty <= tagged Valid BYPASS;
                end
            end
            else begin
                (* split *)
                if (h.size < fromInteger(valueOf(TLog#(nBytes)))) begin
                    ty <= tagged Valid N2W;
                end
                else if (h.size > fromInteger(valueOf(TLog#(nBytes)))) begin
                    dynamicAssert(False, "Unexpected w2n packet in NocN2WAligner!");
                end
                else begin
                    ty <= tagged Valid BYPASS;
                end
            end
        end
        else begin
            ty <= tagged Valid BYPASS;
        end
    endrule

    let n2w_header = interface Get;
        method ActionValue#(`NocTransHeaderG) get if (ty matches tagged Valid N2W);
            let h <- toGet(header_fifo).get;
            // $display("[%0t] n2w get header: ", $time, fshow(h));
            return h;
        endmethod
    endinterface;

    let n2w_payload =  tuple2(
        interface Get;
            method ActionValue#(`NocPayloadG(nBytes)) get if (ty matches tagged Valid N2W);
                let p <- payload_in.get;
                // $display("[%0t] n2w get payload: ", $time, fshow(p));
                return p.p;
            endmethod
        endinterface,
        interface Put;
            method Action put(`NocPayloadG(nBytes) p) if (ty matches tagged Valid N2W);
                payload_out.put(p);
                // $display("[%0t] n2w put payload: ", $time, fshow(p));
            endmethod
        endinterface
    );


    mkNocNormalN2WAligner(n2w_header, n2w_payload);

    let fixed_n2w_header = interface Get;
        method ActionValue#(`NocTransHeaderG) get if (ty matches tagged Valid FIXED_N2W);
            let h <- toGet(header_fifo).get;
            // $display("[%0t] fixed n2w get header: ", $time, fshow(h));
            return h;
        endmethod
    endinterface;

    let fixed_n2w_payload =  tuple2(
        interface Get;
            method ActionValue#(`NocPayloadG(nBytes)) get if (ty matches tagged Valid FIXED_N2W);
                let p <- payload_in.get;
                // $display("[%0t] fixed n2w get payload: ", $time, fshow(p));
                return p.p;
            endmethod
        endinterface,
        interface Put;
            method Action put(`NocPayloadG(nBytes) p) if (ty matches tagged Valid FIXED_N2W);
                payload_out.put(p);
                // $display("[%0t] fixed n2w put payload: ", $time, fshow(p));
            endmethod
        endinterface
    );

    mkNocFixedN2WAligner(fixed_n2w_header, fixed_n2w_payload);

    let bypass_header = interface Get;
        method ActionValue#(`NocTransHeaderG) get if (ty matches tagged Valid BYPASS);
            let h <- toGet(header_fifo).get;
            // // $display("[%0t] bypass get header: ", $time, fshow(h));
            return h;
        endmethod
    endinterface;

    let bypass_payload =  tuple2(
        interface Get;
            method ActionValue#(`NocPayloadG(nBytes)) get if (ty matches tagged Valid BYPASS);
                let p <- payload_in.get;
                // // $display("[%0t] bypass get payload: ", $time, fshow(p));
                return p.p;
            endmethod
        endinterface,
        interface Put;
            method Action put(`NocPayloadG(nBytes) p) if (ty matches tagged Valid BYPASS);
                payload_out.put(p);
                // // $display("[%0t] bypass put payload: ", $time, fshow(p));
            endmethod
        endinterface
    );

    mkNocBypassAligner(bypass_header, bypass_payload);

endmodule

module mkNocW2NAligner#(transLayer in, NocPipePut#(`NocPacketParams, nBytes) out)()
provisos(
    NocSplitable#(transLayer, Get#(`NocTransHeaderG), Get#(`NocTransPayloadG(nBytes))),
    Add#(wLen, x, wAddr)
);

    match {.header_in, .payload_in} = nocSplit(in);
    match {.header_out, .payload_out} = out;

    FIFOF#(`NocTransHeaderG) header_fifo <- mkSizedBypassFIFOF(1);

    Wire#(Maybe#(NocAlignTy)) ty <- mkDWire(tagged Invalid);

    rule forward_header;
        let h <- header_in.get;
        header_out.put(h.h);
        header_fifo.enq(h);
    endrule

    rule route;
        let h = header_fifo.first;
        (* split *)
        if (nocHeaderLen(h.h) != 0) begin
            (* split *)
            if (h.h.ty == FIXED) begin
                (* split *)
                if (h.size < fromInteger(valueOf(TLog#(nBytes))) &&& h.h.op == WR) begin
                    dynamicAssert(False, "Unexpected fixed n2w packet in NocW2NAligner!");
                end
                else if (h.size > fromInteger(valueOf(TLog#(nBytes))) &&& h.h.op == RD) begin
                    ty <= tagged Valid FIXED_W2N;
                end
                else begin
                    ty <= tagged Valid BYPASS;
                end
            end
            else begin
                (* split *)
                if (h.size < fromInteger(valueOf(TLog#(nBytes)))) begin
                    dynamicAssert(False, "Unexpected n2w packet in NocW2NAligner!");
                end
                else if (h.size > fromInteger(valueOf(TLog#(nBytes)))) begin
                    ty <= tagged Valid W2N;
                end
                else begin
                    ty <= tagged Valid BYPASS;
                end
            end
        end
        else begin
            ty <= tagged Valid BYPASS;
        end
    endrule

    let w2n_header = interface Get;
        method ActionValue#(`NocTransHeaderG) get if (ty matches tagged Valid W2N);
            let h <- toGet(header_fifo).get;
            // $display("[%0t] w2n get header: ", $time, fshow(h));
            return h;
        endmethod
    endinterface;

    let w2n_payload =  tuple2(
        interface Get;
            method ActionValue#(`NocPayloadG(nBytes)) get if (ty matches tagged Valid W2N);
                let p <- payload_in.get;
                // $display("[%0t] w2n get payload: ", $time, fshow(p));
                return p.p;
            endmethod
        endinterface,
        interface Put;
            method Action put(`NocPayloadG(nBytes) p) if (ty matches tagged Valid W2N);
                payload_out.put(p);
                // $display("[%0t] w2n put payload: ", $time, fshow(p));
            endmethod
        endinterface
    );

    mkNocNormalW2NAligner(w2n_header, w2n_payload);

    let fixed_w2n_header = interface Get;
        method ActionValue#(`NocTransHeaderG) get if (ty matches tagged Valid FIXED_W2N);
            let h <- toGet(header_fifo).get;
            // $display("[%0t] fixed w2n get header: ", $time, fshow(h));
            return h;
        endmethod
    endinterface;

    let fixed_w2n_payload =  tuple2(
        interface Get;
            method ActionValue#(`NocPayloadG(nBytes)) get if (ty matches tagged Valid FIXED_W2N);
                let p <- payload_in.get;
                // $display("[%0t] fixed w2n get payload: ", $time, fshow(p));
                return p.p;
            endmethod
        endinterface,
        interface Put;
            method Action put(`NocPayloadG(nBytes) p) if (ty matches tagged Valid FIXED_W2N);
                payload_out.put(p);
                // $display("[%0t] fixed w2n put payload: ", $time, fshow(p));
            endmethod
        endinterface
    );

    mkNocFixedW2NAligner(fixed_w2n_header, fixed_w2n_payload);

    let bypass_header = interface Get;
        method ActionValue#(`NocTransHeaderG) get if (ty matches tagged Valid BYPASS);
            let h <- toGet(header_fifo).get;
            // // $display("[%0t] bypass get header: ", $time, fshow(h));
            return h;
        endmethod
    endinterface;

    let bypass_payload =  tuple2(
        interface Get;
            method ActionValue#(`NocPayloadG(nBytes)) get if (ty matches tagged Valid BYPASS);
                let p <- payload_in.get;
                // // $display("[%0t] bypass get payload: ", $time, fshow(p));
                return p.p;
            endmethod
        endinterface,
        interface Put;
            method Action put(`NocPayloadG(nBytes) p) if (ty matches tagged Valid BYPASS);
                payload_out.put(p);
                // // $display("[%0t] bypass put payload: ", $time, fshow(p));
            endmethod
        endinterface
    );

    mkNocBypassAligner(bypass_header, bypass_payload);

endmodule

endpackage