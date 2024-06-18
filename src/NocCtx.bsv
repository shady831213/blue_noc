package NocCtx;
import Vector::*;
import RegFile::*;
import FIFO::*;
import FIFOF::*;
import GetPut::*;
import SpecialFIFOs :: * ;

interface BitMapCell;
    method Action acquire;
    method Bool valid;
    method Bool idle;
    method Action free;
endinterface

module mkBitMapCell(BitMapCell);
    Reg#(Bool) bitmap[2] <- mkCReg(2, False);
    
    method Action acquire;
        bitmap[1] <= True;
    endmethod

    method Bool valid;
        return bitmap[0];
    endmethod

    method Bool idle;
        return !bitmap[1];
    endmethod

    method Action free;
        bitmap[0] <= False;
    endmethod
    
endmodule

interface BitMap#(numeric type n);
    interface Vector#(n, BitMapCell) bits;
endinterface

module mkBitMap(BitMap#(n));
    (* hide *)
    Vector#(n, BitMapCell) _bitmap <- replicateM(mkBitMapCell);
    interface bits = _bitmap;
endmodule

interface Allocator#(numeric type n);
    method ActionValue#(UInt#(TLog#(n))) alloc;
    method Bool valid(UInt#(TLog#(n)) idx);
    method Bool idle(UInt#(TLog#(n)) idx);
    method Action free(UInt#(TLog#(n)) idx);
endinterface

module mkAllocator(Allocator#(n));
    (* hide *)
    BitMap#(n) _bitmap <- mkBitMap;

    function Bool find_idle(BitMapCell in);
        return in.idle;
    endfunction

    method ActionValue#(UInt#(TLog#(n))) alloc if (findIndex(find_idle, _bitmap.bits) matches tagged Valid .idx);
        _bitmap.bits[idx].acquire;
        return idx;
    endmethod

    method Bool valid(UInt#(TLog#(n)) idx);
        return _bitmap.bits[idx].valid;
    endmethod

    method Bool idle(UInt#(TLog#(n)) idx);
        return _bitmap.bits[idx].idle;
    endmethod

    method Action free(UInt#(TLog#(n)) idx);
        _bitmap.bits[idx].free;
    endmethod
    
endmodule


interface CtxBuffer#(numeric type n, type ctxT);
    method ActionValue#(UInt#(TLog#(n))) alloc(ctxT ctx);
    method ctxT lookup(UInt#(TLog#(n)) idx);
    method Action update(UInt#(TLog#(n)) idx, ctxT ctx);
    method Bool valid(UInt#(TLog#(n)) idx);
    method Bool idle(UInt#(TLog#(n)) idx);
    method Action free(UInt#(TLog#(n)) idx);
endinterface

module mkRFCtxBuffer(CtxBuffer#(n, ctxT))
provisos(Bits#(ctxT, x));
    Allocator#(n) allocator <- mkAllocator;

    RegFile#(UInt#(TLog#(n)), ctxT) ctx_buffer <- mkRegFileFull;

    PulseWire update_req <- mkPulseWire;
    PulseWire free_req <- mkPulseWire;

    method ActionValue#(UInt#(TLog#(n))) alloc(ctxT ctx) if(!update_req);
        let idx <- allocator.alloc;
        ctx_buffer.upd(idx, ctx);
        return idx;
    endmethod

    method Action update(UInt#(TLog#(n)) idx, ctxT ctx);
        ctx_buffer.upd(idx, ctx);
        update_req.send;
    endmethod

    method ctxT lookup(UInt#(TLog#(n)) idx);
        return ctx_buffer.sub(idx);
    endmethod

    method valid = allocator.valid;

    method idle = allocator.idle;

    method Action free(UInt#(TLog#(n)) idx);
        allocator.free(idx);
        free_req.send;
    endmethod
    
endmodule


interface TaggedAllocator#(numeric type n, type t);
    method ActionValue#(UInt#(TLog#(n))) alloc(t tag);
    method Maybe#(UInt#(TLog#(n))) valid(t tag);
    method Bool idle(t tag);
    method Action free(t tag);
    method Maybe#(t) tag(UInt#(TLog#(n)) idx);
    method Action free_by_idx(UInt#(TLog#(n)) idx);
endinterface

module mkTaggedAllocator(TaggedAllocator#(n, t))
provisos(Bits#(t, wt),
Eq#(t));
    (* hide *)
    BitMap#(n) _bitmap <- mkBitMap;

    Vector#(n, Reg#(t)) tags <- replicateM(mkReg(?));

    function Bool find_tag_valid(t tag, Tuple2#(BitMapCell, t) i);
        return tag == tpl_2(i) && tpl_1(i).valid;
    endfunction


    function Bool find_tag_not_idle(t tag, Tuple2#(BitMapCell, t) i);
        return tag == tpl_2(i) && !tpl_1(i).idle;
    endfunction

    function Bool find_idle(BitMapCell in);
        return in.idle;
    endfunction

    method Maybe#(UInt#(TLog#(n))) valid(t tag);
        return findIndex(find_tag_valid(tag), zip(_bitmap.bits, readVReg(tags)));
    endmethod

    method Bool idle(t tag);
        return !isValid(findIndex(find_tag_not_idle(tag), zip(_bitmap.bits, readVReg(tags))));
    endmethod

    method ActionValue#(UInt#(TLog#(n))) alloc(t tag) if (findIndex(find_idle, _bitmap.bits) matches tagged Valid .idx);
        _bitmap.bits[idx].acquire;
        tags[idx] <= tag;
        return idx;
    endmethod

    method Action free(t tag);
        if (findIndex(find_tag_valid(tag), zip(_bitmap.bits, readVReg(tags))) matches tagged Valid .idx) begin
            _bitmap.bits[idx].free;
        end
    endmethod

    method Maybe#(t) tag(UInt#(TLog#(n)) idx);
        if (_bitmap.bits[idx].valid) begin
            return tagged Valid tags[idx];
        end
        else begin
            return tagged Invalid;
        end
    endmethod

    method Action free_by_idx(UInt#(TLog#(n)) idx);
        _bitmap.bits[idx].free;
    endmethod
endmodule

interface TaggedCtxBuffer#(numeric type n, type tagT, type ctxT);
    method Action alloc(tagT tag, ctxT ctx);
    method Bool valid(tagT tag);
    method Bool idle(tagT tag);
    method Action update(tagT tag, ctxT ctx);
    method ctxT lookup(tagT tag);
    method Action free(tagT tag);
endinterface

module mkRFTaggedCtxBuffer(TaggedCtxBuffer#(n, tagT, ctxT))
provisos(Bits#(ctxT, x),
Bits#(tagT, y),
Eq#(tagT));
    TaggedAllocator#(n, tagT) allocator <- mkTaggedAllocator;

    RegFile#(UInt#(TLog#(n)), ctxT) ctx_buffer <- mkRegFileFull;

    PulseWire update_req <- mkPulseWire;
    PulseWire free_req <- mkPulseWire;


    method Action alloc(tagT tag, ctxT ctx) if (!update_req);
        let idx <- allocator.alloc(tag);
        ctx_buffer.upd(idx, ctx);
    endmethod

    method Action update(tagT tag, ctxT ctx) if (!free_req);
        if (allocator.valid(tag) matches tagged Valid .idx) begin
            ctx_buffer.upd(idx, ctx);
            update_req.send;
        end
    endmethod

    method ctxT lookup(tagT tag);
        if (allocator.valid(tag) matches tagged Valid .idx) begin
            return ctx_buffer.sub(idx);
        end
        else begin
            return unpack(0);
        end
    endmethod

    method Bool valid(tagT tag);
        return isValid(allocator.valid(tag));
    endmethod

    method idle = allocator.idle;

    method Action free(tagT tag);
        allocator.free(tag);
        free_req.send;
    endmethod
    
endmodule

interface TaggedFIFOF #(numeric type n, type tagT, type elmT);
    method Action alloc(tagT tag, elmT elm);
    method Bool valid(tagT tag);
    method Bool idle(tagT tag);
    method Action enq(tagT tag, elmT elm);
    method Action deq(tagT tag);
    method elmT first(tagT tag);
    method Bool notFull(tagT tag);
    method Bool notEmpty(tagT tag);
    method Action clear(tagT tag);
endinterface


module [m] mkTaggedFIFOF#(m#(FIFOF#(elmT)) fifo)(TaggedFIFOF#(n, tagT, elmT))
provisos(
    IsModule#(m, c),
    Bits#(elmT, x),
    Bits#(tagT, y),
    Eq#(tagT));
    TaggedAllocator#(n, tagT) allocator <- mkTaggedAllocator;

    Vector#(n, FIFOF#(elmT)) fifos <- replicateM( fifo );

    FIFOF#(Tuple2#(UInt#(TLog#(n)), elmT)) enq_fifo <- mkFIFOF;


    Wire#(Bit#(n)) deq_req <- mkDWire(0);
    Wire#(Bit#(n)) clear_req <- mkDWire(0);

    Vector#(n, Wire#(elmT)) firsts <- replicateM(mkDWire(unpack(0)));

    PulseWire update_req <- mkPulseWire;

    function Vector#(n, ty) getWires(Vector#(n, Wire#(ty)) wires)
    provisos(Bits#(ty, w_ty));
        function ty f(Wire#(ty) ifc);
            return ifc._read;
         endfunction
        return map(f, wires);
    endfunction

    for (Integer i = 0; i < valueOf(n); i = i+1) begin
        rule enq_rule(tpl_1(enq_fifo.first) == fromInteger(i));
            match {.*, .elm} = enq_fifo.first;
            enq_fifo.deq;
            // $display("[%0t] TaggedFIFOF enq idx = %x, elm = %x", $time, fromInteger(i), pack(elm));
            fifos[fromInteger(i)].enq(elm);
        endrule

        rule deq_rule(deq_req[i] == 1);
            fifos[i].deq;
        endrule

        rule clear_rule(clear_req[i] == 1);
            fifos[i].clear;
        endrule

        rule free(allocator.tag(fromInteger(i)) matches tagged Valid .tag &&& !fifos[i].notEmpty &&& !update_req &&& !enq_fifo.notEmpty);
            allocator.free_by_idx(fromInteger(i));
            // $display("[%0t] TaggedFIFOF free tag = %x", $time, pack(tag));
        endrule

        rule lookup;
            firsts[i] <= fifos[i].first;
        endrule
    end

    method Action alloc(tagT tag, elmT elm) if (!update_req);
        let idx <- allocator.alloc(tag);
        // $display("[%0t] TaggedFIFOF alloc tag = %x, idx = %x, elm = %x", $time, pack(tag), idx, pack(elm));
        enq_fifo.enq(tuple2(idx, elm));
    endmethod

    method Bool valid(tagT tag);
        return isValid(allocator.valid(tag));
    endmethod

    method idle = allocator.idle;

    method Action enq(tagT tag, elmT elm);
        if (allocator.valid(tag) matches tagged Valid .idx) begin
            enq_fifo.enq(tuple2(idx, elm));
            update_req.send;
        end
    endmethod

    method Action deq(tagT tag);
        if (allocator.valid(tag) matches tagged Valid .idx) begin
            deq_req <= 1 << idx;
        end
    endmethod

    method elmT first(tagT tag);
        let datas = getWires(firsts);
        if (allocator.valid(tag) matches tagged Valid .idx) begin
            return datas[idx];
        end
        else begin
            return unpack(0);
        end
    endmethod

    method Bool notFull(tagT tag);
        if (allocator.valid(tag) matches tagged Valid .idx) begin
            return fifos[idx].notFull;
        end
        else begin
            return True;
        end
    endmethod

    method Bool notEmpty(tagT tag);
        if (allocator.valid(tag) matches tagged Valid .idx) begin
            return fifos[idx].notEmpty;
        end
        else begin
            return False;
        end
    endmethod

    method Action clear(tagT tag);
        if (allocator.valid(tag) matches tagged Valid .idx) begin
            clear_req <= 1 << idx;
        end
    endmethod

endmodule
endpackage