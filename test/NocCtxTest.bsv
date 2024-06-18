package NocCtxTest;
import NocCtx::*;
import Randomizable::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs :: * ;
import Vector::*;
(* synthesize *)
(* scan_insert = 16 *)
module mkCtxTest();  
    CtxBuffer#(4, UInt#(4)) ctx <- mkRFCtxBuffer;
    Randomize#(UInt#(2)) rand_idx <- mkGenericRandomizer;
    Randomize#(UInt#(2)) free_idx <- mkGenericRandomizer;
    Reg#(UInt#(2))  lookup_idx <- mkReg(0);
    
    Reg#(UInt#(32)) count <- mkReg(0);
    rule start;
        rand_idx.cntrl.init;
        free_idx.cntrl.init;
    endrule

    rule rand_lookup_idx;
        let idx <- rand_idx.next;
        lookup_idx <= idx;
    endrule

    rule every;
        count <= count + 1;
    endrule

    rule alloc;
        let idx <- ctx.alloc(truncate(count));
        $display("[%0t] alloced %d for 0x%x", $time, idx, pack(count)[3:0]);
    endrule

    rule lookup(ctx.valid(lookup_idx));
        let v = ctx.lookup(lookup_idx);
        $display("[%0t] lookup %d,get 0x%x", $time, lookup_idx, v);
    endrule

    rule free (count % 2 == 0);
        let idx <- free_idx.next;
        let v = ctx.lookup(idx);
        ctx.free(idx);
        $display("[%0t] free %d, get 0x%x", $time, idx, v);
    endrule

    rule done(count >= 1000);
        $display("Simulation Done!");
        $finish(0);
    endrule
endmodule

(* synthesize *)
module mkTaggedAllocatorTest();  
    TaggedAllocator#(4, UInt#(4)) allocator <- mkTaggedAllocator;

    Reg#(UInt#(32)) count <- mkReg(0);

    FIFOF#(UInt#(4)) in_fifo <- mkSizedFIFOF(4);
    FIFO#(UInt#(4)) out_fifo <- mkSizedFIFO(4);

    Randomize#(UInt#(4)) rand_tag <- mkGenericRandomizer;

    Reg#(UInt#(4)) cur_tag <- mkReg(0);


    rule start;
        rand_tag.cntrl.init;
    endrule

    rule every;
        count <= count + 1;
    endrule

    rule rand_tag_rule(count % 3 == 0);
        let tag <- rand_tag.next;
        cur_tag <= tag;
    endrule

    rule gen;
        let tag = cur_tag;
        in_fifo.enq(tag);
    endrule

    rule alloc(allocator.idle(in_fifo.first));
        let tag = in_fifo.first;
        let idx <- allocator.alloc(tag);
        in_fifo.deq;
        out_fifo.enq(tag);
        $display("[%0t] alloc tag = %x, idx = %x", $time, tag, idx);
    endrule

    rule free(count % 4 == 0);
        let tag = out_fifo.first;
        out_fifo.deq;
        allocator.free(tag);
        $display("[%0t] invalid tag = %x", $time, tag);
    endrule

    rule done(count >= 1000);
        $display("Simulation Done!");
        $finish(0);
    endrule
endmodule


(* synthesize *)
module mkTaggedCtxBufferTest();  
    TaggedCtxBuffer#(4, UInt#(4), Tuple2#(UInt#(2), UInt#(2))) ctx <- mkRFTaggedCtxBuffer;

    Reg#(UInt#(32)) count <- mkReg(0);

    FIFOF#(Tuple3#(UInt#(4), UInt#(2), UInt#(2))) in_fifo <- mkSizedFIFOF(4);
    FIFO#(Tuple3#(UInt#(4), UInt#(2), UInt#(2))) out_fifo <- mkSizedFIFO(4);

    Reg#(UInt#(2)) seq_count <- mkReg(0);
    Randomize#(UInt#(4)) rand_tag <- mkGenericRandomizer;
    Randomize#(UInt#(2)) rand_targ <- mkGenericRandomizer;

    Reg#(UInt#(4)) cur_tag <- mkReg(0);

    rule start;
        rand_tag.cntrl.init;
        rand_targ.cntrl.init;
    endrule

    rule every;
        count <= count + 1;
    endrule

    rule rand_tag_rule(count % 3 == 0);
        let tag <- rand_tag.next;
        cur_tag <= tag;
    endrule

    rule gen;
        let tag = cur_tag;
        let targ <- rand_targ.next;
        let seq_id = seq_count;
        seq_count <= seq_count + 1;
        in_fifo.enq(tuple3(tag, targ, seq_id));
    endrule

    rule same_tag(ctx.valid(tpl_1(in_fifo.first)) &&& tpl_1(ctx.lookup(tpl_1(in_fifo.first))) == tpl_2(in_fifo.first));
        match {.tag, .targ, .seq_id} = in_fifo.first;
        ctx.update(tag, tuple2(targ, seq_id));
        in_fifo.deq;
        out_fifo.enq(tuple3(tag, targ, seq_id));
        $display("[%0t] same_tag tag = %x, targ = %x, seq_id = %x", $time, tag, targ, seq_id);
    endrule

    rule new_tag(ctx.idle(tpl_1(in_fifo.first)));
        match {.tag, .targ, .seq_id} = in_fifo.first;
        ctx.alloc(tag, tuple2(targ, seq_id));
        in_fifo.deq;
        out_fifo.enq(tuple3(tag, targ, seq_id));
        $display("[%0t] new_tag tag = %x, targ = %x, seq_id = %x", $time, tag, targ, seq_id);
    endrule


    rule free(count % 4 == 0);
        match {.tag, .targ, .seq_id} = out_fifo.first;
        out_fifo.deq;
        $display("[%0t] recv tag = %x, targ = %x, seq_id = %x", $time, tag, targ, seq_id);
        if (tpl_2(ctx.lookup(tag)) == seq_id) begin
            ctx.free(tag);
            $display("[%0t] free tag = %x, targ = %x, seq_id = %x", $time, tag, targ, seq_id);
        end
    endrule

    rule done(count >= 1000);
        $display("Simulation Done!");
        $finish(0);
    endrule
endmodule


(* synthesize *)
module mkTaggedFIFOFTest();  
    TaggedFIFOF#(4, UInt#(4), UInt#(2)) ctx <- mkTaggedFIFOF(mkSizedBypassFIFOF(4));

    Reg#(UInt#(32)) count <- mkReg(0);

    FIFOF#(Tuple2#(UInt#(4), UInt#(2))) in_fifo <- mkSizedFIFOF(4);
    FIFO#(UInt#(4)) out_fifo <- mkSizedFIFO(4);

    Reg#(UInt#(2)) seq_count <- mkReg(0);
    Randomize#(UInt#(4)) rand_tag <- mkGenericRandomizer;

    Reg#(UInt#(4)) cur_tag <- mkReg(0);

    rule start;
        rand_tag.cntrl.init;
    endrule

    rule every;
        count <= count + 1;
    endrule

    rule rand_tag_rule;
        let tag <- rand_tag.next;
        cur_tag <= tag;
    endrule

    rule gen;
        let tag = cur_tag;
        let seq_id = seq_count;
        seq_count <= seq_count + 1;
        in_fifo.enq(tuple2(tag, seq_id));
    endrule


    rule alloc(ctx.idle(tpl_1(in_fifo.first)));
        match {.tag, .seq_id} = in_fifo.first;
        ctx.alloc(tag, seq_id);
        in_fifo.deq;
        out_fifo.enq(tag);
        $display("[%0t] alloc tag = %x, seq_id = %x", $time, tag, seq_id);
    endrule

    rule enq(ctx.valid(tpl_1(in_fifo.first)));
        match {.tag, .seq_id} = in_fifo.first;
        ctx.enq(tag, seq_id);
        in_fifo.deq;
        out_fifo.enq(tag);
        $display("[%0t] enq tag = %x, seq_id = %x", $time, tag, seq_id);
    endrule

    for (Integer i=0; i < 16; i= i + 1) begin
        rule lookup(ctx.notEmpty(fromInteger(i)));
            let seq_id = ctx.first(fromInteger(i));
            $display("[%0t] lookup tag = %x, seq_id = %x", $time, fromInteger(i), seq_id);
        endrule
    end

    rule deq(count % 3 == 0 &&& ctx.notEmpty(out_fifo.first));
        let tag = out_fifo.first;
        out_fifo.deq;
        let seq_id = ctx.first(tag);
        ctx.deq(tag);
        $display("[%0t] deq tag = %x, seq_id = %x", $time, tag, seq_id);
    endrule

    rule done(count >= 1000);
        $display("Simulation Done!");
        $finish(0);
    endrule
endmodule
endpackage