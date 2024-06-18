package NocPacket;
import Vector::*;
import Packet::*;
import Utils::*;
import Contexts::*;
export NocAtomic(..);
export NocOpc(..);
export NocType(..);
export NocStatus(..);
export NocErrCode(..);
export NocHeader(..);
export NocTransHeader(..);
export NocRouteId(..);
export NocPayloadByte(..);
export NocTail(..);
export NocPayload(..);
export NocTransPayload(..);
export NocPayloadW(..);
export NocTransPayloadW(..);
export NocPacket(..);
export NocFlit(..);
export NocFlitW(..);
export NocFlitA(..);
export NocFlitAW(..);
export NocPacketItem(..);
export ModuleWithNocPacket(..);
export getNocModule;
export nocHeaderLen;
export headerRange;
export beatLen;
export ctxAsIdx;
export overlapped;
export isExRd;
export isExWr;
export isContRd;
// export wrapRotate;
export wrapSplit;
export wrapPktSplit;
// export needRotate;
import BUtils::*;

`include "NocPacket.defines"

typedef enum {
    NORMAL,
    EXCLUSIVE
} NocAtomic deriving (Eq, Bits, FShow);

typedef enum {
    RD,
    WR,
    URG
} NocOpc deriving (Eq, Bits, FShow);

typedef enum {
    INCR,
    WRAP,
    FIXED
} NocType deriving (Eq, Bits, FShow);

typedef enum {
    REQ,
    RESP,
    CONT,
    FAIL
} NocStatus deriving (Eq, Bits, FShow);

typedef enum {
    SLV,
    DEC,
    UNS,
    DISC,
    SEC,
    HIDE,
    TMO
} NocErrCode deriving (Eq, Bits, FShow);


typedef struct { 
    Maybe#(NocErrCode) err;
    Bit#(wCtx) ctx;
    Bit#(wUser) user;
    Bit#(wAddr) addr;
    UInt#(wLen) len; 
    NocStatus status;
    NocOpc op;
    NocType ty;
    `NocRouteIdG routeId;
    Bit#(wPri) pri; 
    NocAtomic atomic;
} NocHeader#(`NocHeaderParamsD) deriving (Eq, Bits, FShow);

typedef struct {
    `NocHeaderG h;
    Bit#(`wNocSize) size;
} NocTransHeader#(`NocHeaderParamsD) deriving (Eq, Bits, FShow);

typedef struct {
    Bit#(wSeq)  seqId;
    Bit#(wTarg)  targId;
    Bit#(wInit)  initId;
} NocRouteId#(`NocRouteIdParamsD) deriving (Eq, Bits, FShow);


typedef struct {
    Bool last;
    Bool err;
    Vector#(nBytes, NocPayloadByte) bytes;
} NocPayload#(numeric type nBytes) deriving (Eq, Bits, FShow);

typedef struct {
    `NocPayloadG(nBytes) p;
    Vector#(nBytes, Bool) padding;
} NocTransPayload#(numeric type nBytes) deriving (Eq, Bits, FShow);

typedef struct {
    Bool last;
    Bool err;
    Vector#(nChunks, Vector#(nBytes, NocPayloadByte)) bytes;
} NocPayloadW#(numeric type nChunks, numeric type nBytes) deriving (Eq, Bits, FShow);


typedef struct {
    `NocPayloadWG(nChunks, nBytes) p;
    Vector#(nChunks, Vector#(nBytes, Bool)) padding;
} NocTransPayloadW#(numeric type nChunks, numeric type nBytes) deriving (Eq, Bits, FShow);

typedef struct {
    Bool be;
    Bit#(8) data;
} NocPayloadByte deriving (Eq, Bits, FShow);

typedef Bit#(0) NocTail; 

typedef struct {
    `NocHeaderG  header;
    `NocPayloadG(nBytes) payload;
} NocPacket#(`NocPacketParamsD, numeric type nBytes) deriving (Eq, Bits, FShow);

instance Packet#(`NocPacketG(nBytes), `NocHeaderG, `NocPayloadG(nBytes), NocTail);
endinstance

typedef union tagged {
    `NocTransHeaderG Header;
    `NocTransPayloadG(nBytes) Payload;
} NocFlit#(`NocPacketParamsD, numeric type nBytes) deriving (Eq, Bits, FShow);

typedef union tagged {
    `NocTransHeaderG Header;
    `NocTransPayloadWG(nChunks, nBytes) Payload;
} NocFlitW#(`NocPacketParamsD, numeric type nChunks, numeric type nBytes) deriving (Eq, Bits, FShow);

typedef struct {
    Bool is_payload;
    `NocTransPayloadG(nBytes) phit;
} NocFlitA#(numeric type nBytes) deriving (Eq, Bits, FShow);

typedef struct {
    Bool is_payload;
    `NocTransPayloadWG(nChunks, nBytes) phit;
} NocFlitAW#(numeric type nChunks, numeric type nBytes) deriving (Eq, Bits, FShow);

typedef struct {
    `NocHeaderG  header;
    Maybe#(Vector#(nChunks, `NocPayloadG(nBytes))) payload;
} NocPacketItem#(`NocPacketParamsD, numeric type nChunks, numeric type nBytes) deriving (Eq, Bits, FShow);

instance Packet#(`NocPacketItemG(nChunks, nBytes), `NocHeaderG, `NocPayloadG(nBytes), NocTail);
endinstance

typedef ModuleContext#(
    `NocPacketG(1)
    )
ModuleWithNocPacket#(`NocPacketParamsD);

module [Module] getNocModule#(
    `NocPacketG(1) p,
    ModuleWithNocPacket#(
        `NocPacketParams, 
        a) m)(a);
    let {_c,i} <- runWithCompleteContext(p, m);
    return i;
endmodule

function UInt#(wLen) nocHeaderLen(`NocHeaderG h);
    if (h.status == REQ && h.op == WR || (h.status == RESP || h.status == CONT) && h.op == RD) begin
        return h.len;
    end
    else begin
        return 0;
    end
endfunction

function Bool isExRd(`NocHeaderG h);
    return h.op == RD &&& h.atomic == EXCLUSIVE;
endfunction

function Bool isExWr(`NocHeaderG h);
    return h.op == WR &&& h.atomic == EXCLUSIVE;
endfunction

function Bool isContRd(`NocHeaderG h);
    return h.op == RD &&& h.status == CONT;
endfunction

function Bit#(wAddr) alignFAddr(Bit#(wAddr) nBytes, Bit#(wAddr) addr);
    return addr & ~(nBytes - 1);
endfunction

function Bit#(wAddr) alignCAddr(Bit#(wAddr) nBytes, Bit#(wAddr) addr);
    return alignFAddr(nBytes, addr + (nBytes - 1));
endfunction

function Tuple2#(Bit#(wAddr), Bit#(wLen)) headerRange(Integer nBytes, `NocHeaderG h)
provisos(Add#(wLen, x, wAddr));
    case (h.ty) matches
        INCR : begin
            let low = alignFAddr(fromInteger(nBytes), h.addr);
            let high = alignCAddr(fromInteger(nBytes), h.addr + cExtend(pack(h.len)));
            return tuple2(low, truncate(high - low));
        end
        WRAP : begin
            return tuple2(alignFAddr(cExtend(pack(h.len)), h.addr), pack(h.len));
        end
        FIXED : begin
            return tuple2(alignFAddr(fromInteger(nBytes), h.addr), fromInteger(nBytes));
        end
    endcase
endfunction

function UInt#(TSub#(wLen, TLog#(nBytes))) beatLen(`NocHeaderG h, `NocPayloadG(nBytes) _p);
    if (h.ty == FIXED) begin
        return truncate(h.len);
    end
    else begin
        Tuple2#(Bit#(TSub#(wLen, TLog#(nBytes))), Bit#(TLog#(nBytes))) len = split(pack(h.len));
        match {.len_hi, .len_low} = len;
        return unpack(len_low == 0 ? len_hi : len_hi + 1);
    end
endfunction

function UInt#(wIdx) ctxAsIdx(`NocHeaderG h)
provisos(Add#(x, wIdx, wCtx));
    return unpack(truncate(h.ctx));
endfunction

// function Bool needRotate(`NocHeaderG h, Integer nBytes)
// provisos(Add#(wLen, x, wAddr));
//     Bit#(wLen) mask = fromInteger(nBytes) - 1;
//     return (((truncate(h.addr) & mask) + pack(h.len) - 1) & ~beat_mask) == 0 &&& h.ty == WRAP;
// endfunction

// function `NocPayloadG(nBytes) wrapRotate(Bool reverse, Bit#(TLog#(nBytes)) offset, UInt#(TAdd#(TLog#(nBytes), 1)) len, `NocPayloadG(nBytes) p);
//     Bit#(TLog#(nBytes)) len_mask = truncate(pack(len) - 1);
//     let aligned_offset = offset & ~len_mask;
//     let wrap_offset = offset & len_mask;
//     UInt#(TAdd#(TLog#(nBytes), 1)) left = len - unpack({1'b0, wrap_offset});
//     let v1_shift = reverse ? unpack({1'b0, wrap_offset}) : left;
//     let v2_shift = reverse ? left : unpack({1'b0, wrap_offset});

//     Vector#(nBytes, NocPayloadByte) mask = unpack(0);
//     mask = shiftOutFromN(unpack('1), mask, len);

//     let bytes = shiftOutFrom0(unpack(0), p.bytes, {1'b0, aligned_offset});

//     let v1 = shiftOutFrom0(unpack(0), bytes, v1_shift);
//     let v2 = shiftOutFromN(unpack(0), bytes, v2_shift);

//     bytes = unpack((pack(v1) | pack(v2)) & pack(mask));

//     p.bytes = shiftOutFromN(unpack(0), bytes, {1'b0, aligned_offset});
//     return p;
// endfunction

function Tuple2#(`NocHeaderG, Maybe#(`NocHeaderG)) wrapSplit(`NocHeaderG h, Integer nBytes)
provisos(Add#(wLen, x, wAddr));
    let h1 = h;
    let h2 = h;
    h1.ty = INCR;
    h2.ty = INCR;
    Bit#(wLen) mask = pack(h.len) - 1;
    Bit#(wAddr) addr_mask = cExtend(mask);

    h1.len = unpack(pack(h.len) - (truncate(h.addr) & mask));
    if (h1.len == h.len) begin
        return tuple2(h1, tagged Invalid);
    end
    else begin
        h2.len = unpack((truncate(h.addr) & mask));
        h2.addr = h.addr & (~addr_mask);
        return tuple2(h1, tagged Valid h2);
    end

    // if (needRotate(h, nBytes)) begin
    //     return tuple2(h1, tagged Invalid);
    // end
    // else begin
    //     h1.len = unpack(pack(h.len) - (truncate(h.addr) & mask));
    //     if (h1.len == h.len) begin
    //         return tuple2(h1, tagged Invalid);
    //     end
    //     else begin
    //         h2.len = unpack((truncate(h.addr) & mask));
    //         h2.addr = h.addr & (~addr_mask);
    //         return tuple2(h1, tagged Valid h2);
    //     end
    // end
endfunction

function Tuple2#(`NocPacketItemG(nChunks, nBytes), Maybe#(`NocPacketItemG(nChunks, nBytes))) wrapPktSplit(`NocPacketItemG(nChunks, nBytes) p)
provisos(Add#(wLen, x, wAddr));
    if (p.payload matches tagged Valid .payload) begin
        match {.h1, .h2} = wrapSplit(p.header, valueOf(nBytes));
        if (h2 matches tagged Valid .h) begin
            Bit#(wLen) beat_mask = fromInteger(valueOf(nBytes)) - 1;
            Bit#(TLog#(nBytes)) h1_beat_len = ((pack(h1.len) + beat_mask) >> valueOf(TLog#(nBytes)))[valueOf(TSub#(TLog#(nBytes), 1)):0];

            `NocPayloadG(nBytes) zeros = unpack(0);
            `NocPayloadG(nBytes) ones = unpack('1);
            Vector#(nChunks, `NocPayloadG(nBytes)) p1 = payload;
            p1[h1_beat_len - 1].last = True;
            Vector#(nChunks, `NocPayloadG(nBytes)) p2 = payload;
            p2 = shiftOutFrom0(zeros, p2, {1'b0, h1_beat_len});

            Vector#(nChunks, `NocPayloadG(nBytes)) p1_mask = unpack(0);
            p1_mask = shiftOutFromN(ones, p1_mask, {1'b0, h1_beat_len});
            Vector#(nChunks, `NocPayloadG(nBytes)) p2_mask = unpack(~pack(p1_mask));
            p2_mask = shiftOutFrom0(zeros, p2_mask, {1'b0, h1_beat_len});

            let pkt1 = NocPacketItem {header: h1, payload: tagged Valid unpack(pack(p1) & pack(p1_mask))};
            let pkt2 = NocPacketItem {header: h, payload: tagged Valid unpack(pack(p2) & pack(p2_mask))};
            return tuple2(pkt1, tagged Valid pkt2);
        end
        else begin
            return tuple2(NocPacketItem {header: h1, payload: tagged Valid payload}, tagged Invalid);
        end
    end
    else begin
        return tuple2(p, tagged Invalid);
    end
endfunction


function Bool overlapped1(Tuple2#(Bit#(wAddr), Bit#(wLen)) r1, Tuple2#(Bit#(wAddr), Bit#(wLen)) r2);
    match {.addr1, .len1} = r1;
    match {.addr2, .len2} = r2;
    return (addr1 <= addr2) &&& (addr2 < addr1 + cExtend(len1));
endfunction

function Bool overlapped(Tuple2#(Bit#(wAddr), Bit#(wLen)) r1, Tuple2#(Bit#(wAddr), Bit#(wLen)) r2);
    return overlapped1(r1, r2) || overlapped1(r2, r1);
endfunction

endpackage