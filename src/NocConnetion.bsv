package NocConnetion;
import GetPut::*;
import NocPacket::*;
import Packet::*;
import Vector::*;
import Connectable::*;
import Probe::*;
import FIFOF::*;
import SpecialFIFOs :: * ;
import ClientServer::*;

`include "NocPacket.defines"


typedef Put#(`NocFlitG(nBytes)) NocFlitPut#(`NocPacketParamsD, numeric type nBytes);
typedef Get#(`NocFlitG(nBytes)) NocFlitGet#(`NocPacketParamsD, numeric type nBytes);

typedef Put#(`NocFlitWG(nChunks, nBytes)) NocFlitWPut#(`NocPacketParamsD, numeric type nChunks, numeric type nBytes);
typedef Get#(`NocFlitWG(nChunks, nBytes)) NocFlitWGet#(`NocPacketParamsD, numeric type nChunks, numeric type nBytes);

typedef Tuple2#(Put#(`NocTransHeaderG), Put#(`NocTransPayloadG(nBytes))) NocFlitPPut#(`NocPacketParamsD, numeric type nBytes);
typedef Tuple2#(Get#(`NocTransHeaderG), Get#(`NocTransPayloadG(nBytes))) NocFlitPGet#(`NocPacketParamsD, numeric type nBytes);

typedef Tuple2#(Put#(`NocTransHeaderG), Put#(`NocTransPayloadWG(nChunks, nBytes))) NocFlitPWPut#(`NocPacketParamsD, numeric type nChunks, numeric type nBytes);
typedef Tuple2#(Get#(`NocTransHeaderG), Get#(`NocTransPayloadWG(nChunks, nBytes))) NocFlitPWGet#(`NocPacketParamsD, numeric type nChunks, numeric type nBytes);

typedef Put#(`NocFlitAG(nBytes)) NocFlitAPut#(numeric type nBytes);
typedef Get#(`NocFlitAG(nBytes)) NocFlitAGet#(numeric type nBytes);
typedef Put#(`NocFlitAWG(nChunks, nBytes)) NocFlitAWPut#(numeric type nChunks, numeric type nBytes);
typedef Get#(`NocFlitAWG(nChunks, nBytes)) NocFlitAWGet#(numeric type nChunks, numeric type nBytes);


typedef Tuple2#(Put#(`NocHeaderG), Put#(`NocPayloadG(nBytes))) NocPipePut#(`NocPacketParamsD, numeric type nBytes);
typedef Tuple2#(Get#(`NocHeaderG), Get#(`NocPayloadG(nBytes))) NocPipeGet#(`NocPacketParamsD, numeric type nBytes);
typedef Server#(`NocHeaderG, `NocHeaderG) NocHeaderS#(`NocHeaderParamsD);
typedef Client#(`NocHeaderG, `NocHeaderG) NocHeaderC#(`NocHeaderParamsD);
typedef Server#(`NocPayloadG(nBytes), `NocPayloadG(nBytes)) NocPayloadS#(numeric type nBytes);
typedef Client#(`NocPayloadG(nBytes), `NocPayloadG(nBytes)) NocPayloadC#(numeric type nBytes);
typedef Tuple2#(NocHeaderS#(`NocHeaderParams), NocPayloadS#(nBytes))  NocPipeS#(`NocPacketParamsD, numeric type nBytes);
typedef Tuple2#(NocHeaderC#(`NocHeaderParams), NocPayloadC#(nBytes))  NocPipeC#(`NocPacketParamsD, numeric type nBytes);
interface NocPipe#(`NocPacketParamsD, numeric type nBytes);
    interface  NocPipeC#(`NocPacketParams, nBytes) upstream;
    interface  NocPipeS#(`NocPacketParams, nBytes) downstream;
endinterface

typedef NocPipe#(`NocPacketParams, nBytes) NocCtxPipe#(numeric type outStanding, `NocPacketParamsD, numeric type nBytes);

// module fromTransGet#(getTrans trans)(NocPipeGet#(`NocPacketParams, nBytes))
// provisos(
//     NocSplitable#(getTrans,  Get#(`NocTransHeaderG), Get#(`NocTransPayloadG(nBytes)))
// );
//     match {.header, .payload} = nocSplit(trans);
//     return tuple2(
//         interface Get;
//             method ActionValue#(`NocHeaderG) get;
//                 let h <- header.get;
//                 return h.h;
//             endmethod
//         endinterface,
//         interface Get;
//             method ActionValue#(`NocPayloadG(nBytes)) get;
//                 let p <- payload.get;
//                 return p.p;
//             endmethod
//         endinterface
//     );
// endmodule

module fromTransPut#(putTrans trans)(NocPipePut#(`NocPacketParams, nBytes))
provisos(
    NocSplitable#(putTrans,  Put#(`NocTransHeaderG), Put#(`NocTransPayloadG(nBytes)))
);
    match {.header, .payload} = nocSplit(trans);
    return tuple2(
        interface Put;
            method Action put(`NocHeaderG h);
                header.put(NocTransHeader {h:h, size: fromInteger(valueOf(TLog#(nBytes)))});
            endmethod
        endinterface,
        interface Put;
            method Action put(`NocPayloadG(nBytes) p);
                payload.put(NocTransPayload {p:p, padding: unpack(0)});
            endmethod
        endinterface
    );
endmodule

module mkTransClient#(NocPipeC#(`NocPacketParams, nBytes) client)(Tuple2#(NocFlitPGet#(`NocPacketParams, nBytes), NocFlitPPut#(`NocPacketParams, nBytes)));
    match {.header, .payload} = client;
    
    return tuple2(
        tuple2(
            interface Get;
                method ActionValue#(`NocTransHeaderG) get;
                    let h <- header.request.get;
                    return NocTransHeader {h:h, size:fromInteger(valueOf(TLog#(nBytes)))};
                endmethod
            endinterface,
            interface Get;
                method ActionValue#(`NocTransPayloadG(nBytes)) get;
                    let p <- payload.request.get;
                    return NocTransPayload {p:p, padding: unpack(0)};
                endmethod
            endinterface
        ),
        tuple2(
            interface Put;
                method Action put(`NocTransHeaderG h);
                    header.response.put(h.h);
                endmethod
            endinterface,
            interface Put;
                method Action put(`NocTransPayloadG(nBytes) p);
                    payload.response.put(p.p);
                endmethod
            endinterface
        )
    );

endmodule

module mkTransServer#(NocPipeS#(`NocPacketParams, nBytes) server)(Tuple2#(NocFlitPPut#(`NocPacketParams, nBytes), NocFlitPGet#(`NocPacketParams, nBytes)));
    match {.header, .payload} = server;
    
    return tuple2(
        tuple2(
            interface Put;
                method Action put(`NocTransHeaderG h);
                    header.request.put(h.h);
                endmethod
            endinterface,
            interface Put;
                method Action put(`NocTransPayloadG(nBytes) p);
                    payload.request.put(p.p);
                endmethod
            endinterface
        ),
        tuple2(
            interface Get;
                method ActionValue#(`NocTransHeaderG) get;
                    let h <- header.response.get;
                    return NocTransHeader {h:h, size: fromInteger(valueOf(TLog#(nBytes)))};
                endmethod
            endinterface,
            interface Get;
                method ActionValue#(`NocTransPayloadG(nBytes)) get;
                    let p <- payload.response.get;
                    return NocTransPayload {p:p, padding: unpack(0)};
                endmethod
            endinterface
        )
    );

endmodule



function Vector#(m, Vector#(n, a)) to2Dverctor(Vector#(TMul#(m, n), a) in);
    function Vector#(n, a) gen(Integer i);
        function a _gen(Integer j);
            return in[ i * valueOf(n) + j];
        endfunction
        return genWith(_gen);
    endfunction
    return genWith(gen);
endfunction

typeclass ToPutM#(type a, type b);
    module toPutM#(a x)(Put#(b));
endtypeclass

instance ToPutM#(a, b)
provisos(ToPut#(a, b));
    module toPutM#(a x)(Put#(b));
        return toPut(x);
    endmodule
endinstance

typeclass ToGetM#(type a, type b);
    module toGetM#(a x)(Get#(b));
endtypeclass

instance ToGetM#(a, b)
provisos(ToGet#(a, b));
    module toGetM#(a x)(Get#(b));
        return toGet(x);
    endmodule
endinstance

// instance ToPutM#(Put#(a), b)
// provisos(
//     Bits#(b, bs),
//     Connectable#(Get#(b), Put#(a))
//     );
//     module toPutM#(Put#(a) x)(Put#(b));
//         FIFOF#(b) fifo <- mkSizedBypassFIFOF(1);
//         mkConnection(toGet(fifo), x);
//         return toPut(fifo);
//     endmodule
// endinstance

module mkPutM#(Put#(a) x)(Put#(b))
provisos(
    Bits#(b, bs),
    Connectable#(Get#(b), Put#(a))
    );
    FIFOF#(b) fifo <- mkSizedBypassFIFOF(1);
    mkConnection(toGet(fifo), x);
    return toPut(fifo);
endmodule


module mkGetM#(Get#(a) x)(Get#(b))
provisos(
    Bits#(b, bs),
    Connectable#(Get#(a), Put#(b))
    );
    FIFOF#(b) fifo <- mkSizedBypassFIFOF(1);
    mkConnection(x, toPut(fifo));
    return toGet(fifo);
endmodule

instance ToGet#(Get#(`NocFlitG(k)), `NocFlitWG(m, nBytes))
provisos(Mul#(m, nBytes, k));
    function Get#(`NocFlitWG(m, nBytes)) toGet(Get#(`NocFlitG(k)) ax);
        return interface Get;
            method ActionValue#(`NocFlitWG(m, nBytes)) get();
                let v <- ax.get;
                case (v) matches
                    tagged Header .h: return tagged Header h;
                    tagged Payload .p: begin
                        return tagged Payload NocTransPayloadW {p : NocPayloadW {last: p.p.last, err: p.p.err, bytes: to2Dverctor(p.p.bytes)}, padding: to2Dverctor(p.padding)};
                    end
                endcase
            endmethod
        endinterface;
    endfunction
endinstance

instance ToGet#(Get#(`NocFlitWG(m, nBytes)), `NocFlitG(k))
provisos(Mul#(m, nBytes, k));
    function Get#(`NocFlitG(k)) toGet(Get#(`NocFlitWG(m, nBytes)) ax);
        return interface Get;
            method ActionValue#(`NocFlitG(k)) get();
                let v <- ax.get;
                case (v) matches
                    tagged Header .h: return tagged Header h;
                    tagged Payload .p: begin
                        return tagged Payload NocTransPayload {p: NocPayload {last: p.p.last, err: p.p.err, bytes: concat(p.p.bytes)}, padding: concat(p.padding)};
                    end
                endcase
            endmethod
        endinterface;
    endfunction
endinstance

instance ToGetM#(Get#(`NocFlitG(nBytes)), `NocFlitWG(nChunks, nBytes));
    module toGetM#(Get#(`NocFlitG(nBytes)) x)(Get#(`NocFlitWG(nChunks, nBytes)));
        (* hide *)
        let _ifc <- mkGetM(x);
        return _ifc;
    endmodule
endinstance

instance ToGetM#(Get#(`NocFlitWG(nChunks, nBytes)), `NocFlitG(nBytes));
    module toGetM#(Get#(`NocFlitWG(nChunks, nBytes)) x)(Get#(`NocFlitG(nBytes)));
        (* hide *)
        let _ifc <- mkGetM(x);
        return _ifc;
    endmodule
endinstance

instance ToPut#(Put#(`NocFlitG(k)), `NocFlitWG(m, nBytes))
provisos(Mul#(m, nBytes, k));
    function Put#(`NocFlitWG(m, nBytes)) toPut(Put#(`NocFlitG(k)) ax);
        return interface Put;
            method Action put(`NocFlitWG(m, nBytes) v);
                case (v) matches
                    tagged Header .h: ax.put(tagged Header h);
                    tagged Payload .p: ax.put(tagged Payload NocTransPayload {p: NocPayload {last: p.p.last, err: p.p.err, bytes: concat(p.p.bytes)}, padding: concat(p.padding)});
                endcase
            endmethod
        endinterface;
    endfunction
endinstance

instance ToPut#(Put#(`NocFlitWG(m, nBytes)), `NocFlitG(k))
provisos(Mul#(m, nBytes, k));
    function Put#(`NocFlitG(k)) toPut(Put#(`NocFlitWG(m, nBytes)) ax);
        return interface Put;
            method Action put(`NocFlitG(k) v);
                case (v) matches
                    tagged Header .h: ax.put(tagged Header h);
                    tagged Payload .p: ax.put(tagged Payload NocTransPayloadW {p: NocPayloadW {last: p.p.last, err: p.p.err, bytes: to2Dverctor(p.p.bytes)}, padding: to2Dverctor(p.padding)});
                endcase
            endmethod
        endinterface;
    endfunction
endinstance

instance ToPutM#(Put#(`NocFlitG(nBytes)), `NocFlitWG(nChunks, nBytes));
    module toPutM#(Put#(`NocFlitG(nBytes)) x)(Put#(`NocFlitWG(nChunks, nBytes)));
        (* hide *)
        let _ifc <- mkPutM(x);
        return _ifc;
    endmodule
endinstance

instance ToPutM#(Put#(`NocFlitWG(nChunks, nBytes)), `NocFlitG(nBytes));
    module toPutM#(Put#(`NocFlitWG(nChunks, nBytes)) x)(Put#(`NocFlitG(nBytes)));
        (* hide *)
        let _ifc <- mkPutM(x);
        return _ifc;
    endmodule
endinstance

instance ToGet#(Get#(`NocTransPayloadG(k)), `NocTransPayloadWG(m, nBytes))
provisos(Mul#(m, nBytes, k));
    function Get#(`NocTransPayloadWG(m, nBytes)) toGet(Get#(`NocTransPayloadG(k)) ax);
        return interface Get;
            method ActionValue#(`NocTransPayloadWG(m, nBytes)) get();
                let p <- ax.get;
                return NocTransPayloadW {p: NocPayloadW {last: p.p.last, err: p.p.err, bytes: to2Dverctor(p.p.bytes)}, padding: to2Dverctor(p.padding)};
            endmethod
        endinterface;
    endfunction
endinstance

instance ToGet#(Get#(`NocTransPayloadWG(m, nBytes)), `NocTransPayloadG(k))
provisos(Mul#(m, nBytes, k));
    function Get#(`NocTransPayloadG(k)) toGet(Get#(`NocTransPayloadWG(m, nBytes)) ax);
        return interface Get;
            method ActionValue#(`NocTransPayloadG(k)) get();
                let p <- ax.get;
                return NocTransPayload {p: NocPayload {last: p.p.last, err: p.p.err, bytes: concat(p.p.bytes)}, padding: concat(p.padding)};
            endmethod
        endinterface;
    endfunction
endinstance

instance ToGetM#(Get#(`NocTransPayloadG(nBytes)), `NocTransPayloadWG(nChunks, nBytes));
    module toGetM#(Get#(`NocTransPayloadG(nBytes)) x)(Get#(`NocTransPayloadWG(nChunks, nBytes)));
        (* hide *)
        let _ifc <- mkGetM(x);
        return _ifc;
    endmodule
endinstance

instance ToGetM#(Get#(`NocTransPayloadWG(nChunks, nBytes)), `NocTransPayloadG(nBytes));
    module toGetM#(Get#(`NocTransPayloadWG(nChunks, nBytes)) x)(Get#(`NocTransPayloadG(nBytes)));
        (* hide *)
        let _ifc <- mkGetM(x);
        return _ifc;
    endmodule
endinstance

instance ToPut#(Put#(`NocTransPayloadG(k)), `NocTransPayloadWG(m, nBytes))
provisos(Mul#(m, nBytes, k));
    function Put#(`NocTransPayloadWG(m, nBytes)) toPut(Put#(`NocTransPayloadG(k)) ax);
        return interface Put;
            method Action put(`NocTransPayloadWG(m, nBytes) p);
                ax.put(NocTransPayload {p: NocPayload {last: p.p.last, err: p.p.err, bytes: concat(p.p.bytes)}, padding: concat(p.padding)});
            endmethod
        endinterface;
    endfunction
endinstance

instance ToPut#(Put#(`NocTransPayloadWG(m, nBytes)),`NocTransPayloadG(k))
provisos(Mul#(m, nBytes, k));
    function Put#(`NocTransPayloadG(k)) toPut(Put#(`NocTransPayloadWG(m, nBytes)) ax);
        return interface Put;
            method Action put(`NocTransPayloadG(k) p);
                ax.put(NocTransPayloadW {p: NocPayloadW {last: p.p.last, err: p.p.err, bytes: to2Dverctor(p.p.bytes)}, padding: to2Dverctor(p.padding)});
            endmethod
        endinterface;
    endfunction
endinstance

instance ToPutM#(Put#(`NocTransPayloadG(nBytes)), `NocTransPayloadWG(nChunks, nBytes));
    module toPutM#(Put#(`NocTransPayloadG(nBytes)) x)(Put#(`NocTransPayloadWG(nChunks, nBytes)));
        (* hide *)
        let _ifc <- mkPutM(x);
        return _ifc;
    endmodule
endinstance

instance ToPutM#(Put#(`NocTransPayloadWG(nChunks, nBytes)), `NocTransPayloadG(nBytes));
    module toPutM#(Put#(`NocTransPayloadWG(nChunks, nBytes)) x)(Put#(`NocTransPayloadG(nBytes)));
        (* hide *)
        let _ifc <- mkPutM(x);
        return _ifc;
    endmodule
endinstance

instance ToGet#(Get#(`NocFlitAG(k)), `NocFlitAWG(m, nBytes))
provisos(Mul#(m, nBytes, k));
    function Get#(`NocFlitAWG(m, nBytes)) toGet(Get#(`NocFlitAG(k)) ax);
        return interface Get;
            method ActionValue#(`NocFlitAWG(m, nBytes)) get();
                let v <- ax.get;
                if (v.is_payload) begin
                    return NocFlitAW {is_payload: v.is_payload, phit: NocTransPayloadW  {p: NocPayloadW {last: v.phit.p.last, err: v.phit.p.err, bytes: to2Dverctor(v.phit.p.bytes)}, padding: to2Dverctor(v.phit.padding)}};
                end
                else begin
                    let bits = pack(v.phit);
                    `NocTransPayloadWG(m, nBytes) phit = unpack(bits);
                    return NocFlitAW {is_payload: v.is_payload, phit: phit};
                end
            endmethod
        endinterface;
    endfunction
endinstance

instance ToGetM#(Get#(`NocFlitAG(nBytes)), `NocFlitAWG(nChunks, nBytes));
    module toGetM#(Get#(`NocFlitAG(nBytes)) x)(Get#(`NocFlitAWG(nChunks, nBytes)));
        (* hide *)
        let _ifc <- mkGetM(x);
        return _ifc;
    endmodule
endinstance

instance ToGetM#(Get#(`NocFlitAWG(nChunks, nBytes)), `NocFlitAG(nBytes));
    module toGetM#(Get#(`NocFlitAWG(nChunks, nBytes)) x)(Get#(`NocFlitAG(nBytes)));
        (* hide *)
        let _ifc <- mkGetM(x);
        return _ifc;
    endmodule
endinstance

instance ToGetM#(Get#(`NocFlitAWG(nChunks, nBytes)), `NocFlitG(nBytes));
    module toGetM#(Get#(`NocFlitAWG(nChunks, nBytes)) x)(Get#(`NocFlitG(nBytes)));
        (* hide *)
        let _ifc <- mkGetM(x);
        return _ifc;
    endmodule
endinstance

instance ToGetM#(Get#(`NocFlitWG(nChunks, nBytes)), `NocFlitAG(nBytes));
    module toGetM#(Get#(`NocFlitWG(nChunks, nBytes)) x)(Get#(`NocFlitAG(nBytes)));
        (* hide *)
        let _ifc <- mkGetM(x);
        return _ifc;
    endmodule
endinstance

instance ToGetM#(Get#(`NocTransPayloadWG(nChunks, nBytes)), `NocTransHeaderG);
    module toGetM#(Get#(`NocTransPayloadWG(nChunks, nBytes)) x)(Get#(`NocTransHeaderG));
        (* hide *)
        let _ifc <- mkGetM(x);
        return _ifc;
    endmodule
endinstance

instance ToGetM#(Get#(`NocTransHeaderG), `NocTransPayloadWG(nChunks, nBytes));
    module toGetM#(Get#(`NocTransHeaderG) x)(Get#(`NocTransPayloadWG(nChunks, nBytes)));
        (* hide *)
        let _ifc <- mkGetM(x);
        return _ifc;
    endmodule
endinstance

instance ToGetM#(Get#(`NocTransHeaderG), `NocTransPayloadG(nBytes));
    module toGetM#(Get#(`NocTransHeaderG) x)(Get#(`NocTransPayloadG(nBytes)));
        (* hide *)
        let _ifc <- mkGetM(x);
        return _ifc;
    endmodule
endinstance

instance ToPut#(Put#(`NocFlitAG(k)), `NocFlitAWG(m, nBytes))
provisos(Mul#(m, nBytes, k));
    function Put#(`NocFlitAWG(m, nBytes)) toPut(Put#(`NocFlitAG(k)) ax);
        return interface Put;
            method Action put(`NocFlitAWG(m, nBytes) v);
                if (v.is_payload) begin
                    ax.put(NocFlitA {is_payload: v.is_payload, phit: NocTransPayload {p: NocPayload {last: v.phit.p.last, err: v.phit.p.err, bytes: concat(v.phit.p.bytes)}, padding: concat(v.phit.padding)}});
                end
                else begin
                    let bits = pack(v.phit);
                    `NocTransPayloadG(k) phit = unpack(bits);
                    ax.put(NocFlitA {is_payload: v.is_payload, phit: phit});
                end
            endmethod
        endinterface;
    endfunction
endinstance

instance ToPutM#(Put#(`NocFlitAG(nBytes)), `NocFlitAWG(nChunks, nBytes));
    module toPutM#(Put#(`NocFlitAG(nBytes)) x)(Put#(`NocFlitAWG(nChunks, nBytes)));
        (* hide *)
        let _ifc <- mkPutM(x);
        return _ifc;
    endmodule
endinstance

instance ToPutM#(Put#(`NocFlitAWG(nChunks, nBytes)), `NocFlitAG(nBytes));
    module toPutM#(Put#(`NocFlitAWG(nChunks, nBytes)) x)(Put#(`NocFlitAG(nBytes)));
        (* hide *)
        let _ifc <- mkPutM(x);
        return _ifc;
    endmodule
endinstance

instance ToPutM#(Put#(`NocFlitAWG(nChunks, nBytes)), `NocFlitG(nBytes));
    module toPutM#(Put#(`NocFlitAWG(nChunks, nBytes)) x)(Put#(`NocFlitG(nBytes)));
        (* hide *)
        let _ifc <- mkPutM(x);
        return _ifc;
    endmodule
endinstance

instance ToPutM#(Put#(`NocFlitWG(nChunks, nBytes)), `NocFlitAG(nBytes));
    module toPutM#(Put#(`NocFlitWG(nChunks, nBytes)) x)(Put#(`NocFlitAG(nBytes)));
        (* hide *)
        let _ifc <- mkPutM(x);
        return _ifc;
    endmodule
endinstance

instance ToPutM#(Put#(`NocTransPayloadWG(nChunks, nBytes)), `NocTransHeaderG);
    module toPutM#(Put#(`NocTransPayloadWG(nChunks, nBytes)) x)(Put#(`NocTransHeaderG));
        (* hide *)
        let _ifc <- mkPutM(x);
        return _ifc;
    endmodule
endinstance

instance ToPutM#(Put#(`NocTransHeaderG), `NocTransPayloadWG(nChunks, nBytes));
    module toPutM#(Put#(`NocTransHeaderG) x)(Put#(`NocTransPayloadWG(nChunks, nBytes)));
        (* hide *)
        let _ifc <- mkPutM(x);
        return _ifc;
    endmodule
endinstance

instance ToPutM#(Put#(`NocTransHeaderG), `NocTransPayloadG(nBytes));
    module toPutM#(Put#(`NocTransHeaderG) x)(Put#(`NocTransPayloadG(nBytes)));
        (* hide *)
        let _ifc <- mkPutM(x);
        return _ifc;
    endmodule
endinstance

typeclass NocSplitable#(type a, type b, type c) dependencies (a determines (b, c));
    function Tuple2#(b, c) nocSplit(a ax);
endtypeclass

instance NocSplitable#(NocFlitGet#(`NocPacketParams, nBytes), Get#(`NocTransHeaderG), Get#(`NocTransPayloadG(nBytes)));
    function Tuple2#(Get#(`NocTransHeaderG), Get#(`NocTransPayloadG(nBytes))) nocSplit(NocFlitGet#(`NocPacketParams, nBytes) ax);
        return tuple2(
            interface Get;
                method ActionValue#(`NocTransHeaderG) get() if (peekGet(ax) matches tagged Header .h);
                    let _h <- ax.get;
                    return h;
                endmethod
            endinterface,
            interface Get;
                method ActionValue#(`NocTransPayloadG(nBytes)) get() if (peekGet(ax) matches tagged Payload .p);
                    let _p <- ax.get;
                    return p;
                endmethod
            endinterface
        );
    endfunction
endinstance

instance NocSplitable#(NocFlitPut#(`NocPacketParams, nBytes), Put#(`NocTransHeaderG), Put#(`NocTransPayloadG(nBytes)));
    function Tuple2#(Put#(`NocTransHeaderG), Put#(`NocTransPayloadG(nBytes))) nocSplit(NocFlitPut#(`NocPacketParams, nBytes) ax);
        return tuple2(
            interface Put;
                method Action put(`NocTransHeaderG h);
                    ax.put(tagged Header h);
                endmethod
            endinterface,
            interface Put;
                method Action put(`NocTransPayloadG(nBytes) p);
                    ax.put(tagged Payload p);
                endmethod
            endinterface
        );
    endfunction
endinstance

instance NocSplitable#(NocFlitWGet#(`NocPacketParams, nChunks, nBytes), Get#(`NocTransHeaderG), Get#(`NocTransPayloadWG(nChunks, nBytes)));
    function Tuple2#(Get#(`NocTransHeaderG), Get#(`NocTransPayloadWG(nChunks, nBytes))) nocSplit(NocFlitWGet#(`NocPacketParams, nChunks, nBytes) ax);
        return tuple2(
            interface Get;
                method ActionValue#(`NocTransHeaderG) get() if (peekGet(ax) matches tagged Header .h);
                    let _h <- ax.get;
                    return h;
                endmethod
            endinterface,
            interface Get;
                method ActionValue#(`NocTransPayloadWG(nChunks, nBytes)) get() if (peekGet(ax) matches tagged Payload .p);
                    let _p <- ax.get;
                    return p;
                endmethod
            endinterface
        );
    endfunction
endinstance

instance NocSplitable#(NocFlitWPut#(`NocPacketParams, nChunks, nBytes), Put#(`NocTransHeaderG), Put#(`NocTransPayloadWG(nChunks, nBytes)));
    function Tuple2#(Put#(`NocTransHeaderG), Put#(`NocTransPayloadWG(nChunks, nBytes))) nocSplit(NocFlitWPut#(`NocPacketParams, nChunks, nBytes) ax);
        return tuple2(
            interface Put;
                method Action put(`NocTransHeaderG h);
                    ax.put(tagged Header h);
                endmethod
            endinterface,
            interface Put;
                method Action put(`NocTransPayloadWG(nChunks, nBytes) p);
                    ax.put(tagged Payload p);
                endmethod
            endinterface
        );
    endfunction
endinstance


instance NocSplitable#(NocFlitAGet#(nBytes), Get#(`NocTransPayloadG(nBytes)), Get#(`NocTransPayloadG(nBytes)));
    function Tuple2#(Get#(`NocTransPayloadG(nBytes)), Get#(`NocTransPayloadG(nBytes))) nocSplit(NocFlitAGet#(nBytes) ax);
        return tuple2(
            interface Get;
                method ActionValue#(`NocTransPayloadG(nBytes)) get() if (!peekGet(ax).is_payload);
                    let h <- ax.get;
                    return h.phit;
                endmethod
            endinterface,
            interface Get;
                method ActionValue#(`NocTransPayloadG(nBytes)) get() if (peekGet(ax).is_payload);
                    let p <- ax.get;
                    return p.phit;
                endmethod
            endinterface
        );
    endfunction
endinstance

instance NocSplitable#(NocFlitAPut#(nBytes), Put#(`NocTransPayloadG(nBytes)), Put#(`NocTransPayloadG(nBytes)));
    function Tuple2#(Put#(`NocTransPayloadG(nBytes)), Put#(`NocTransPayloadG(nBytes))) nocSplit(NocFlitAPut#(nBytes) ax);
        return tuple2(
            interface Put;
                method Action put(`NocTransPayloadG(nBytes) h);
                    ax.put(NocFlitA{is_payload: False, phit: h});
                endmethod
            endinterface,
            interface Put;
                method Action put(`NocTransPayloadG(nBytes) p);
                    ax.put(NocFlitA{is_payload: True, phit: p});
                endmethod
            endinterface
        );
    endfunction
endinstance


instance NocSplitable#(NocFlitAWGet#(nChunks, nBytes), Get#(`NocTransPayloadWG(nChunks, nBytes)), Get#(`NocTransPayloadWG(nChunks, nBytes)));
    function Tuple2#(Get#(`NocTransPayloadWG(nChunks, nBytes)), Get#(`NocTransPayloadWG(nChunks, nBytes))) nocSplit(NocFlitAWGet#(nChunks, nBytes) ax);
        return tuple2(
            interface Get;
                method ActionValue#(`NocTransPayloadWG(nChunks, nBytes)) get() if (!peekGet(ax).is_payload);
                    let p <- ax.get;
                    return p.phit;
                endmethod
            endinterface,
            interface Get;
                method ActionValue#(`NocTransPayloadWG(nChunks, nBytes)) get() if (peekGet(ax).is_payload);
                    let p <- ax.get;
                    return p.phit;
                endmethod
            endinterface
        );
    endfunction
endinstance

instance NocSplitable#(NocFlitAWPut#(nChunks, nBytes), Put#(`NocTransPayloadWG(nChunks, nBytes)), Put#(`NocTransPayloadWG(nChunks, nBytes)));
    function Tuple2#(Put#(`NocTransPayloadWG(nChunks, nBytes)), Put#(`NocTransPayloadWG(nChunks, nBytes))) nocSplit(NocFlitAWPut#(nChunks, nBytes) ax);
        return tuple2(
            interface Put;
                method Action put(`NocTransPayloadWG(nChunks, nBytes) h);
                    ax.put(NocFlitAW{is_payload: True, phit: h});
                endmethod
            endinterface,
            interface Put;
                method Action put(`NocTransPayloadWG(nChunks, nBytes) p);
                    ax.put(NocFlitAW{is_payload: True, phit: p});
                endmethod
            endinterface
        );
    endfunction
endinstance


instance NocSplitable#(Tuple2#(a, b), a, b);
    function Tuple2#(a, b) nocSplit(Tuple2#(a, b) ax);
        return ax;
    endfunction
endinstance


instance Connectable#(Get#(`NocTransPayloadG(nBytes)), Put#(`NocTransPayloadWG(nChunks, nBytes)));
    module mkConnection#(Get#(`NocTransPayloadG(nBytes)) x1, Put#(`NocTransPayloadWG(nChunks, nBytes)) x2)();
        Reg#(UInt#(TLog#(nChunks))) cnt <- mkReg(0);
        Reg#(Bool) err <- mkReg(False);

        Vector#(TSub#(nChunks, 1), Reg#(Vector#(nBytes, NocPayloadByte))) chunks;
        chunks <- replicateM( mkReg( unpack(0) ) );
        

        Vector#(TSub#(nChunks, 1), Reg#(Vector#(nBytes, Bool))) paddings;
        paddings <- replicateM( mkReg( replicate(True) ) );
        // Probe#(UInt#(2)) probe <- mkProbe();

        rule flush(peekGet(x1).p.last || cnt == fromInteger(valueOf(nChunks) - 1));
            let p <- x1.get;
            cnt <= 0;
            err <= False;
            writeVReg(chunks, unpack(0));
            writeVReg(paddings, replicate(replicate(True)));
            let data = rotate(cons(unpack(0), readVReg(chunks)));
            let data_padding = rotate(cons(replicate(True), readVReg(paddings)));
            data[cnt] = p.p.bytes;
            data_padding[cnt] = p.padding;
            x2.put(NocTransPayloadW {p: NocPayloadW {last:p.p.last, err: err || p.p.err, bytes: data}, padding: data_padding});
            // probe <= 2;
        endrule

        rule buffer(!peekGet(x1).p.last &&& cnt != fromInteger(valueOf(nChunks) - 1));
            let p <- x1.get;
            cnt <= cnt + 1;
            chunks[cnt] <= p.p.bytes;
            paddings[cnt] <= p.padding;
            err <= err || p.p.err;
            // probe <= 1;
        endrule
    endmodule
endinstance

instance Connectable#(Get#(`NocTransPayloadWG(nChunks, nBytes)), Put#(`NocTransPayloadG(nBytes)));
    module mkConnection#(Get#(`NocTransPayloadWG(nChunks, nBytes)) x1, Put#(`NocTransPayloadG(nBytes)) x2)();
        Reg#(UInt#(TLog#(nChunks))) cnt <- mkReg(0);
        Vector#(TSub#(nChunks, 1),Reg#(Bool)) padding_map;
        padding_map <- replicateM( mkReg( False ) );
        // Probe#(UInt#(2)) probe <- mkProbe();

        function Bool no_padding(Vector#(nBytes, Bool) padding);
            return pack(map( \not , padding)) != 0;
        endfunction

        function Action handle_last(Vector#(TSub#(nChunks, 1), Bool) m, `NocTransPayloadWG(nChunks, nBytes) p);
            return action
                if (pack(m) == 0) begin
                    let _p <- x1.get;
                    writeVReg(padding_map, replicate(False));
                    cnt <= 0;
                    x2.put(NocTransPayload {p: NocPayload{last: True, err: p.p.err, bytes: p.p.bytes[cnt]}, padding: p.padding[cnt]});
                    // probe <= 2;
                end
                else begin
                    writeVReg(padding_map, m);
                    cnt <= cnt + 1;
                    x2.put(NocTransPayload {p: NocPayload{last: False, err: p.p.err, bytes: p.p.bytes[cnt]}, padding: p.padding[cnt]});
                    // probe <= 1;
                end
            endaction;
        endfunction

        rule last_first(peekGet(x1).p.last &&& cnt == 0);
            let p = peekGet(x1);
            Vector#(nChunks, Bool) _m = map(no_padding, p.padding);
            Vector#(TSub#(nChunks, 1), Bool) m = drop(_m);
            handle_last(m, p);
        endrule

        rule last_rest(peekGet(x1).p.last &&& cnt != 0);
            let p = peekGet(x1);
            let m = shiftOutFrom0(False, readVReg(padding_map), 1);
            handle_last(m, p);
        endrule

        rule next(!peekGet(x1).p.last &&& cnt == fromInteger(valueOf(nChunks) - 1));
            let p <- x1.get;
            cnt <= 0;
            x2.put(NocTransPayload {p: NocPayload{last: False, err: p.p.err, bytes: p.p.bytes[cnt]}, padding: p.padding[cnt]});
            // probe <= 2;
        endrule

        rule suspend(!peekGet(x1).p.last &&& cnt != fromInteger(valueOf(nChunks) - 1));
            let p = peekGet(x1);
            cnt <= cnt + 1;
            x2.put(NocTransPayload {p: NocPayload{last: False, err: p.p.err, bytes: p.p.bytes[cnt]}, padding: p.padding[cnt]});
            // probe <= 1;
        endrule
    endmodule
endinstance


instance Connectable#(Get#(`NocTransPayloadG(nBytes)), Put#(`NocTransHeaderG))
provisos(
    Bits#(`NocTransPayloadG(nBytes), a),
    Bits#(`NocTransHeaderG, b),
    Div#(b, a, nPhit)
    );
    module mkConnection#(Get#(`NocTransPayloadG(nBytes)) x1, Put#(`NocTransHeaderG) x2)();
        Reg#(UInt#(TLog#(nPhit))) cnt <- mkReg(0);

        Vector#(TSub#(nPhit, 1), Reg#(`NocTransPayloadG(nBytes))) phits;
        phits <- replicateM( mkReg( unpack(0) ) );

        function `NocTransHeaderG toHeader(Vector#(nPhit, `NocTransPayloadG(nBytes)) data);
            Bit#(b) h = 0;
            let bits = pack(data);
            for (int i = 0; i < fromInteger(valueOf(b)); i = i + 1) begin
                h[i] = bits[i];
            end
            return unpack(h);
        endfunction

                // Probe#(UInt#(2)) probe <- mkProbe();
        rule flush(cnt == fromInteger(valueOf(nPhit) - 1));
            let phit <- x1.get;
            cnt <= 0;
            writeVReg(phits, replicate(unpack(0)));
            let data = rotate(cons(unpack(0), readVReg(phits)));
            data[cnt] = phit;
            `NocTransHeaderG h = toHeader(data);
            x2.put(h);
            // probe <= 2;
        endrule

        rule buffer(cnt != fromInteger(valueOf(nPhit) - 1));
            let phit <- x1.get;
            cnt <= cnt + 1;
            phits[cnt] <= phit;
            // probe <= 1;
        endrule
    endmodule
endinstance

instance Connectable#(Get#(`NocTransHeaderG), Put#(`NocTransPayloadG(nBytes)))
provisos(
    Bits#(`NocTransPayloadG(nBytes), a),
    Bits#(`NocTransHeaderG, b),
    Div#(b, a, nPhit)
    );
    module mkConnection#(Get#(`NocTransHeaderG) x1, Put#(`NocTransPayloadG(nBytes)) x2)();
        Reg#(UInt#(TLog#(nPhit))) cnt <- mkReg(0);

        function Vector#(nPhit, Bit#(a)) headToPhits(Bit#(b) h);
            Bit#(TMul#(nPhit, a)) bits = 0;
            for (int i = 0; i < fromInteger(valueOf(b)); i = i + 1) begin
                bits[i] = h[i];
            end
            return unpack(bits);
        endfunction

        rule next(cnt == fromInteger(valueOf(nPhit) - 1));
            let h <- x1.get;
            cnt <= 0;
            x2.put(unpack(headToPhits(pack(h))[cnt]));
            // probe <= 2;
        endrule

        rule suspend(cnt != fromInteger(valueOf(nPhit) - 1));
            let h = peekGet(x1);
            cnt <= cnt + 1;
            x2.put(unpack(headToPhits(pack(h))[cnt]));
            // probe <= 1;
        endrule
    endmodule
endinstance

instance Connectable#(Get#(`NocTransPayloadWG(nChunks, nBytes)), Put#(`NocTransHeaderG))
provisos(
    Bits#(`NocTransPayloadWG(nChunks, nBytes), a),
    Bits#(`NocTransHeaderG, b),
    Div#(b, a, nPhit)
    );
    module mkConnection#(Get#(`NocTransPayloadWG(nChunks, nBytes)) x1, Put#(`NocTransHeaderG) x2)();
        Get#(`NocTransPayloadG(TMul#(nChunks, nBytes))) get = toGet(x1);
        mkConnection(get, x2);
    endmodule
endinstance

instance Connectable#(Get#(`NocTransHeaderG), Put#(`NocTransPayloadWG(nChunks, nBytes)))
provisos(
    Bits#(`NocTransPayloadWG(nChunks, nBytes), a),
    Bits#(`NocTransHeaderG, b),
    Div#(b, a, nPhit)
    );
    module mkConnection#(Get#(`NocTransHeaderG) x1, Put#(`NocTransPayloadWG(nChunks, nBytes)) x2)();
        Put#(`NocTransPayloadG(TMul#(nChunks, nBytes))) put = toPut(x2);
        mkConnection(x1, put);
    endmodule
endinstance

typeclass NocPayloadFlit#(type p);
    function Bool isLast(p pkt);
endtypeclass

instance NocPayloadFlit#(`NocTransPayloadG(nBytes));
    function Bool isLast(`NocTransPayloadG(nBytes) pkt);
        return pkt.p.last;
    endfunction
endinstance

instance NocPayloadFlit#(`NocTransPayloadWG(nChunks, nBytes));
    function Bool isLast(`NocTransPayloadWG(nChunks, nBytes) pkt);
        return pkt.p.last;
    endfunction
endinstance

module mkNocPtoNpConnect#(in x1, out x2)()
provisos(
    NocSplitable#(in, Get#(`NocTransHeaderG), Get#(p1)),
    NocSplitable#(out, Put#(h), Put#(p2)),
    NocPayloadFlit#(p2),
    Connectable#(Tuple2#(Get#(`NocTransHeaderG), Get#(p1)), Tuple2#(Put#(h), Put#(p2))));
    match {.header_get, .payload_get} = nocSplit(x1);
    match {.header_put, .payload_put} = nocSplit(x2);
    Reg#(Bool) is_payload <- mkReg(False);
    mkConnection(
        tuple2(
            interface Get;
                method ActionValue#(`NocTransHeaderG) get() if (!is_payload);
                    let h <- header_get.get;
                    is_payload <= nocHeaderLen(h.h) != 0;
                    return h;
                endmethod
            endinterface,
            payload_get
        ),
        tuple2(
            header_put,
            interface Put;
                method Action put(p2 p) if (is_payload);
                    payload_put.put(p);
                    is_payload <= !isLast(p);
                endmethod
            endinterface
        ));
endmodule

instance Connectable#(NocFlitPGet#(`NocPacketParams, nBytes), NocFlitPut#(`NocPacketParams, nBytes));
    module mkConnection#(NocFlitPGet#(`NocPacketParams, nBytes) x1, NocFlitPut#(`NocPacketParams, nBytes) x2)();
        mkNocPtoNpConnect(x1, x2);
    endmodule
endinstance


instance Connectable#(NocFlitPWGet#(`NocPacketParams, nChunks, nBytes), NocFlitPut#(`NocPacketParams, nBytes));
    module mkConnection#(NocFlitPWGet#(`NocPacketParams, nChunks, nBytes) x1, NocFlitPut#(`NocPacketParams, nBytes) x2)();
        mkNocPtoNpConnect(x1, x2);
    endmodule
endinstance


instance Connectable#(NocFlitPGet#(`NocPacketParams, nBytes), NocFlitWPut#(`NocPacketParams, nChunks, nBytes));
    module mkConnection#(NocFlitPGet#(`NocPacketParams, nBytes) x1, NocFlitWPut#(`NocPacketParams, nChunks, nBytes) x2)();
        mkNocPtoNpConnect(x1, x2);
    endmodule
endinstance


instance Connectable#(NocFlitPWGet#(`NocPacketParams, nChunks, nBytes), NocFlitWPut#(`NocPacketParams, nChunks, nBytes));
    module mkConnection#(NocFlitPWGet#(`NocPacketParams, nChunks, nBytes) x1, NocFlitWPut#(`NocPacketParams, nChunks, nBytes) x2)();
        mkNocPtoNpConnect(x1, x2);
    endmodule
endinstance

instance Connectable#(NocFlitPGet#(`NocPacketParams, nBytes), NocFlitAPut#(nBytes));
    module mkConnection#(NocFlitPGet#(`NocPacketParams, nBytes) x1, NocFlitAPut#(nBytes) x2)();
        mkNocPtoNpConnect(x1, x2);
    endmodule
endinstance

instance Connectable#(NocFlitPGet#(`NocPacketParams, nBytes), NocFlitAWPut#(nChunks, nBytes));
    module mkConnection#(NocFlitPGet#(`NocPacketParams, nBytes) x1, NocFlitAWPut#(nChunks, nBytes) x2)();
        mkNocPtoNpConnect(x1, x2);
    endmodule
endinstance

instance Connectable#(NocFlitPWGet#(`NocPacketParams, nChunks, nBytes), NocFlitAPut#(nBytes));
    module mkConnection#(NocFlitPWGet#(`NocPacketParams, nChunks, nBytes) x1, NocFlitAPut#(nBytes) x2)();
        mkNocPtoNpConnect(x1, x2);
    endmodule
endinstance

instance Connectable#(NocFlitPWGet#(`NocPacketParams, nChunks, nBytes), NocFlitAWPut#(nChunks, nBytes));
    module mkConnection#(NocFlitPWGet#(`NocPacketParams, nChunks, nBytes) x1, NocFlitAWPut#(nChunks, nBytes) x2)();
        mkNocPtoNpConnect(x1, x2);
    endmodule
endinstance

instance Connectable#(Server#(reqs, resps), Client#(reqc, respc))
provisos (
    Connectable#(Get#(reqc), Put#(reqs)),
    Connectable#(Get#(resps), Put#(respc))
);
    module mkConnection#(Server#(reqs, resps) x1, Client#(reqc, respc) x2)();
        mkConnection(x2.request, x1.request);
        mkConnection(x1.response, x2.response);
    endmodule
endinstance

instance Connectable#(Client#(reqc, respc), Server#(reqs, resps))
provisos (
    Connectable#(Get#(reqc), Put#(reqs)),
    Connectable#(Get#(resps), Put#(respc))
);
    module mkConnection#(Client#(reqc, respc) x1,  Server#(reqs, resps) x2)();
        mkConnection(x2, x1);
    endmodule
endinstance

instance Connectable#(a, b)
provisos (
    NocSplitable#(a, aa, ab),
    NocSplitable#(b, ba, bb),
    Connectable#(aa, ba),
    Connectable#(ab, bb)
);
    module mkConnection#(a x1, b x2)();
        match {.xx, .xy} = nocSplit(x1);
        match {.yx, .yy} = nocSplit(x2);

        mkConnection(xx, yx);
        mkConnection(xy, yy);

    endmodule
endinstance

endpackage