package NocFlowCtrl;
import GetPut::*;
import NocPacket::*;
import Packet::*;
import Vector::*;
import Connectable::*;
import FIFOF::*;
import SpecialFIFOs :: * ;
import ClientServer::*;
import NocConnetion::*;
import NocCtx::*;
import BUtils::*;
`include "NocPacket.defines"

typedef struct {
    Bit#(wCtx) ctx;
} NocIssueCtx#(numeric type wCtx) deriving (Eq, Bits, FShow);

typedef CtxBuffer#(outStanding, NocIssueCtx#(wCtx)) NocIssueCtxBufffer#(numeric type outStanding, numeric type wCtx);


module mkNocIssue#(
    NocIssueCtxBufffer#(outStanding, wCtx) ctx,
    NocCtxPipe#(outStanding, `NocPacketParams, nBytes) pipe
    )()
provisos(Add#(x, TLog#(outStanding), wCtx));

    match {.up_header, .up_payload} = pipe.upstream;
    match {.down_header, .down_payload} = pipe.downstream;
    
    rule req_header;
        let h <- up_header.request.get;
        let idx <- ctx.alloc(NocIssueCtx {ctx: h.ctx});
        h.ctx = cExtend(pack(idx));
        down_header.request.put(h);
    endrule

    mkConnection(up_payload.request, down_payload.request);

    rule resp_header;
        let h <- down_header.response.get;
        let idx = ctxAsIdx(h);
        let c = ctx.lookup(idx);
        h.ctx = c.ctx;
        if (h.status == RESP) begin
            ctx.free(idx);
        end
        up_header.response.put(h);
    endrule

    mkConnection(down_payload.response, up_payload.response);

endmodule

endpackage