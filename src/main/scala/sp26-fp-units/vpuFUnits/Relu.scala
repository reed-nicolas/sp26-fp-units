package sp26FPUnits

import chisel3._
import chisel3.util._
import sp26FPUnits._
import sp26FPUnits.hardfloat._      
import sp26FPUnits.hardfloat.consts._

// Input bundle for Relu
class ReluReq(wordWidth: Int, numLanes: Int, tagWidth: Int) extends Bundle {
    val tag = UInt(tagWidth.W)
    val laneMask = UInt(numLanes.W)
    val whichBank = UInt(4.W) 
    val wRow = UInt(8.W)
    val aVec = Vec(numLanes, UInt(wordWidth.W))
}

// Output bundle for Relu
class ReluResp(wordWidth: Int, numLanes: Int, tagWidth: Int) extends Bundle {
    val tag = UInt(tagWidth.W)
    val laneMask = UInt(numLanes.W)
    val whichBank = UInt(4.W) 
    val wRow = UInt(8.W)      
    val result = Vec(numLanes, UInt(wordWidth.W))
}

class Relu(BF16T: AtlasFPType, numLanes: Int = 16, tagWidth: Int = 8) extends Module {
    val io = IO(new Bundle {
        val req = Flipped(Valid(new ReluReq(BF16T.wordWidth, numLanes, tagWidth)))
        val resp = Valid(new ReluResp(BF16T.wordWidth, numLanes, tagWidth))
    })

    // Software-scheduled interface: no backpressure, just a one-cycle pipeline.
    val reqReg = Reg(new ReluReq(BF16T.wordWidth, numLanes, tagWidth))
    when (io.req.valid) {
        reqReg := io.req.bits
    }

    // LOGIC for relu operation
    for (i <- 0 until numLanes) {
        val inVal = reqReg.aVec(i)
        
        // Check the sign bit - msb
        // If MSB is 1, value is negative -> return 0, otherwise pass value through
        val isNegative = inVal(BF16T.wordWidth - 1)
        io.resp.bits.result(i) := Mux(isNegative, 0.U(BF16T.wordWidth.W), inVal)
    }
    
    // output signals
    io.resp.valid := RegNext(io.req.valid, false.B)
    io.resp.bits.tag := reqReg.tag
    io.resp.bits.laneMask := reqReg.laneMask
    io.resp.bits.whichBank := reqReg.whichBank
    io.resp.bits.wRow := reqReg.wRow
}
