package sp26FPUnits

import chisel3._
import chisel3.util._
import sp26FPUnits.hardfloat._


/**
  * 4-input MAC-tree style unit:
  *   out = (in0 * in1) * in2 + in3
  *
  * - Inputs are IEEE bitpatterns (width = inT.ieeeWidth)
  * - Output is RECoded HardFloat bitpattern (width = outT.recodedWidth)
  * - If any input AtlasFPType is < 8b, asserts.
  * - If any input AtlasFPType is exactly 8b (fp8), it uses fp8ToE5M3(...) then recodes as E5M3.
  
  **/


class GenericMulMulAddRecFN(in0T: AtlasFPType, in1T: AtlasFPType, in2T: AtlasFPType, in3T: AtlasFPType, outT: AtlasFPType, pipeStagesMul: Int = 0, pipeStagesFma: Int = 0) extends Module {
    
    require(in0T.ieeeWidth >= 8 && in1T.ieeeWidth >= 8 && in2T.ieeeWidth >= 8 && in3T.ieeeWidth >= 8, "All input AtlasFPTypes must be >= 8 bits (IEEE width).")

    val io = IO(new Bundle {
        val validIn = Input(Bool())
        val altfmt  = Input(Bool()) // only meaningful if any input is fp8 (8-bit)

        val in0 = Input(UInt(in0T.ieeeWidth.W))
        val in1 = Input(UInt(in1T.ieeeWidth.W))
        val in2 = Input(UInt(in2T.ieeeWidth.W))
        val in3 = Input(UInt(in3T.ieeeWidth.W))

        val validOut = Output(Bool())
        val out      = Output(UInt(outT.recodedWidth.W)) // recoded
    })

    private def recodeInput(bits: UInt, t: AtlasFPType) : UInt = {
        if (t.ieeeWidth == 8) {
            AtlasFPType.E5M3.recode(fp8ToE5M3(bits, io.altfmt))
        } else {
            t.recode(bits)
        }
    }
    
    private def widen(in: UInt, inT: AtlasFPType, outT: AtlasFPType, active: Bool): UInt = {
        if (inT.exp == outT.exp && inT.sig == outT.sig) {
            Mux(active, in, 0.U(outT.recodedWidth.W))
        } else {
            val widen = Module(new RecFNToRecFN(inT.exp, inT.sig, outT.exp, outT.sig))
            widen.io.in := Mux(active, in, 0.U)
            widen.io.roundingMode := consts.round_near_even
            widen.io.detectTininess := consts.tininess_afterRounding
            widen.io.out
        }
    }

    // ---- Recode inputs (IEEE -> recoded) ----
    val in0Rec = recodeInput(io.in0, in0T)
    val in1Rec = recodeInput(io.in1, in1T)
    val in2Rec = recodeInput(io.in2, in2T)
    val in3Rec = recodeInput(io.in3, in3T)

    // Widen all operands into out type
    def widenFromActualType(inRec: UInt, originalT: AtlasFPType, active: Bool): UInt = {
        val actualT = if (originalT.ieeeWidth == 8) AtlasFPType.E5M3 else originalT
        widen(inRec, actualT, outT, active)
    }

    val aW = widenFromActualType(in0Rec, in0T, io.validIn)
    val bW = widenFromActualType(in1Rec, in1T, io.validIn)
    val cW = widenFromActualType(in2Rec, in2T, io.validIn)
    val dW = widenFromActualType(in3Rec, in3T, io.validIn)

    // TODO (nicolas): Replace these single cycle Modules with MulAddRecPipes

    // ---- Stage 1: mul = in0 * in1 (FMA with c=0) ----
    val mul = Module(new MulAddRecFN(outT.exp, outT.sig))
    mul.io.op := 0.U // FMA op
    mul.io.roundingMode := consts.round_near_even
    mul.io.detectTininess := consts.tininess_afterRounding
    mul.io.a := aW
    mul.io.b := bW
    mul.io.c := 0.U(outT.recodedWidth.W)

    // ---- Stage 2: out = (mul) * in2 + in3 ----
    val fma = Module(new MulAddRecFN(outT.exp, outT.sig))
    fma.io.op := 0.U // FMA op
    fma.io.roundingMode := consts.round_near_even
    fma.io.detectTininess := consts.tininess_afterRounding
    fma.io.a := mul.io.out
    fma.io.b := cW
    fma.io.c := dW

    io.validOut := io.validIn
    io.out := fma.io.out

}