package sp26FPUnits

import chisel3._
import chisel3.util._

class E4M3ToBF16 extends Module {
  val io = IO(new Bundle {
    val e4m3In  = Input(UInt(8.W))
    val bf16Out = Output(UInt(16.W))
  })
  val sign = io.e4m3In(7)
  val exp  = io.e4m3In(6, 3)
  val mant = io.e4m3In(2, 0)
  val isZero = (exp === 0.U) && (mant === 0.U)
  val isSub  = (exp === 0.U) && (mant =/= 0.U)
  val isNaN  = (exp === 0xF.U) && (mant === 0x7.U)
  val bf16 = Wire(UInt(16.W))
  when(isZero || isSub || isNaN) {
    bf16 := Cat(sign, 0.U(15.W))
  }.otherwise {
    val bf16Exp  = (exp +& 120.U)(7, 0)
    val bf16Mant = Cat(mant, 0.U(4.W))
    bf16 := Cat(sign, bf16Exp, bf16Mant)
  }
  io.bf16Out := bf16
}
