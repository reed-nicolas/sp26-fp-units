/*
Lossless E4M3 x E4M3 multiplier.

Input:  two 8-bit OCP E4M3 — S(1) E(4, bias=7) M(3)
Output: 13-bit custom float — S(1) E(5, bias=13) M(7)

The 13-bit format is the minimum width that preserves every product
bit of two normal E4M3 significands.

NaN and subnormals silently produce garbage with no architectural side-effects.
*/

package sp26FPUnits

import chisel3._
import chisel3.util._

class E4M3Mul extends Module {
  val io = IO(new Bundle {
    val a   = Input(UInt(8.W))
    val b   = Input(UInt(8.W))
    val out = Output(UInt(13.W))
  })

  // field extraction
  val aSign = io.a(7)
  val aExp  = io.a(6, 3)
  val aMan  = io.a(2, 0)

  val bSign = io.b(7)
  val bExp  = io.b(6, 3)
  val bMan  = io.b(2, 0)

//   // input-class assertions
//   assert(!(aExp === 15.U && aMan === 7.U), "E4M3Mul: input a is NaN")
//   assert(!(bExp === 15.U && bMan === 7.U), "E4M3Mul: input b is NaN")
//   assert(!(aExp === 0.U  && aMan =/= 0.U), "E4M3Mul: input a is subnormal")
//   assert(!(bExp === 0.U  && bMan =/= 0.U), "E4M3Mul: input b is subnormal")

  // zero detect
  val aZero  = aExp === 0.U
  val bZero  = bExp === 0.U
  val isZero = aZero || bZero

  // sign
  val outSign = aSign ^ bSign

  // significand multiply
  val aSig = Mux(isZero, 0.U(4.W), Cat(1.U(1.W), aMan))
  val bSig = Mux(isZero, 0.U(4.W), Cat(1.U(1.W), bMan))
  val prodSig = aSig * bSig   // 8 bits

  // normalization
  val needShift = prodSig(7)

  val outMan = Mux(needShift,
    prodSig(6, 0),
    Cat(prodSig(5, 0), 0.U(1.W)))

  // exponent
  // biased_out = aExp + bExp − 1 + needShift
  val outExp = (aExp +& bExp) +& needShift - 1.U

  // pack
  io.out := Mux(isZero,
    Cat(outSign, 0.U(12.W)),
    Cat(outSign, outExp(4, 0), outMan))
}
