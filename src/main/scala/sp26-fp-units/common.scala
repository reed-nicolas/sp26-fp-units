package sp26FPUnits

import chisel3._
import sp26FPUnits.hardfloat._

final case class AtlasFPType(exp: Int, sig: Int) {

  /** IEEE width: sign + exp + fraction(sig-1) */
  val ieeeWidth: Int = 1 + exp + (sig - 1)

  /** HardFloat recoded width */
  val recodedWidth: Int = 1 + exp + sig + 1

  def recode(x: UInt) = recFNFromFN(exp, sig, x)
}

object AtlasFPType {
  // handy common presets (optional)
  val E4M3  = AtlasFPType(exp = 4,  sig = 4)   // if you treat sig as (1+mantissa)
  val E5M2  = AtlasFPType(exp = 5,  sig = 3)
  val E5M3  = AtlasFPType(exp = 5,  sig = 4)
  val BF16      = AtlasFPType(exp = 8,  sig = 8)
  val FP32      = AtlasFPType(exp = 8,  sig = 24)
  val FP64      = AtlasFPType(exp = 11, sig = 53)
}