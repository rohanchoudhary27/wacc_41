package wacc.backend

enum RegWidth {
  case B
  case W
  case X

  override def toString: String = this match {
    case B => "b"
    case W => "w"
    case X => "x"
  }
}

case class Register(id: Int, width: RegWidth)

object ShiftAmounts {
  opaque type ShiftAmount = Int
  val _0: ShiftAmount = 0
  val _16: ShiftAmount = 16
  val _32: ShiftAmount = 32 
  val _48: ShiftAmount = 48

  def value(shift: ShiftAmount): Int = shift
}

sealed trait ArmAddress
case class ArmAddrRegOffset(baseAddr: Register, offset: Register) extends  ArmAddress
case class ArmAddrImmOffset(baseAddr: Register, offset: ArmImmediate) extends ArmAddress
case class ArmAddrLabel(name: String) extends ArmAddress
case class ArmAddrStack(offset: Int) extends ArmAddress

case class ArmImmediate(value: Int)

/* ---------------------------------------------------------------------------------------------- */

sealed trait ArmInstruction

/* Arithmetic and Logic */

/* Register arithmetic instructions */
case class AddReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class SubReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class MulReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class DivReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class ModReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class LslReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class LsrReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class AsrReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction

/* Immediate arithmetic instructions */
case class AddImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
case class SubImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
case class ModImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
case class LslImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
case class LsrImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
case class AsrImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction

/* Register logical instructions */
case class OrReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class AndReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class NotReg(reg: Register, src: Register) extends ArmInstruction

/* Immediate logical instructions */
case class OrImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
case class AndImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
case class NotImm(reg: Register, src: ArmImmediate) extends ArmInstruction

/* Data Movement*/
case class MovReg(reg: Register, src: Register) extends ArmInstruction
case class MovKeep(reg: Register,
                   src: ArmImmediate,
                   shift: ShiftAmounts.ShiftAmount) extends ArmInstruction

case class ArmLoad(reg: Register, src: ArmAddress) extends ArmInstruction
case class ArmStore(reg: Register, src: ArmAddress) extends ArmInstruction

