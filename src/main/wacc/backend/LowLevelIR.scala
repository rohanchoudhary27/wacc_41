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

