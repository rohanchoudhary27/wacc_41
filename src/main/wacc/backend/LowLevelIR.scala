package wacc.backend

/* A physical register (placeholder to be removed
   when register allocator is merged) */
case class Register(id: Int, width: Size)

/* Addressing modes */
sealed trait ArmAddress
case class ArmAddrRegOffset(baseAddr: Register, offset: Register) extends  ArmAddress
case class ArmAddrImmOffset(baseAddr: Register, offset: ArmImmediate) extends ArmAddress
case class ArmAddrRegShift(base: Register,
                           offset: Register,
                           shift: ShiftAmounts.ShiftAmount) extends ArmAddress
case class ArmAddrLabel(name: String) extends ArmAddress

/* An immeidate value */
case class ArmImmediate(value: Int)

/* ---------------------------------------------------------------------------------------------- */

sealed trait ArmInstruction

/* Arithmetic and Logic */

/* Register arithmetic instructions */
case class AddReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class SubReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class MulReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class DivReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class LslReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class LsrReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class AsrReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction

/* Immediate arithmetic instructions */
case class AddImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
case class SubImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
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

/* Data Movement*/
case class MovReg(reg: Register, src: Register) extends ArmInstruction
case class MovKeep(reg: Register,
                   src: ArmImmediate,
                   shift: ShiftAmounts.ShiftAmount) extends ArmInstruction
case class ArmLoad(reg: Register, src: ArmAddress) extends ArmInstruction
case class ArmStore(reg: Register, src: ArmAddress) extends ArmInstruction

/* Conditionals */
case class CmpReg(src1: Register, src2: Register) extends ArmInstruction
case class CmpImm(src1: Register, src2: ArmImmediate) extends ArmInstruction
case class ArmBranchUncond(label: ArmAddrLabel) extends ArmInstruction
case class ArmBranchCond(cond: Condition, label: ArmAddrLabel) extends ArmInstruction

/* Functions */
case class Bl(label: ArmAddrLabel) extends ArmInstruction
case object ArmRet extends ArmInstruction

/* Stack operations */
case class PushTwo(src1: Register, src2: Register) extends ArmInstruction
case class PopTwo(dest1: Register, dest2: Register) extends ArmInstruction