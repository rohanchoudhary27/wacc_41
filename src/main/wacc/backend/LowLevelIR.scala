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
case class ArmAddReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class ArmSubReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class ArmMulReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class ArmDivReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class ArmLslReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class ArmLsrReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class ArmAsrReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction

/* Immediate arithmetic instructions */
case class ArmAddImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
case class ArmSubImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
case class ArmLslImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
case class ArmLsrImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
case class ArmAsrImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction

/* Register logical instructions */
case class ArmOrReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class ArmAndReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class ArmNotReg(reg: Register, src: Register) extends ArmInstruction

/* Immediate logical instructions */
case class ArmOrImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction
case class ArmAndImm(reg: Register, src1: Register, src2: ArmImmediate) extends ArmInstruction

/* Data Movement*/
case class ArmMovReg(reg: Register, src: Register) extends ArmInstruction
case class ArmMovKeep(reg: Register,
                   src: ArmImmediate,
                   shift: ShiftAmounts.ShiftAmount) extends ArmInstruction
case class ArmLoad(reg: Register, src: ArmAddress) extends ArmInstruction
case class ArmStore(reg: Register, src: ArmAddress) extends ArmInstruction
case class ArmLea(destReg: Register, label: ArmAddrLabel) extends ArmInstruction

/* Conditionals */
case class ArmCmpReg(src1: Register, src2: Register) extends ArmInstruction
case class ArmCmpImm(src1: Register, src2: ArmImmediate) extends ArmInstruction
case class ArmBranchUncond(label: ArmAddrLabel) extends ArmInstruction
case class ArmBranchCond(cond: Condition, label: ArmAddrLabel) extends ArmInstruction
case class ArmLabelDef(label: ArmAddrLabel) extends ArmInstruction

/* Functions */
case class ArmBl(label: ArmAddrLabel) extends ArmInstruction
case object ArmRet extends ArmInstruction

/* Stack operations */
case class ArmPushTwo(src1: Register, src2: Register) extends ArmInstruction
case class ArmPopTwo(dest1: Register, dest2: Register) extends ArmInstruction

/* ---------------------------------------------------------------------------------------------- */

case class ArmDataDirective(label: ArmAddrLabel, storedValue: String)
case class ArmProgram(data: List[ArmDataDirective], body: List[ArmInstruction])