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

/* An immediate value */
case class ArmImmediate(value: Int)

/* ---------------------------------------------------------------------------------------------- */

sealed trait ArmInstruction

/* Arithmetic and Logic */

/* Register arithmetic instructions */
case class ArmAddReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class ArmSubReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class ArmMulReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class ArmDivReg(reg: Register, src1: Register, src2: Register) extends ArmInstruction
case class ArmNeg(reg: Register) extends ArmInstruction
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
case class ArmMovImm(reg: Register, src: ArmImmediate) extends ArmInstruction
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

/* Alias instructions (real aarch64 but they're convenient wrappers
   over multiple other aarch64 instructions */

/* w1, w2, w3:  dest = w3 - (w1 * w2)
   `MSub` makes the mod operator much easier */
case class ArmMSub(dest: Register, mult1: Register,
                   mult2: Register,
                   sub: Register) extends ArmInstruction

/* Sets the value of the condition flag `cond` to the destination register */
case class ArmCSet(dest: Register, cond: Condition) extends ArmInstruction

/* Easily check for overflow on multiplication as it is "multiply long" meaning
   we can check the top 32-bits of the 64-bit result and if it's not zero, then there
   was overflow as WACC uses 32-bit ints.
   
   The S stands for sign-extend so it's going to a bigger result size instead of truncating.
*/
case class ArmSMull(dest: Register, src1: Register, src2: Register) extends ArmInstruction

/* Sign extend a word to promote a number from a 32-bit regster
   to a 64-bit register */
case class ArmSxtw(dest: Register, src: Register) extends ArmInstruction

/* Branch if the test register is equal to 0 (prevents cmp + branch) */
case class ArmCbz(reg: Register, label: ArmAddrLabel) extends ArmInstruction  

/* Branch if the test register is not equal to 0 (prevents cmp + branch) */
case class ArmCbnz(reg: Register, label: ArmAddrLabel) extends ArmInstruction 

/* ---------------------------------------------------------------------------------------------- */

case class ArmDataDirective(label: ArmAddrLabel, storedValue: String)
case class ArmProgram(data: List[ArmDataDirective], body: List[ArmInstruction])