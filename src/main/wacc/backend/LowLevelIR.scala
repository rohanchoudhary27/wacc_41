package wacc.backend

/* Addressing modes */
sealed trait ArmAddress
case class ArmAddrRegOffset(baseAddr: PhysicalReg, offset: PhysicalReg) extends  ArmAddress
case class ArmAddrImmOffset(baseAddr: PhysicalReg, offset: ArmImmediate) extends ArmAddress
case class ArmAddrRegShift(base: PhysicalReg,
                           offset: PhysicalReg,
                           shift: ShiftAmounts.ShiftAmount) extends ArmAddress
case class ArmAddrLabel(name: String) extends ArmAddress

/* An immediate value */
case class ArmImmediate(value: Int)

/* ---------------------------------------------------------------------------------------------- */

sealed trait ArmInstruction

/* Arithmetic and Logic */

/* PhysicalReg arithmetic instructions */
case class ArmAddReg(reg: PhysicalReg, src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction
case class ArmSubReg(reg: PhysicalReg, src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction
case class ArmMulReg(reg: PhysicalReg, src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction
case class ArmDivReg(reg: PhysicalReg, src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction
case class ArmNeg(reg: PhysicalReg) extends ArmInstruction
case class ArmLslReg(reg: PhysicalReg, src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction
case class ArmLsrReg(reg: PhysicalReg, src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction
case class ArmAsrReg(reg: PhysicalReg, src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction

/* Immediate arithmetic instructions */
case class ArmAddImm(reg: PhysicalReg, src1: PhysicalReg, src2: ArmImmediate) extends ArmInstruction
case class ArmSubImm(reg: PhysicalReg, src1: PhysicalReg, src2: ArmImmediate) extends ArmInstruction
case class ArmLslImm(reg: PhysicalReg, src1: PhysicalReg, src2: ArmImmediate) extends ArmInstruction
case class ArmLsrImm(reg: PhysicalReg, src1: PhysicalReg, src2: ArmImmediate) extends ArmInstruction
case class ArmAsrImm(reg: PhysicalReg, src1: PhysicalReg, src2: ArmImmediate) extends ArmInstruction

/* Register logical instructions */
case class ArmOrReg(reg: PhysicalReg, src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction
case class ArmAndReg(reg: PhysicalReg, src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction
case class ArmNotReg(reg: PhysicalReg, src: PhysicalReg) extends ArmInstruction

/* Immediate logical instructions */
case class ArmOrImm(reg: PhysicalReg, src1: PhysicalReg, src2: ArmImmediate) extends ArmInstruction
case class ArmAndImm(reg: PhysicalReg, src1: PhysicalReg, src2: ArmImmediate) extends ArmInstruction

/* Data Movement*/
case class ArmMovReg(reg: PhysicalReg, src: PhysicalReg) extends ArmInstruction
case class ArmMovImm(reg: PhysicalReg, src: ArmImmediate) extends ArmInstruction
case class ArmMovKeep(reg: PhysicalReg,
                   src: ArmImmediate,
                   shift: ShiftAmounts.ShiftAmount) extends ArmInstruction
case class ArmLoad(reg: PhysicalReg, src: ArmAddress) extends ArmInstruction
case class ArmStore(reg: PhysicalReg, src: ArmAddress) extends ArmInstruction
case class ArmLea(destReg: PhysicalReg, label: ArmAddrLabel) extends ArmInstruction

/* Conditionals */
case class ArmCmpReg(src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction
case class ArmCmpImm(src1: PhysicalReg, src2: ArmImmediate) extends ArmInstruction
case class ArmBranchUncond(label: ArmAddrLabel) extends ArmInstruction
case class ArmBranchCond(cond: Condition, label: ArmAddrLabel) extends ArmInstruction
case class ArmLabelDef(label: ArmAddrLabel) extends ArmInstruction

/* Functions */
case class ArmBl(label: ArmAddrLabel) extends ArmInstruction
case object ArmRet extends ArmInstruction

/* Stack operations */
case class ArmPushTwo(src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction
case class ArmPopTwo(dest1: PhysicalReg, dest2: PhysicalReg) extends ArmInstruction

/* Alias instructions (real aarch64 but they're convenient wrappers
   over multiple other aarch64 instructions */

/* w1, w2, w3:  dest = w3 - (w1 * w2)
   `MSub` makes the mod operator much easier */
case class ArmMSub(dest: PhysicalReg, mult1: PhysicalReg,
                   mult2: PhysicalReg,
                   sub: PhysicalReg) extends ArmInstruction

/* Sets the value of the condition flag `cond` to the destination PhysicalReg */
case class ArmCSet(dest: PhysicalReg, cond: Condition) extends ArmInstruction

/* Easily check for overflow on multiplication as it is "multiply long" meaning
   we can check the top 32-bits of the 64-bit result and if it's not zero, then there
   was overflow as WACC uses 32-bit ints.
   
   The S stands for sign-extend so it's going to a bigger result size instead of truncating.
*/
case class ArmSMull(dest: PhysicalReg, src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction

/* Sign extend a word to promote a number from a 32-bit regster
   to a 64-bit register */
case class ArmSxtw(dest: PhysicalReg, src: PhysicalReg) extends ArmInstruction

/* Branch if the test register is equal to 0 (prevents cmp + branch) */
case class ArmCbz(reg: PhysicalReg, label: ArmAddrLabel) extends ArmInstruction  

/* Branch if the test register is not equal to 0 (prevents cmp + branch) */
case class ArmCbnz(reg: PhysicalReg, label: ArmAddrLabel) extends ArmInstruction 

/* ---------------------------------------------------------------------------------------------- */

case class ArmDataDirective(label: ArmAddrLabel, storedValue: String)
case class ArmProgram(data: List[ArmDataDirective], body: List[ArmInstruction])