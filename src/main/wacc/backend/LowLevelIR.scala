package wacc.backend

/* Addressing modes */
sealed trait ArmAddress
case class ArmAddrRegOffset(baseAddr: PhysicalReg, offset: PhysicalReg) extends  ArmAddress
case class ArmAddrImmOffset(baseAddr: PhysicalReg, offset: ArmImmediate) extends ArmAddress
case class ArmAddrRegShift(base: PhysicalReg,
                           offset: PhysicalReg,
                           shift: ShiftAmounts.ShiftAmount) extends ArmAddress
case class ArmAddrPreIndex(baseAddr: PhysicalReg,
                           offset: ArmImmediate) extends ArmAddress /* E.g. [sp, #-16]! */
case class ArmAddrPostIndex(baseAddr: PhysicalReg,
                           offset: ArmImmediate) extends ArmAddress /* E.g. [sp], #16 */
case class ArmAddrLabel(name: String) extends ArmAddress

/* An immediate value */
case class ArmImmediate(value: Int)

/* ---------------------------------------------------------------------------------------------- */

sealed trait ArmInstruction

/* Arithmetic and Logic */

/* PhysicalReg arithmetic instructions */
case class ArmAddReg(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg,
                     src2: PhysicalReg) extends ArmInstruction
case class ArmSubReg(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg,
                     src2: PhysicalReg) extends ArmInstruction
case class ArmMulReg(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg,
                     src2: PhysicalReg) extends ArmInstruction
case class ArmDivReg(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg,
                     src2: PhysicalReg) extends ArmInstruction
case class ArmNeg(size: Size,
                  reg: PhysicalReg,
                  src: PhysicalReg) extends ArmInstruction
case class ArmLslReg(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg,
                     src2: PhysicalReg) extends ArmInstruction
case class ArmLsrReg(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg,
                     src2: PhysicalReg) extends ArmInstruction
case class ArmAsrReg(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg,
                     src2: PhysicalReg) extends ArmInstruction

/* Immediate arithmetic instructions */
case class ArmAddImm(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg,
                     src2: ArmImmediate) extends ArmInstruction
case class ArmSubImm(size: Size,
                     reg: PhysicalReg,
                    src1: PhysicalReg,
                    src2: ArmImmediate) extends ArmInstruction
case class ArmLslImm(size: Size,
                    reg: PhysicalReg,
                    src1: PhysicalReg,
                    src2: ArmImmediate) extends ArmInstruction
case class ArmLsrImm(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg, src2: ArmImmediate) extends ArmInstruction
case class ArmAsrImm(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg,
                     src2: ArmImmediate) extends ArmInstruction

/* Register logical instructions */
case class ArmOrReg(size: Size,
                    reg: PhysicalReg,
                    src1: PhysicalReg,
                    src2: PhysicalReg) extends ArmInstruction
case class ArmAndReg(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg,
                     src2: PhysicalReg) extends ArmInstruction

/* Immediate logical instructions */
case class ArmOrImm(size: Size,
                    reg: PhysicalReg,
                    src1: PhysicalReg,
                    src2: ArmImmediate) extends ArmInstruction
case class ArmAndImm(size: Size,
                    reg: PhysicalReg,
                    src1: PhysicalReg,
                    src2: ArmImmediate) extends ArmInstruction

/* Data Movement*/
case class ArmMovReg(size: Size, reg: PhysicalReg, src: PhysicalReg) extends ArmInstruction
case class ArmMovImm(size: Size, reg: PhysicalReg, src: ArmImmediate) extends ArmInstruction
case class ArmMovKeep(size: Size, reg: PhysicalReg,
                   src: ArmImmediate,
                   shift: ShiftAmounts.ShiftAmount) extends ArmInstruction
case class ArmLoad(size: Size, reg: PhysicalReg, src: ArmAddress) extends ArmInstruction
case class ArmStore(size: Size, reg: PhysicalReg, src: ArmAddress) extends ArmInstruction
case class ArmLdur(size: Size, reg: PhysicalReg, src: ArmAddress) extends ArmInstruction
case class ArmStur(size: Size, reg: PhysicalReg, src: ArmAddress) extends ArmInstruction
case class ArmLea(destReg: PhysicalReg, label: ArmAddrLabel) extends ArmInstruction

/* Conditionals */
case class ArmCmpReg(size: Size, src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction
case class ArmCmpImm(size: Size, src1: PhysicalReg, src2: ArmImmediate) extends ArmInstruction
case class ArmBranchUncond(label: ArmAddrLabel) extends ArmInstruction
case class ArmBranchCond(cond: Condition, label: ArmAddrLabel) extends ArmInstruction
case class ArmLabelDef(label: ArmAddrLabel) extends ArmInstruction

/* Functions */
case class ArmBl(label: ArmAddrLabel) extends ArmInstruction
case object ArmRet extends ArmInstruction

/* Stack operations */
case class ArmStorePair(size: Size, 
                        src1: PhysicalReg, 
                        src2: PhysicalReg, 
                        addr: ArmAddress) extends ArmInstruction

case class ArmLoadPair(size: Size, 
                       dest1: PhysicalReg, 
                       dest2: PhysicalReg, 
                       addr: ArmAddress) extends ArmInstruction

/* Alias instructions (real aarch64 but they're convenient wrappers
   over multiple other aarch64 instructions */

/* w1, w2, w3:  dest = w3 - (w1 * w2)
   `MSub` makes the mod operator much easier */
case class ArmMSub(size: Size, dest: PhysicalReg, mult1: PhysicalReg,
                   mult2: PhysicalReg,
                   sub: PhysicalReg) extends ArmInstruction

/* Sets the value of the condition flag `cond` to the destination PhysicalReg */
case class ArmCSet(size: Size, dest: PhysicalReg, cond: Condition) extends ArmInstruction

/* Easily check for overflow on multiplication as it is "multiply long" meaning
   we can check the top 32-bits of the 64-bit result and if it's not zero, then there
   was overflow as WACC uses 32-bit ints.
   
   The S stands for sign-extend so it's going to a bigger result size instead of truncating.

   It's always 32 bit regs in and 64 bit reg out by design so `Size` is not needed
*/
case class ArmSMull(dest: PhysicalReg, src1: PhysicalReg, src2: PhysicalReg) extends ArmInstruction

/* Sign extend a word to promote a number from a 32-bit regster
   to a 64-bit register

   Always 32 bit to 64 bit so no `Size` needed
*/
case class ArmSxtw(dest: PhysicalReg, src: PhysicalReg) extends ArmInstruction

/* Branch if the test register is equal to 0 (prevents cmp + branch) */
case class ArmCbz(size: Size, reg: PhysicalReg, label: ArmAddrLabel) extends ArmInstruction  

/* Branch if the test register is not equal to 0 (prevents cmp + branch) */
case class ArmCbnz(size: Size, reg: PhysicalReg, label: ArmAddrLabel) extends ArmInstruction 

/* ---------------------------------------------------------------------------------------------- */

case class ArmDataDirective(label: ArmAddrLabel, storedValue: String)
case class ArmProgram(data: List[ArmDataDirective], body: List[ArmInstruction])