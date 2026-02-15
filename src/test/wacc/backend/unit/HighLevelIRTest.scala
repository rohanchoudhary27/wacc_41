package wacc.backend.unit

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import wacc.backend.*

/**
* Unit tests for the high level internal representation
*/
class HighLevelIRTest extends AnyFlatSpec with Matchers {

    "The High level IR should" should "validate register size on binary operations" in {
        val reg32 = VirtualReg(1, Size.W)
        val reg64 = VirtualReg(2, Size.X)
        val imm = Immediate(5)

        /* dest, src1 mismatch */
        an [IllegalArgumentException] should be thrownBy {
            Sub(reg64, reg32, imm) 
        }

        /* src1, src2 mismatch */
        an [IllegalArgumentException] should be thrownBy {
            Sub(reg64, reg64, reg32) 
        }

        noException should be thrownBy {
            Sub(reg64, reg64, imm)
        }

    }

    it should "validate register size on unary operations" in {
        val reg32 = VirtualReg(1, Size.W)
        val reg64 = VirtualReg(2, Size.X)

        /* Mismatch size */
        an [IllegalArgumentException] should be thrownBy {
            Not(reg64, reg32) 
        }

        /* Mismatch size */
        an [IllegalArgumentException] should be thrownBy {
            Not(reg32, reg64)
        }

        noException should be thrownBy {
            Not(reg32, reg32)
        }
    }

    it should "validate register size on pointer operations" in {
        val reg32 = VirtualReg(1, Size.W)
        val reg64 = VirtualReg(2, Size.X)

        /* Pointer can't be 32 bit */
        an [IllegalArgumentException] should be thrownBy {
            PairLoad(reg32, PairPos.Fst, reg32)
        }

        noException should be thrownBy {
            PairStore(reg64, PairPos.Snd, reg64)
        }
    }

    it should "validate array index size on array operations" in {
        val reg32 = VirtualReg(1, Size.W)
        val reg64 = VirtualReg(2, Size.X)

        /* Index must be 32 bit */
        an [IllegalArgumentException] should be thrownBy {
            ArrayLoad(reg64, reg64, reg32)
        }

        noException should be thrownBy {
            ArrayLoad(reg64, reg32, reg32)
        }
    }

    it should "validate register size on comparison operations" in {
        val reg32 = VirtualReg(1, Size.W)
        val reg64 = VirtualReg(2, Size.X)

        /* two registers must be same size */
        an [IllegalArgumentException] should be thrownBy {
            Cmp(reg32, reg64)
        }

        noException should be thrownBy {
            Cmp(reg32, reg32)
        }
    }

    it should "validate address base register size" in {
        val reg32 = VirtualReg(1, Size.W)
        val reg64 = VirtualReg(2, Size.X)
        val imm = Immediate(4)

        /* Base register must be 64 bit*/
        an [IllegalArgumentException] should be thrownBy {
            AddrImmOffset(reg32, imm)
        }

        noException should be thrownBy {
            AddrRegOffset(reg64, reg32)
        }
    }
}
