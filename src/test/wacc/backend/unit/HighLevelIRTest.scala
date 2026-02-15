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

        an [IllegalArgumentException] should be thrownBy {
            Sub(reg64, reg32, imm) 
        }

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

        an [IllegalArgumentException] should be thrownBy {
            Not(reg64, reg32) 
        }

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

        an [IllegalArgumentException] should be thrownBy {
            ArrayLoad(reg32, reg32, reg32)
        }

        noException should be thrownBy {
            ArrayStore(reg64, reg32, reg64)
        }
    }

    it should "validate register size on comparison operations" in {
        val reg32 = VirtualReg(1, Size.W)
        val reg64 = VirtualReg(2, Size.X)

        an [IllegalArgumentException] should be thrownBy {
            Cmp(reg32, reg64)
        }

        noException should be thrownBy {
            Cmp(reg32, reg32)
        }
    }
}
