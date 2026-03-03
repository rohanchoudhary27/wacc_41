package wacc.frontend

import scala.collection.mutable

/**
  * State class for the tail recursive traversal containing the set of
  * `Call` nodes to which tail recursive call nodes are added to
  */
private class TailRecState {

    /* Set of tail recursive call nodes */
    private val tailCalls: mutable.Set[Call[SemanticData]] = mutable.Set.empty

    /** Retrieve immutable set of tail recursive calls */
    def getTailCalls: Set[Call[SemanticData]] = tailCalls.toSet

    /**
      * Add a tail recursive call node to the set of tailrec `Call` nodes
      *
      * @param callNode A tail recursive call node to be added.
      */
    def addTailCall(callNode: Call[SemanticData]): Unit = {
        tailCalls += callNode
    }
}

