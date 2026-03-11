package wacc.frontend.lsa

import wacc.frontend.*
import wacc.frontend.lsa.*

/* 
 * The analysis state is an immutable (because then handling control flow joins is 
 * easier instead of copying the map to 2 different states if we had mutable maps) map
 * from variable identifiers to their abstract values at a given program point.
 */

/** 
  * A unique identifier for a variable in the program.
  * Mirrors the renamer's output.
  *
  * @param name The variable name
  * @param renameId The unique id assigned by the renamer
  */
case class VariableId(name: String, renameId: Int)

object VariableId {

    /** Turn `Ident` AST node into `VariableId`
      *
      * @param ident A renamed identifier node
      * @return The corresponding `VariableId`
      */
    def fromIdent(ident: Ident[SemanticData]): Option[VariableId] =
        ident.info match {
            case SemanticData.Var(_, renameId, _) =>
                Some(VariableId(ident.name, renameId))
            case _ => None
        }
}

/** The abstract state at a program point.
  *
  * Maps each tracked variable to its abstract value. Variables not
  * present in the map are `Unknown`.
  *
  * The state is immutable and all mutation operations return a new state.
  * This is essential for correctly handling branching control flow where
  * we need independent copies of the state for each branch.
  *
  * @param bindings The current variable to abstract value mappings
  * @param unreachable True if this program point is provably unreachable (e.g. after
  *                    an `exit` or `return` statement).
  */
case class AnalysisState(
    private val bindings: Map[VariableId, AbstractValue] = Map.empty,
    val unreachable: Boolean = false
) {

    /** Look up the abstract value of a variable.
      *
      * Returns `Unknown` for variables not currently tracked, which is
      * safe because `Unknown` imposes no assumptions
      *
      * @param varId The variable to look up
      * @return The abstract value, or `Unknown` if not tracked
      */
    def lookup(varId: VariableId): AbstractValue = bindings.getOrElse(varId, Unknown)

    /** Update a variable's abstract value, returning a new state.
      *
      * If the new value is `Unknown`, we still store it explicitly so that
      * the keys reflects all variables that have been encountered
      *
      * @param varId The variable to update
      * @param value The new abstract value
      * @return A new state with the updated binding
      */
    def update(varId: VariableId, value: AbstractValue): AnalysisState = 
        copy(bindings = bindings.updated(varId, value))

    /** Invalidate a variable (set it to `Unknown`)
      * Used when a variable's value becomes unknowable
      *
      * @param varId The variable to invalidate
      * @return A new state with the variable set to `Unknown`
      */
    def invalidate(varId: VariableId): AnalysisState = update(varId, Unknown)

    /** Invalidate all the given variables at once.
      *
      * @param varIds The set of variables to invalidate
      * @return A new state with all specified variables set to `Unknown`
      */
    def invalidateAll(varIds: Set[VariableId]): AnalysisState =
        varIds.foldLeft(this)((state, varId) => state.invalidate(varId))

    /** Merge this state with another at a control flow join.
      *
      * For each variable present in either state, the abstract values are
      * joined. If a variable is present in one state but not the other, the
      * missing side is treated as `Unknown`.
      *
      * @param other The state from the other control flow branch
      * @return The joined state
      */
    def join(other: AnalysisState): AnalysisState = {
        if (this.unreachable) other
        else if (other.unreachable) this
        else {
            val allKeys = this.bindings.keySet ++ other.bindings.keySet

            val mergedBindings = allKeys.map { varId =>
                val thisVal = this.lookup(varId)
                val otherVal = other.lookup(varId)
                varId -> thisVal.join(otherVal)
            }.toMap

            AnalysisState(mergedBindings)
        }
    }

    /** Remove variables from the state that are not in the given scope.
      *
      * Used when leaving a `begin...end` block to clean up variables
      * declared inside the block.
      *
      * @param keepIds The set of variable ids that should be retained
      * @return A new state containing only the retained variables
      */
    def restrictTo(keepIds: Set[VariableId]): AnalysisState =
        copy(bindings = bindings.view.filterKeys(keepIds.contains).toMap)

    /** Mark this state as unreachable */
    def markUnreachable: AnalysisState =
        copy(unreachable = true)

    /** Get all variable ids currently tracked in this state */
    def trackedVariables: Set[VariableId] = bindings.keySet

    /** Check whether a specific variable is being tracked */
    def isTracked(varId: VariableId): Boolean = bindings.contains(varId)
}

object AnalysisState {

    /** The initial empty state */
    val empty: AnalysisState = AnalysisState()
}