package wacc

import scala.collection.mutable

/**
* Scope management for the renamer.
* 
* Stores the current scope as a mutable map and all parent scopes
* flattened into an immutable map for O(1) lookups.
*
* @param curScope: Mutable map for variables declared in the current scope
* @param parentScope: Immutable map containing all parent scope bindings
*/
final class RenamerScope(
    val curScope: mutable.Map[String, SemanticData.Var],
    parentScope: Map[String, SemanticData.Var]
) {
    
    /**
    * Looks up a variable name in both the current and parent scopes.
    *
    * @param name: The variable name to look up
    * @return: Some(SemanticData.Var) if found, None otherwise
    */
    def lookup(name: String): Option[SemanticData.Var] = {
        curScope.get(name).orElse(parentScope.get(name))
    }

    /**
    * Adds a variable to the current scope.
    *
    * @param name: The variable name
    * @param info: The variable's semantic data
    * @return: true if successfully added, false if name already exists in current scope
    */
    def set(name: String, info: SemanticData.Var): Boolean = {
        if (curScope.contains(name)) false
        else {
            curScope(name) = info
            true
        }
    }
    
    /**
    * Creates a new child scope, flattening the current scope into the parent.
    *
    * @return: A new RenamerScope with an empty current scope and flattened parents
    */
    def newScope: RenamerScope = {
        RenamerScope(mutable.Map.empty, parentScope ++ curScope)
    }
}

object RenamerScope {
    /** Creates an initial empty scope with no parents */
    def empty: RenamerScope = RenamerScope(mutable.Map.empty, Map.empty)
}
