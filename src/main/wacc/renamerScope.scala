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
    private val curScope: mutable.Map[String, SemanticData.Var],
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
    * Checks if a variable name exists in the current scope.
    *
    * @param name: The variable name to check
    * @return: true if the name exists in the current scope
    */
    def contains(name: String): Boolean = curScope.contains(name)

    /**
    * Adds a variable to the current scope.
    *
    * @param name: The variable name
    * @param info: The variable's semantic data
    */
    def add(name: String, info: SemanticData.Var): Unit = {
        curScope(name) = info
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
