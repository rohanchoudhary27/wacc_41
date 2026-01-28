package wacc

import scala.collection.mutable.Map
import scala.annotation.tailrec

/**
* Linked list of maps(name, SymbolInfo).
* Each renamer scope forms a link with a pointer to the previous scope.
*
* @param prev: pointer to previous scope. The global scope has None as prev.
*/
final class RenamerScope(val prev: Option[RenamerScope]) {
    
    /**
    * The hashmap for the current scope
    */
    private val mapForScope = Map[String, SemanticData.Var]()

    /**
    * Takes a key value pair and adds to the map for current scope.
    * If the key is in the scope return false else adds the pair and returns true.
    *
    * @param name: The name of the key.
    * @param info: The meta data.
    *
    * @return: true if successfully added to scope, false if key already in map for current scope.
    */
    def set(name: String, info: SemanticData.Var): Boolean = {
        if (mapForScope.contains(name)) false
        else {
            mapForScope += (name -> info)
            true
        }
    }

    /**
    * Use tail rec to identify if the key is in the linked list of RenamerScope.
    *
    * @param name: The name of the key to find in the linked list.
    *
    * @return Option[SymbolInfo]: Returns Some(SymbolInfo) if the key is in the linked list, else None.
    */
    @tailrec
    def get(name: String): Option[SemanticData.Var] = {
        mapForScope.get(name) match {
            case Some(info) => Some(info)
            case None => prev match {
                case Some(p) => p.get(name)
                case None => None
            }
        }
    }
}

/**
* Extension method for RenamerScope to create child scopes
*/
extension (scope: RenamerScope) {
  def newScope: RenamerScope = new RenamerScope(Some(scope))
}
