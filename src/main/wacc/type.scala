package wacc

/**
 * Constraints represent expectations imposed by the context in which an 
 * expression is type checked.
 *
 * They are used in a top-down traversal of the AST, where the parent node 
 * imposes a Constraint and the child expression is checked against it.
 */
enum Constraint {
    /* The expression must have a type that matches `t`. Matching is defined 
       by the `~` relation on SemanticType (not strict equality).  */
    case Is(t: SemanticType)      

    /* The expression must have a type that is one of the given set */
    case OneOf(ts: Set[SemanticType])
}

object Constraint {
    /* Means that the expression may have any type. This is typically used
       when the context does not impose a specific expectation and the type 
       should be inferred bottom up (type inference). */
    val Infer = Is(?) 

    /* Constraint requiring the expression to be an array of some element type. */
    val IsArray = Is(KnownType.Array(?))

    /* Constraint for types that may be compared using operators <, <=, >, >= */
    val IsComparable = OneOf(Set(KnownType.Int, KnownType.Char))

    /* Constraint for types that may be compared using == or != */
    val IsEquatable = Is(?)
}

