# TheWaccLanguage Specification

### Second Year Computing Laboratory

### Department of Computing

### Imperial College London

## What isWacc?

Wacc(pronounced “whack”) is a simple variant on the While family of languages encountered in many
program reasoning/verification courses (in particular in the Models of Computation course taught to
our 2nd year undergraduates). It features all of the common language constructs you would expect of a
While-like language, such as program variables, simple expressions, conditional branching, looping and
no-ops. It also features a rich set of extra constructs, such as simple types, functions, arrays and basic
tuple creation on the heap.
TheWacclanguage is intended to help unify the material taught in our more theoretical courses
(such as Models of Computation) with the material taught in our more practical courses (such as
Compilers). The core of the language should be simple enough to reason about and the extensions
should pose some interesting challenges and design choices for anyone implementing it.
This specification is carefully worded to ensure that the contents are non-ambiguous: ambiguity is
the worst-enemy of a language designer. While there may be undefined behaviour in the language, the
following keywords are given specific meanings that should be adhered to carefully throughout the
implementation.

1. MUST This word, or the terms “REQUIRED” or “SHALL”, denotes an absolute requirement
    of the specification.
2. MUST NOT This phrase, or the phrase “SHALL NOT”, denotes an absolute prohibition of the
    specification.
3. SHOULD This word, or the adjective “RECOMMENDED”, mean that there may exist valid
    reasons in particular circumstances to ignore a particular item, but the full implications must be
    understood and carefully weighed before choosing a different course.
4. SHOULD NOT This phrase, or the phrase “NOT RECOMMENDED” mean that there may
    exist valid reasons in particular circumstances when the particular behavior is acceptable or even
    useful, but the full implications should be understood and the case carefully weighed before
    implementing any behavior described with this label.
5. MAY This word, or the adjective “OPTIONAL”, mean that an item is truly optional.


## Contents

- 1 WaccLanguage Syntax
   - 1.1 Lexical Structure
   - 1.2 Expressions
   - 1.3 Types
   - 1.4 Statements
   - 1.5 Additional Syntactic Restrictions
- 2 WaccLanguage Semantics
   - 2.1 Types
      - 2.1.1 Basic Types
      - 2.1.2 Arrays
      - 2.1.3 Pairs
      - 2.1.4 Expressions and Statements
   - 2.2 Scoping
      - 2.2.1 Variable Declarations
      - 2.2.2 Nested Scopes
      - 2.2.3 Functions
   - 2.3 Behaviour
      - 2.3.1 Runtime Representations
      - 2.3.2 Expressions
      - 2.3.3 Function Definitions
      - 2.3.4 Sequentialisation and No-ops
      - 2.3.5 Assignments and Declarations
      - 2.3.6 IO Statements
      - 2.3.7 Control-Flow Statements
      - 2.3.8 Freeing Memory


## 1 WaccLanguage Syntax

```
The syntax of theWacclanguage is described in Backus-Naur Form (bnf) notation. To make the
presentation more precise, the following extensions are provided tobnfnotation:
```
- (𝑥-𝑦)stands for ‘range’, meaning any value from𝑥to𝑦inclusive;
- (𝑥)? stands for ‘optional’, meaning that𝑥can occur zero or one times;
- (𝑥)+ stands for ‘repeatable’, meaning that𝑥can occur one or more times;
- (𝑥)* stands for ‘optional and repeatable’, meaning that𝑥can occur zero or more times.
Definition 1 Two grammatical constructs _x_ and _y_ are said to bejuxtaposedwhen separated by whitespace,
for example _xy_.

### 1.1 Lexical Structure

The syntax ofWaccis formed up of lexical tokens. These tokens are all indivisible, and whitespace
(including comments)MUST NOToccur within any token. TokensSHOULDbe parsed according to
“longest-match”, such that the longest continuous matching token is recognised after the end of the
previous token. Within this sub-section, juxtaposition in the grammar does not implicitly allow for
whitespace.
⟨int-liter⟩ ::= ⟨int-sign⟩?⟨digit⟩+

```
⟨digit⟩ ::= (‘ 0 ’-‘ 9 ’)
```
```
⟨int-sign⟩ ::= ‘+’ | ‘-’
```
```
⟨bool-liter⟩ ::= ‘true’ | ‘false’
```
```
⟨char-liter⟩ ::= ‘'’⟨character⟩‘'’
```
```
⟨str-liter⟩ ::= ‘"’⟨character⟩* ‘"’
```
```
⟨character⟩ ::= any-graphic-ASCII-character-except-‘\’-‘'’-‘"’ (graphic𝑔≥‘ ’)
| ‘\’⟨escaped-char⟩
```
```
⟨escaped-char⟩ ::= ‘ 0 ’ | ‘b’ | ‘t’ | ‘n’ | ‘f’ | ‘r’ | ‘"’ | ‘'’ | ‘\’
```
```
⟨pair-liter⟩ ::= ‘null’
```
```
⟨ident⟩ ::= ( ‘_’ | ‘a’-‘z’ | ‘A’-‘Z’ ) ( ‘_’ | ‘a’-‘z’ | ‘A’-‘Z’ | ‘ 0 ’-‘ 9 ’ )* (not keyword)
```
```
⟨comment⟩ ::= ‘#’ (any-character-except-EOL)* (⟨EOL⟩|⟨EOF⟩)
Further to the above described tokens, which may appear in the subsequent sub-sections, any literal
string from this point, for instance‘if’, will be treated as a token in its own right. Whitespace and
comments, which can be found between tokens, have no semantic meaning within the program. From
this point, there is implicit optional whitespace between the juxtaposition of terminals, non-terminals,
and symbols.
```

```
Precedence Associativity Operators
0 (tightest) prefix ‘!’, ‘-’, ‘len’, ‘ord’, ‘chr’
1 infix left ‘*’, ‘%’, ‘/’
2 infix left ‘+’, ‘-’
3 infix non ‘>’, ‘>=’, ‘<’, ‘<=’
4 infix non ‘==’, ‘!=’
5 infix right ‘&&’
6 (weakest) infix right ‘||’
Table 1: The precedence of operators in theWacclanguage.
```
### 1.2 Expressions

Expressions consist of literals, parenthesised expressions and array indexing, separated by unary and
binary operations. For simplicity of grammar presentation, binary and unary operations have been
coalesced; in practice all operators are assigned a precedence and associativity.

⟨expr⟩ ::= ⟨unary-oper⟩ ⟨expr⟩
| ⟨expr⟩ ⟨binary-oper⟩ ⟨expr⟩
| ⟨atom⟩

⟨atom⟩ ::= ⟨int-liter⟩
| ⟨bool-liter⟩
| ⟨char-liter⟩
| ⟨str-liter⟩
| ⟨pair-liter⟩
| ⟨ident⟩
| ⟨array-elem⟩
| ‘(’⟨expr⟩‘)’

⟨unary-oper⟩ ::= ‘!’ | ‘-’ | ‘len’ | ‘ord’ | ‘chr’

⟨binary-oper⟩ ::= ‘*’ | ‘/’ | ‘%’ | ‘+’ | ‘-’ | ‘>’ | ‘>=’ | ‘<’ | ‘<=’ | ‘==’ | ‘!=’ | ‘&&’ | ‘||’

⟨array-elem⟩ ::= ⟨ident⟩(‘[’⟨expr⟩‘]’)+

The precedence and associativities of the operators are described in table 1. The precedence ordering
of the operatorsMUSTbe respected. A left-associative operatorMUSTbe able to appear repeatedly
andMUSTimplicitly bracket to the left, for instance,‘5 + 6 + 7’and‘(5 + 6) + 7’represent the same
expression. A right-associative operatorMUSTbe able to appear repeatedly andMUSTimplicitly
bracket to the right, for instance,‘p && q && r’and‘p && (q && r)’represent the same expressions.
A non-associative operatorSHOULD NOTappear repeatedly, and therefore does not bracket – for
example, ‘x < y < z’SHOULD NOTbe syntactically legal.
Brackets have no semantic meaning, they just override syntactic precedence.


### 1.3 Types

```
The syntactic description of types within theWacclanguage is given below:
⟨type⟩ ::= ⟨base-type⟩|⟨array-type⟩|⟨pair-type⟩
```
```
⟨base-type⟩ ::= ‘int’ | ‘bool’ | ‘char’ | ‘string’
```
```
⟨array-type⟩ ::= ⟨type⟩‘[’ ‘]’
```
```
⟨pair-type⟩ ::= ‘pair’ ‘(’⟨pair-elem-type⟩‘,’⟨pair-elem-type⟩‘)’
```
```
⟨pair-elem-type⟩ ::= ⟨base-type⟩|⟨array-type⟩| ‘pair’
```
```
Importantly, non-erased pair typesMUST NOTappear directly within another non-erased pair, the
rationale for this is discussed in section 2.1.3. However, note that array typesMUSTbe able to interleave
with pair types, such that ‘pair(int, pair(int, char)[])’ is syntactically (and semantically) legal.
```
### 1.4 Statements

Wacc, as described in this specification, is a procedural language that is statement-oriented. Statements
are described as follows:

```
⟨program⟩ ::= ‘begin’⟨func⟩*⟨stmt⟩‘end’
```
```
⟨func⟩ ::= ⟨type⟩ ⟨ident⟩‘(’⟨param-list⟩? ‘)’ ‘is’⟨stmt⟩‘end’
```
```
⟨param-list⟩ ::= ⟨param⟩( ‘,’⟨param⟩)*
```
```
⟨param⟩ ::= ⟨type⟩ ⟨ident⟩
```
```
⟨stmt⟩ ::= ‘skip’
| ⟨type⟩ ⟨ident⟩‘=’⟨rvalue⟩
| ⟨lvalue⟩‘=’⟨rvalue⟩
| ‘read’⟨lvalue⟩
| ‘free’⟨expr⟩
| ‘return’⟨expr⟩
| ‘exit’⟨expr⟩
| ‘print’⟨expr⟩
| ‘println’⟨expr⟩
| ‘if’⟨expr⟩‘then’⟨stmt⟩‘else’⟨stmt⟩‘fi’
| ‘while’⟨expr⟩‘do’⟨stmt⟩‘done’
| ‘begin’⟨stmt⟩‘end’
| ⟨stmt⟩‘;’⟨stmt⟩
```
```
⟨lvalue⟩ ::= ⟨ident⟩|⟨array-elem⟩|⟨pair-elem⟩
```

```
⟨rvalue⟩ ::= ⟨expr⟩
| ⟨array-liter⟩
| ‘newpair’ ‘(’⟨expr⟩‘,’⟨expr⟩‘)’
| ⟨pair-elem⟩
| ‘call’⟨ident⟩‘(’⟨arg-list⟩? ‘)’
```
```
⟨arg-list⟩ ::= ⟨expr⟩(‘,’⟨expr⟩)*
```
```
⟨pair-elem⟩ ::= ‘fst’⟨lvalue⟩| ‘snd’⟨lvalue⟩
```
```
⟨array-liter⟩ ::= ‘[’ (⟨expr⟩(‘,’⟨expr⟩)* )? ‘]’
```
Note that⟨stmt⟩allows multiple statements to be delimited by semi-colons. The statement delimiter,
‘;’, is treated as an infix operator with an unspecified associativity – a trailing‘;’SHALL NOTbe legal.
There is no parenthesisation allowed of sequenced statements.
AWaccfile (extension.wacc) only ever contains a singleWaccprogram.

### 1.5 Additional Syntactic Restrictions

```
All errors generated as a result of a failure for a user program to adhere to the synactic rules described
in sections 1.1 to 1.4MUSTbe denoted assyntax errors. However, in addition to these, the violation of
the following two requirementsMUSTalso be reported as syntax errors.
```
- Integer literals,⟨int-liter⟩,MUSTbe within the range of a signed 32-bit decimal number. The
    maximum possible value is 231 − 1 and the minimum possible value is− 231.
- It isREQUIREDthat all functions,⟨func⟩, can only be exited via a‘return’or‘exit’statement.
    A block of statements is calledreturningif the last statement in the block is either:
       1. a ‘return’ statement
       2. an ‘exit’ statement
       3. an ‘if’ statement with tworeturningblocks.
    All function bodiesMUSTbereturningblocks.


## 2 WaccLanguage Semantics

The semantics of theWacclanguage are split into typing, scoping, and behaviour.

### 2.1 Types

TheWacclanguage isstronglyandstaticallytyped. This means that, with some noted exceptions:

- Each variable’s type can be determined at compile-time (statically) from its declaration site.
- Types are specific and cannot be freely coerced between, nor is there any casting permitted.

In short, the exceptions to these rules are that nested pair types are subject to erasure and there is a
permitted weakening of‘char[]’into‘string’. Throughout this sub-section, types such as𝜏and𝜎
denote arbitrary types.
Any programs that are ill-typed do not compile, andMUSTproduce asemantic error. Valid
implementationsMAYchoose to give errors arising from ill-typed programs more specific names.

#### 2.1.1 Basic Types

The basic types in theWacclanguage are:

- int: a 32-bit signed integer type.
- bool: the two valued boolean type consisting oftrueandfalse.
- char: 7-bitasciicharacter.
- string: a (statically) immutable sequence of characters.

#### 2.1.2 Arrays

An array of elements of type𝜏is denoted by‘𝜏[]’. The element type𝜏can be any type except for an
erased‘pair’(see section 2.1.3). The length of the arrayMUSTbe fixed at its creation, and itMUST
be heap allocated. ArraysMUSTtrack their sizedynamically– the size of an arrayMUST NOTbe
tracked only at compile-time.

String weakening The typechar[]may take the place ofstringfor the purposes of type-checking.
Such arraysMUST NOTbe frozen and can still be modified by the programmer. It is the programmer’s
responsiblity to retain a reference to the original array so that it can be freed – the‘free’statement
cannot be used withstring. This is a uni-directional type coercion: astringMUST NOTtake the
place ofchar[], as astringmay not be modified.

Definition 2 A type𝜏iscompatiblewith another type𝜎if and only if𝜏≡𝜎or𝜏can be weakened to𝜎.

Definition 3 Thelowest common ancestor𝜎of a set of types{𝜏 1 , .. ,𝜏𝑛}is the most specific type such
that each of𝜏 1 through𝜏𝑛is compatible with𝜎.


```
Invariant As arrays are mutable, they are necessarilyinvariantin their type parameter. This means
thatchar[][]MUST NOTbe compatible withstring[]. Were this the case, it would allow a dangerous
mutable view onstring. However, it remains possible to store achar[]into astring[]by the regular
weakening relation – this is not unsound.
```
#### 2.1.3 Pairs

```
Pair types inWaccare binary tuples that can be used to build up any other datastructure. Given two
(possibly distinct) types,𝜏and𝜎, the type‘pair(𝜏,𝜎)’denotes a pair whose first element has type𝜏
and second element has type𝜎. The literal ‘null’ has typepair(𝜏,𝜎)for all types𝜏and𝜎.
```
Erasure When either of𝜏or𝜎within a pair are themselves a pair, theyMUSThave anerasedtype
‘pair’. For instance,‘pair(int, pair)’is legal, but‘pair(int, pair(int, bool))’is not. Note that
single-element arrays can be used to losslessly nest pairs.pair(𝜏,𝜎)can be weakened topairand
paircan be weakened topair(𝜏,𝜎), in other wordspairandpair(𝜏,𝜎)are fully coercible.
The implication of the nested-pair erasure is that some typing information is necessarily lost, but
this allows for recursively constructed datastructures arising from nested-pairs: if nested pairs were
not subject to erasure, this would involve infinite types.
A consequence of erasure is thatWacc’s type system isunsound: it is possible to perform unsafe
coercions between types by storing and extracting them from a nested-pair. Neither the runtime or
compiler are required to prevent these coercions, and using an improperly coerced value isundefined
behaviour.
Since the typepairis not itself a legal type of a variable, nested pairs will cease to be erased
once they are extracted; at this point, the full type of the pairMUSTbe known – this is discussed in
section 2.1.4.

```
Invariant As a mutable type, pairs are also necessarilyinvariantin their type parameters. This means
that‘pair(char[], int)’MUST NOTbe compatible with the type‘pair(string, int)’. Similarly to
arrays, it is still possible to assign achar[]to the left-hand argument ofpair(string, int), as this
does not create an unsoundness.
```
#### 2.1.4 Expressions and Statements

```
Array literals Array literals can appear on the right-hand side of assignments. The set of types of all
elements in the arrayMUSThave a lowest common ancestor𝜏; the type of the literalSHALLthen be
𝜏[]. For instance, an array literal containing a mix ofchar[]andstringSHALLhave overall type
string[]. An empty array literal ‘[]’ has type𝜏[]with no specifically discernable type.
```
```
Operators Each of the binary and unary operators withinWaccwork on a specific set of types, as
illustrated in tables 2 and 3. Equality is well defined for all types, so long as the two operands can be
weakenedto the same type:int[]andchar[]are not comparable with each other, even though they
are both arrays, butstringandchar[]are. Comparison can only be performed between arguments of
weakly-matching types as well.
```

```
Operator Argument Type Return Type
‘!’ bool bool
‘-’ int int
‘len’ 𝜏[] int
‘ord’ char int
‘chr’ int char
Table 2: The types of unary operators inWacc.
Operators Left-Hand Type Right-Hand Type Return Type
‘*’, ‘/’, ‘%’, ‘+’, ‘-’ int int int
‘>’, ‘>=’, ‘<’, ‘<=’ int/char int/char bool
‘==’, ‘!=’ 𝜏 𝜏 bool
‘&&’, ‘||’ bool bool bool
Table 3: The types of binary operators inWacc.
```
Return statements The type of values provided to a return statementMUSTbe compatible with the
return type of the enclosing function – since the main scope has no return type,‘return’SHALL NOT
be legal within the main body of the program, this is asemantic error.

Printing Print statements may print values of any type, though the behaviour of the printing may
vary – this is discussed in section 2.3.6.

Reading The‘read’statement takes a destination⟨lvalue⟩which must be of either typeintor type
char. No other types of values may be read.

Exit statements The ‘exit’ statement must be provided with an exit code of typeint.

Memory freeing The ‘free’ statement takes an argument that must be of type𝜏[]orpair(𝜏,𝜎).

Control-flow statements Both ‘if’ and ‘while’ constructs have a condition of typebool.

Declaration and assignments When a variable of type𝜏is declared, the right-hand side of the
assignmentMUSTbe compatible with the type𝜏. A variable’s typeMUSTbe fixed at its declaration
site. Similarly, the right-hand side of a non-declaration assignmentMUSTbe compatible with the
left-hand side. The right-hand sides of assignments are⟨rvalue⟩s, which includes expressions, as well
as array literals, pair construction, pair extraction, and function calls.
A function call’s return typeMUSTbe compatible with the left-hand type of the assignment and
the callMUSTbe fully saturated, provided with all its arguments, and every argumentMUSTbe
compatible with the corresponding parameter in the function’s declaration.
Given two expressions𝑒 1 of type𝜏and𝑒 2 of type𝜎, ‘newpair(𝑒 1 ,𝑒 2 )’ will have typepair(𝜏,𝜎).

Definition 4 A type𝜅isknownif it is compatible with𝜏[],pair(𝜏,𝜎),int,char,bool, orstring.

Both⟨lvalue⟩s and⟨rvalue⟩s can represent (nested) pair extraction. This has some more complex typing
rules thatMUSTbe carefully followed:


- The type of‘fst p’, where‘p’has typepair(𝜏,𝜎), is𝜏. An update to‘fst p’MUSTbe compatible
    with𝜏, and an extraction from‘fst p’MUSTensure𝜏is compatible with the left-hand side of
    the assignment.
- The type of‘snd p’, where‘p’has typepair(𝜏,𝜎), is𝜎. An update to‘snd p’MUSTbe compatible
    with𝜎, and an extraction from‘snd p’MUSTensure𝜎is compatible with the left-hand side of
    the assignment.
- When considering anestedpair extraction or update, i.e.‘fst fst p’, where‘p’necessarily has
    typepair(pair,𝜏), the type of the extractee is not known as it comes frompair.
    When‘p’has typepair, the type of‘fst p’or‘snd p’as either an update or extraction is instead
    a known type𝜅determined by the other side of the assignment. Any unsoundness caused by
    type coercion is the fault of the programmer.
    If the type of the other side of the assignment is not known, then thisMUSTresult in a type error.
    For example‘fst fst p = fst fst q’MUSTraise an error, since neither side of the assignment is
    of some known type𝜅.

### 2.2 Scoping

InWacc, scopes are introduced by‘begin .. end’, functions, if-statements, and while-loops. The
top-level global scope contains all other definitions within the program.

Definition 5 Abindingis a name associated with either a variable or a function argument.

Definition 6 Lexical(orstatic) scoping is where variables are in scope based on their position within the
source file, and not on the dynamic execution path of the program.

#### 2.2.1 Variable Declarations

Variables are always declared within a scope, andMUSTbe unique within that scope. The lifetime of
any variable is static: all uses of a variableMUSTappear below the declaration site. A variableMUST
be assigned its type at declaration. When a scope is exited, any variables declared withinMUSTno
longer be accessible. A variableMUST NOTbe in scope in the right-hand side of its own declaration.

#### 2.2.2 Nested Scopes

A scope may be created inside another scope. The child scopeMUSTinherit all bindings from the
parent scope – this cascades, so bindings are accessible from grandparent scopes etc. When a scope
declares a variable with the same name as a binding from an enclosing scope, itMUSTshadowthat
binding so that it cannot be referred to from that point until the new variable goes out of scope. As
a consequence, a statement such as‘int x = x’may be legal, so long as‘x’is declared in an enclosing
scope (this is to be read as‘int x 2 = x 1 ’). A shadowed variableMUSTbe able to have a different type to
the pre-existing bindings.


#### 2.2.3 Functions

Functions can only be declared at the start of the main scope (as demanded by the syntax ofWacc). A
function body’s parent scopeMUSTcontain the bindings for each argument of the function. The body
then follows the scoping rules of section 2.2.2. As functions are declared within the global scope, they
SHALLbe accessible everywhere, and are allowed to referred to before they are lexically defined: this
allows for recursive as well as mutually recursive functions.
InWacc, functions are not first-class values in the language. They can only be called unambiguously
via a‘call’statement. As such, variables and functions may share the same names without conflict: for
exmaple, ‘int foo(bool foo) is return 0 end’ is a legal function.

### 2.3 Behaviour

This section describes the operational behaviour of the constructs within theWacclanguage. All
behaviours assume a syntactically-valid, well-typed, and well-scoped program.

#### 2.3.1 Runtime Representations

Each of the runtime values inWaccMUSTbe able to accommodate the following values:

- intMUSTsupport the full range of values between− 231 and 231 − 1. No runtime value outside
    of this range is expressible within the program.
- boolMAYbe as small as 1-bit wide.
- charMUSTsupport 7-bitascii.
- strings and arraysSHOULDshare a common representation (to allow for easy conversion).
    TheyMUSTbe represented as pointers and track their number of characters/elements in their
    allocated data. While arraysMUSTbe heap-allocated, stringsSHOULDbe stored within the
    data-segment of the executable to avoid memory leaks. While both the empty string and empty
    array are valid values, there is no such “null” value for these types. ArraysMUSTbe explicitly
    deallocated with ‘free’; it is the programmer’s responsibility to ensure no memory-leaks.
- PairsMUSTbe represented as pointers. As unsafe pair coercion isundefined behaviour, and
    pair extraction is only legal on aknowntype (which each have a statically known size), it is
    possible to have the representation of the elements of a pair vary based on type. Pairs are mutable,
    andMUSTbe allocated on the heap; like arrays, pairs can be deallocated with a‘free’statement.
    The ‘null’ value does exist for pairs, it is not allocated on the heap.

The runtime representations of these typesMAYbe represented by wider sizes, however theyMUST
remain within the stated value ranges from the programmer’s point-of-view. The size of a pointer is
platform dependent. The runtime representation can change as convenient for the current storage – for
instance, it might not be feasible to store each value in less than 64-bits when occupying a register, but
SHOULDbe more space efficient when in memory.


#### 2.3.2 Expressions

The expressions of theWacclanguage have been chosen to be side-effect free. This means that the
process of evaluating an expression does not change the program state, though they can throw runtime
errors. They are evaluated as follows:

Literals All literals withinWaccexpressions plainly represent themselves and are already evaluated.

Variables The evaluation of a variable simply returns the value stored in that variable.

Array Index Evaluation of an array index ‘arr[i]’ proceeds as follows:

1. evaluate the expression ‘arr’ to obtain a pointer to the array,𝑝
2. evaluating the index ‘i’ obtaining an integer𝑛
3. checking that 0 ≤𝑛<length(𝑝), if not a runtime errorMUSTbe thrown
4. return the the𝑛thelement in the array𝑝

Unary Operators A unary operator has a single sub-expression, which is evaluated to obtain a value,
𝑥, of the correct type. Then:

- ‘!’ performs a logical negation of𝑥, such that ‘true’ becomes ‘false’, and vice-versa.
- ‘-’behaves as 0 −𝑥, inverting the sign of the value𝑥. If any overflow or underflow occurs, this
    MUSTresult in a runtime error.
- ‘len’ returns the length of the array pointed to by𝑥. ThisMUSTbe computed inO( 1 ).
- ‘ord’ converts the character𝑥into itsasciiordinal value. For example ‘ord ’a’’ is ‘ 97 ’.
- ‘chr’converts the integer𝑥into the correspondingasciicharacter for that ordinal. ItMUSTbe
    true that 0 ≤𝑥≤ 127 , if this is not the case, a runtime errorMUSTbe raised.

Binary Operators A binary operator has two sub-expressions, which are evaluated, in either order,
to obtain values𝑥and𝑦of the correct types. Then:

- The‘*’,‘+’and‘-’operators all have their standard mathematical behaviour on integers. Any
    operation that results in overflow or underflowMUSTthrow a runtime error.
- Both‘/’,‘%’must both first check that the divisor𝑦is not 0 – if it is theyMUSTthrow a runtime
    error. They both have their standard mathematical meanings. For division, if both𝑥and𝑦have
    the same sign, the result is positive and negative otherwise. For modulus, the sign of the result
    is the same as that of the dividend𝑥. Overflow in division is left asundefined behaviourand
    throwing a runtime error is not required.
- The ‘>’, ‘>=’, ‘<’ and ‘<=’ operators all perform a standard comparison on𝑥and𝑦.


- The‘==’operator compares𝑥and𝑦for equality. Forint,char,boolthis standard value equality;
    for𝜏[],string, andpair(𝜏,𝜎), however, thisMUSTbe a referential equality on the pointers.
- The ‘!=’ operator behaves like ‘!(𝑥==𝑦)’.
- The‘&&’operator evaluates totruewhen both𝑥and𝑦aretrue, andfalseotherwise. This
    operationMAYshort-circuit and only evaluate𝑥if it isfalse.
- The‘||’operator evaluates tofalsewhen both𝑥and𝑦arefalse, andtrueotherwise. This
    operationMAYshort-circuit and only evaluate𝑥if it istrue.

#### 2.3.3 Function Definitions

As noted in section 2.2.3, the execution of a function runs in an empty lexical scope with a parent
containing the bindings for the arguments. Waccis an eager pass-by-value language, as such, all
function argumentsMUSTbe evaluated before being passed into the function; the order-of-execution
isnot specified, since expression evaluation does not have side-effects.
Note that since pairs, arrays, and stringsMUSTall be represented as pointers, any mutable updates
SHALLbe reflected outside the function call: this isnotpass-by-reference, as a reassignment to an
argumentSHALL NOTbe reflected in the caller’s scope.

Return statements A‘return’statement can only be present in the body of a function; when
executed, it will evaluate its argument, and pass the resulting value back to the caller of the function,
exiting the current call immediately.

#### 2.3.4 Sequentialisation and No-ops

The‘skip’statement has no effect on the program when executed. Given‘𝑆 1 ;𝑆 2 ’, first, the statement
𝑆 1 is executed, and then the statement𝑆 2 is executed, observing any changes that𝑆 1 may have made
during execution. Note that ‘skip ;𝑆’ and ‘𝑆; skip’MUSTboth be semantically equivalent to ‘𝑆’.
The‘skip’statement can be used to ignore unused branches of conditional statements, for instance.

#### 2.3.5 Assignments and Declarations

Both regular assignments and declarations have the same runtime behaviours when considering their
right-hand sides. Each case is explained below, with each case producing a single value referred to as𝑥.

⟨rvalue⟩evaluation

- A plain expressionSHALLbe evaluated into the value𝑥.
- An array literal is evaluated into a value as follows:
    1. allocate enough memory on the heap to store all elements and the size of the array; if
       allocation fails, thisMAYproduce a runtime error
    2. evaluate all elements of the array (the order-of-evaluation is unspecified)
    3. store each evaluated element into its corresponding slot in the array


```
Steps (2) and (3) can be interleaved, or done sequentially. The value𝑥SHALLbe the pointer
returned by the memory allocation.
```
- A function call proceeds as outlined in section 2.3.3. The return value of the callSHALLbe𝑥.
- A ‘newpair’ will:
    1. allocate enough memory on the heap to store the pair (see section 2.3.1); if allocation fails,
       thisMAYproduce a runtime error
    2. evaluate both provided expressions (order-of-evaluation is unspecified)
    3. store each evaluated element into its corresponding slot in the pair

```
Steps (2) and (3) can be interleaved, or done sequentially. The value𝑥SHALLbe the pointer
returned by the memory allocation.
```
- A pair extraction (‘fst p’ or ‘snd p’) is evaluated into a value by:
    1. obtaining thereference,𝑟, to the location of the pair‘p’according to the rules outlined for
       ⟨lvalue⟩s
    2. following𝑟to obtain a pointer,𝑝, to the pair
    3. if𝑝is thenullpair, thisMUSTthrow a runtime error
    4. extracting the first or second element of𝑝respectively, according to its underlying runtime
       representation

```
The extracted elementSHALLbe the value𝑥.
```
Definition 7 Areferencedescribes a location, which could either be a specific register, a stack address, or
a heap pointer. It has no specific representation, and serves to denote a place that can befollowedor stored
into. References cannot be nullary.

Definition 8 A reference can befollowedto obtain the value that exists at that location. For a register,
this means refering directly to that register; or for a memory address this means dereferencing the pointer.

⟨lvalue⟩evaluation Obtaining a reference,𝑟, to the location represented by an⟨lvalue⟩requires
evaluating the term until a concrete location is obtained.

- an identifier ‘v’ is already a reference,𝑟.
- a reference to an array index ‘arr[i]’ is obtained by:
    1. determining the reference,𝑟arr, to the location of the array ‘arr’
    2. following𝑟arrto obtain a pointer representing the array,𝑝
    3. evaluating the index ‘i’ obtaining an integer𝑛
    4. checking that 0 ≤𝑛<length(𝑝), if not a runtime errorMUSTbe thrown
    5. 𝑟represents the address of the𝑛thelement in the array𝑝


```
The evaluation of the array and index can happen in any order.
```
- a reference to a pair element ‘fst p’ or ‘snd p’ is obtained by:
    1. determining the reference𝑟p, to the location of the pair ‘p’
    2. following𝑟pto obtain a pointer to the pair,𝑝
    3. if𝑝is thenullpair, thisMUSTthrow a runtime error
    4. 𝑟represents the address of the first or second element in the pair𝑝
Given a value to store,𝑥, and reference,𝑟, an assignment will write the value𝑥into the location
represented by𝑟.

#### 2.3.6 IO Statements

Wacccan interact with the outside world in two ways: reading input from and printing to the terminal.

```
Read Statements A read statement‘read’extracts a “token” from the input streamstdinand stores
it in a given location. The input stream will consist of space-separated characters and integers. First, a
reference𝑟is obtained from the given⟨rvalue⟩(as described in section 2.3.5); then a value𝑥is read
from the input stream according to the following rules:
```
- If the type of the read ischar, a singleasciicharacter is read from the input.
- If the type of the read isint, a whole integer is taken from the input stream, which may consist
    of many characters.

```
If no input can be read, thereadstatementMUSTdo nothing and continue, otherwise the value𝑥read
from the stream is written into the location represented by𝑟. If the read did fail, then the value stored
at𝑟MUST NOThave been modified.
```
```
Expression
Type
```
```
Behaviour Example
Expression
```
```
Example
Output
int Output the integer converted to a decimal string. 10 “10”
bool Output “true” if the boolean istrueand “false” otherwise. false “false”
char Outputs the character, respecting the special meanings in
table 5.
```
```
'c' “c”
```
```
stringor
char[]
```
```
Output each character in order, respecting the special
meanings in table 5.
```
```
"hello" “hello”
```
```
Other
Array
Types
```
```
Output the start address of the array in memory in
hexadecimal.
```
```
N/A “0x12008”
```
```
pair Output the start address of the pair in memory in
hexadecimal. A ‘null’MUSTprint “(nil)” instead.
```
```
N/A “0x13458”
```
```
Table 4: The behaviour of the print statements for each type of expression.
```

```
Representation ASCII Value Description Symbol
\0 0x00 null terminator NUL
\b 0x08 backspace BS
\t 0x09 horizontal tab HT
\n 0x0a linefeed LF
\f 0x0c form feed FF
\r 0x0d carriage return CR
\" 0x22 double quote "
\’ 0x27 single quote ’
\\ 0x5c backslash \
Table 5: The escape-characters available in theWacclanguage.
```
Print Statements The two print statements,‘print’and‘println’behave similarly, except that
‘println’outputs an additional newline at the end of the print. The output representation of a printed
expression depends its type, this is summarised in table 4.

#### 2.3.7 Control-Flow Statements

Wacchas five statements for controlling the flow of a program’s execution. Function calls and returns
have been covered in section 2.3.3; the others are conditional statements – often called‘if’-statements,
‘while’-loops, and ‘exit’-statements.

```
Conditionals A conditional statement,‘if𝑐then𝑆 1 else𝑆 2 fi’, must first evaluate the expression
𝑐; if𝑐evaluates totrue, then𝑆 1 is executed, otherwise𝑆 2 is executed.
```
```
Looping A while loop,‘while𝑐do𝑆done’, first evaluates the expression𝑐; if𝑐evaluates totrue,
then𝑆is executed and the loop repeats with the updated program state. When𝑐no longer evaluates to
true, the statement𝑆is not executed again and the loop finishes.
```
```
Exit statements An exit statement‘exit’terminates the program. The given expression is first
evaluated to provide an integer, and the 8 least-significant bits are taken as the exit code. The program
is terminated immediately by calling theexitsystem call with the computed code.
```
#### 2.3.8 Freeing Memory

```
The‘free’statement is used to deallocate heap memory allocated by arrays and pairs. For nested
structures, only the outer-most pointer is deallocated, meaning inner pointers can leak unless they
have already been freed. Using a deallocated pointer after freeing isundefined behaviour. When
considering pairs, attempting to deallocate thenullvalueMUSTproduce a runtime error. Any memory
leaks or dead pointers are the programmer’s responsibility.
```

