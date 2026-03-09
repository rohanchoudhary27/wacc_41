# The Wacc Compiler: Milestone 3

# Extensions

### COMP50007.3 - Advanced Laboratory 2

### Department of Computing

### Imperial College London

## Summary

For the final milestone of the Wacc project, you are expected to extend your compiler and/or the Wacc
language with some new features. You are free to add any features you want to any aspect of the
compiler, from the language specification to the code generation. This is your chance to implement
some of the more exciting compiler features that you were exposed to during the lectures.
You will also need to write a short report on your Wacc project, to help you reflect on what you
have achieved during this lab exercise.

## Details

Recall the three stages of the compilation process:

1. Perform Syntactic Analysis: optionally tokenising, and then parsing the string/tokens into an
    abstract syntax tree representing the source language.
2. Perform Semantic Analysis: working out and ensuring the integrity of the meaning of the
    input file.
3. Generate Target: synthesizing output in the target language, maintaining the semantic meaning
    of the source program.

At this point in the project, you should have a complete compiler for the Wacc language that can handle
all three of these stages.
For this last milestone you need to pick some aspect(s) of your compiler to extend. Your extensions
can cover any or all of the stages listed above, or could even be at the language level. We deliberately
give you a great deal of freedom so that you can concentrate on something that interests you; however,
you must implement at least one optimisation^1 extension, to be analysed in your report.

## Submit by 19:00 on Friday 13th March 2026

(^1) Optimisations should be above a certain level of difficulty, so that the discussion in the report is deep enough; extensions
coloured in blue offer interesting points of investigation and discussion – you are advised to pick one of these.


## What To Do:

1. Decide on what to build for your Wacc compiler extension(s), including an optimisation.
2. Implement your chosen extension(s) to your Wacc compiler.
3. Write a report that critically reflects on your end product and design choices during the Wacc
    project as well as describing your extensions, with critical analysis of an optimisation performed.

## Compiler Extensions

There are many possible extensions and we recommend that you choose something that interests you.
Over the following pages, we give some suggestions of possible extensions sorted by their expected
level of difficulty. You will need to carefully consider the syntax and precise semantics of any
changes you make to the language, and how these interact with other features. Consider how
you can best re-use the existing parts of your compiler when implementing extensions.
The marks awarded for this milestone will not depend solely on the number and difficulty of the
extensions attempted. We will also consider the functionality and correctness of your extensions,
and the quality and coherence of their design.
We will automatically take your group size into account when determing your “Technical Challenge”
mark, scaling up the marks for smaller groups. Additionally, we expect you to implement at least one
extension per group member (of any difficulty), otherwise a cap will be applied to your “Functional-
ity/Correctness” mark proportional to the number of extensions you have attempted (e.g. if a group of
4 attempt only 3 extensions, then they will not be able to score more than 3/4 * 10 = 7.5 marks for this
criteria). This means that you cannot just implement one Easy extension and expect to score 10/10 for
“Functionality/Correctness” (unless you are working alone for this milestone of the project).
Note that we expect any language specification extension to be backed up by an implementation
of that extension. You are also welcome to propose your own extensions, but you must have these
approved by the lab organiser before beginning implementation to ensure that they are appropriate; all
extensions must be relevant to compilers and programming language design.

```
Note: Trivial Extensions Depending on the tooling you are using, some extensions may be consid-
ered as trivial, so you should consider the work required to implement it: if usingparsleyfor instance,
we know that supporting the purely syntactic hexadecimal, octal, or binary literals are trivial – they
only require turning on a single flag. As a result, doing these extensions as aparsleygroup is likely to
be considered for only for a tiny amount of credit – as a rule of thumb, if you can do it by changing a
handful of characters, it will probably not be worth anything! The exception to that would be that you
designed your original system so well that adding an extension becomes very easy as a result of your
work – this is perfectly fine, and encouraged! If in doubt, just ask us.
```
```
Note: Law of Diminishing Returns To reward groups that attempt the more challenging extensions,
we impose a sub-linear mark reduction on each group’s “Technical Challenge” mark based on the number
𝑛of easier (1-2 mark) extensions attempted. This reduction follows the number of non-triangular
numbers≤ 𝑛sequence^2 , and can be computed directly as:𝐿𝐷𝑅(𝑛)= 𝑛−⌊
```
#### √

```
2 ∗𝑛+ 1 −^12 ⌋. This means
that you cannot just implement 10 Easy extensions and expect to score 10/10 for “Technical Challenge”.
```
(^2) see https://oeis.org/A083920 for more details


## Easy [approx. 1-2 marks each]

```
Numeric Literals Extend the Wacc language to support additional integer literal formats other than
just base 10.
```
```
Bitwise Operators Extend the expressions of the Wacc language to include bitwise operators such
as ‘&’, ‘|’ or ‘∼’.
```
```
Statically-Sized Array Types Extend the Wacc language so that arrays can also be given a static
length. You should consider the interactions this has with other language features.
```
```
Void Types Extend the Wacc language with the‘void’type to allow for the definition of functions
that do not return a value. You will need to consider how this change will interact with the original
function syntax requirement that the body of every function must end with either a‘return’’statement
or an ‘exit’ statement.
```
Conditional Expressions Extend the Wacc language by allowing‘if’expressions, which are like
‘if’ statements but can appear within expressions.

```
Loop Control Extend the Wacc language to include constructs like‘break’and‘continue’found in
other languages.
```
```
Additional Loop Control Statements Extend the Wacc language with additional looping constructs,
for example ‘for’, ‘do-while’.
```
```
Additional Branching Statements Extend the Wacc language with additional conditional branching
statements, for example ‘if’ statements without an ‘else’ branch or ‘switch’ statements.
```
```
Text-Substitution Macros Extend the Wacc language to support the definition and use of macros in
the style of the C pre-processor, which operates by textual subsistution.
```
```
Different-Sized Integers Extend the Wacc language to support multiple types of integer. The
current Wacc integer type is effectively a 32-bit signed integer. You could extend the integer types to
include the common formats of byte (8-bit), short (16-bit), and long (64-bit).
```
```
Floating-point Type Extend the Wacc language with single and/or double-precision floating-point
data types.
```
```
String Manipulation Extend the Wacc language with additional operations on strings, such as
concatenation and substring tests. You may wish to allow for indexing on strings as well, but you
should be careful to ensure string literals remain immutable! There should be ways of both creating
and processing strings.
```

Side-effecting Expressions Extend the expressions of the Wacc language to allow for side-effects.
For example, adding‘+=’,‘++’,‘x = 5’, etc... as valid expressions. You will need to carefully consider
which side-effecting expressions you allow and the semantics of these expressions when composed
with existing ones. Consider how you can best re-use the existing parts of your compiler.

Basic Constant Folding Extend your Wacc compiler with optimisation techniques that evaluate
constant expressions at compile-time rather than runtime. For example, the source code may include
an operation such as‘1 + 2’, which the compiler can evaluate to‘ 3 ’instead. Doing this extension at an
easy level involves operations on the ast or on a single instruction at a time.

Basic Control-Flow Optimisation Extend your Wacc compiler with optimisation techniques that
simplify the source code. For example, a conditional branch statement of the formif false then C 1
else C 2 can be simplified toC 2 , as we know the‘then’branch of the statement will never be executed.
Similar techniques can be applied to while-loops and other statements as well. This simple variant
requires no further complex analysis.

Full Pair Types Extend the Wacc language with a full nested typing for pairs that does not lose type
information for nested pairs.

Local Type Inference Extend your Wacc compiler with support for locally infered types: these are
the return types of functions and the types of local variables at the definition-site (but not function
arguments). mutually exclusive with Global Type Inference

Records Extend the Wacc language with support for new simple user-defined datatypes. These could
be similar to C structs: the types are monomorphic and not polymorphic. You will need to consider how
these are represented and passed around. This is the root of the OOP chain, which you can build on to
raise the difficulty category (potentially up to “very hard”), as follows:

- with encapsulation: add some notion of data-encapsulation to the records, which would necessitate
    some form of methods (i.e. classes).
- with constructors: add some notion of object constructor or record initialisation syntax.
- with static: adding static methods and fields to abstract common object-agnostic behaviours of a
    class.
- with inheritance: allow a simple form of subtype polymorphism which allows one class to extend
    an (possibly abstract) parent, but without overriding any methods. At this level, you should
    consider how this might be useful in the language when there is not yet overriding present.
- with virtual dispatch: improve the inheritance model to allow for overriding of (possibly abstract)
    methods by ensuring that methods are called with dynamic resolution.
- with multiple inheritance: allow for one class to extend many others, you will need to decide how
    to resolve the “deadly diamond of death” problem.

Each of the “with” add-ons do not count as separate extensions for the purposes of the one-extension-
per-group-member requirement.


Imports Extend the Wacc language so that source files can import/include other source files. This
would allow you to define functions in one file and use them in another. You will need to carefully
consider the syntax and semantics of file imports, and any affect this might have on compiling your
assembly code. This may be extended in the following ways (potentially increasing difficulty to “hard”):

- with standard library: extends the Wacc language with some meaningful pre-defined modules –
    what will be most useful to add?
- with parallel compilation: improve your compiler design so that it has the ability to compile
    separate modules in parallel, for a shorter overall compilation time. Think about how you could
    make information available as soon as possible to other compilation units.

Each of the “with” add-ons do not count as separate extensions for the purposes of the one-extension-
per-group-member requirement.

## Medium [approx. 2-4 marks each]

Interpreter Build an interpreter for Wacc into your compiler, which allows you to execute programs
directly without generating assembly. You may wish to try interpreting your frontend’s ast, or perhaps
your backend’s ir. This may be extended with the following add-on:

- with repl: use the interpreter to develop a Read-Eval-Print Loop interface to your compiler –
    you’ll want to think about the features that would be most useful to the end users.

Each of the “with” add-ons do not count as separate extensions for the purposes of the one-extension-
per-group-member requirement.

Pointer Types Extend the Wacc language with a C-style pointer type. You may also find it useful
to provide an address operator (similar to‘&’in C) that returns the address of a variable. This may be
extended with the following add-on:

- with pass-by-reference: extend your pointer to support a syntactic sugar for passing passing
    around pointers to stack-allocated values (see C++).

Each of the “with” add-ons do not count as separate extensions for the purposes of the one-extension-
per-group-member requirement.

Safe Pair Types Extend the Wacc language by closing the unsoundness loophole with pair types: to
do this, track the “true” type of a pair as runtime information so that unsafe coercions as a result of
nested pairs result in a type error at runtime. You should consider how to lift the “nested pair extraction”
and “read into nested pair extraction” restrictions using your new mechanism.

Parametric Polymorphism Extend the Wacc language with parametric polymorphism. That is,
extend your complex types and functions so that they accept type parameters. For example, a list of
integers could be declared as‘list(int)’. There are many different ways of handling generic functions
and structures, and you may wish to consider issues like boxing, erasure, and specialisation^3. You may
wish to experiment:

(^3) The reference compiler deals with the generic nature of pairs via boxing: this is why the reference compiler pairs are
always two full sized words. The Jvm languages use boxing and erasure, but C++ uses template specialisation.


- with specialisation: investigate adding (controlled) specialisation to Wacc, including for pairs,
    which may improve the performance of heavily used functions/data-structures of specific types.
    What can go wrong with this, can you demonstrate cases where this may not be beneficial?

```
Each of the “with” add-ons do not count as separate extensions for the purposes of the one-extension-
per-group-member requirement.
```
```
Stack Allocated Types Extend your Wacc compiler to allow for stack allocation of all types that
were previously limited to the heap. You should consider under what circumstances this would be legal.
You should consider how to distinguish the difference at the syntactic level – how does a language like
C/C++ do it?
```
```
Function Overloading Extend the Wacc language with function overloading. That is, allow for
multiple functions to have the same name, but accept different numbers/types of arguments or return
values. Your compiler should be able to determine which function is most suitable, at compile-time,
according to the static types of its arguments.
```
```
First-Class Functions Extend the Wacc language with the ability to store functions in variables and
pass them as arguments to functions. This is the root of the Functional chain, which you can build on to
raise the difficulty category (potentially up to “very hard”), as follows:
```
- with lambdas and/or nested functions: allows for (non-capturing) functions to be defined within
    other functions, either as full definitions or anonymous lambda functions.
- with by-value closures: nested functions/lambdas can capture variables from their enclosing scope,
    called a closure – these are copied into the closure’s state, as opposed to referring to the parent
    stack frame directly.
- with by-reference closures: closures can capture references to the enclosing scope, allowing them
    to modify their parent stack frame directly. This is much more challenging as you may need to
    break away from the “classic” model of a function-call stack to one where functions keep their
    local data and arguments on a heap-allocated environment record, to which a pointer is kept on
    the stack. Looking up a variable may require traversal down a linked-list of such records, and
    closures keep their parent’s environment records alive even after the parent returns. You would
    need to consider how to manage the memory of these records, as so not create memory leaks.
- with currying: use closures to support a natural currying and partial application.
Each of the “with” add-ons do not count as separate extensions for the purposes of the one-extension-
per-group-member requirement.

Reference Counting Implement a simple form of garbage collection via reference counting on heap
objects. You will need to carefully consider how and when to update the reference counts and when you
will reclaim the corresponding memory. You should be able to discuss the limitations of this approach.
(mutually exclusive with Garbage Collection)


Inlining Extend your Wacc compiler so that it can perform heuristic-guided inlining of function
calls. You should be careful to consider what functions to inline and under what circumstances. Your
compiler must be robust against very large functions. What kinds of heuristics work well, what happens
when they go wrong?

Peephole Optimisation Extend your Wacc compiler with optimisation techniques that simplify the
generated assembly code (e.g. peephole optimisations). Your compiler should be able to analyse your
generated code and remove redundant instructions with a variety of different rules. For example, you
may have a store instruction followed by a load instruction that target the same register from the same
address – the load is redundant and can be removed. You should investigate the effectiveness of the
rules you’ve chosen to implement: did you find any that were (surprisingly) ineffective, why?

Concurrency Extend the Wacc lanuage to allow for the creation of new threads. This isn’t particularly
interesting, so you could raise the difficulty potentially all the way to “very hard” by incorporating the
following elements:

- with sync: add support for simple synchronisation methods, like locks.
- with high-level control structures: introduce syntactic functionality to help control fork/joining,
    without explicit intervention.
- with message passing: introduce a way for threads to communicate with each other safely, perhaps
    by using channels/pipes.
- with green threads: introduce a more comprehensive framework for managing concurrent work-
    loads, like co-routines/green threads. You should draw inspiration from other languages. You
    may need to implement features like‘sleep’or asynchronous file IO to demonstrate the benefits.

Each of the “with” add-ons do not count as separate extensions for the purposes of the one-extension-
per-group-member requirement.

```
∗ The “Law of Diminishing Returns” no longer applies below this point∗
```
Cross-Compiler Extend your Wacc compiler with the ability to compile Wacc programs to an
alternative target architecture (e.g. aarch64,x86-64, orRISC-V). Note, that llvm does not count
towards this. Supporting more than one extra target will be worth more, but less than the first.

Local Static Analysis Extend your Wacc compiler so that it performs static analysis to help catch
normally runtime problems at compile-time, and ideally remove the associated runtime checks by just
looking at the information within a function. This might be something like array-bound analysis to
remove length checks, tracking the current resident type inside a pair to prevent unsound coercions,
identifying division-by-zero, null pointer use, and so on. Such techniques will not be able to catch
all issues: consider carefully what information you can determine at compile-time and when that
information is made stale and therefore invalid.


Control-Flow and Data-Flow Optimisations Extend your Wacc compiler will more fully-fledged
analyses and optimisations based on classic compiler algorithms and techniques, building on top of a
control-flow graph:

- Perform control-flow optimisation by removing indirect branches, unreachable code, and so on.
- Perform constant propagation to identify where variables contain known constants and inline
    them directly to their use-sites. To take this further, simplify resulting ir instructions and
    iteratively continue with this process. This will require something like use-defs analysis.
- Use liveness information about the program to improve the allocation of variables using techniques
    like graph colouring. What about when you run out of cheap registers, what heuristics for
    determining what to put into stack work well? Can liveness help you identify opportunies to
    remove unneeded instructions?
- Identify where one variable can be substituted for another, removing redundant copies (copy
    propagation).

You should consider the impact of your optimisations: did they perform how you expected? How do
they interact with each other?

Tail-Recursion Optimisation Extend your Wacc compiler so that it can optimise tail-recursive
functions so that they act more like loops (like Scala’s@tailrecannotation ensures). You will not need
to consider arbitrary tail-calls at this level, or mutually recursive functions. To raise this extension to
hard difficulty, you may also consider:

- with general tail-calls: extend this functionality to not just work for self-recursive functions.
    You should carefully consider how to manage the different stack frame sizes between different
    functions and ensure that no stack-space is leaked from a tail-recursive set of functions.

Each of the “with” add-ons do not count as separate extensions for the purposes of the one-extension-
per-group-member requirement.

Ad-hoc Polymoprhism Extend your Wacc compiler so that it can support ad-hoc polymorphism,
which is commonly implemented via typeclasses. This extension builds on top of Parametric Poly-
morphism to allow for generic types to access methods available by implicit resolution of an “instance”
(this might, for instance, be a struct full of function pointers). You may wish to consider the effect of
specialisation on highly ad-hoc polymorphic programs.

Global Type Inference Extend your Wacc compiler with type inference for function arguments as
well as variables and return types. You should be able to deduce the types of a function from how it is
used internally and externally (so long as you have not introduced true subtyping!). The types deducted
here should all be monomorphic, which is to say known types likeint,char, and so on. However, to
raise difficulty to “hard”, you may consider:

- with polymorphic inference: you should be able to infer the most general type of a function to
    make it reusable (even if the underlying language doesn’t have a way of writing parametrically
    polymorphic functions already!)

Each of the “with” add-ons do not count as separate extensions for the purposes of the one-extension-
per-group-member requirement.


Lazy Evaluation and Pass-by-Name/Need Extend your Wacc compiler with laziness. You should
want to support different kinds of laziness, such as by-name parameter passing, where the entire
expression passed is evaluated every time it is used in the body (Scala’s=>Atype), or Haskell’s by-need
evaluation, which only computes each argument once on demand. You may wish to look into thunks.
To take this extension further and lifting it into the “hard” category, you should also consider;

- with lazy datastructures: how can pairs, arrays, etc be made lazy?
- with lazy bindings: how can local variables within a function be made lazy?
- with knot-tying: how can self-referential datastructures be constructed “in-place”?

Each of the “with” add-ons do not count as separate extensions for the purposes of the one-extension-
per-group-member requirement.

## Hard [approx. 4-6 marks each]

Debugging Symbols Extend your compiler with a flag that allows for the generation of debugging
symbols into the assembly file: this will allow a tool (you may wish to specialise to gdb) to properly
link the assembly back to the source file, including allowing for the tracking of variables and dumping
a functions stack frame usefully. It should allow for the user of wacc to debug their own programs, as
opposed to just a tool for the compiler-writer to debug their own errant code-generators.

Dynamic Typing Change the Wacc language so that it is dynamically typed rather than statically
typed. You will need to carefully consider how this will affect the syntax and semantics of the language.
In particular, you will probably find that you have a lot more work to do at run-time.

Exceptions Extend the Wacc language with an exception handling system. You will need to care-
fully consider the syntax for generating/catching excpetions and the semantics of caught/uncaught
exceptions.

Garbage Collection Extend the Wacc language with dynamic garbage collection on the heap. You
will need to carefully consider how you detect garbage and how you arrange for its collection. To take
this further into the “Very Hard” category, you may wish to explore:

- with generations: implement a garbage collector that makes use of generational sub-heaps, so
    that short-lived objects are processed at a higher-rate than long-lived ones. You could assess
    the impact this has on programs of different memory use requirements – how effective is it in
    practice?
- with parallelism: instead of performing garbage collection during object allocation, you could
    build a parallel garbage collector that performs collection in the background. Does this produce
    any new complications or bottlenecks?

Each of the “with” add-ons do not count as separate extensions for the purposes of the one-extension-
per-group-member requirement. Mutually exclusive with Reference Counting


```
# computes x^n but on a statically known n
code(int) stagedPower(int n, code(int) qx) is
if n == 0 then return [| 1 |]
else
code(int) powerNMinus1 = call stagedPower(n-1, qx);
return [| $qx * $powerNMinus1 |]
fi
end
# the definition of square will expand to`return x * (x * 1)' by recursive splicing
int square(int x) is return $(stagedPower(2, [|x|])) end
```
```
Figure 1: Staged power example
```
## Very Hard / Insane [approx. 6-10 marks each]

```
Global Static Analysis Extend your Wacc compiler with more advanced static analysis that attempts
to perform more aggressive static analysis across the whole-program. It should be able to catch a wider
range of issues by considering the interactions between multiple functions. You may find it helpful
to introduce static annotations like@unaliased,@const, or@nonnullto allow the user to inject extra
information into your system that can be verified within the function or at call-site – or even try and
infer these properties from the function or callers themselves! To be considered at this difficulty level,
this work must be extensive!
```
Staged Meta-programming Support As opposed to simple C-like macros, some language have
support for full metaprogramming: this is where code can itself serve as a first-class value. Such lan-
guages usually require an Interpreter, as code of the language being compiled will need to be executed
at compile-time to generate more code. With this model it is possible to construct ast fragments as
values using quasi-quotations: for instance, the expression‘[| 4 + 6 |]’^4 of type‘code(int)’would rep-
resent the treeAdd(IntLit(4), IntLit(6)). These code fragments can be combined using anti-quotes
(also called splices): givenqidxof type‘code(int)’andqarrof type‘code(char[])’, the expression
‘[| $qarr[$qidx] |]’would have type‘code(char)’and would represent an index operation: the types
will need to have been checked. When a splice is found outside of a quote, this signals that the compiler
should execute the Wacc code inside the splice at compile-time with the interpreter to produce an
ast to include at this point. This mechanism is used to create very specialised code that can optimise
in ways a general-purpose compiler cannot. The “hello world” example of staging, the staged power
function, could look something like the code in Figure 1.
You would need to ensure that variables are only quoted when they are runtime variables and not
compile-time ones. An exception to this rule is for “liftable” values: for instance you should be able to
quote a compile-time variable‘x’if it has type‘int’, because it is easy to construct the corresponding
‘IntLit’ value.

```
Implement aWacc compiler usingWacc Write a compiler for the Wacc language in the Wacc
language. Be aware that this is no easy task, and would likely incorporate many of the extensions listed
above (such as generic types, classes with inheritance and a full C-style heap). We will, however, be
very impressed if a group can manage this.
```
(^4) This is Template Haskell syntax, some languages, like Scala 3, choose a syntax like ‘’{4 + 6}’ instead.


## Project Report

Write a short report (seven pages maximum)^5 providing a summary of your project. This report must
use 11pt font with 2.5cm page margins. Reports need not reach this limit: we expect concise but
thoughtful writing throughout – overly verbose/generic reports will be limited in their maximum grade.
The main goal of writing your report is to promote reflection on your experiences during the Wacc
lab. Assessment will focus on the professionalism and critical analysis you are able to demonstrate. You
should be honest about the shortcomings of your work and provide a balanced and critical discussion
throughout.
The report should be divided into 4 compulsory sections:

- Reflection on Final Product: you should reflect critically on the following aspects of your
    compiler as a piece of production software and discuss:
       - The usability of your compiler and the effectiveness of its errors.
       - The correctness of your compiler: how can you be sure about this?
       - The robustness of your compiler and whether there are any pathological programs that
          might impact your compile times.
       - Now that you have built your own compiler, what things you now appreciate more about
          other compilers that you have worked with over the past two (or more) years.

```
Strong discussion will address most of the above prompts with insightful commentary and demon-
stration of new knowledge. You should limit this discussion to 1–1½ sides of A4 maximum:
excessively verbose or unfocused discussion may limit your marks in this section.
```
- Reflection on Project: you will need to reflect on your engagement with the project as a whole.
    This may be evidenced either through a section in the report of roughly 250 words, or through
    participation in an anonymous survey (this will not be included in your page limits, for fairness).
    The written section should be a reflection of your experience working on Wacc, discussing
    the teams communication, organisation, and how you engaged with course resources, like the
    laboratory hours. You should discuss the positive learning moments you will carry forward, and
    things you would look to improve in future.
    Alternatively, the anonymous survey forms part of a collaborative research study with two other
    Russell group universities aimed at understanding student engagement in Computer Engineering
    programmes; the purpose of the study is to explore perceptions of engagement in Computing
    courses and to provide insight into how we can improve engagement and student learning. Your
    honest input is appreciated and may help inform future enhancement of the course.
- Design Decisions: this section evaluates the architectural quality of your compiler and your
    ability to reason about its design and evolution. Firstly:
       - At the start of this project, you were asked to carefully consider the trade-off between
          flexibility and safety when designing your AST. This involved considering how many ASTs
          you had, how specific they were, or even experimenting with parametricity. Discuss:
             ∗ Where your design fell in this space.

(^5) This is generous, as the discussion in the report has been reworked this year: you hopefully shouldn’t need it all!


```
∗ Now that your work is completed, what you would change if you could go back.
∗ Conversely, how did your design help you during your implementation.
Be specific.
```
```
Furthermore, you should consider some of the following topics:
```
- How you chose your compiler’s host language: what did this language help you accomplish
    cleanly? Is there anything the language made harder to accomplish? (Focus on the language
    itself, rather than its tooling)
- Any sufficiently large project accrues technical debt;
    ∗ Were there any early poor design decisions that impacted later development?
    ∗ How easy was integrating your extensions into the system? What did your design
       facilitate/rule out?
- What efficiency considerations did you make when writing your compiler? What could be
    improved?
- How well separated were the major interfaces and components of the system (frontend,
    backend, etc)?
- What would you improve about your overall system design with hindsight? What impact
    might this have had?

```
Strong discussion will address at least three above prompts with insightful commentary and
depth. You should limit this discussion to 2–2½ sides of A4 maximum: excessively verbose or
unfocused discussion may limit your marks in this section. The very best discussions will offer
insight into evolution of the codebase and the knock-on effects of the design and demonstrate
clear system design thinking.
```
- Experimental Evaluation: the last core section should focus on a critical experimental evalua-
    tion of one of your optimisation extensions. You should explore how your optimisation performs
    in practice, and if relevant discuss different heuristics you developed and the impact they had on
    different kinds of programs.
    You should present quantative results with at least one clearly labelled graph or structured results
    table (more may be placed into an appendix, though markers will not be required to read it
    unless they are interested). The axes/titles must be clearly labelled and the results explained
    clearly. You should also clearly outline your methodology and setup for your experiment (what
    architecture/machine/environment, how might we replicate your findings?).
    Good discussion will then focus on a clear critical analysis of the results: optimisations rarely
    work perfectly all the time, you should be able to identify where it doesn’t perform as well as
    expected (or makes things slower!). Stronger discussion will then offer clear hypotheses for the
    results (including bad ones).
    The strongest discussion will be able to critically evaluate the shortcomings of your approach,
    including the effects of hardware, limitations with benchmarking setup, bias in your test programs;
    they would also be able to provide commentary on what your findings mean for your compiler
    and even generalise your findings further to other languages.


```
It is expected that you build on solid foundations: weaker early discussion may limit the achieve-
ment of the overall section. The discussion of your experiment should be limited to 3 sides of
A4 maximum, remembering that concise discussion is favoured. You may be limited by poor
choice of optimisation here, so be mindful: marks are not awarded for “impressive speedups”,
rather balanced analysis – a small optimisation can achieve full marks if analysed deeply.
```
Appendix While not directly assessed, you must provide an appendix of your report that provides
brief details of all the extensions you have implemented (and if relevant the flags needed to enable
them). This will help inform your Technical Challenge mark, and direct the discussion of your work in
the code review. You should keep this clear and concise. Extensions missed here may not be considered
for credit.
Ideally, your extensions should not break the core behaviour of the Wacc language, or your compiler,
although in some cases this may reasonably not be the case. If you know that certain tests will fail as a
necessary result of your extensions, detail this in your appendix.


## Submission

```
As you work, you should add, commit and push your changes to your Git repository. Your GitLab
repository should contain all of the source code for your program. You should ensure that this includes:
```
- Any files required to build your compiler, along with theMakefileandcompilescript as before.
- A configuration text filewacc.targetin the root directory which specifies your compiler’s target
    architecture.
LabTS can be used to test any version of your work. However, you will still need to submit a commit
id to Scientia so that we know which version of your code you have submitted as final: do not leave
this to the last minute, you can always resubmit pre-deadline.
Prior to submission, you should check the state of your GitLab repository using LabTS (https:
//teaching.doc.ic.ac.uk/labts): if you click through to yourwacc_<group>repository you will see
a list of the different versions of your work that you have pushed. Next to each commit you will see a
link to that commit on GitLab as well as a button to submit that version of your code to Scientia.
You should submit to Scientia the version of your code that you consider to be “final”. You can
change this later by submitting a different version to Scientia. The submission button will be replaced
with a confirmation message if the submission has been successful.
You should submit the chosen version of your code and your project report (report.pdf) to Scientia
by 19:00 on Friday 13th March 2026. If you were part of a JMC group that received an extension for
Backend, this will be carried forward here too.

## Assessment

```
In total there are 50 marks available in this milestone. These are allocated as follows:
```
```
Compiler Extensions 30
Technical Challenge 10
Functionality/Correctness 10
Design Quality 10
Project Report 20
Reflection on Product 3
Reflection on Project 1
Design Decisions 8
Evaluating an Optimisation 8
This milestone will constitute 20% of the marks for the Wacc Compiler exercise. Your work will be
assessed by an interactive code review session in the week beginning on Monday 18th March, where
personalised feedback will be given to your group.
```

