# TheWaccCompiler: Milestone 2

# Compiler Backend

### COMP50007.1 - Laboratory 2

### Department of Computing

### Imperial College London

## Summary

```
So far in theWaccproject you have only built the frontend of yourWacccompiler. For this project
milestone, you will now implement a full compiler for theWacclanguage. That is, you will add a code
generator backend to yourWacccompiler for one of theaarch64,arm32orx86-64architectures (you
may choose which).
```
## Details

```
Recall the four stages of the compilation process:
```
1. Perform Syntactic Analysis:optionally tokenising, and then parsing the string/tokens into an
    abstract syntax tree representing the source language.
2. Perform Semantic Analysis:working out and ensuring the integrity of the meaning of the
    input file.
3. Generate Target:synthesizing output in the target language, maintaining the semantic meaning
    of the source program.

You should already have built the frontend of yourWacccompiler, which should handle the first two
stages. This means that you should be able to perform syntactic analysis and semantic analysis on
Waccprograms to determine if they are valid. You should also be able to detect invalidWaccprograms
and report appropriate error messages: throughout this milestone this behaviour must be preserved!
For this milestone you need to work on the backend of your compiler, the code generation stage.
Your compiler must be able to produce assembly code, for one of theaarch64(armv8-a),arm32(armv6)
orx86-64architectures (you may choose which syntax) when given a validWaccprogram. It is likely
that you may need to slightly modify your parser and/or semantic analyser so that they produce suitable
output for the code generator.
As with the previous milestone, you will probably find it useful to refer to the reference implemen-
tation of theWacccompiler. The reference compiler generates somesimplisticassembly code for each
input file that it successfully parses. Looking at these output files should help you design the output for
your ownWacccompiler. Technically, the code it is generating is a combination of stack and register,
which aims to eliminate the redundant pushes and pops later: you shouldn’t try and model it exactly.


```
Important! During this milestone of the project you should be concentrating on thefunctional
correctnessof your compiler. The code does not need to be optimised for speed, however groups
that will be undertaking theextensionsmilestone will be required to develop at least one kind of
optimisation later. You should plan ahead for this: a purely stack-based approach now will make
this more difficult; forJMCs, however, I recommend developing a simpler stack machine.
```
## Submit by 19:00 on Friday 28th February 2025

## What To Do:

1. Familiarise yourself with the tools for assembling and emulating assembly code on the lab ma-
    chines. The tools that you will need to use here will depend on your choice of target architecture.

```
ARM aarch64 : 64-bit ARM code will need to be cross-compiled for its intended target archi-
tecture. We have provided you with agcccross-compiler that will assemble and linkaarch
assembly programs accordingly. This is installed on the lab machines asaarch64-linux-gnu-gcc.
You can run the cross-compiler to generate code suitable for anyarmv8-aprocessor as follows:
prompt> aarch64-linux-gnu-gcc -o EXEName -z noexecstack -march=armv8-a ARMCode.s
```
```
We have also provided you with an ARM emulator which allows you to run these assembled
programs. This is installed on the lab machines asqemu-aarch64. You can run the ARM emulator
for aarmv6processor as follows:
prompt> qemu-aarch64 -L /usr/aarch64-linux-gnu/ EXEName
```
```
ARM arm32 : 32-bit ARM code will also need to be cross-compiled before it can be emulated,
but a different cross-complier to the one above is needed for this. We have provided you with
agcccross-compiler that will assemble and linkarm32assembly programs accordingly. This
is installed on the lab machines asarm-linux-gnueabi-gcc. You can run the cross-compiler to
generate code suitable for anyarmv6processor as follows:
prompt> arm-linux-gnueabi-gcc -o EXEName -z noexecstack -march=armv6 ARMCode.s
```
```
We have also provided you with an ARM emulator which allows you to run these assembled
programs. This is installed on the lab machines asqemu-arm. You can run the ARM emulator for
aarmv6processor as follows:
```
```
prompt> qemu-arm -L /usr/arm-linux-gnueabi/ EXEName
```
```
x86-64 : 64-bitx86code (in Intel or AT&T syntax) can be assembled viagccas follows:
```
```
prompt> gcc -o EXEName -z noexecstack X86Code.s
The resulting program can then be run directly on anx86system (which all Lab Linux machines
are) via the command-line as:
prompt> ./EXEName
```

```
Regardless of your target architecture, we suggest that you copy some of the code generated
by the reference compiler for a simple program in that architecture and run this though the
assembly and emulation instructions described above. You should be using these tools to test
your generated code during this milestone.
```
2. Get your CI working with your chosen toolchain: you will need to installboththe emulator and
    compiler. Your integration tests will now need to run the generated assembly and compare its
    behaviour against the specificationfound in each file:do not compare against the reference
    compiler in CI. You are responsible for sorting this test suite out and the parsing of specification.
3. Write the code generator for yourWacccompiler. Your code generator needs to pass over your
    internal representation of the input program to construct assembly code that implements the
    desired behaviour. The input to this phase will be the final generated structure from your frontend.
    You will need to be mindful of proper abstraction and separation of concerns throughout.

On completing this milestone, you should have a full compiler for the wholeWacclanguage.

## Testing:

Your compiler will be tested by an automated script which will first build your compiler:

```
prompt> make
```
and then run your compiler on each exampleWaccprogram

```
prompt> ./compile PATH/FILENAME.wacc
```
before assembling and executing (or emulating) the resulting assembly code via the process described
above for your target architecture.
Themakecommand should build your compiler and thecompilecommand should call your compiler
on the supplied file. You must therefore provide aMakefilewhich builds your compiler and a front-end
commandcompilewhich takes the path to a fileFILENAME.waccas an argument and runs it through
your compiler, either successfully producing assembly code for your target architecture in the file
FILENAME.s(at the root level of your repository), or generating error messages as appropriate.
As with the previous milestone, your compiler should generate the appropriate error code for the
given input: 100 for syntax error, 200 for semantic error, and 0 for valid programs.

Important! If compilation fails due to syntax or semantic errors, no code should be emitted.

The behaviour of your generated code should match the input/output behaviour of the code
generated for the reference compiler. For instance, the automated test program may run:

```
prompt> ./compile waccExamples/valid/print/print.wacc
```
and expect to successfully parse the input fileprint.waccreturning the exit status 0 and generating an
assembly program in the fileprint.sin the current working directory of the compiler. LabTS will then
assemble and emulate your generated code (you shouldn’t do this yourself ), which produces the output:

```
Hello World!
```
with exit status 0.


```
LabTS will also run your compiler over programs that are known to be ill-formed, e.g.:
```
```
prompt> ./compile waccExamples/invalid/syntaxErr/basic/skpErr.wacc
```
In this case it will expect the compilation to fail with exit status 100 and an error message. Since the
compilation process failed, we will not try to assemble or emulate any code.
As usual, if you visithttps://teaching.doc.ic.ac.uk/labtsand go to yourwacc_<group>repos-
itory you will see a list of the different versions of your work that you have pushed. Next to each
commit you will see a button that will allow you to request that this version of your work is run through
the automated test process for the different milestones of the project.

Important! In order for us to be able to test your generated assembly code, LabTS will need to know
which architecture you are targetting. To specify this, you must to provide aplain text configuration
filecalledwacc.targetin the root level of your project repository. This file should contain asingle
stringthat specifies your target architecture, one of:aarch64,arm32orx86-64(for Intel or AT&T
syntax). If no such file is provided, then we will assume that your target architecture is **aarch**.

Important! Code that fails to compile and run will be awarded0 marksfor functional correctness!
You should be periodically (but not continuously) testing your code on LabTS. If you are experiencing
problems with the compilation or execution of your code then please seek help as soon as possible.

## Debugging ARM Assembly Code

If you want to debug your generated ARM assembly code, then we recommend doing this viagdb.
You will need to installgdb-multiarchon your machine, then you can emulate your ARM code (as
described above) adding-g <port>to the command options. Then you can run GDB on this code as
follows:

```
prompt> gdb-multiarch
gdb> target remote localhost:<port>
```
You will need to replace<port>with a port number of your choice, e.g. 1234.


## WaccCompiler Reference Implementation

```
To help you with this lab, we have provided you with restricted access to a reference implementation of
aWacccompiler. You can find a web interface to the reference compiler at:
```
- https://wacc-vm.doc.ic.ac.uk.

We have also provided you with a runnable JARwacc-reference-cli.jarthat provides command-line
access to the web interface. You should ensure it is up-to-date by runningjava -jar wacc-reference-cli.jar
update. A full user guide for the reference compiler is included in thewacc-examplesGitLab repository
and can also be found online.
You can view the code generated by the reference compiler by running it with the--print-assembly
flag. You donotneed to generate exactly the same code as the reference compiler (it would be a waste
of time to try and figure out how). You should, however, ensure that your generated code has the same
input/output behaviour when assembled and executed/emulated. You can get the reference compiler to
assemble and execute the code it generates by running it with the-x(or--execute) flag.
Note that (as before) we arenotexpecting your compiler to handle options flags, we will just be
running yourcompilescript as discussed above.

## Additional Help Getting Started

```
You will need to ensure that your parser generates a tree describing the structure of the program which
has been parsed. It is likely that you have already done this in the previous milestone, but you may
need to go back and add missing information or fix bugs first.
Your code generator will almost certainly take the form of a recursive pattern matching function.
It is a good idea to start with simple programs that use the exit system call to return values, rather
than trying to implement print statements straight away.
We recommend that, at least initially, you avoid worrying about optimising your generated code.
This milestone is principally concerned with thefunctional correctnessof the code you generate. Of
course, thinking about any future optimisations you will want to make is a good idea so that you can
make sure that your code is designed with such extensions in mind.
Some other general points to consider:
```
- If you discover that some parts of your code from Milestone 1 do not fit with the requirements of
    this milestone, do not hesitate to apply significant modifications to your previous work.
- As before you are advised to implement iteratively. Donottry to implement every feature in one
    session. Applythin-slicingacross the compiler to build up incrementally.
- As with all labs in the second year, time management is key. Donotleave everything until the
    final week, you willnotbe able to complete the work in time: this is the number 1 milestone
    where students start failing tests because they just couldn’t get everything done on time – use
    the first week!
If you want to assemble and emulate your generated code on your own machine, then you will need
to install the appropriategcccross-compiler (aarch64-linux-gnu-gccorarm-linux-gnueabi-gcc)
and ARM emulator (qemu-aarch64orqemu-arm). On Linux, these programs can be installed from the
gcc-aarch64-linux-gnu,gcc-arm-linux-gnueabi,qemuandqemu-userpackages.


## Submission

```
As you work, you shouldadd,commitandpushyour changes to your Git repository. Your GitLab
repository should contain all of the source code for your program. You should ensure that this includes:
```
- Any files required to build your compiler, along with theMakefileandcompilescript as before.
- A configuration text filewacc.targetin the root directory which specifies your compiler’s target
    architecture.
LabTS can be used to test any version of your work. However, you will still need to submit acommit
idto Scientia so that we know which version of your code you have submitted as final:do not leave
this to the last minute, you can always resubmit pre-deadline.
Prior to submission, you should check the state of your GitLab repository using LabTS (https:
//teaching.doc.ic.ac.uk/labts): if you click through to yourwacc_<group>repository you will see
a list of the different versions of your work that you have pushed. Next to each commit you will see a
link to that commit on GitLab as well as a button to submit that version of your code to Scientia.
You should submit the chosen version of your code by 19:00 on Friday 27th February 2026; groups
with JMC members will receive extensions similar to PintOS Task 2.

## Assessment

```
In total there are 80 marks available in this milestone. These are allocated as follows:
```
```
Functional Correctness 20
Expressions and statements 9
Datastructures 4
Functions 4
Errors 1
Frontend correctness (invalid programs) 2
Design, Style and Readability 60
Intermediate Representation Quality 12
Code Generator Design 16
Pre-defined Functions/Widget Abstraction 7
Assembly Formatter/Printer Design 7
Git Use 3
Continuous Integration and Testing 4
Overall Project Quality 10
LabTS Mark 1
This milestone will constitute 40% of the marks for theWaccCompiler exercise. Your work will be
assessed by an interactive code review session during the week beginning on the week following the
deadline, where personalised feedback will be given to your group. JMC groups with 8-day extensions
will have their code review on the following Monday.
```
```
LabTS Mark For this milestone, the carrot mark requires you tolegitimatelypass at least one
test each fromvalid/basic/exitandvalid/IO/print7 days before your group’s backend deadline.
```

