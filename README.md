# NUMEX-Interpreter
Creating a sample interpreter with Racket.

Design and Implementation of Programming Languages
Project Specification

Here is the definition of NUMEX's syntax:

- If 𝑠 is a Racket string, then (var 𝑠) is a NUMEX expression (variables).
- If 𝑛 is a Racket integer, then (num 𝑛) is a NUMEX expression (number constants).
- If 𝑏 is a Racket boolean, then (bool 𝑏) is a NUMEX expression (boolean constants).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (plus 𝑒1 𝑒2) is a NUMEX expression (addition).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (minus 𝑒1 𝑒2) is a NUMEX expression (subtraction).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (mult 𝑒1 𝑒2) is a NUMEX expression (multiplication).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (div 𝑒1 𝑒2) is a NUMEX expression (division).
- If 𝑒1 is a NUMEX expression, then (neg 𝑒1) is a NUMEX expression (negation).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (andalso 𝑒1 𝑒2) is a NUMEX expression (logical conjunction).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (orelse 𝑒1 𝑒2) is a NUMEX expression (logical disjunction).
- If 𝑒1, 𝑒2, and 𝑒3 are NUMEX expressions, then (cnd 𝑒1 𝑒2 𝑒3) is a NUMEX expression. It is a
condition where the result is 𝑒2 if 𝑒1 is true, else the result is 𝑒3. Only one of 𝑒2 and 𝑒3 is
evaluated.
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (iseq 𝑒1 𝑒2) is a NUMEX expression.
(comparison).
- If 𝑒1, 𝑒2, and 𝑒3 are NUMEX expressions, then (ifnzero 𝑒1 𝑒2 𝑒3) is a NUMEX expression.
It is a condition where the result is 𝑒2 if 𝑒1 is not zero, else the result is 𝑒3. Only one of
𝑒2 and 𝑒3 is evaluated.
- If 𝑒1, 𝑒2, 𝑒3, and 𝑒4 are NUMEX expressions, then (ifleq 𝑒1 𝑒2 𝑒3 𝑒4) is a NUMEX
expression. It is a conditional where the result is 𝑒4 if 𝑒1 is strictly greater than 𝑒2, else
the result is 𝑒3. Only one of 𝑒3 and 𝑒4 is evaluated.
- If 𝑠1 and 𝑠2 are Racket strings and 𝑒 is a NUMEX expression, then (lam 𝑠1 𝑠2 𝑒) is a
NUMEX expression (a function). In 𝑒, 𝑠1 is bound to the function itself (for recursion) and
𝑠2 is bound to the only argument. Also, (lam null 𝑠2 𝑒) is allowed for anonymous
nonrecursive functions.
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (apply 𝑒1 𝑒2) is a NUMEX expression (function
application).
- If 𝑠 is a Racket string, and 𝑒1 and 𝑒2 are NUMEX expressions, then (with 𝑠 𝑒1 𝑒2) is a
NUMEX expression (a let expression where the value of 𝑒1 is bound to 𝑠 in 𝑒2).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (apair 𝑒1 𝑒2) is a NUMEX expression (pair
constructor).
- If 𝑒1 is a NUMEX expression, then (1st 𝑒1) is a NUMEX expression (the first part of a
pair).
- If 𝑒1 is a NUMEX expression, then (2nd 𝑒1) is a NUMEX expression (the second part of a
pair).
- (munit) is a NUMEX expression (holding no data, much like () in ML or null in Racket).
Notice (munit) is a NUMEX expression, but munit is not.
- If 𝑒1 is a NUMEX expression, then (ismunit 𝑒1) is a NUMEX expression (testing for
(munit)).
- (closure 𝑒𝑛𝑣 𝑓) is a NUMEX value where 𝑓 is a NUMEX function and 𝑒𝑛𝑣 is an
environment that maps variables to values. Closures do not appear in programs; they
result from evaluating functions.
- If 𝑠1 is a Racket string and 𝑠2 is a Racket string and 𝑒1 is a NUMEX expression and 𝑒2 is
NUMEX expression and 𝑒3 is a NUMEX expression, then (letrec 𝑠1 𝑒1 𝑠2 𝑒2 𝑒3) is a
NUMEX expression (a letrec expression for recursive definitions where the the value of
𝑒1 is bound to 𝑠1 and the value of 𝑒2 is bound to 𝑠2 in the 𝑒3).
- If s is a Racket string and e is a NUMEX expression, then (key s e) is a NUMEX
expression (key contructor).
- If k is a NUMEX key and m is a NUMEX munit, then (record k m) is a NUMEX
expression (record contructor).
- If k is a NUMEX key and r is a NUMEX record, then (record k r) is a NUMEX
expression (record constructor).
- If s is a Racket string and r is a NUMEX record, then (value s r) is a NUMEX expression
(value of string in record).

A NUMEX 𝑣𝑎𝑙𝑢𝑒 is a NUMEX number constant, a NUMEX boolean constant, a NUMEX closure, a
NUMEX munit, or a NUMEX pair of NUMEX values. Similar to Racket, we can build list values out
of nested pair values that end with a NUMEX munit. Such a NUMEX value is called a NUMEX list.
You should 𝑛𝑜𝑡 assume NUMEX programs are syntactically correct (e.g., things like (num "hi")
or (num (num 37)) must be handled). And do 𝑛𝑜𝑡 assume NUMEX programs are free of type
errors like (plus (munit) (num 7)), (1st (num 7)) or (div (bool #t) (num 2)).

Problems:
1. Warm-Up

(a) Write a Racket function racketlist->numexlist that takes a Racket list, which may
even be a list of NUMEX values, and produces a NUMEX list with the same elements in
the same order.

(b) Write a Racket function numexlist->racketlist that takes a NUMEX list and
produces a Racket list with the same elements in the same order.

2. Implementing NUMEX

Write an interpreter for NUMEX. It should be a Racket function eval-exp that takes a
NUMEX expression e and either returns the NUMEX value that e evaluates to under the
empty environment or calls Racket's error if evaluation encounters a run-time NUMEX type
error or unbound NUMEX variables.

An NUMEX expression is evaluated in an environment (for evaluating variables, as usual).
In your interpreter, use a Racket list of Racket pairs to represent this environment
(which is initially empty) so that you can use the envlookup function, after completing
it. Here is a description of the semantics of NUMEX expressions:

- All values (including closures) evaluate to themselves. For example, (eval-exp
(num 17)) would return (num 17), not 17.
- A variable evaluates to the value associated with it in the given environment.
- An arithmetic operation (addition, subtraction, multiplication, and division) evaluates
to the result of what its operands evaluate to. Note: the operands must be numbers.
- A logical operation (andalso and orelse) evaluates to the result of what its operands
evaluate to. Note: short-circuit evaluations are desired, and the operands must be
booleans.
- A negation (neg 𝑒) evaluates to the opposite (negation) of what 𝑒 evaluates to.
Note: 𝑒 can be a number or a boolean.
- For (cnd 𝑒1 𝑒2 𝑒3), the expression 𝑒1 first evaluates to a boolean value. If the
resulting value is (bool #t), the whole expression evaluates to what 𝑒2 evaluates to.
The expression evaluates to the value of 𝑒3 otherwise.
- The evaluation of (iseq 𝑒1 𝑒2) involves the evaluation of 𝑒1 and 𝑒2. The resulting
values is (bool #t) if the value of 𝑒1 equals to the value of 𝑒2. Otherwise, the
expression evaluates to (bool #f). Note: 𝑒1 and 𝑒2 can be numbers/booleans.
- For (ifnzero 𝑒1 𝑒2 𝑒3), the expression 𝑒1 first evaluates to a value. If the resulting
value is not zero, the whole expression evaluates to what 𝑒2 evaluates to. The
expression evaluates to the value of 𝑒3 otherwise.
- The evaluation of (ifleq 𝑒1 𝑒2 𝑒3 𝑒4) involves the evaluation of 𝑒1 and 𝑒2. If the value
of 𝑒1 is strictly greater than the value of 𝑒3, then it evaluates to the value of 𝑒4.
Otherwise, 𝑒3 must be evaluated.
- For (with 𝑠 𝑒1 𝑒2), the expression 𝑒2 evaluates to a value in an environment
extended to map the name 𝑠 to the evaluated value of 𝑒1.
- Functions are lexically scoped in the sense that a function evaluates to a closure
holding the function and the current environment.
- For (apply 𝑒1 𝑒2), the expression 𝑒1 first evaluates to a value. If the resulting value is
not a closure, an error should be arisen. Otherwise, it evaluates the closure's
function's body in the closure's environment extended to map the function's name
to the closure (unless the name field is null) and the function's argument to the
result of the evaluation of 𝑒2.
- The (apair 𝑒1 𝑒2) construct makes a (new) pair holding the results of the
evaluations of 𝑒1 and 𝑒2.
- If the result of evaluating 𝑒1 in (1st 𝑒1) is an apair, then the first part is returned.
Otherwise, it returns an error. Similarly, the evaluation of (2nd 𝑒1) is the second
part of the given pair.
- For (ismunit 𝑒1), the expression 𝑒1 first evaluates to a value. If the resulting value is
a munit expression, then the result is the NUMEX value (bool #t), else it is the
NUMEX value (bool #f).
- For (letrec 𝑠1 𝑒1 𝑠2 𝑒2 𝑒3) the expression 𝑒3 evaluates to a value in an environment
extended to map the name 𝑠1 to the evaluated value of 𝑒1 and the name 𝑠2 to the
evaluated value of 𝑒2.
- If s is a Racket string and the result of evaluating e is a NUMEX expression, the (key
s e) construct makes a key holding corresponding value of s which is e. Otherwise, it
returns an error.
- If the result of evaluating k is a key and the result of evaluating m is a munit, the
(record k m) construct makes a record holding the result of the evaluation of k and
the result of the evaluation of m. Otherwise, it returns an error.
- If the result of evaluating k is a key and the result of evaluating r is a record, the
(record k r) construct makes a record holding the results of the evaluation of k and
the evaluation of r. Otherwise, it returns an error.
- If the result of evaluating s is a string and the result of evaluating r is a record, the
(value s r) returns corresponding value of s in r. If s does not exist in r, the (value s
r) returns (munit). Otherwise, it returns an error.

3. Extending the Language

NUMEX is a small language, but we can write Racket functions that act like NUMEX macros so
that users of these functions feel like NUMEX is larger. The Racket functions produce NUMEX
expressions that could then be put inside larger NUMEX expressions or passed to evalexp.
In implementing these Racket functions, do not use closure (which is used only
internally in eval-exp). Also do not use eval-exp (we are creating a program, not
running it).

(a) Write a Racket function ifmunit that takes three NUMEX expressions 𝑒1, 𝑒2, and 𝑒3. It
returns a NUMEX expression that first evaluates 𝑒1. If the resulting value is NUMEX's
munit, then it evaluates 𝑒2 and that is the overall result. Otherwise, 𝑒3 must be
evaluated.

(b) Write a Racket function with∗ that takes a Racket list of Racket pairs
’((𝑠1 . 𝑒1) … (𝑠𝑖 . 𝑒𝑖) … (𝑠𝑛 . 𝑒𝑛)) and a final NUMEX expression 𝑒𝑛+1. In each pair, assume 𝑠𝑖
is a Racket string and 𝑒𝑖 is a NUMEX expression. with∗ returns a NUMEX expression
whose value is 𝑒𝑛+1 evaluated in an environment where each 𝑠𝑖 is a variable bound to the
result of evaluating the corresponding 𝑒𝑖 for 1 ≤ 𝑖 ≤ 𝑛. The bindings are done
sequentially, so that each 𝑒𝑖 is evaluated in an environment where 𝑠1 through 𝑠𝑖−1 have
been previously bound to the values 𝑒1 through 𝑒𝑖−1.

(c) Write a Racket function ifneq that takes four NUMEX expressions 𝑒1, 𝑒2, 𝑒3, and 𝑒4 and
returns a NUMEX expression that acts like ifleq except 𝑒3 is evaluated if and only if 𝑒1
and 𝑒2 are not equal numbers/booleans. Otherwise, the whole expression evaluates to
what 𝑒4 evaluates to. Assume none of the arguments to ifneq use the NUMEX variables
_x or _y. Use this assumption so that when an expression returned from ifneq is
evaluated, 𝑒1 and 𝑒2 are evaluated exactly once each.

4. Using the Language
We can write NUMEX expressions directly in Racket using the constructors for the structs
and (for convenience) the functions we wrote in the previous problem.

(a) Bind to the Racket variable numex-filter a NUMEX function that acts like map (as we
use in ML). Your function should be curried: it should take a NUMEX function and return
a NUMEX function that takes a NUMEX list and applies the function to every element of
the list returning a new NUMEX list with all the elements for which the function returns
a number other than zero (causing an error if the function returns a non-number).
Recall a NUMEX list is munit or a pair where the second component is a NUMEX list.

(b) Bind to the Racket variable numex-all-gt a NUMEX function that takes a NUMEX
number 𝑖 and returns a NUMEX function that takes a NUMEX list of NUMEX numbers and
returns a new NUMEX list of NUMEX numbes containing the elements of the input list
(in order) that are greater than 𝑖 (hint: Use numex-filter).

5. Challenging Problem
Write a second version of eval-exp (bound to eval-exp-c) that builds closures with
smaller environments: When building a closure, it uses an environment that is like the
current environment but holds only variables that are free variables in the function part of
the closure. (A free variable is a variable that appears in the function without being under
some shadowing binding for the same variable.)
Avoid computing a function's free variables more than once. Do this by writing a function
compute-free-vars that takes an expression and returns a different expression that uses
fun-challenge everywhere in place of fun. The new struct fun-challenge (provided
to you; do not change it) has a field freevars to store exactly the set of free variables for
the function. Store this set as a Racket set of Racket strings. (Sets are predefined in Racket's
standard library; consult the documentation for useful functions such as set, set-add,
set-member?, set-remove, set-union, and any other functions you wish.)
You must have a top-level function compute-free-vars that works as just described −
storing the free variables of each function in the freevars field − so the grader can test it
directly. Then write a new “main part” of the interpreter that expects the sort of NUMEX
expression that compute-free-vars returns. The case for function definitions is the
interesting one.
