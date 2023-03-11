# Assignment 7: Nano Types (105 (+15 EC) public points, 210 (+30 EC) overall)

## Forming groups
If you wish, this assignment may be completed by groups of two.  If working in a group, only one partner should submit to Gradescope, and should choose the other partner as a "Team Member" on Gradescope when submitting.

## Submission Instructions
To submit your code, first commit and push your repository to GitHub. To see the output of the auto-grader tests, click 'Pull Requests' and select Feedback. You can see the output of the tests under the Checks tab.

**When you have completed the assignment, submit your repo to gradescope by selecting your repository and the branch containing your completed assignment. Unless you created your own branches, the branch is called 'main'**. You can access Gradescope through the Canvas assignment page.

## Overview

The overall objective of this assignment is to
better understand the notions of type checking, type inference, and polymorphism
by implementing a type inference algorithm for Nano.

The assignment is in the file:

+ [TypeCheck.hs](/src/Language/Nano/TypeCheck.hs)

and

+ [tests/Test.hs](/tests/Test.hs) has some sample tests,
  and testing code that you will use to check your
  assignments before submitting.

You should only need to modify the parts of the files which say:

```haskell
error "TBD: ..."
```

with suitable Haskell implementations.

**Note:** Start early! Type inference has many subtle points that can take a while to get right.


## Assignment Testing and Evaluation


Most of the points, will be awarded automatically, by
**evaluating your functions against a given test suite**.

[Tests.hs](/tests/Test.hs) contains a very small suite
of tests which gives you a flavor of these tests.
When you run

```shell
$ stack test
```

Your last lines should have

```
All N tests passed (...)
OVERALL SCORE = ... / ...
```

**or**

```
K out of N tests failed
OVERALL SCORE = ... / ...
```

**If your output does not have one of the above your code will receive a zero**

If for some problem, you cannot get the code to compile,
leave it as is with the `error ...` with your partial
solution enclosed below as a comment.

The other lines will give you a readout for each test.
You are encouraged to try to understand the testing code,
but you will not be graded on this.

## Data Structures and Overview

This assignment builds upon our previous implementation of Nano from HW4,
so most data types in `Types.hs` are the same.
We have added the following definitions to support type inference.

### Types

We will use the following Haskell datatypes to represent Nano's types and poly-types:

```haskell
data Type
  = TInt             -- Int
  | TBool            -- Bool
  | Type :=> Type    -- function type: T1 -> T2
  | TVar TVar        -- type variable: a, b, c
  | TList Type       -- list type: [T]
    
data Poly = Mono Type        -- mono-type 
          | Forall TVar Poly -- polymorphic type
```

where `TVar` is just a type alias for `String` used to represent type variable names:

```haskell
type Id = String
```

For example, a Nano type `Int -> Int` is represented inside your type checker as:

```haskell
TInt :=> TInt
```

whereas a polymorphic Nano type `forall a . List a -> a` is represented as:

```haskell
Forall "a" (Mono (TList (TVar "a") :=> TVar "a"))
```

(or simply `forall "a" (list "a" :=> "a")` using convenience functions `forall` and `list`, also defined in `Types.hs`).

### Type Environment

A **type environment** is simply a dictionary that maps program variables to poly-types.
As before, the dictionary is implemented as a list of pairs:

```haskell
type TypeEnv = [(Id, Poly)]
```

### Type Substitution

A **type substitution** is also a dictionary, but it maps *type variables* to *types* they should be replaced with:

```haskell
type Subst = [(TVar, Type)] 
```

Throughout the assignment, make sure that the keys (type variables) in the substitution are *unique*.
You also get to *assume* this property when you implement operations on substitutions.



## Problem 1: Warm-up (70 points, 35 public)

In this problem, you will implement some helper functions for your type-checker
which you will need later on.


### (a) Free type variables: 20 points (10 public)

First implement two functions that compute the list containing the set of 
*free type variables* in a type and in a poly-type. Even though a list allows for
duplicate elements, you want to make sure that the elements of your lists
for `freeTVars` are unique. 

Note: to enable overloading (using a function with the same name for types and poly-types),
this function is defined as part of a type class `HasTVars`.

```haskell
instance HasTVars Type where
  freeTVars t     = error "TBD: ..."

instance HasTVars Poly where
  freeTVars s     = error "TBD: ..."
```

As always, you are allowed (and encouraged) to replace `freeTVars t` and `freeTVars s` with multiple equations for different patterns.

Throughout this assignment, you are allowed to use any library functions,
as long as you don't add new `import` statements.
In particular, some useful functions from the `Data.List` library include 
`delete` (remove an element), `nub` (a.k.a "dedupe" from the midterm).

[Some of the functions](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-List.html#g:20)
in `Data.List` mimic set operations. In particular, you may find it useful
to learn about `union` and `\\` (set difference).

You can read all about the `Data.List` library 
on [Hackage here](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-List.html).
You're free to use anything you find in there. 
You may also look up functions on [Hoogle](https://hoogle.haskell.org/).

Note: the `Data.List` library is imported *qualified*:

```haskell
import qualified Data.List as L
```

So whenever you want to use a function from that library, you have to prefix its name with `L.` (e.g. `L.delete`).

When you are done you should get the following behavior:

```haskell
>>> freeTVars TInt
[]

>>> freeTVars (TVar "a")
["a"]

>>> freeTVars (Forall "a" (Mono (TList (TVar "a") :=> (TVar "a"))))
[]

>>> freeTVars (TList (TList (TVar "a")))
["a"]

>>> sort (freeTVars (TVar "a" :=> TVar "b"))
["a", "b"]

>>> freeTVars (TVar "a" :=> TVar "a")
["a"]

>>> freeTVars (Forall "a" (Mono (TList (TVar "a") :=> (TVar "b"))))
["b"]
```


### (b) Substitutions: 50 points (25 public)

Implement a function to lookup what a type variable maps to in a substitution: 

```haskell
lookupTVar :: TVar -> Subst -> Type
```
and a function to remove the mapping for a given type variable from a substitution
(**remember** that all variables in a substitution are supposed to be **unique**):

```haskell
removeTVar :: TVar -> Subst -> Subst
```

If your type variable `a` is not mapped in the substitution, then `lookupTVar` should
just return `(TVar a)` -- i.e. it makes no substitution -- and `removeTVar` should
return the substitution unchanged.

When you are done you should get the following behavior:

```haskell
>>> lookupTVar "a" [("a", TInt)]
Int  -- this is the pretty-printed `TInt`

>>> lookupTVar "a" [("b", TInt)]
a    -- this is the pretty-printed `TVar "a"`

>>> lookupTVar "g" [("g", TBool), ("a", TInt)]
Bool -- this is the pretty-printed `TBool`

>>> removeTVar "a" [("a", TInt)]
[]

>>> removeTVar "a" [("b", TInt)]
[("b",Int)]

>>> removeTVar "a" [("a", TInt), ("b", TBool)]
[("b",Int)]
```

Next, use `lookupTVar` and `removeTVar` to write 
two functions that *apply a substitution*
to a type and to a poly-type
(both named `apply`):

```haskell
instance Substitutable Type where  
  apply sub t         = error "TBD: ..."

instance Substitutable Poly where    
  apply sub p         = error "TBD: ..."
```

Once you have implemented this functionality and
recompiled, you should get the following behavior:

```haskell
>>> apply [("a",TInt)] (TList (TVar "a"))
[Int]            -- i.e., TList TInt

>>> apply [("a",TInt)] (Forall "a" $ Mono (TList (TVar "a")))
forall a . [a]   -- i.e., Forall "a" (Mono (TList (TVar "a")))

>>> apply [("a", TList (TInt)), ("b", TInt)] (TVar "b" :=> TVar "a")
Int -> [Int]     -- i.e., TInt :=> TList (TInt)
```

Finally, use `apply` to implement the function `extendSubst`,
which extends a substitution with a new type assignment:

```haskell
extendSubst :: Subst -> TVar -> Type -> Subst
```
When you are done you should get the following behavior.
Pay close attention to the second and third test cases:

```haskell
>>> extendSubst [("a", TInt)] "b" TBool
[("b",Bool), ("a",Int)]

>>> extendSubst [("a", list "b")] "b" TBool
[("b",Bool), ("a",[Bool])]

>>> extendSubst [("a", TInt)] "b" (TList (TVar "a"))
[("b",[Int]), ("a",Int)] 
-- i.e. [("b", TList (TInt)), ("a", TInt)]
```

**Remember**, you can assume that `extendSubst` will never be called with a 
TVar that is already mapped in the substitution (and you should be careful
not to do this accidentally in Problem 2 -- even though you might not directly
call `extendSubst` you will likely call `extendState` which does call `extendSubst`).


## Problem 2: Unification (50 points, 25 public)

Unification is a crucial part of type inference.
In a nutshell, the inference algorithm works by:
  
  * recursively traversing the program, assembling expression types from the types of their sub-expressions;
  * when some type is not yet known, it picks a **fresh type variable** to represent this type;
  * whenever two types must be the same, the algorithm tries to **unify** them and figure out what these type variables actually stand for.

A *fresh type variable* is a type variable that is distinct from all type variables the algorithm has generated before.
Our implementation will name the fresh type variables `a0, a1, ...`.
In order to remember the index of the first unused fresh variable,
we introduce a new datatype:

```haskell
data InferState = InferState { 
    stSub :: Subst -- ^ current substitution
  , stCnt :: Int   -- ^ number of fresh type variables generated so far
}
```

which represents the "current state" of the unification and inference algorithms.
It remembers how many fresh type variables the algorithm has generated so far,
alongside the "current substitution".

### (a) Unification for type variables: 20 points (10 public)

Implement a function `unifyTVar st a t`
that tries to unify a type variable `a` with a type `t`.
If it succeeds, it records the new type assignment (for `a`) 
by extending the state `st`.
If it fails, it throws an exception.

```haskell
unifyTVar :: InferState -> TVar -> Type -> InferState
```

You can use the provided helper function `extendState` 
to extend a substitution inside a state -- it's probably simpler to use 
that than to call `extendSubst` directly.

**Assumption:** 
You can *assume* that `a` does not appear in the *domain* of `stSub st`
(i.e. among the type variables it maps);
remember to *guarantee* this property whenever you call `unifyTVar`.

When you are done you should get the following behavior. Note that 
`initInferState == []`:

```haskell
>>> stSub $ unifyTVar initInferState "a" (list "b")
[("a",[b])]

>>> stSub $ unifyTVar initInferState "a" "a"
[]

>>> stSub $ unifyTVar initInferState "a" (list "a")
*** Exception: Error {errMsg = "type error: cannot unify a and [a] (occurs check)"}
```

*Hints:* This one is easier to do by **not** pattern matching on the 
constructors for the `Type` in the input. Rather, there are basically
three different outcomes that can happen that correspond to the 
three test cases above: either we learn a constraint (for the type 
variable) and add it to our unifying substitution, or it turns out
no new constraint is needed, or we find ourselves in a situation 
where we can't possible unify `TVar "a"` with the input type
and we have to throw an error. Think carefully about when exactly
we end up in each of these three situations and then implement your 
`unifyTVar` function.

**Note:** When you need to throw an error, use 
`throw (Error "message goes here")`. The tests that are looking for an error
won't pass if you use the `error` keyword instead. You don't need to reproduce
the whole fancy message above, but you do need to have the phrase `"type error"`
appear in your message.

### (b) Unification for types: 30 points (15 public)

Implement a function `unify st t1 t2`
that tries to unify the types `t1` and `t2`.
If it succeeds, it records the new type assignment by extending the state `st`.
If it fails, it throws an exception.

```haskell
unify :: InferState -> Type -> Type -> InferState
```

**Assumption:** 
You can *assume* that any type variables in `t1` and `t2` do not appear in the domain of `stSub st`;
remember to *guarantee* this property whenever you call `unify` (either recursively here or
where you call `unify` from the `infer` function).

When you are done you should get the following behavior:

```haskell
>>> stSub $ unify initInferState TInt TInt
[]

>>> stSub $ unify initInferState TInt TBool
*** Exception: Error {errMsg = "type error: cannot unify Int and Bool"}

>>> stSub $ unify initInferState (TInt :=> TInt) ("a" :=> "a")
[("a",Int)]

>>> stSub $ unify initInferState ("a" :=> TInt) (TBool :=> "b")
[("b",Int),("a",Bool)]

>>> stSub $ unify initInferState (list "a") (list $ list "a")
*** Exception: Error {errMsg = "type error: cannot unify a and [a] (occurs check)"}
```

*Hints*: This function will need to be recursive, so think about how you
might take a recursive approach -- which cases will have subproblems and
what are those subproblems? 

## Problem 3: Type Inference (120 points, 60 public)

Now we have everything in place to implement type inference!

### (a) Mono-types: 20 points (10 public)

Let's first consider the fragment of the language without:

  * binary operators (we already implemented `+` for you)
  * conditionals
  * let polymorphism
  * recursion
  * lists

Implement the function `infer st gamma e` for this fragment.
To be precise, you need to implement the following cases to pass
part (a):

  * integer constants (EInt)
  * boolean constants (EBool)
  * variables (EVar), but you can assume for (a) that whatever you get
    from your typing environment doesn't contain `forall`s.
  * lambda abstractions (ELam)
  * applications (EApp)
  * let bindings without polymorphism (ELet), but don't do any generalizing in part (a)

This function attempts to infer the type of `e` in type environment `gamma`
given that the current state is `st`.
If `e` imposes constraints on the types of its sub-expressions, 
`infer` calls `unify` to enforce these constraints. 
If all unification steps succeed, `infer` returns the inferred type of `e` and a new state 
(possibly extended with new type assignments generated during unification).

In this part, you can assume that the type environment maps all variables to mono-types, 
i.e. every type in the type environment has the structure `Mono t`.

In particular, you should observe the following behavior:

```haskell
>>> typeOfString "True"
Bool   -- TBool

>>> typeOfString "1"
Int    -- TInt

>>> typeOfString "(\\x -> x) 5"
Int    -- TInt

>>> typeOfString "(\\x -> x + 1) True"
*** Exception: Error {errMsg = "type error: cannot unify Int and Bool"}
```

### (b) Polymorphism: 40 points (20 public)

Now let's add support for polymorphism!

  * In this version, variables in the type environment can have polymorphic types, like `forall a . a -> a`.
  
  * Whenever we use a variable, we have to **instantiate** its poly-type, 
    i.e. replace all bound type variable with *fresh free type variables*.
    
  * Whenever we define a variable using a `let`, 
    we have to **generalize** its type into a poly-type.

First implement the function `generalize gamma t` that generalizes `t` into a poly-type,
i.e. binds all its type variables that are *not free* in `gamma` with a `Forall`:

```haskell
generalize :: TypeEnv -> Type -> Poly
```

When you are done you should observe the following behavior:

```haskell
>>> generalize [] ("a" :=> "a")
forall a . (a) -> a   -- Forall "a" $ Mono $ (TVar "a") :=> (TVar "a")

>>> generalize [("x", Mono "a")] ("a" :=> "a")
(a) -> a              -- Mono $ (TVar "a") :=> (TVar "a")
```

Next implement the function `instantiate n s` that instantiates a poly-type `s`
by replacing its bound type variables with fresh type variables.
Here `n` is the index of the first unused fresh type variable.

```haskell
instantiate :: Int -> Poly -> (Int, Type)
```

**Assumption:** 
In `instantiate n s`, you can assume that bound type variables in `s`
cannot clash with `ai` where `i >= n`. 
Remember to *guarantee* this property whenever you construct your own polymorphic types.

When you are done you should observe the following behavior:

```haskell
>>> instantiate 2 (Forall "h" $ Mono $ TList (TVar "h"))
(3, [a2])       -- TList (TVar "a2")

>>> instantiate 2 (Forall "a" $ Mono $ (TVar "a") :=> (TVar "b"))
(3, (a2) -> b)  -- (TVar "a2") :=> (TVar "b"))

>>> instantiate 0 (Forall "a" (Forall "b" (Mono ("a" :=> "b"))))
(2,(a0) -> a1)
```

Finally, modify the function `infer` to support polymorphism.
You'll have to change the `EVar` and `ELet` cases you wrote in part (a).

When you are done, your implementation should be able to 
type-check the "double identity" example from the lecture:

```haskell
>>> typeOfString "let id = \\x -> x in let y = id 5 in id (\\z -> z + y)"
(Int) -> Int
```

**Hint:** To help figure out where `instantiate` should be used in `infer`, think
about the return type of `infer`.

### (c) Built-in functions: 30 points (15 public)

Instead of implementing separate type inference for binary operators, conditionals, and list,
we decided to represent them as **built-in functions**
and reuse the type inference for variables and function application.

Fill in the types of built-in functions inside `preludeTypes`
(we have pre-filled the one for `+`).
Remember that you are not allowed to use the names `a0, a1, ...` as bound type variables
because those are reserved for use as free type variables by the algorithm.

You are not writing any function definitions here: you just need to supply a type 
(object of type Poly) that expresses the most general type for these built-in functions.

### (d) EXTRA CREDIT: Recursion: 30 extra points (15 extra public)

Modify your implementation of `infer` to support recursive function definitions.
Once you are done, all tests should pass. See `tests/input/3dtest1.hs` through 
`tests/input/3dtest5.hs` for some examples.
