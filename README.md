advanced-readtable
==================

Features
- per-package aliases for packages
- per-package shortcuts for package hierarchies
- extendable find-package and find-symbol
- local use package in form package:(here form where package used)
- local intern package like in SBCL: package::(symbol1 symbol2) will intern
                                     package::symbol1 and package::symbol2

To start
--------

Either use named-readtables and write

    (in-readtable :advanced)
    
or simply add to advanced-readtable to current readtable

    (advanced-readtable:!)

Hierarchy packages
------------------

Advanced-readtable has fully functional built-in support of hierarchy-packages.
.name means "subpackage name in current package", ..name -- "subpackage name in above package",
...name -- "subpackage in two-level-up package" and so on.
In in-package you may use .. for above package, ... for two level up, and so on.

    CL-USER> (defpackage .test (:use cl)))
    #<PACKAGE "COMMON-LISP-USER.TEST">
    CL-USER> (in-package .test)
    TEST> (in-package ..)
    CL-USER> (defpackage .test.a (:use cl))
    #<PACKAGE "COMMON-LISP-USER.TEST.A">
    CL-USER> (in-package .test.a)
    A> '...::car
    CAR
    A> (eq '...::car 'cl:car)
    T
    A> (in-package ...test)
    TEST> (in-package ..)
    CL-USER>


API
===

_push-import-prefix_ -- enables import prefix on package name
--------------------------------------------

For example, you have packages com.clearly-useful.iterator-protocol, com.clearly-useful.reducers, ...
You may use them as

    (push-import-prefix :com.clearly-useful)
    (iterator-protocol:do-iterator ...)
    (reducers:r/map #'1+ data)

and so on.
Package prefix is enabled per package so it is safe to use it in your package.

If there is package, which name coincides with shortcut, package name has priority.

So, if you make

    (defpackage :reducers ...)

after that reducers:... will refer to new package, not com.clearly-useful.reducers.

_push-local-nickname_ -- enables nickname for package in current package
-------------------------------------------

Enables package nickname in CURRENT-PACKAGE.
For example, you found COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST package and want to use
it. But don't want to USE-PACKAGE them, because some exported symbols from it are clashing 
with yours.

You may do it right:

    (push-local-nickname :com.informatimago.common-lisp.cesarum.list :ilist)
    (ilist:circular-length l)

Local-nicknames are local, so you may use it freely.

If package A wants package LIB version 1, and package B wants package LIB version 2, one can simply 
rename LIB version 1 to LIB1 and LIB version 2 to LIB2 and make

    (push-local-nickname :lib1 :lib :a)
    (push-local-nickname :lib2 :lib :b)

This command also adds local subpackage alias. In the previous example a.lib 
and b.lib will be aliases to lib1 and lib2. If there is a real package with 
such name, alias will be shadowed, so don't worry too much about it.

_push-local-package_ -- sets local-package for a symbol
----------------------------------------------

Many macroses use there own clauses. 
For example, ITERATE uses FOR, COLLECT and so on. 
If you don't want to USE-PACKAGE iterate, this function will help.

    (push-local-package 'iter:iter :iterate)
    (iter:iter (for i from 1 to 10) (collect i))

Caution: this function enables package substitution in all cases, 
where SYMBOL is the car of a list.
For example, this will be error:

    (let (iter:iter for) (list iter:iter for))
    
, because first for is in ITERATE package, but second -- is not.

Be careful: this change is not local to your package.

_set-macro-symbol_ - syntax is like set-macro-character, 
------------------

But FUNC is binded to SYMBOL, not character. This symbol will be processed 
in all cases, where it is not bounded by ||.

Now you may make something like 

    html:[body [table (as-html sql:[select * from t1])]]

html:[ and sql:[ will have different handlers and you may mix them in
one expression.

Also it allows to make simple symbol-aliases. For example:

    (set-macro-symbol '|ALIAS| (lambda (stream symbol)
                                 (declare (ignore stream symbol))
                                   'advanced-readtables:push-local-package))
Now you may do

    (alias 'iter:iter :iterate)

Moreover, you may alias variables from other packages and set them through 
alias. But be careful: this change is not local to your package.
                                   

_get-macro-symbol_ - syntax is like get-macro-character, 
------------------

Returns function, assigned by set-macro-symbol

Low-level API
-------------

There are five lists:
-  *package-finders* -- global for find-package
-  *symbol-finders* -- global for find-symbol
-  (package-finders package) -- per-package for find-package
-  (symbol-finders package) -- per-package for find-symbol
-  (extra-finders symbol) -- per-symbol for (symbol ....) package substitution

They are all alists. Key denotes handler and should be uniq for the list.
Value should have form (lambda (name package) ...) and return symbol for
symbol-finders and extra-finders and return pacakge for package-finders.

You may freely change them to develop your own symbol or package schemes
(for example, hierarchy-packages, conduits and so on).

Middle-level API
----------------

To simplify adding new handlers with keys there is macro _set-handler_

    (set-handler (package-finders pack) '(:my handler1) #'handler-func)

will set handler for package pack, if there are no hanler with key 
(:my handler1). So you may set it in your file and not be afraid, that it
will duplicate on reloading.
