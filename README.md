advanced-readtable
==================

Features
- per-package aliases for packages
- per-package shortcuts for package hierarchies
- extendable find-package and find-symbol
- local use package in form package:(here form where package used)
- local intern package like in SBCL: package::(symbol1 symbol2) will intern
                                     package::symbol1 and package::symbol2

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

