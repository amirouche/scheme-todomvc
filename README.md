# Scheme *todomvc*

demo @ https://amirouche.github.io/scheme-todomvc/

## Introduction

This repository host a version of [todomvc](http://todomvc.com/) that
is powered by [biwascheme interpreter](http://biwascheme.org/), by
[snabbdom library](https://github.com/snabbdom/snabbdom/) and inspired
from [react](https://facebook.github.io/react/) /
[redux](http://redux.js.org/) architecture.

biwascheme is a scheme interpreter written in javascript that implements
r7rs and r6rs. It both works in the browser and in nodejs.

snabbdom is functional virtual DOM library.

reactjs is javascript library which introduced the use of declarative views
using virtual dom library which diff and patch the DOM efficently.

redux is “a predictable state container for JavaScript apps” otherwise said
it handles what reactjs doesn't *ie.* it deals with the communication between
view components and the backend. It also has the responsability to deal with
the global state of the app.

## How it works

### State

There is a global state (called store in redux) that must be initialized to
something, for instance:

```scheme
(define *state* '())
```

### View

Views are procedures that takes state and return scheme xml (sxml). A global
`view` procedure is used to bootstrap the rendering of the app.

Here is an example `view` procedure:

```scheme
(define (view state)
  `(h1 ,(if (null? state) "Héllo World" state)))
```

The returned sxml is processed by snabbdom.

### actions

Actions are created from scheme procedure which have the following
signature: `state -> event -> state`.

Here is an example action:

```scheme
(define (title-clicked state)
  (lambda (event)
	(if (null? state) 
	    "How are you doing?"
		'())))
```

Actions are directly bound to events in the sxml via the `make-action`
procedure. For instance:

```
(define (view state)
  `(h1 (@ (on-click . ,(make-action title-clicked)))
       "Héllo World!"))
```

There is no reducers.

## Read the source

All this is implemented in
[`main.scm`](https://github.com/amirouche/scheme-todomvc/blob/master/main.scm).

## Where do we go from here?

Here are the things that can be done to improve further this prototype:

- Make use of ajax
- Build isomorphic app
- Make use of history api

Anything else?

[Comments and feedback welcome](https://github.com/amirouche/scheme-todomvc/issues/new).
