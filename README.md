# Scheme *todomvc*

demo @ https://amirouche.github.io/scheme-todomvc/

## Introduction

This repository host a version of [todomvc](http://todomvc.com/) that
is powered by [biwascheme interpreter](http://biwascheme.org/), by
[snabbdom library](https://github.com/snabbdom/snabbdom/) and inspired
from [react](https://facebook.github.io/react/) /
[redux](http://redux.js.org/) and [elm](https://guide.elm-lang.org/)
architecture.

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

The entry point is the `app` procedure which takes as arguments the following:

- `container` the dom node that will be used for rendering
- `init` a procedure that will return the initial state of the app
- `view` the procedure that takes `state` as argument and will render
  the application according to that state. It must return sxml-like
  datastructure with `on-foo` attribute to bind events to callbacks
  called *actions*.

### actions

Actions are created from scheme procedure which have the following
signature: `state -> event -> state`. Hence they return the new 
state of the application.

Here is an example action:

```scheme
(define (title-clicked state)
  (lambda (event)
	(if (null? state) 
	    "How are you doing?"
		'())))
```

Actions are directly bound to events in the sxml. For instance:

```scheme
(define (view state)
  `(h1 (@ (on-click . ,title-clicked))
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
