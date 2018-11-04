<h1 align="center">
  Purescript Concur
</h1>
<p align="center">
   <img src="docs/logo.png" height="100">
</p>
<p align="center">
  <a href="https://gitter.im/concurhaskell" rel="nofollow">
      <img src="https://camo.githubusercontent.com/9fb4e2dde684214e7454d930a369f97190d1ecf2/68747470733a2f2f696d672e736869656c64732e696f2f62616467652f6769747465722d6a6f696e253230636861742532302545322538362541332d626c75652e737667" alt="Join the chat at https://gitter.im/concurhaskell" data-canonical-src="https://img.shields.io/badge/gitter-join%20chat%20%E2%86%A3-blue.svg" style="max-width:100%;">
   </a>
   <a href="https://www.reddit.com/r/concurhaskell/" rel="nofollow">
      <img src="https://img.shields.io/badge/reddit-join%20the%20discussion%20%E2%86%A3-1158c2.svg" alt="Join the chat at https://gitter.im/concurhaskell" style="max-width:100%;">
   </a>
</p>

[Concur UI Lib](https://github.com/ajnsit/concur) is a brand new client side Web UI framework that explores an entirely new paradigm. It does not follow FRP (think Reflex or Reactive Banana), or Elm architecture, but aims to combine the best parts of both. This repo contains the Concur implementation for Purescript, using the React backend.

## Documentation

Work in progress tutorials are published in the [Concur Documentation site](https://github.com/ajnsit/concur-documentation/blob/master/README.md)

API documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-concur-react).

## Ports to other languages

1. [Concur for Haskell](https://github.com/ajnsit/concur) - The original version of Concur written in Haskell.
2. [Concur for Javascript](https://github.com/ajnsit/concur-js) - An official but experimental port to Javascript.

## Installation

You can quickly get a production setup going by cloning the [Purescript Concur Webpack Starter](https://github.com/ajnsit/purescript-concur-webpack-starter).

Else you can also install purescript-concur manually using bower -

```bash
bower install purescript-concur-react
```

## Building from source

```bash
git clone https://github.com/ajnsit/purescript-concur.git
cd purescript-concur
yarn # or npm install
psc-package update # or bower install
# Build library
npm run-script build
# Build examples
npm run-script build-examples
# Check examples
open html/index.html
```

## External React Components

Concur supports using external React components. For example, there is an ongoing effort to create concur bindings for [SemanticUI-React](https://react.semantic-ui.com). Look at the [Sources](https://github.com/ajnsit/purescript-concur-semantic), and the [Demo](https://ajnsit.github.io/purescript-concur-semantic/).

## Examples

[Demo](https://ajnsit.github.io/purescript-concur/) and [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Main.purs) for composing all the examples in one page.

Individual example sources -

1. Hello World! Shows simple effectful widgets. [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Hello.purs).
2. A simple counter widget. [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Counter.purs).
3. A login widget. [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Login.purs).
4. Concur has Signals! Example counting widget implemented with Signals! [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Signals.purs).
5. A full-featured TodoMVC implementation with Signals. [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Todos.purs).
6. A fully editable tree in ~30 lines of code (with Signals). [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/EditHeadings.purs).
7. A postfix calculator. [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Calc.purs).
8. Using AJAX and handling JSON responses. [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Ajax.purs).
9. A small widget to visualise CSS color codes. [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Color.purs).
10. Asynchronous timers which can be cancelled. [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Timers.purs).
11. Performance test - A huge list of 50 thousand parallel buttons. This has two variants, fast (uses slightly lower level interface) and slow (idiomatic concur code). [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/SlowButtonList.purs).
