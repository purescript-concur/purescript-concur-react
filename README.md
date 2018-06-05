# Purescript Concur (React Backend)

[Concur UI Lib](https://github.com/ajnsit/concur) implementation for Purescript. React Backend.

## Documentation

API documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-concur-react).

## Installation

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
3. Concur has Signals! Example counting widget implemented with Signals! [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Signals.purs).
4. A postfix calculator. [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Calc.purs).
5. Using AJAX and handling JSON responses. [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Ajax.purs).
6. A small widget to visualise CSS color codes. [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Color.purs).
7. Asynchronous timers which can be cancelled. [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/Timers.purs).
8. Performance test - A huge list of 50 thousand parallel buttons. This has two variants, fast (uses slightly lower level interface) and slow (idiomatic concur code). [Source](https://github.com/ajnsit/purescript-concur/blob/master/examples/Test/SlowButtonList.purs).
