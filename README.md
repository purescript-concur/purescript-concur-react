# purescript-concur-react

[Concur UI Lib](https://github.com/ajnsit/concur) implementation for Purescript. React Backend.

The API needs a little bit more work, but it does work.

## Examples

[Demo](https://ajnsit.github.io/purescript-concur/) and [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/Main.purs) for composing all the examples in one page.

Individual example sources -

1. Hello World! Shows simple effectful widgets. [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/Hello.purs).
2. A simple counter widget. [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/Counter.purs).
3. Using AJAX and handling JSON responses. [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/Ajax.purs).
4. A small widget to visualise CSS color codes. [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/Color.purs).
5. Asynchronous timers which can be cancelled. [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/Timers.purs).
6. Performance test - A huge list of parallel buttons. This currently performs terribly. [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/SlowButtonList.purs).

## Building the example from source

```bash
git clone https://github.com/ajnsit/purescript-concur.git
cd purescript-concur
yarn
psc-package update
npm run-script build
open html/index.html
```
