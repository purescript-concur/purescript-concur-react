# purescript-concur-react

[Concur UI Lib](https://github.com/ajnsit/concur) implementation for Purescript. React Backend.

The API needs a little bit more work, but it does work.

## Examples

[Demo](https://ajnsit.github.io/purescript-concur/) and [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/Main.purs) for composing all the examples in one page.

Individual example sources -

1. Hello World! Shows simple effectful widgets. [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/Hello.purs).
2. A simple counter widget. [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/Counter.purs).
3. Using AJAX and handling JSON responses. [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/Ajax.purs).
4. Tail Recursion demo. [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/TailRec.purs). Currently disabled due to a bug. Showed how Widgets written using tail recursion are stack safe, even though Purescript is strict.

## Building the example from source

```bash
git clone https://github.com/ajnsit/purescript-concur.git
cd purescript-concur
yarn
psc-package update
npm run-script build
open html/index.html
```
