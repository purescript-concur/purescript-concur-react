# purescript-concur-react

[Concur UI Lib](https://github.com/ajnsit/concur) implementation for Purescript. React Backend.

The API needs a little bit more work, but it does work.

## Examples

[Demo](https://ajnsit.github.io/purescript-concur/) and [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/Main.purs) for composing all the examples in one page.

Individual example sources -

1. Hello World! Shows simple effectful widgets. [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/Hello.purs).
2. Two counter widgets side by side. [Source](https://github.com/ajnsit/purescript-concur/blob/master/src/Test/Counter.purs).

## Building the example from source

```bash
git clone https://github.com/ajnsit/purescript-concur.git
cd purescript-concur
yarn
psc-package update
npm run-script build
open html/index.html
```
