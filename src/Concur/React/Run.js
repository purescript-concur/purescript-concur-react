"use strict";

import ReactDOMClient from "react-dom/client";

export const createRoot = (container) => () =>
  ReactDOMClient.createRoot(container);

export const render = (root) => (reactElement) => () =>
  root.render(reactElement);
