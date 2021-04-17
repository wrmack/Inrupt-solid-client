import { nodeResolve } from "@rollup/plugin-node-resolve";
import commonjs from '@rollup/plugin-commonjs';

export default {
  input: "./node_modules/@inrupt/solid-client/dist/index.es.js",

  output: [
    {
      file: "solid-client.bundle.js",
      format: "iife",
      name: "SolidClient",
      inlineDynamicImports: true,
    },
  ],
  plugins: [
    nodeResolve({preferBuiltins: false, browser: true}),
    commonjs(),
  ],
};