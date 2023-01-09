import typescript from "@rollup/plugin-typescript";
import commonjs from "rollup-plugin-commonjs";
import { nodeResolve } from "@rollup/plugin-node-resolve";
import copy from "rollup-plugin-copy";
import livereload from 'rollup-plugin-livereload';
import serve from 'rollup-plugin-serve';

export default [
  {
    input: ["src/index.ts"],
    output: {
      dir: "public",
      format: "iife",
      sourcemap: true,
    },
    plugins: [
      copy({
        targets: [{ src: "static/*", dest: "public" }],
      }),
      nodeResolve({
        browser: true,
        preferBuiltins: true,
      }),
      commonjs(),
      typescript({ sourceMap: true }),
      livereload(),
      serve('public'),
    ],
  },
];
