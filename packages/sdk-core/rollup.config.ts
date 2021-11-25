import typescript from "@rollup/plugin-typescript";
import json from "@rollup/plugin-json";
import resolve from "@rollup/plugin-node-resolve";
import commonjs from "@rollup/plugin-commonjs";

const cjsConfig = {
    input: "src/index.ts",
    output: [
        {
            file: "dist/main/index.js",
            format: "cjs",
        },
    ],
    plugins: [typescript({ tsconfig: "./tsconfig.json" }), json()],
};

const esConfig = {
    input: "src/index.ts",
    output: [
        {
            file: "dist/module/index.js",
            format: "es",
        },
    ],
    plugins: [typescript(), json()],
};

const umdConfig = {
    input: "src/index.ts",
    output: [
        {
            name: "umdcore",
            file: "dist/umd/index.js",
            format: "umd",
            globals: {
                stream: "stream",
                https: "https",
                http: "http",
                zlib: "zlib",
                path: "path",
                fs: "fs",
            },
        },
    ],
    plugins: [typescript(), json(), resolve(), commonjs()],
    external: ["stream", "https", "http", "zlib", "path", "fs"],
};

export default [cjsConfig, esConfig, umdConfig];
