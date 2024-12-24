import path from "node:path"
import { fileURLToPath } from "node:url"
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import fable from "vite-plugin-fable"
import Inspect from "vite-plugin-inspect"

const root = path.dirname(fileURLToPath(import.meta.url));
const fsproj = path.resolve(root, "./Client.fsproj");
console.log("transpilling fsproj", fsproj);

const proxyPort = process.env.SERVER_PROXY_PORT || "8085";
const proxyTarget = "http://localhost:" + proxyPort;
console.log("proxying to", proxyTarget);

// https://vite.dev/config/
export default defineConfig({
  root : root,
  server: {
    proxy: {
        // redirect requests that start with /api/ to the server on port 8085
        "/api": {
            target: proxyTarget,
            changeOrigin: true,
        }
    }
  },
  plugins: [
    Inspect(),
    fable({ fsproj: fsproj, jsx: "automatic" }),
    react({ include: /\.(fs|js|jsx|ts|tsx)$/, jsxRuntime: "automatic" })
  ],
})

