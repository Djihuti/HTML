import { defineConfig } from "vite";

// Fable schreibt das kompilierte JavaScript nach ./build; index.html bindet
// /build/Main.js ein. Vite liefert es im Dev-Modus aus und buendelt es fuer dist/.
export default defineConfig({
  clearScreen: false,
  server: {
    port: 5180,
    strictPort: true,
    watch: {
      // F#-Quellen und .NET-Zwischenstaende beobachtet Fable selbst.
      ignored: ["**/*.fs", "**/*.fsproj", "**/bin/**", "**/obj/**"],
    },
  },
  preview: {
    port: 4180,
    strictPort: true,
  },
  build: {
    outDir: "dist",
    emptyOutDir: true,
    target: "es2022",
    sourcemap: false,
  },
});
