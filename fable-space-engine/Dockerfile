# syntax=docker/dockerfile:1

# =====================================================================================
#  Stufe 1 - Build: .NET SDK (Fable-Compiler) + Node.js (Vite)
# =====================================================================================
FROM node:24-bookworm-slim AS node

FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build

# Node.js samt npm aus dem offiziellen Image uebernehmen (kein apt/curl noetig).
COPY --from=node /usr/local/bin/node /usr/local/bin/node
COPY --from=node /usr/local/lib/node_modules /usr/local/lib/node_modules
RUN ln -s ../lib/node_modules/npm/bin/npm-cli.js /usr/local/bin/npm \
 && ln -s ../lib/node_modules/npm/bin/npx-cli.js /usr/local/bin/npx

ENV DOTNET_CLI_TELEMETRY_OPTOUT=1 \
    DOTNET_NOLOGO=1 \
    CI=1

WORKDIR /app

# Abhaengigkeiten zuerst - diese Layer bleiben im Cache, solange sich nur Quellcode aendert.
COPY global.json ./
COPY .config/dotnet-tools.json .config/dotnet-tools.json
COPY src/App.fsproj src/App.fsproj
RUN dotnet tool restore && dotnet restore src/App.fsproj

COPY package.json package-lock.json ./
RUN npm ci

# Quellcode kompilieren: F# -> JS (Fable) -> gebuendeltes dist/ (Vite)
COPY index.html vite.config.js ./
COPY src/ src/
RUN npm run build

# =====================================================================================
#  Stufe 2 - Runtime: nur nginx + statische Dateien (kein SDK, kein Node im Image)
# =====================================================================================
FROM nginx:stable-alpine AS runtime

COPY nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=build /app/dist /usr/share/nginx/html

EXPOSE 80

HEALTHCHECK --interval=30s --timeout=3s --retries=3 \
  CMD wget -q --spider http://127.0.0.1/ || exit 1
