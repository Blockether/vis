import tailwindcss from '@tailwindcss/vite';
import react from '@vitejs/plugin-react';
import { defineConfig, loadEnv } from 'vite';

// https://vite.dev/config/
export default defineConfig(({ mode }) => {
  const gatewayProxyToken = loadEnv(mode, process.cwd(), '').VIS_COMPANION_GATEWAY_TOKEN;

  return {
    plugins: [
      {
        name: 'vis-gateway-proxy-auth',
        configureServer(server) {
          server.middlewares.use('/gateway', (req, res, next) => {
            if (!gatewayProxyToken) {
              res.statusCode = 503;
              res.end('gateway proxy token is not configured');
              return;
            }

            if (req.headers.authorization !== `Bearer ${gatewayProxyToken}`) {
              res.statusCode = 401;
              res.setHeader('Content-Type', 'application/json');
              res.end(JSON.stringify({ error: { message: 'missing or invalid bearer token' } }));
              return;
            }

            next();
          });
        },
      },
      react(),
      tailwindcss(),
    ],
    optimizeDeps: {
      // Pre-bundle Prism + its language components together at startup so a
      // lazy cold re-optimize can't reload them out of dependency order
      // (prism-tsx extends prism-typescript + prism-jsx and crashes if they
      // haven't executed first).
      include: [
        'prismjs',
        'prismjs/components/prism-bash',
        'prismjs/components/prism-clojure',
        'prismjs/components/prism-css',
        'prismjs/components/prism-diff',
        'prismjs/components/prism-java',
        'prismjs/components/prism-json',
        'prismjs/components/prism-markdown',
        'prismjs/components/prism-python',
        'prismjs/components/prism-typescript',
        'prismjs/components/prism-jsx',
        'prismjs/components/prism-tsx',
        'prismjs/components/prism-yaml',
      ],
    },
    server: {
      host: true,
      port: 5273,
      proxy: {
        '/gateway': {
          target: 'http://127.0.0.1:7890',
          changeOrigin: true,
          rewrite: (path) => path.replace(/^\/gateway/, ''),
        },
      },
    },
  };
});
