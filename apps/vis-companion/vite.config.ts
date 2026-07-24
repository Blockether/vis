import tailwindcss from '@tailwindcss/vite';
import react from '@vitejs/plugin-react';
import { defineConfig, loadEnv } from 'vite';

// https://vite.dev/config/
export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '');
  const gatewayProxyToken = env.VIS_COMPANION_GATEWAY_TOKEN;
  // The gateway binds whatever `vis gateway start --host` was given. When that is
  // a Tailscale/LAN IP (not loopback), point the dev proxy at it via this env var,
  // e.g. VIS_COMPANION_GATEWAY_TARGET=http://100.109.18.77:7890
  const gatewayTarget = env.VIS_COMPANION_GATEWAY_TARGET || 'http://127.0.0.1:7890';

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
          target: gatewayTarget,
          changeOrigin: true,
          rewrite: (path) => path.replace(/^\/gateway/, ''),
        },
      },
    },
  };
});
