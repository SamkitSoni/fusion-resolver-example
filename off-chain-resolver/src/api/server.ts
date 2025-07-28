import express from 'express';
import helmet from 'helmet';
import cors from 'cors';
import { ConfigManager } from '../config/config';
import { Logger } from '../utils/logger';
import { OrderManager } from '../managers/order-manager';
import { TransactionManager } from '../managers/transaction-manager';
import { HealthChecker } from '../monitors/health-checker';

/**
 * API Server for the Fusion Resolver Service
 * Provides REST endpoints for monitoring and control
 */
export class APIServer {
    private app: express.Application;
    private server: any;
    private config: ConfigManager;
    private logger: Logger;
    private orderManager: OrderManager;
    private transactionManager: TransactionManager;
    private healthChecker: HealthChecker;

    constructor(
        config: ConfigManager,
        logger: Logger,
        orderManager: OrderManager,
        transactionManager: TransactionManager,
        healthChecker: HealthChecker
    ) {
        this.config = config;
        this.logger = logger;
        this.orderManager = orderManager;
        this.transactionManager = transactionManager;
        this.healthChecker = healthChecker;
        
        this.app = express();
        this.setupMiddleware();
        this.setupRoutes();
    }

    /**
     * Setup Express middleware
     */
    private setupMiddleware(): void {
        // Security middleware
        this.app.use(helmet());
        this.app.use(cors());
        
        // Body parsing
        this.app.use(express.json({ limit: '10mb' }));
        this.app.use(express.urlencoded({ extended: true }));
        
        // Request logging
        this.app.use((req, res, next) => {
            this.logger.debug(`${req.method} ${req.path}`, {
                ip: req.ip,
                userAgent: req.get('User-Agent')
            });
            next();
        });

        // Authentication middleware (if enabled)
        if (this.config.get('ENABLE_AUTH')) {
            this.app.use(this.authMiddleware.bind(this));
        }
    }

    /**
     * Authentication middleware
     */
    private authMiddleware(req: express.Request, res: express.Response, next: express.NextFunction): void {
        const apiKey = req.headers['x-api-key'] || req.query.apiKey;
        const expectedKey = this.config.get('API_KEY');
        
        if (!expectedKey || apiKey === expectedKey) {
            next();
        } else {
            res.status(401).json({ error: 'Unauthorized' });
        }
    }

    /**
     * Setup API routes
     */
    private setupRoutes(): void {
        // Health check endpoint
        this.app.get('/health', async (req, res) => {
            try {
                const health = await this.healthChecker.performHealthCheck();
                res.status(health.healthy ? 200 : 503).json(health);
            } catch (error) {
                res.status(500).json({ error: 'Health check failed' });
            }
        });

        // Detailed status endpoint
        this.app.get('/status', async (req, res) => {
            try {
                const status = await this.healthChecker.getDetailedStatus();
                res.json(status);
            } catch (error) {
                res.status(500).json({ error: 'Status check failed' });
            }
        });

        // Orders endpoints
        this.app.get('/orders', async (req, res) => {
            try {
                const orders = this.orderManager.getTrackedOrders();
                res.json({
                    total: orders.length,
                    orders: orders.map(order => ({
                        orderHash: order.orderHash,
                        status: order.status,
                        profit: order.profitability.expectedProfit,
                        isProfitable: order.profitability.isProfitable,
                        shouldExecute: order.profitability.shouldExecute,
                        addedAt: order.addedAt,
                        lastUpdate: order.lastUpdate
                    }))
                });
            } catch (error) {
                res.status(500).json({ error: 'Failed to get orders' });
            }
        });

        this.app.get('/orders/executable', async (req, res) => {
            try {
                const executableOrders = this.orderManager.getExecutableOrders();
                res.json({
                    total: executableOrders.length,
                    orders: executableOrders
                });
            } catch (error) {
                res.status(500).json({ error: 'Failed to get executable orders' });
            }
        });

        this.app.get('/orders/:orderHash', async (req, res) => {
            try {
                const order = this.orderManager.getOrder(req.params.orderHash);
                if (!order) {
                    return res.status(404).json({ error: 'Order not found' });
                }
                res.json(order);
            } catch (error) {
                res.status(500).json({ error: 'Failed to get order' });
            }
        });

        // Transactions endpoints
        this.app.get('/transactions', async (req, res) => {
            try {
                const transactions = this.transactionManager.getPendingTransactions();
                res.json({
                    total: transactions.length,
                    transactions: transactions.map(tx => ({
                        id: tx.id,
                        orderHash: tx.orderHash,
                        type: tx.type,
                        status: tx.status,
                        createdAt: tx.createdAt,
                        updatedAt: tx.updatedAt,
                        initiationTxHash: tx.initiationTxHash,
                        completionTxHash: tx.completionTxHash,
                        retryCount: tx.retryCount || 0
                    }))
                });
            } catch (error) {
                res.status(500).json({ error: 'Failed to get transactions' });
            }
        });

        this.app.get('/transactions/:id', async (req, res) => {
            try {
                const transaction = this.transactionManager.getTransaction(req.params.id);
                if (!transaction) {
                    return res.status(404).json({ error: 'Transaction not found' });
                }
                res.json(transaction);
            } catch (error) {
                res.status(500).json({ error: 'Failed to get transaction' });
            }
        });

        this.app.delete('/transactions/:id', async (req, res) => {
            try {
                await this.transactionManager.cancelTransaction(req.params.id);
                res.json({ message: 'Transaction cancelled' });
            } catch (error: any) {
                res.status(400).json({ error: error?.message || 'Failed to cancel transaction' });
            }
        });

        // Metrics endpoints
        this.app.get('/metrics', async (req, res) => {
            try {
                const metrics = {
                    uptime: process.uptime(),
                    memory: process.memoryUsage(),
                    trackedOrders: this.orderManager.getTrackedOrders().length,
                    executableOrders: this.orderManager.getExecutableOrders().length,
                    pendingTransactions: this.transactionManager.getPendingTransactionCount(),
                    lastHealthCheck: this.healthChecker.getLastHealthCheck()?.timestamp || 0
                };
                res.json(metrics);
            } catch (error) {
                res.status(500).json({ error: 'Failed to get metrics' });
            }
        });

        // Configuration endpoints (read-only)
        this.app.get('/config', async (req, res) => {
            try {
                const safeConfig = {
                    minProfitThreshold: this.config.get('MIN_PROFIT_THRESHOLD_ETH'),
                    maxGasPrice: this.config.get('MAX_GAS_PRICE_GWEI'),
                    maxPositionSize: this.config.get('MAX_POSITION_SIZE_ETH'),
                    auctionPollInterval: this.config.get('AUCTION_POLL_INTERVAL_MS'),
                    network: {
                        ethereum: {
                            chainId: this.config.get('ETHEREUM_CHAIN_ID'),
                            rpcUrl: this.config.get('ETHEREUM_RPC_URL')?.split('@')[1] || 'hidden' // Hide credentials
                        },
                        cardano: {
                            network: this.config.get('CARDANO_NETWORK')
                        }
                    }
                };
                res.json(safeConfig);
            } catch (error) {
                res.status(500).json({ error: 'Failed to get configuration' });
            }
        });

        // Version endpoint
        this.app.get('/version', (req, res) => {
            try {
                const packageJson = require('../../package.json');
                res.json({
                    name: packageJson.name,
                    version: packageJson.version,
                    description: packageJson.description,
                    nodeVersion: process.version,
                    platform: process.platform,
                    arch: process.arch
                });
            } catch (error) {
                res.status(500).json({ error: 'Failed to get version info' });
            }
        });

        // Root endpoint
        this.app.get('/', (req, res) => {
            res.json({
                message: 'Fusion Resolver Service API',
                version: require('../../package.json').version,
                uptime: process.uptime(),
                healthy: this.healthChecker.isHealthy(),
                endpoints: [
                    'GET /health',
                    'GET /status',
                    'GET /orders',
                    'GET /orders/executable',
                    'GET /orders/:orderHash',
                    'GET /transactions',
                    'GET /transactions/:id',
                    'DELETE /transactions/:id',
                    'GET /metrics',
                    'GET /config',
                    'GET /version'
                ]
            });
        });

        // Error handling middleware
        this.app.use((error: any, req: express.Request, res: express.Response, next: express.NextFunction) => {
            this.logger.error('API error:', error, {
                method: req.method,
                path: req.path,
                ip: req.ip
            });

            res.status(500).json({
                error: 'Internal server error',
                timestamp: new Date().toISOString()
            });
        });

        // 404 handler
        this.app.use((req: express.Request, res: express.Response) => {
            res.status(404).json({
                error: 'Endpoint not found',
                path: req.path,
                method: req.method
            });
        });
    }

    /**
     * Start the API server
     */
    async start(): Promise<void> {
        return new Promise((resolve, reject) => {
            const port = this.config.get('API_PORT');
            const host = this.config.get('API_HOST');

            this.server = this.app.listen(port, host, () => {
                this.logger.info(`API server started on ${host}:${port}`);
                resolve();
            });

            this.server.on('error', (error: any) => {
                this.logger.error('API server error:', error);
                reject(error);
            });
        });
    }

    /**
     * Stop the API server
     */
    async stop(): Promise<void> {
        return new Promise((resolve) => {
            if (this.server) {
                this.server.close(() => {
                    this.logger.info('API server stopped');
                    resolve();
                });
            } else {
                resolve();
            }
        });
    }

    /**
     * Get Express app instance
     */
    getApp(): express.Application {
        return this.app;
    }
}
