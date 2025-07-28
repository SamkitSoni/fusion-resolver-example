import dotenv from 'dotenv';

dotenv.config();

/**
 * Configuration Manager for the Fusion Resolver Service
 */
export class ConfigManager {
    private config: Map<string, any> = new Map();

    constructor() {
        this.loadConfiguration();
    }

    private loadConfiguration(): void {
        // Blockchain Configuration
        this.set('ETHEREUM_RPC_URL', process.env.ETHEREUM_RPC_URL);
        this.set('ETHEREUM_CHAIN_ID', parseInt(process.env.ETHEREUM_CHAIN_ID || '1'));
        this.set('ETHEREUM_WSS_URL', process.env.ETHEREUM_WSS_URL);

        // Resolver Contract
        this.set('RESOLVER_CONTRACT_ADDRESS', process.env.RESOLVER_CONTRACT_ADDRESS);
        this.set('RESOLVER_PRIVATE_KEY', process.env.RESOLVER_PRIVATE_KEY);

        // 1inch Fusion+ Configuration
        this.set('FUSION_API_URL', process.env.FUSION_API_URL || 'https://api.1inch.dev/fusion');
        this.set('FUSION_API_KEY', process.env.FUSION_API_KEY);
        this.set('FUSION_WEBSOCKET_URL', process.env.FUSION_WEBSOCKET_URL || 'wss://ws.1inch.dev/fusion');

        // Cardano Configuration
        this.set('CARDANO_NETWORK', process.env.CARDANO_NETWORK || 'mainnet');
        this.set('CARDANO_SOCKET_PATH', process.env.CARDANO_SOCKET_PATH);
        this.set('CARDANO_TESTNET_MAGIC', parseInt(process.env.CARDANO_TESTNET_MAGIC || '1097911063'));

        // Trading Configuration
        this.set('MIN_PROFIT_THRESHOLD_ETH', parseFloat(process.env.MIN_PROFIT_THRESHOLD_ETH || '0.01'));
        this.set('MAX_GAS_PRICE_GWEI', parseInt(process.env.MAX_GAS_PRICE_GWEI || '50'));
        this.set('SAFETY_MARGIN_PERCENT', parseInt(process.env.SAFETY_MARGIN_PERCENT || '10'));
        this.set('MAX_POSITION_SIZE_ETH', parseFloat(process.env.MAX_POSITION_SIZE_ETH || '1.0'));

        // Monitoring Configuration
        this.set('AUCTION_POLL_INTERVAL_MS', parseInt(process.env.AUCTION_POLL_INTERVAL_MS || '1000'));
        this.set('PRICE_UPDATE_INTERVAL_MS', parseInt(process.env.PRICE_UPDATE_INTERVAL_MS || '5000'));
        this.set('HEALTH_CHECK_INTERVAL_MS', parseInt(process.env.HEALTH_CHECK_INTERVAL_MS || '30000'));

        // Redis Configuration
        this.set('REDIS_URL', process.env.REDIS_URL || 'redis://localhost:6379');
        this.set('REDIS_PASSWORD', process.env.REDIS_PASSWORD);

        // Logging
        this.set('LOG_LEVEL', process.env.LOG_LEVEL || 'info');
        this.set('LOG_FILE', process.env.LOG_FILE || './logs/resolver.log');

        // API Server
        this.set('API_PORT', parseInt(process.env.API_PORT || '3000'));
        this.set('API_HOST', process.env.API_HOST || '0.0.0.0');

        // Security
        this.set('ENABLE_AUTH', process.env.ENABLE_AUTH === 'true');
        this.set('API_KEY', process.env.API_KEY);

        // Alerts and Notifications
        this.set('WEBHOOK_URL', process.env.WEBHOOK_URL);
        this.set('SLACK_WEBHOOK_URL', process.env.SLACK_WEBHOOK_URL);
        this.set('TELEGRAM_BOT_TOKEN', process.env.TELEGRAM_BOT_TOKEN);
        this.set('TELEGRAM_CHAT_ID', process.env.TELEGRAM_CHAT_ID);

        // Additional trading parameters
        this.set('MIN_ORDER_SIZE_ETH', process.env.MIN_ORDER_SIZE_ETH || '0.1');
        this.set('MAX_ORDER_SIZE_ETH', process.env.MAX_ORDER_SIZE_ETH || '10');
        this.set('MAX_SLIPPAGE_PERCENT', parseInt(process.env.MAX_SLIPPAGE_PERCENT || '5'));
        this.set('GAS_LIMIT_MULTIPLIER', parseFloat(process.env.GAS_LIMIT_MULTIPLIER || '1.2'));

        // Cross-chain timing
        this.set('ETH_CONFIRMATION_BLOCKS', parseInt(process.env.ETH_CONFIRMATION_BLOCKS || '3'));
        this.set('CARDANO_CONFIRMATION_TIME_MS', parseInt(process.env.CARDANO_CONFIRMATION_TIME_MS || '120000'));
        this.set('ATOMIC_SWAP_TIMEOUT_HOURS', parseInt(process.env.ATOMIC_SWAP_TIMEOUT_HOURS || '24'));

        // Performance tuning
        this.set('MAX_CONCURRENT_ORDERS', parseInt(process.env.MAX_CONCURRENT_ORDERS || '10'));
        this.set('ORDER_CLEANUP_INTERVAL_MS', parseInt(process.env.ORDER_CLEANUP_INTERVAL_MS || '300000'));
        this.set('CACHE_TTL_SECONDS', parseInt(process.env.CACHE_TTL_SECONDS || '300'));
    }

    /**
     * Get configuration value
     */
    get<T = any>(key: string, defaultValue?: T): T {
        return this.config.has(key) ? this.config.get(key) : defaultValue as T;
    }

    /**
     * Set configuration value
     */
    set(key: string, value: any): void {
        this.config.set(key, value);
    }

    /**
     * Check if configuration key exists
     */
    has(key: string): boolean {
        return this.config.has(key);
    }

    /**
     * Get all configuration as object
     */
    getAll(): { [key: string]: any } {
        const result: { [key: string]: any } = {};
        for (const [key, value] of this.config.entries()) {
            result[key] = value;
        }
        return result;
    }

    /**
     * Validate required configuration
     */
    validateRequired(requiredKeys: string[]): void {
        const missing: string[] = [];
        
        for (const key of requiredKeys) {
            if (!this.has(key) || this.get(key) === undefined || this.get(key) === null) {
                missing.push(key);
            }
        }
        
        if (missing.length > 0) {
            throw new Error(`Missing required configuration keys: ${missing.join(', ')}`);
        }
    }

    /**
     * Get trading configuration
     */
    getTradingConfig(): TradingConfig {
        return {
            minProfitThreshold: this.get('MIN_PROFIT_THRESHOLD_ETH'),
            maxGasPrice: this.get('MAX_GAS_PRICE_GWEI'),
            safetyMargin: this.get('SAFETY_MARGIN_PERCENT'),
            maxPositionSize: this.get('MAX_POSITION_SIZE_ETH'),
            maxSlippage: this.get('MAX_SLIPPAGE_PERCENT'),
            gasLimitMultiplier: this.get('GAS_LIMIT_MULTIPLIER'),
            minOrderSize: parseFloat(this.get('MIN_ORDER_SIZE_ETH')),
            maxOrderSize: parseFloat(this.get('MAX_ORDER_SIZE_ETH')),
            maxConcurrentOrders: this.get('MAX_CONCURRENT_ORDERS')
        };
    }

    /**
     * Get monitoring configuration
     */
    getMonitoringConfig(): MonitoringConfig {
        return {
            auctionPollInterval: this.get('AUCTION_POLL_INTERVAL_MS'),
            priceUpdateInterval: this.get('PRICE_UPDATE_INTERVAL_MS'),
            healthCheckInterval: this.get('HEALTH_CHECK_INTERVAL_MS'),
            orderCleanupInterval: this.get('ORDER_CLEANUP_INTERVAL_MS'),
            cacheTTL: this.get('CACHE_TTL_SECONDS')
        };
    }

    /**
     * Get cross-chain configuration
     */
    getCrossChainConfig(): CrossChainConfig {
        return {
            ethConfirmationBlocks: this.get('ETH_CONFIRMATION_BLOCKS'),
            cardanoConfirmationTime: this.get('CARDANO_CONFIRMATION_TIME_MS'),
            atomicSwapTimeout: this.get('ATOMIC_SWAP_TIMEOUT_HOURS') * 3600 * 1000, // Convert to ms
            cardanoNetwork: this.get('CARDANO_NETWORK'),
            cardanoSocketPath: this.get('CARDANO_SOCKET_PATH'),
            cardanoTestnetMagic: this.get('CARDANO_TESTNET_MAGIC')
        };
    }
}

// Configuration interfaces
export interface TradingConfig {
    minProfitThreshold: number;
    maxGasPrice: number;
    safetyMargin: number;
    maxPositionSize: number;
    maxSlippage: number;
    gasLimitMultiplier: number;
    minOrderSize: number;
    maxOrderSize: number;
    maxConcurrentOrders: number;
}

export interface MonitoringConfig {
    auctionPollInterval: number;
    priceUpdateInterval: number;
    healthCheckInterval: number;
    orderCleanupInterval: number;
    cacheTTL: number;
}

export interface CrossChainConfig {
    ethConfirmationBlocks: number;
    cardanoConfirmationTime: number;
    atomicSwapTimeout: number;
    cardanoNetwork: string;
    cardanoSocketPath: string;
    cardanoTestnetMagic: number;
}
