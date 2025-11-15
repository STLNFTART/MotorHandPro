import axios, { AxiosInstance, AxiosError } from 'axios';
import {
  RegulatoryFinding,
  FDARecall,
  FDAAdverseEvent,
  FDADeviceEvent,
  RateLimitInfo
} from '../types';

/**
 * openFDA API Client
 *
 * Provides access to FDA public data feeds:
 * - Drug/device/food recalls
 * - Adverse event reports
 * - Device event reports
 *
 * API Documentation: https://open.fda.gov/apis/
 *
 * LEGAL NOTICE:
 * - openFDA is a public API with no authentication required for most endpoints
 * - Rate limits: 240 requests per minute, 120,000 requests per day (without API key)
 * - With API key: 240 requests per minute, no daily limit
 * - Usage subject to openFDA Terms of Service
 * - Do not use for clinical decision-making without proper validation
 */
export class FDAClient {
  private client: AxiosInstance;
  private apiKey?: string;
  private baseURL = 'https://api.fda.gov';
  private rateLimitInfo: RateLimitInfo = { limit: 240, remaining: 240 };

  constructor(apiKey?: string) {
    this.apiKey = apiKey;
    this.client = axios.create({
      baseURL: this.baseURL,
      timeout: 30000,
      headers: {
        'Accept': 'application/json',
        'User-Agent': 'MotorHandPro-RegulatoryAPI/1.0'
      }
    });

    // Add response interceptor to track rate limits
    this.client.interceptors.response.use(
      response => {
        // Extract rate limit headers if present
        const limit = response.headers['x-ratelimit-limit'];
        const remaining = response.headers['x-ratelimit-remaining'];
        if (limit) this.rateLimitInfo.limit = parseInt(limit);
        if (remaining) this.rateLimitInfo.remaining = parseInt(remaining);
        return response;
      },
      error => {
        return Promise.reject(error);
      }
    );
  }

  /**
   * Search drug recalls
   * @param query Search query (e.g., product name, firm name)
   * @param options Additional filter options
   */
  async searchDrugRecalls(
    query: string,
    options: {
      limit?: number;
      skip?: number;
      classification?: '1' | '2' | '3'; // Class I, II, or III
      dateRange?: { start: string; end: string };
    } = {}
  ): Promise<RegulatoryFinding> {
    const { limit = 10, skip = 0, classification, dateRange } = options;

    // Build search query
    let searchQuery = `product_description:"${query}"`;
    if (classification) {
      searchQuery += `+AND+classification:"Class+${classification}"`;
    }
    if (dateRange) {
      searchQuery += `+AND+recall_initiation_date:[${dateRange.start}+TO+${dateRange.end}]`;
    }

    const params: Record<string, any> = {
      search: searchQuery,
      limit,
      skip
    };

    if (this.apiKey) {
      params.api_key = this.apiKey;
    }

    try {
      const response = await this.client.get('/drug/enforcement.json', { params });

      return {
        source: 'FDA',
        type: 'drug_recall',
        timestamp: new Date().toISOString(),
        data: response.data.results as FDARecall[],
        metadata: {
          apiVersion: 'v1',
          queryParams: params,
          resultCount: response.data.meta?.results?.total || response.data.results.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'searchDrugRecalls');
    }
  }

  /**
   * Search device recalls
   * @param query Search query (e.g., product description, device name)
   * @param options Additional filter options
   */
  async searchDeviceRecalls(
    query: string,
    options: {
      limit?: number;
      skip?: number;
      classification?: '1' | '2' | '3';
      dateRange?: { start: string; end: string };
    } = {}
  ): Promise<RegulatoryFinding> {
    const { limit = 10, skip = 0, classification, dateRange } = options;

    let searchQuery = `product_description:"${query}"`;
    if (classification) {
      searchQuery += `+AND+classification:"Class+${classification}"`;
    }
    if (dateRange) {
      searchQuery += `+AND+recall_initiation_date:[${dateRange.start}+TO+${dateRange.end}]`;
    }

    const params: Record<string, any> = {
      search: searchQuery,
      limit,
      skip
    };

    if (this.apiKey) {
      params.api_key = this.apiKey;
    }

    try {
      const response = await this.client.get('/device/enforcement.json', { params });

      return {
        source: 'FDA',
        type: 'device_recall',
        timestamp: new Date().toISOString(),
        data: response.data.results as FDARecall[],
        metadata: {
          apiVersion: 'v1',
          queryParams: params,
          resultCount: response.data.meta?.results?.total || response.data.results.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'searchDeviceRecalls');
    }
  }

  /**
   * Search drug adverse event reports
   * @param drugName Drug/medicinal product name
   * @param options Additional filter options
   */
  async searchAdverseEvents(
    drugName: string,
    options: {
      limit?: number;
      skip?: number;
      serious?: boolean;
      dateRange?: { start: string; end: string };
    } = {}
  ): Promise<RegulatoryFinding> {
    const { limit = 10, skip = 0, serious, dateRange } = options;

    let searchQuery = `patient.drug.medicinalproduct:"${drugName}"`;
    if (serious !== undefined) {
      searchQuery += `+AND+serious:${serious ? '1' : '2'}`;
    }
    if (dateRange) {
      searchQuery += `+AND+receivedate:[${dateRange.start}+TO+${dateRange.end}]`;
    }

    const params: Record<string, any> = {
      search: searchQuery,
      limit,
      skip
    };

    if (this.apiKey) {
      params.api_key = this.apiKey;
    }

    try {
      const response = await this.client.get('/drug/event.json', { params });

      return {
        source: 'FDA',
        type: 'adverse_event',
        timestamp: new Date().toISOString(),
        data: response.data.results as FDAAdverseEvent[],
        metadata: {
          apiVersion: 'v1',
          queryParams: params,
          resultCount: response.data.meta?.results?.total || response.data.results.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'searchAdverseEvents');
    }
  }

  /**
   * Search device adverse event reports (MAUDE database)
   * @param productDescription Device product description
   * @param options Additional filter options
   */
  async searchDeviceEvents(
    productDescription: string,
    options: {
      limit?: number;
      skip?: number;
      dateRange?: { start: string; end: string };
    } = {}
  ): Promise<RegulatoryFinding> {
    const { limit = 10, skip = 0, dateRange } = options;

    let searchQuery = `device.generic_name:"${productDescription}"`;
    if (dateRange) {
      searchQuery += `+AND+date_received:[${dateRange.start}+TO+${dateRange.end}]`;
    }

    const params: Record<string, any> = {
      search: searchQuery,
      limit,
      skip
    };

    if (this.apiKey) {
      params.api_key = this.apiKey;
    }

    try {
      const response = await this.client.get('/device/event.json', { params });

      return {
        source: 'FDA',
        type: 'device_event',
        timestamp: new Date().toISOString(),
        data: response.data.results as FDADeviceEvent[],
        metadata: {
          apiVersion: 'v1',
          queryParams: params,
          resultCount: response.data.meta?.results?.total || response.data.results.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'searchDeviceEvents');
    }
  }

  /**
   * Get current rate limit information
   */
  getRateLimitInfo(): RateLimitInfo {
    return { ...this.rateLimitInfo };
  }

  /**
   * Error handler with retry logic
   */
  private handleError(error: AxiosError, operation: string): Error {
    if (error.response) {
      const status = error.response.status;
      const data = error.response.data as any;

      switch (status) {
        case 404:
          return new Error(`FDA API: No results found for ${operation}`);
        case 429:
          const resetTime = error.response.headers['x-ratelimit-reset'];
          return new Error(
            `FDA API: Rate limit exceeded. ${resetTime ? `Resets at ${new Date(parseInt(resetTime) * 1000)}` : 'Try again later'}`
          );
        case 400:
          return new Error(
            `FDA API: Invalid query for ${operation}. ${data.error?.message || 'Check query syntax'}`
          );
        default:
          return new Error(
            `FDA API: ${operation} failed with status ${status}. ${data.error?.message || error.message}`
          );
      }
    } else if (error.request) {
      return new Error(`FDA API: Network error during ${operation}. ${error.message}`);
    } else {
      return new Error(`FDA API: Unexpected error during ${operation}. ${error.message}`);
    }
  }
}
