import axios, { AxiosInstance, AxiosError } from 'axios';
import {
  RegulatoryFinding,
  FAAWeatherData,
  FAAObstructionData
} from '../types';

/**
 * FAA API Client
 *
 * Provides access to FAA public data feeds:
 * - Aviation Weather (METAR, TAF, SIGMET, AIRMETs)
 * - Digital Obstruction File (DOF)
 * - Airspace data
 *
 * API Documentation:
 * - AviationWeather.gov: https://www.aviationweather.gov/data/api/
 * - FAA Data & Research: https://www.faa.gov/air_traffic/flight_info/aeronav/digital_products/
 *
 * LEGAL NOTICE:
 * - FAA public data APIs are free to use
 * - Some endpoints may require API key registration
 * - Weather data provided "as-is" - not for flight planning without verification
 * - Obstruction data may not be complete or current
 * - Respect rate limits (not explicitly published but be reasonable)
 */
export class FAAClient {
  private weatherClient: AxiosInstance;
  private apiKey?: string;
  private weatherBaseURL = 'https://aviationweather.gov/api/data';

  constructor(apiKey?: string) {
    this.apiKey = apiKey;

    this.weatherClient = axios.create({
      baseURL: this.weatherBaseURL,
      timeout: 30000,
      headers: {
        'Accept': 'application/json',
        'User-Agent': 'MotorHandPro-RegulatoryAPI/1.0'
      }
    });
  }

  /**
   * Get METAR weather data for an airport or area
   * @param station ICAO station identifier (e.g., "KJFK", "KLAX")
   * @param options Additional options
   */
  async getMETAR(
    station: string,
    options: {
      hoursBeforeNow?: number; // Look back this many hours (max 15 days)
      mostRecent?: boolean; // Only return most recent observation
    } = {}
  ): Promise<RegulatoryFinding> {
    const { hoursBeforeNow = 2, mostRecent = true } = options;

    const params: Record<string, any> = {
      ids: station.toUpperCase(),
      format: 'json',
      hours: hoursBeforeNow
    };

    if (mostRecent) {
      params.mostRecent = 'true';
    }

    try {
      const response = await this.weatherClient.get('/metar', { params });

      // Response is an array of METAR observations
      const data = Array.isArray(response.data) ? response.data : [response.data];

      return {
        source: 'FAA',
        type: 'metar',
        timestamp: new Date().toISOString(),
        data: data as FAAWeatherData[],
        metadata: {
          apiVersion: 'aviationweather.gov',
          queryParams: params,
          resultCount: data.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'getMETAR');
    }
  }

  /**
   * Get TAF (Terminal Aerodrome Forecast) data
   * @param station ICAO station identifier
   * @param options Additional options
   */
  async getTAF(
    station: string,
    options: {
      hoursBeforeNow?: number;
      mostRecent?: boolean;
    } = {}
  ): Promise<RegulatoryFinding> {
    const { hoursBeforeNow = 6, mostRecent = true } = options;

    const params: Record<string, any> = {
      ids: station.toUpperCase(),
      format: 'json',
      hours: hoursBeforeNow
    };

    if (mostRecent) {
      params.mostRecent = 'true';
    }

    try {
      const response = await this.weatherClient.get('/taf', { params });

      const data = Array.isArray(response.data) ? response.data : [response.data];

      return {
        source: 'FAA',
        type: 'taf',
        timestamp: new Date().toISOString(),
        data: data,
        metadata: {
          apiVersion: 'aviationweather.gov',
          queryParams: params,
          resultCount: data.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'getTAF');
    }
  }

  /**
   * Get SIGMET (Significant Meteorological Information) data
   * @param region Region code (e.g., "all", "us", "ak", "hi", "gm")
   */
  async getSIGMET(region: string = 'all'): Promise<RegulatoryFinding> {
    const params: Record<string, any> = {
      format: 'json',
      level: 'all'
    };

    if (region !== 'all') {
      params.hazard = region;
    }

    try {
      const response = await this.weatherClient.get('/sigmet', { params });

      const data = Array.isArray(response.data) ? response.data : [response.data];

      return {
        source: 'FAA',
        type: 'sigmet',
        timestamp: new Date().toISOString(),
        data: data,
        metadata: {
          apiVersion: 'aviationweather.gov',
          queryParams: params,
          resultCount: data.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'getSIGMET');
    }
  }

  /**
   * Get AIRMET (Airmen's Meteorological Information) data
   * @param region Region code
   */
  async getAIRMET(region: string = 'all'): Promise<RegulatoryFinding> {
    const params: Record<string, any> = {
      format: 'json'
    };

    if (region !== 'all') {
      params.hazard = region;
    }

    try {
      const response = await this.weatherClient.get('/airmet', { params });

      const data = Array.isArray(response.data) ? response.data : [response.data];

      return {
        source: 'FAA',
        type: 'airmet',
        timestamp: new Date().toISOString(),
        data: data,
        metadata: {
          apiVersion: 'aviationweather.gov',
          queryParams: params,
          resultCount: data.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'getAIRMET');
    }
  }

  /**
   * Get weather for a flight route
   * @param departure Departure ICAO code
   * @param arrival Arrival ICAO code
   */
  async getRouteWeather(
    departure: string,
    arrival: string
  ): Promise<RegulatoryFinding> {
    try {
      // Get METAR for both endpoints
      const [depMETAR, arrMETAR] = await Promise.all([
        this.getMETAR(departure),
        this.getMETAR(arrival)
      ]);

      return {
        source: 'FAA',
        type: 'route_weather',
        timestamp: new Date().toISOString(),
        data: {
          departure: {
            station: departure,
            metar: depMETAR.data
          },
          arrival: {
            station: arrival,
            metar: arrMETAR.data
          }
        },
        metadata: {
          apiVersion: 'aviationweather.gov',
          queryParams: { departure, arrival }
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'getRouteWeather');
    }
  }

  /**
   * Get PIREPs (Pilot Reports) for an area
   * @param station ICAO station or region
   * @param radiusNm Radius in nautical miles around station
   */
  async getPIREPs(
    station: string,
    radiusNm: number = 100
  ): Promise<RegulatoryFinding> {
    const params: Record<string, any> = {
      id: station.toUpperCase(),
      distance: radiusNm,
      format: 'json'
    };

    try {
      const response = await this.weatherClient.get('/pirep', { params });

      const data = Array.isArray(response.data) ? response.data : [response.data];

      return {
        source: 'FAA',
        type: 'pirep',
        timestamp: new Date().toISOString(),
        data: data,
        metadata: {
          apiVersion: 'aviationweather.gov',
          queryParams: params,
          resultCount: data.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'getPIREPs');
    }
  }

  /**
   * Query obstruction data (simulated - actual DOF data requires different access)
   * Note: Real DOF data is typically distributed as CSV/XML files, not a REST API
   * This is a placeholder for when/if you integrate with actual DOF data
   */
  async getObstructionData(
    latitude: number,
    longitude: number,
    radiusMiles: number = 5
  ): Promise<RegulatoryFinding> {
    // In production, you would query actual DOF database
    // For now, return structure showing how it would work
    return {
      source: 'FAA',
      type: 'obstruction_data',
      timestamp: new Date().toISOString(),
      data: {
        message: 'Obstruction data requires Digital Obstruction File (DOF) integration',
        queryParams: { latitude, longitude, radiusMiles },
        note: 'DOF is distributed as bulk files, not REST API. Requires local database.'
      },
      metadata: {
        apiVersion: 'placeholder',
        queryParams: { latitude, longitude, radiusMiles }
      }
    };
  }

  /**
   * Error handler
   */
  private handleError(error: AxiosError, operation: string): Error {
    if (error.response) {
      const status = error.response.status;
      const data = error.response.data as any;

      switch (status) {
        case 404:
          return new Error(`FAA API: No data found for ${operation}`);
        case 400:
          return new Error(
            `FAA API: Invalid parameters for ${operation}. ${data.message || error.message}`
          );
        case 503:
          return new Error(`FAA API: Service temporarily unavailable for ${operation}`);
        default:
          return new Error(
            `FAA API: ${operation} failed with status ${status}. ${data.message || error.message}`
          );
      }
    } else if (error.request) {
      return new Error(`FAA API: Network error during ${operation}. ${error.message}`);
    } else {
      return new Error(`FAA API: Unexpected error during ${operation}. ${error.message}`);
    }
  }
}
