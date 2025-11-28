import axios, { AxiosInstance, AxiosError } from 'axios';
import {
  RegulatoryFinding,
  NHTSAVehicleInfo,
  NHTSARecall,
  NHTSAComplaint
} from '../types';

/**
 * NHTSA API Client
 *
 * Provides access to NHTSA public data feeds:
 * - vPIC (Vehicle Product Information Catalog) - VIN decoding, specs
 * - Safety recalls
 * - Consumer complaints (ODI - Office of Defects Investigation)
 * - Crash test ratings
 *
 * API Documentation:
 * - vPIC: https://vpic.nhtsa.dot.gov/api/
 * - Safety API: https://www.nhtsa.gov/nhtsa-datasets-and-apis
 *
 * LEGAL NOTICE:
 * - NHTSA APIs are public and free to use
 * - No API key required for most endpoints
 * - Rate limits not explicitly published but be respectful
 * - Data provided as-is; verify critical safety information independently
 */
export class NHTSAClient {
  private vpicClient: AxiosInstance;
  private safetyClient: AxiosInstance;
  private vpicBaseURL = 'https://vpic.nhtsa.dot.gov/api/vehicles';
  private safetyBaseURL = 'https://api.nhtsa.gov';

  constructor() {
    this.vpicClient = axios.create({
      baseURL: this.vpicBaseURL,
      timeout: 30000,
      headers: {
        'Accept': 'application/json',
        'User-Agent': 'MotorHandPro-RegulatoryAPI/1.0'
      }
    });

    this.safetyClient = axios.create({
      baseURL: this.safetyBaseURL,
      timeout: 30000,
      headers: {
        'Accept': 'application/json',
        'User-Agent': 'MotorHandPro-RegulatoryAPI/1.0'
      }
    });
  }

  /**
   * Decode a VIN to get vehicle specifications
   * @param vin Vehicle Identification Number (17 characters)
   */
  async decodeVin(vin: string): Promise<RegulatoryFinding> {
    // VIN must be 17 characters and only contain allowed VIN characters: A-H, J-N, P, R-Z, 0-9 (I,O,Q excluded)
    const vinRegex = /^[A-HJ-NPR-Z0-9]{17}$/i;
    if (!vinRegex.test(vin)) {
      throw new Error('VIN must be exactly 17 characters and contain only allowed VIN characters (A-H, J-N, P, R-Z, 0-9; I, O, Q excluded)');
    }

    try {
      const response = await this.vpicClient.get(
        `/DecodeVinValues/${vin}?format=json`
      );

      const results = response.data.Results?.[0] || {};

      return {
        source: 'NHTSA',
        type: 'vehicle_info',
        timestamp: new Date().toISOString(),
        data: results as NHTSAVehicleInfo,
        metadata: {
          apiVersion: 'vPIC',
          queryParams: { vin }
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'decodeVin');
    }
  }

  /**
   * Get vehicle info by Make, Model, Year
   * @param make Vehicle make (e.g., "Toyota")
   * @param model Vehicle model (e.g., "Camry")
   * @param modelYear Model year (e.g., 2020)
   */
  async getVehicleInfo(
    make: string,
    model: string,
    modelYear: number
  ): Promise<RegulatoryFinding> {
    try {
      const response = await this.vpicClient.get(
        `/GetModelsForMakeYear/make/${encodeURIComponent(make)}/modelyear/${modelYear}?format=json`
      );

      const results = response.data.Results || [];
      const filtered = results.filter((v: any) =>
        v.Model_Name?.toLowerCase().includes(model.toLowerCase())
      );

      return {
        source: 'NHTSA',
        type: 'vehicle_models',
        timestamp: new Date().toISOString(),
        data: filtered,
        metadata: {
          apiVersion: 'vPIC',
          queryParams: { make, model, modelYear },
          resultCount: filtered.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'getVehicleInfo');
    }
  }

  /**
   * Search recalls by Make, Model, Year
   * @param make Vehicle make
   * @param model Vehicle model
   * @param modelYear Model year
   */
  async getRecallsByMakeModelYear(
    make: string,
    model: string,
    modelYear: number
  ): Promise<RegulatoryFinding> {
    try {
      const response = await this.safetyClient.get(
        `/recalls/recallsByVehicle`,
        {
          params: {
            make,
            model,
            modelYear
          }
        }
      );

      const results = response.data.results || [];

      return {
        source: 'NHTSA',
        type: 'recalls',
        timestamp: new Date().toISOString(),
        data: results as NHTSARecall[],
        metadata: {
          apiVersion: 'safety',
          queryParams: { make, model, modelYear },
          resultCount: results.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'getRecallsByMakeModelYear');
    }
  }

  /**
   * Search recalls by campaign number
   * @param campaignNumber NHTSA campaign number (e.g., "20V123000")
   */
  async getRecallByCampaignNumber(
    campaignNumber: string
  ): Promise<RegulatoryFinding> {
    try {
      const response = await this.safetyClient.get(
        `/recalls/recallsByCampaignNumber`,
        {
          params: { campaignNumber }
        }
      );

      const results = response.data.results || [];

      return {
        source: 'NHTSA',
        type: 'recall_detail',
        timestamp: new Date().toISOString(),
        data: results[0] || null,
        metadata: {
          apiVersion: 'safety',
          queryParams: { campaignNumber }
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'getRecallByCampaignNumber');
    }
  }

  /**
   * Search consumer complaints (ODI)
   * @param make Vehicle make
   * @param model Vehicle model
   * @param modelYear Model year
   */
  async getComplaintsByMakeModelYear(
    make: string,
    model: string,
    modelYear: number
  ): Promise<RegulatoryFinding> {
    try {
      const response = await this.safetyClient.get(
        `/complaints/complaintsByVehicle`,
        {
          params: {
            make,
            model,
            modelYear
          }
        }
      );

      const results = response.data.results || [];

      return {
        source: 'NHTSA',
        type: 'complaints',
        timestamp: new Date().toISOString(),
        data: results as NHTSAComplaint[],
        metadata: {
          apiVersion: 'safety',
          queryParams: { make, model, modelYear },
          resultCount: results.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'getComplaintsByMakeModelYear');
    }
  }

  /**
   * Get manufacturer info
   * @param manufacturer Manufacturer name
   */
  async getManufacturerInfo(manufacturer: string): Promise<RegulatoryFinding> {
    try {
      const response = await this.vpicClient.get(
        `/GetManufacturerDetails/${encodeURIComponent(manufacturer)}?format=json`
      );

      const results = response.data.Results || [];

      return {
        source: 'NHTSA',
        type: 'manufacturer_info',
        timestamp: new Date().toISOString(),
        data: results,
        metadata: {
          apiVersion: 'vPIC',
          queryParams: { manufacturer },
          resultCount: results.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'getManufacturerInfo');
    }
  }

  /**
   * Get safety ratings (5-Star Rating)
   * @param make Vehicle make
   * @param model Vehicle model
   * @param modelYear Model year
   */
  async getSafetyRatings(
    make: string,
    model: string,
    modelYear: number
  ): Promise<RegulatoryFinding> {
    try {
      const response = await this.safetyClient.get(
        `/SafetyRatings/modelyear/${modelYear}/make/${encodeURIComponent(make)}/model/${encodeURIComponent(model)}`
      );

      const results = response.data.Results || [];

      return {
        source: 'NHTSA',
        type: 'safety_ratings',
        timestamp: new Date().toISOString(),
        data: results,
        metadata: {
          apiVersion: 'safety',
          queryParams: { make, model, modelYear },
          resultCount: results.length
        }
      };
    } catch (error) {
      throw this.handleError(error as AxiosError, 'getSafetyRatings');
    }
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
          return new Error(`NHTSA API: No results found for ${operation}`);
        case 400:
          return new Error(
            `NHTSA API: Invalid parameters for ${operation}. ${data.message || error.message}`
          );
        default:
          return new Error(
            `NHTSA API: ${operation} failed with status ${status}. ${data.message || error.message}`
          );
      }
    } else if (error.request) {
      return new Error(`NHTSA API: Network error during ${operation}. ${error.message}`);
    } else {
      return new Error(`NHTSA API: Unexpected error during ${operation}. ${error.message}`);
    }
  }
}
