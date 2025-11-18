// Core types for regulatory API responses

export interface RegulatoryFinding {
  source: 'FDA' | 'NHTSA' | 'FAA' | 'CMS';
  type: string;
  timestamp: string;
  data: any;
  metadata: {
    apiVersion?: string;
    queryParams?: Record<string, any>;
    resultCount?: number;
  };
}

// FDA Types
export interface FDARecall {
  recall_number?: string;
  recall_initiation_date?: string;
  status?: string;
  classification?: string;
  product_description?: string;
  code_info?: string;
  product_quantity?: string;
  reason_for_recall?: string;
  recalling_firm?: string;
  city?: string;
  state?: string;
  country?: string;
}

export interface FDAAdverseEvent {
  safetyreportid?: string;
  receivedate?: string;
  serious?: string;
  seriousnessdeath?: string;
  patient?: {
    patientagegroup?: string;
    patientsex?: string;
    patientweight?: string;
  };
  drug?: Array<{
    medicinalproduct?: string;
    drugcharacterization?: string;
  }>;
  reaction?: Array<{
    reactionmeddrapt?: string;
  }>;
}

export interface FDADeviceEvent {
  report_number?: string;
  device_date_of_manufacturer?: string;
  date_received?: string;
  product_description?: string;
  device_event_key?: string;
  event_type?: string;
  adverse_event_flag?: string;
  product_problem_flag?: string;
}

// NHTSA Types
export interface NHTSAVehicleInfo {
  Make?: string;
  Model?: string;
  ModelYear?: string;
  VIN?: string;
  PlantCountry?: string;
  Manufacturer?: string;
  VehicleType?: string;
  BodyClass?: string;
  EngineModel?: string;
  FuelTypePrimary?: string;
}

export interface NHTSARecall {
  NHTSACampaignNumber?: string;
  Manufacturer?: string;
  Subject?: string;
  Component?: string;
  Summary?: string;
  Consequence?: string;
  Remedy?: string;
  ReportReceivedDate?: string;
  ModelYear?: string;
  Make?: string;
  Model?: string;
}

export interface NHTSAComplaint {
  ODINumber?: string;
  Manufacturer?: string;
  Crash?: string;
  Fire?: string;
  NumberOfInjuries?: string;
  DateOfIncident?: string;
  Components?: string;
  Summary?: string;
  ModelYear?: string;
  Make?: string;
  Model?: string;
}

// FAA Types
export interface FAAWeatherData {
  icaoId?: string;
  receiptTime?: string;
  reportTime?: string;
  temp?: number;
  dewp?: number;
  wdir?: number;
  wspd?: number;
  wgst?: number;
  visib?: number;
  altim?: number;
  slp?: number;
  qcField?: number;
  metar?: string;
  rawOb?: string;
}

export interface FAAObstructionData {
  oas_number?: string;
  verification_status?: string;
  type_code?: string;
  quantity?: number;
  marking_indicator?: string;
  lighting_indicator?: string;
  horizontal_accuracy?: string;
  latitude_degrees?: number;
  latitude_minutes?: number;
  latitude_seconds?: number;
  latitude_hemisphere?: string;
  longitude_degrees?: number;
  longitude_minutes?: number;
  longitude_seconds?: number;
  longitude_hemisphere?: string;
  state_name?: string;
  city_name?: string;
}

// Rate limiting / API health
export interface RateLimitInfo {
  limit: number;
  remaining: number;
  reset?: Date;
}

export interface APIHealthStatus {
  service: string;
  status: 'healthy' | 'degraded' | 'down';
  latency?: number;
  lastCheck: Date;
  rateLimitInfo?: RateLimitInfo;
}
