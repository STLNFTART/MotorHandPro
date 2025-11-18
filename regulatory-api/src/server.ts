import express, { Request, Response, NextFunction } from 'express';
import cors from 'cors';
import rateLimit from 'express-rate-limit';
import dotenv from 'dotenv';
import { FDAClient } from './reg/fda';
import { NHTSAClient } from './reg/nhtsa';
import { FAAClient } from './reg/faa';

// Load environment variables
dotenv.config();

const app = express();
const PORT = process.env.PORT || 3000;

// Initialize regulatory clients
const fdaClient = new FDAClient(process.env.FDA_API_KEY);
const nhtsaClient = new NHTSAClient();
const faaClient = new FAAClient(process.env.FAA_API_KEY);

// Middleware
app.use(cors());
app.use(express.json());

// Rate limiting: 100 requests per 15 minutes per IP
const limiter = rateLimit({
  windowMs: 15 * 60 * 1000,
  max: 100,
  message: 'Too many requests from this IP, please try again later.',
  standardHeaders: true,
  legacyHeaders: false,
});

app.use('/api/', limiter);

// Health check
app.get('/health', (req: Request, res: Response) => {
  res.json({
    status: 'healthy',
    timestamp: new Date().toISOString(),
    services: {
      fda: 'available',
      nhtsa: 'available',
      faa: 'available'
    }
  });
});

// ========== FDA ENDPOINTS ==========

/**
 * GET /api/fda/recalls/drug
 * Query params: query (required), limit, skip, classification, startDate, endDate
 */
app.get('/api/fda/recalls/drug', async (req: Request, res: Response) => {
  try {
    const { query, limit, skip, classification, startDate, endDate } = req.query;

    if (!query || typeof query !== 'string') {
      return res.status(400).json({ error: 'Query parameter required' });
    }

    const options: any = {};
    if (limit) options.limit = parseInt(limit as string);
    if (skip) options.skip = parseInt(skip as string);
    if (classification) options.classification = classification;
    if (startDate && endDate) {
      options.dateRange = { start: startDate as string, end: endDate as string };
    }

    const result = await fdaClient.searchDrugRecalls(query, options);
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/fda/recalls/device
 * Query params: query (required), limit, skip, classification, startDate, endDate
 */
app.get('/api/fda/recalls/device', async (req: Request, res: Response) => {
  try {
    const { query, limit, skip, classification, startDate, endDate } = req.query;

    if (!query || typeof query !== 'string') {
      return res.status(400).json({ error: 'Query parameter required' });
    }

    const options: any = {};
    if (limit) options.limit = parseInt(limit as string);
    if (skip) options.skip = parseInt(skip as string);
    if (classification) options.classification = classification;
    if (startDate && endDate) {
      options.dateRange = { start: startDate as string, end: endDate as string };
    }

    const result = await fdaClient.searchDeviceRecalls(query, options);
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/fda/events/adverse
 * Query params: drug (required), limit, skip, serious, startDate, endDate
 */
app.get('/api/fda/events/adverse', async (req: Request, res: Response) => {
  try {
    const { drug, limit, skip, serious, startDate, endDate } = req.query;

    if (!drug || typeof drug !== 'string') {
      return res.status(400).json({ error: 'Drug parameter required' });
    }

    const options: any = {};
    if (limit) options.limit = parseInt(limit as string);
    if (skip) options.skip = parseInt(skip as string);
    if (serious) options.serious = serious === 'true';
    if (startDate && endDate) {
      options.dateRange = { start: startDate as string, end: endDate as string };
    }

    const result = await fdaClient.searchAdverseEvents(drug, options);
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/fda/events/device
 * Query params: product (required), limit, skip, startDate, endDate
 */
app.get('/api/fda/events/device', async (req: Request, res: Response) => {
  try {
    const { product, limit, skip, startDate, endDate } = req.query;

    if (!product || typeof product !== 'string') {
      return res.status(400).json({ error: 'Product parameter required' });
    }

    const options: any = {};
    if (limit) options.limit = parseInt(limit as string);
    if (skip) options.skip = parseInt(skip as string);
    if (startDate && endDate) {
      options.dateRange = { start: startDate as string, end: endDate as string };
    }

    const result = await fdaClient.searchDeviceEvents(product, options);
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/fda/ratelimit
 */
app.get('/api/fda/ratelimit', (req: Request, res: Response) => {
  res.json(fdaClient.getRateLimitInfo());
});

// ========== NHTSA ENDPOINTS ==========

/**
 * GET /api/nhtsa/vin/:vin
 * Decode VIN
 */
app.get('/api/nhtsa/vin/:vin', async (req: Request, res: Response) => {
  try {
    const vin = req.params.vin;
    if (!vin) {
      return res.status(400).json({ error: 'VIN parameter required' });
    }
    const result = await nhtsaClient.decodeVin(vin);
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/nhtsa/vehicle
 * Query params: make, model, year (all required)
 */
app.get('/api/nhtsa/vehicle', async (req: Request, res: Response) => {
  try {
    const { make, model, year } = req.query;

    if (!make || !model || !year) {
      return res.status(400).json({ error: 'Make, model, and year parameters required' });
    }

    const result = await nhtsaClient.getVehicleInfo(
      make as string,
      model as string,
      parseInt(year as string)
    );
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/nhtsa/recalls
 * Query params: make, model, year (all required)
 */
app.get('/api/nhtsa/recalls', async (req: Request, res: Response) => {
  try {
    const { make, model, year, campaign } = req.query;

    if (campaign) {
      const result = await nhtsaClient.getRecallByCampaignNumber(campaign as string);
      return res.json(result);
    }

    if (!make || !model || !year) {
      return res.status(400).json({
        error: 'Either campaign number OR (make, model, year) required'
      });
    }

    const result = await nhtsaClient.getRecallsByMakeModelYear(
      make as string,
      model as string,
      parseInt(year as string)
    );
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/nhtsa/complaints
 * Query params: make, model, year (all required)
 */
app.get('/api/nhtsa/complaints', async (req: Request, res: Response) => {
  try {
    const { make, model, year } = req.query;

    if (!make || !model || !year) {
      return res.status(400).json({ error: 'Make, model, and year parameters required' });
    }

    const result = await nhtsaClient.getComplaintsByMakeModelYear(
      make as string,
      model as string,
      parseInt(year as string)
    );
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/nhtsa/safety-ratings
 * Query params: make, model, year (all required)
 */
app.get('/api/nhtsa/safety-ratings', async (req: Request, res: Response) => {
  try {
    const { make, model, year } = req.query;

    if (!make || !model || !year) {
      return res.status(400).json({ error: 'Make, model, and year parameters required' });
    }

    const result = await nhtsaClient.getSafetyRatings(
      make as string,
      model as string,
      parseInt(year as string)
    );
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/nhtsa/manufacturer
 * Query params: name (required)
 */
app.get('/api/nhtsa/manufacturer', async (req: Request, res: Response) => {
  try {
    const { name } = req.query;

    if (!name || typeof name !== 'string') {
      return res.status(400).json({ error: 'Manufacturer name required' });
    }

    const result = await nhtsaClient.getManufacturerInfo(name);
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

// ========== FAA ENDPOINTS ==========

/**
 * GET /api/faa/weather/metar
 * Query params: station (required), hours, mostRecent
 */
app.get('/api/faa/weather/metar', async (req: Request, res: Response) => {
  try {
    const { station, hours, mostRecent } = req.query;

    if (!station || typeof station !== 'string') {
      return res.status(400).json({ error: 'Station parameter required (ICAO code)' });
    }

    const options: any = {};
    if (hours) options.hoursBeforeNow = parseInt(hours as string);
    if (mostRecent !== undefined) options.mostRecent = mostRecent === 'true';

    const result = await faaClient.getMETAR(station, options);
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/faa/weather/taf
 * Query params: station (required), hours, mostRecent
 */
app.get('/api/faa/weather/taf', async (req: Request, res: Response) => {
  try {
    const { station, hours, mostRecent } = req.query;

    if (!station || typeof station !== 'string') {
      return res.status(400).json({ error: 'Station parameter required (ICAO code)' });
    }

    const options: any = {};
    if (hours) options.hoursBeforeNow = parseInt(hours as string);
    if (mostRecent !== undefined) options.mostRecent = mostRecent === 'true';

    const result = await faaClient.getTAF(station, options);
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/faa/weather/route
 * Query params: departure, arrival (both required, ICAO codes)
 */
app.get('/api/faa/weather/route', async (req: Request, res: Response) => {
  try {
    const { departure, arrival } = req.query;

    if (!departure || !arrival) {
      return res.status(400).json({
        error: 'Departure and arrival station codes required'
      });
    }

    const result = await faaClient.getRouteWeather(
      departure as string,
      arrival as string
    );
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/faa/weather/sigmet
 * Query params: region (optional, default 'all')
 */
app.get('/api/faa/weather/sigmet', async (req: Request, res: Response) => {
  try {
    const { region } = req.query;
    const result = await faaClient.getSIGMET((region as string) || 'all');
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/faa/weather/airmet
 * Query params: region (optional, default 'all')
 */
app.get('/api/faa/weather/airmet', async (req: Request, res: Response) => {
  try {
    const { region } = req.query;
    const result = await faaClient.getAIRMET((region as string) || 'all');
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

/**
 * GET /api/faa/weather/pirep
 * Query params: station (required), radius (optional, default 100 nm)
 */
app.get('/api/faa/weather/pirep', async (req: Request, res: Response) => {
  try {
    const { station, radius } = req.query;

    if (!station || typeof station !== 'string') {
      return res.status(400).json({ error: 'Station parameter required' });
    }

    const radiusNm = radius ? parseInt(radius as string) : 100;
    const result = await faaClient.getPIREPs(station, radiusNm);
    res.json(result);
  } catch (error: any) {
    res.status(500).json({ error: error.message });
  }
});

// Error handling middleware
app.use((err: Error, req: Request, res: Response, next: NextFunction) => {
  console.error('Server error:', err);
  res.status(500).json({
    error: 'Internal server error',
    message: err.message
  });
});

// Start server
app.listen(PORT, () => {
  console.log(`Regulatory API server running on port ${PORT}`);
  console.log(`\nAvailable endpoints:`);
  console.log(`  GET /health - API health check`);
  console.log(`\n  FDA endpoints:`);
  console.log(`    GET /api/fda/recalls/drug`);
  console.log(`    GET /api/fda/recalls/device`);
  console.log(`    GET /api/fda/events/adverse`);
  console.log(`    GET /api/fda/events/device`);
  console.log(`\n  NHTSA endpoints:`);
  console.log(`    GET /api/nhtsa/vin/:vin`);
  console.log(`    GET /api/nhtsa/vehicle`);
  console.log(`    GET /api/nhtsa/recalls`);
  console.log(`    GET /api/nhtsa/complaints`);
  console.log(`    GET /api/nhtsa/safety-ratings`);
  console.log(`\n  FAA endpoints:`);
  console.log(`    GET /api/faa/weather/metar`);
  console.log(`    GET /api/faa/weather/taf`);
  console.log(`    GET /api/faa/weather/route`);
  console.log(`    GET /api/faa/weather/sigmet`);
  console.log(`    GET /api/faa/weather/airmet`);
  console.log(`    GET /api/faa/weather/pirep`);
});

export default app;
