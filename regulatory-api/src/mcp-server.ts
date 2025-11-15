#!/usr/bin/env node
/**
 * MCP Server for Regulatory APIs
 *
 * Exposes FDA, NHTSA, and FAA public data as MCP tools
 * for use by LLM agents (Claude, etc.)
 *
 * Usage:
 *   node dist/mcp-server.js
 *
 * Add to Claude Desktop config:
 *   {
 *     "mcpServers": {
 *       "regulatory": {
 *         "command": "node",
 *         "args": ["/path/to/regulatory-api/dist/mcp-server.js"],
 *         "env": {
 *           "FDA_API_KEY": "your-key-here"
 *         }
 *       }
 *     }
 *   }
 */

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
} from '@modelcontextprotocol/sdk/types.js';
import dotenv from 'dotenv';
import { FDAClient } from './reg/fda.js';
import { NHTSAClient } from './reg/nhtsa.js';
import { FAAClient } from './reg/faa.js';

dotenv.config();

// Initialize regulatory clients
const fdaClient = new FDAClient(process.env.FDA_API_KEY);
const nhtsaClient = new NHTSAClient();
const faaClient = new FAAClient(process.env.FAA_API_KEY);

// Create MCP server
const server = new Server(
  {
    name: 'regulatory-api',
    version: '1.0.0',
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

// List available tools
server.setRequestHandler(ListToolsRequestSchema, async () => {
  return {
    tools: [
      // FDA Tools
      {
        name: 'fda_search_drug_recalls',
        description: 'Search FDA drug recall database. Returns recalls matching product name/description.',
        inputSchema: {
          type: 'object',
          properties: {
            query: {
              type: 'string',
              description: 'Product name or description to search for',
            },
            limit: {
              type: 'number',
              description: 'Max results to return (default 10)',
              default: 10,
            },
            classification: {
              type: 'string',
              description: 'Recall classification: 1 (most serious), 2, or 3',
              enum: ['1', '2', '3'],
            },
          },
          required: ['query'],
        },
      },
      {
        name: 'fda_search_device_recalls',
        description: 'Search FDA medical device recall database.',
        inputSchema: {
          type: 'object',
          properties: {
            query: {
              type: 'string',
              description: 'Device name or description',
            },
            limit: {
              type: 'number',
              description: 'Max results (default 10)',
              default: 10,
            },
            classification: {
              type: 'string',
              enum: ['1', '2', '3'],
            },
          },
          required: ['query'],
        },
      },
      {
        name: 'fda_search_adverse_events',
        description: 'Search FDA adverse event reports (FAERS) for a drug/medication.',
        inputSchema: {
          type: 'object',
          properties: {
            drug: {
              type: 'string',
              description: 'Drug/medication name',
            },
            limit: {
              type: 'number',
              default: 10,
            },
            serious: {
              type: 'boolean',
              description: 'Filter to only serious adverse events',
            },
          },
          required: ['drug'],
        },
      },
      {
        name: 'fda_search_device_events',
        description: 'Search FDA MAUDE database for medical device adverse events.',
        inputSchema: {
          type: 'object',
          properties: {
            product: {
              type: 'string',
              description: 'Device product description',
            },
            limit: {
              type: 'number',
              default: 10,
            },
          },
          required: ['product'],
        },
      },

      // NHTSA Tools
      {
        name: 'nhtsa_decode_vin',
        description: 'Decode a Vehicle Identification Number (VIN) to get vehicle specifications.',
        inputSchema: {
          type: 'object',
          properties: {
            vin: {
              type: 'string',
              description: '17-character VIN',
              pattern: '^[A-HJ-NPR-Z0-9]{17}$',
            },
          },
          required: ['vin'],
        },
      },
      {
        name: 'nhtsa_get_recalls',
        description: 'Get NHTSA vehicle safety recalls by make/model/year.',
        inputSchema: {
          type: 'object',
          properties: {
            make: {
              type: 'string',
              description: 'Vehicle make (e.g., Toyota, Tesla)',
            },
            model: {
              type: 'string',
              description: 'Vehicle model (e.g., Camry, Model 3)',
            },
            year: {
              type: 'number',
              description: 'Model year',
            },
          },
          required: ['make', 'model', 'year'],
        },
      },
      {
        name: 'nhtsa_get_complaints',
        description: 'Get consumer complaints from NHTSA ODI database.',
        inputSchema: {
          type: 'object',
          properties: {
            make: { type: 'string' },
            model: { type: 'string' },
            year: { type: 'number' },
          },
          required: ['make', 'model', 'year'],
        },
      },
      {
        name: 'nhtsa_get_safety_ratings',
        description: 'Get NHTSA 5-Star Safety Ratings for a vehicle.',
        inputSchema: {
          type: 'object',
          properties: {
            make: { type: 'string' },
            model: { type: 'string' },
            year: { type: 'number' },
          },
          required: ['make', 'model', 'year'],
        },
      },

      // FAA Tools
      {
        name: 'faa_get_metar',
        description: 'Get current METAR weather observations for an airport (ICAO code).',
        inputSchema: {
          type: 'object',
          properties: {
            station: {
              type: 'string',
              description: 'ICAO station code (e.g., KJFK, KLAX)',
              pattern: '^[A-Z]{4}$',
            },
            hours: {
              type: 'number',
              description: 'Hours to look back (default 2)',
              default: 2,
            },
          },
          required: ['station'],
        },
      },
      {
        name: 'faa_get_taf',
        description: 'Get Terminal Aerodrome Forecast (TAF) for an airport.',
        inputSchema: {
          type: 'object',
          properties: {
            station: {
              type: 'string',
              pattern: '^[A-Z]{4}$',
            },
            hours: {
              type: 'number',
              default: 6,
            },
          },
          required: ['station'],
        },
      },
      {
        name: 'faa_get_route_weather',
        description: 'Get weather for a flight route (departure and arrival airports).',
        inputSchema: {
          type: 'object',
          properties: {
            departure: {
              type: 'string',
              description: 'Departure ICAO code',
              pattern: '^[A-Z]{4}$',
            },
            arrival: {
              type: 'string',
              description: 'Arrival ICAO code',
              pattern: '^[A-Z]{4}$',
            },
          },
          required: ['departure', 'arrival'],
        },
      },
      {
        name: 'faa_get_pireps',
        description: 'Get pilot reports (PIREPs) for an area around a station.',
        inputSchema: {
          type: 'object',
          properties: {
            station: {
              type: 'string',
              pattern: '^[A-Z]{4}$',
            },
            radius: {
              type: 'number',
              description: 'Radius in nautical miles (default 100)',
              default: 100,
            },
          },
          required: ['station'],
        },
      },
    ],
  };
});

// Handle tool calls
server.setRequestHandler(CallToolRequestSchema, async (request) => {
  const { name, arguments: args } = request.params;

  if (!args) {
    return {
      content: [{ type: 'text', text: 'Error: No arguments provided' }],
      isError: true,
    };
  }

  try {
    switch (name) {
      // FDA tools
      case 'fda_search_drug_recalls': {
        const params = args as any;
        const result = await fdaClient.searchDrugRecalls(params.query, {
          limit: params.limit,
          classification: params.classification,
        });
        return {
          content: [{ type: 'text', text: JSON.stringify(result, null, 2) }],
        };
      }

      case 'fda_search_device_recalls': {
        const params = args as any;
        const result = await fdaClient.searchDeviceRecalls(params.query, {
          limit: params.limit,
          classification: params.classification,
        });
        return {
          content: [{ type: 'text', text: JSON.stringify(result, null, 2) }],
        };
      }

      case 'fda_search_adverse_events': {
        const params = args as any;
        const result = await fdaClient.searchAdverseEvents(params.drug, {
          limit: params.limit,
          serious: params.serious,
        });
        return {
          content: [{ type: 'text', text: JSON.stringify(result, null, 2) }],
        };
      }

      case 'fda_search_device_events': {
        const params = args as any;
        const result = await fdaClient.searchDeviceEvents(params.product, {
          limit: params.limit,
        });
        return {
          content: [{ type: 'text', text: JSON.stringify(result, null, 2) }],
        };
      }

      // NHTSA tools
      case 'nhtsa_decode_vin': {
        const params = args as any;
        const result = await nhtsaClient.decodeVin(params.vin);
        return {
          content: [{ type: 'text', text: JSON.stringify(result, null, 2) }],
        };
      }

      case 'nhtsa_get_recalls': {
        const params = args as any;
        const result = await nhtsaClient.getRecallsByMakeModelYear(
          params.make,
          params.model,
          params.year
        );
        return {
          content: [{ type: 'text', text: JSON.stringify(result, null, 2) }],
        };
      }

      case 'nhtsa_get_complaints': {
        const params = args as any;
        const result = await nhtsaClient.getComplaintsByMakeModelYear(
          params.make,
          params.model,
          params.year
        );
        return {
          content: [{ type: 'text', text: JSON.stringify(result, null, 2) }],
        };
      }

      case 'nhtsa_get_safety_ratings': {
        const params = args as any;
        const result = await nhtsaClient.getSafetyRatings(
          params.make,
          params.model,
          params.year
        );
        return {
          content: [{ type: 'text', text: JSON.stringify(result, null, 2) }],
        };
      }

      // FAA tools
      case 'faa_get_metar': {
        const params = args as any;
        const result = await faaClient.getMETAR(params.station, {
          hoursBeforeNow: params.hours,
        });
        return {
          content: [{ type: 'text', text: JSON.stringify(result, null, 2) }],
        };
      }

      case 'faa_get_taf': {
        const params = args as any;
        const result = await faaClient.getTAF(params.station, {
          hoursBeforeNow: params.hours,
        });
        return {
          content: [{ type: 'text', text: JSON.stringify(result, null, 2) }],
        };
      }

      case 'faa_get_route_weather': {
        const params = args as any;
        const result = await faaClient.getRouteWeather(
          params.departure,
          params.arrival
        );
        return {
          content: [{ type: 'text', text: JSON.stringify(result, null, 2) }],
        };
      }

      case 'faa_get_pireps': {
        const params = args as any;
        const result = await faaClient.getPIREPs(params.station, params.radius);
        return {
          content: [{ type: 'text', text: JSON.stringify(result, null, 2) }],
        };
      }

      default:
        throw new Error(`Unknown tool: ${name}`);
    }
  } catch (error: any) {
    return {
      content: [
        {
          type: 'text',
          text: `Error executing ${name}: ${error.message}`,
        },
      ],
      isError: true,
    };
  }
});

// Start the server
async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error('Regulatory API MCP server running on stdio');
}

main().catch((error) => {
  console.error('Fatal error:', error);
  process.exit(1);
});
