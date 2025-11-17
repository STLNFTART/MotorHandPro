/**
 * LAM API Client for React Native
 * Connects to LAM REST API server
 */
import axios from 'axios';

const API_BASE_URL = process.env.LAM_API_URL || 'http://localhost:8000';

class LAMClient {
  constructor(baseURL = API_BASE_URL) {
    this.client = axios.create({
      baseURL,
      timeout: 30000,
      headers: {
        'Content-Type': 'application/json',
      },
    });
  }

  async getStatus() {
    try {
      const response = await this.client.get('/status');
      return response.data;
    } catch (error) {
      console.error('Status error:', error);
      throw error;
    }
  }

  async planTrip(destination, departureDate, returnDate, budget = null) {
    try {
      const response = await this.client.post('/trip/plan', {
        destination,
        departure_date: departureDate,
        return_date: returnDate,
        budget,
      });
      return response.data;
    } catch (error) {
      console.error('Trip planning error:', error);
      throw error;
    }
  }

  async makeReservation(venueType, venueName, date, time, partySize) {
    try {
      const response = await this.client.post('/reservation/make', {
        venue_type: venueType,
        venue_name: venueName,
        date,
        time,
        party_size: partySize,
      });
      return response.data;
    } catch (error){
      console.error('Reservation error:', error);
      throw error;
    }
  }

  async orderFood(restaurant, items, deliveryAddress) {
    try {
      const response = await this.client.post('/food/order', {
        restaurant,
        items,
        delivery_address: deliveryAddress,
      });
      return response.data;
    } catch (error) {
      console.error('Food order error:', error);
      throw error;
    }
  }

  async askQuestion(question) {
    try {
      const response = await this.client.post('/ask', {
        question,
      });
      return response.data;
    } catch (error) {
      console.error('Question error:', error);
      throw error;
    }
  }

  async completeTask(task) {
    try {
      const response = await this.client.post('/task', {
        task,
      });
      return response.data;
    } catch (error) {
      console.error('Task error:', error);
      throw error;
    }
  }

  async getMetrics() {
    try {
      const response = await this.client.get('/metrics');
      return response.data;
    } catch (error) {
      console.error('Metrics error:', error);
      throw error;
    }
  }
}

export default new LAMClient();
