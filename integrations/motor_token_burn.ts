/**
 * Motor Control to Token Burn Integration
 *
 * This module connects the MotorHandPro actuator control system to the
 * Recursive Planck Operator ($RPO) token burning mechanism.
 *
 * Mechanism:
 * - 1 second of actuator operation = 1 $RPO token burned (configurable via TOKEN_RATE)
 * - Tracks cumulative actuation time
 * - Burns tokens from user's wallet based on usage
 *
 * Copyright 2025 Donte Lightfoot
 */

import { ethers } from "ethers";
import * as dotenv from "dotenv";
import fs from "fs";
import path from "path";

dotenv.config();

interface ActuationEvent {
  timestamp: number;
  duration: number; // in seconds
  actuatorId: string;
  energyUsed: number;
}

interface BurnRecord {
  actuationSeconds: number;
  tokensBurned: string;
  txHash: string;
  timestamp: number;
}

export class MotorTokenBurnIntegration {
  private provider: ethers.Provider;
  private contract: ethers.Contract;
  private signer: ethers.Signer;
  private tokenRate: number;
  private minTokensRequired: number;
  private accumulatedTime: number = 0;
  private burnRecords: BurnRecord[] = [];

  // Track total actuation time and burns
  private totalActuationSeconds: number = 0;
  private totalTokensBurned: bigint = 0n;

  constructor() {
    // Load configuration
    this.tokenRate = parseFloat(process.env.TOKEN_RATE || "1.0");
    this.minTokensRequired = parseFloat(process.env.MIN_TOKENS_REQUIRED || "0.1");

    // Initialize provider
    const rpcUrl = process.env.HEDERA_RPC_URL || process.env.HEDERA_TESTNET_RPC;
    if (!rpcUrl) {
      throw new Error("No RPC URL configured in .env");
    }

    this.provider = new ethers.JsonRpcProvider(rpcUrl);

    // Initialize signer
    const privateKey = process.env.PRIVATE_KEY;
    if (!privateKey) {
      throw new Error("No PRIVATE_KEY configured in .env");
    }

    this.signer = new ethers.Wallet(privateKey, this.provider);

    // Initialize contract
    const contractAddress = process.env.RPO_CONTRACT_ADDRESS;
    if (!contractAddress) {
      throw new Error("No RPO_CONTRACT_ADDRESS configured in .env. Deploy the contract first.");
    }

    // Load contract ABI
    const artifactPath = path.join(__dirname, "../artifacts/contracts/RecursivePlanckOperator.sol/RecursivePlanckOperator.json");
    const artifact = JSON.parse(fs.readFileSync(artifactPath, "utf-8"));

    this.contract = new ethers.Contract(contractAddress, artifact.abi, this.signer);

    console.log("🔗 MotorTokenBurn Integration initialized");
    console.log("   Contract:", contractAddress);
    console.log("   Token Rate:", this.tokenRate, "$RPO per second");
    console.log("   Min Tokens Required:", this.minTokensRequired, "$RPO");
  }

  /**
   * Record actuator usage event
   * This should be called whenever an actuator is used
   */
  async recordActuation(event: ActuationEvent): Promise<void> {
    console.log(`⚙️  Actuator ${event.actuatorId} used for ${event.duration}s`);

    // Accumulate time
    this.accumulatedTime += event.duration;
    this.totalActuationSeconds += event.duration;

    // Calculate tokens to burn
    const tokensToBurn = this.accumulatedTime * this.tokenRate;

    console.log(`   Accumulated time: ${this.accumulatedTime.toFixed(2)}s`);
    console.log(`   Tokens to burn: ${tokensToBurn.toFixed(4)} $RPO`);

    // Check if we should trigger a burn
    if (tokensToBurn >= this.minTokensRequired) {
      await this.executeBurn(tokensToBurn);
      this.accumulatedTime = 0; // Reset accumulator after burn
    }
  }

  /**
   * Execute token burn transaction
   */
  private async executeBurn(tokenAmount: number): Promise<void> {
    try {
      console.log(`\n🔥 BURNING ${tokenAmount.toFixed(4)} $RPO...`);

      // Convert to wei (18 decimals)
      const amountWei = ethers.parseEther(tokenAmount.toString());

      // Check balance first
      const signerAddress = await this.signer.getAddress();
      const balance = await this.contract.balanceOf(signerAddress);

      console.log(`   Wallet: ${signerAddress}`);
      console.log(`   Balance: ${ethers.formatEther(balance)} $RPO`);

      if (balance < amountWei) {
        console.error(`   ❌ Insufficient balance! Need ${tokenAmount} $RPO but have ${ethers.formatEther(balance)} $RPO`);
        throw new Error("Insufficient $RPO balance for burn");
      }

      // Execute burn
      const tx = await this.contract.burn(amountWei);
      console.log(`   Transaction submitted: ${tx.hash}`);

      const receipt = await tx.wait();
      console.log(`   ✅ Burn confirmed in block ${receipt.blockNumber}`);

      // Record burn
      const burnRecord: BurnRecord = {
        actuationSeconds: this.totalActuationSeconds,
        tokensBurned: tokenAmount.toString(),
        txHash: tx.hash,
        timestamp: Date.now()
      };

      this.burnRecords.push(burnRecord);
      this.totalTokensBurned += amountWei;

      // Save burn history
      this.saveBurnHistory();

      console.log(`   Total burned: ${ethers.formatEther(this.totalTokensBurned)} $RPO\n`);
    } catch (error) {
      console.error("❌ Burn failed:", error);
      throw error;
    }
  }

  /**
   * Manual burn - burn tokens immediately regardless of accumulation
   */
  async manualBurn(tokenAmount: number): Promise<void> {
    console.log(`🔥 Manual burn requested: ${tokenAmount} $RPO`);
    await this.executeBurn(tokenAmount);
  }

  /**
   * Get burn statistics
   */
  getBurnStats(): {
    totalActuationSeconds: number;
    totalTokensBurned: string;
    averageTokensPerSecond: number;
    burnCount: number;
    accumulatedUnburned: number;
  } {
    return {
      totalActuationSeconds: this.totalActuationSeconds,
      totalTokensBurned: ethers.formatEther(this.totalTokensBurned),
      averageTokensPerSecond: this.totalActuationSeconds > 0
        ? Number(ethers.formatEther(this.totalTokensBurned)) / this.totalActuationSeconds
        : 0,
      burnCount: this.burnRecords.length,
      accumulatedUnburned: this.accumulatedTime * this.tokenRate
    };
  }

  /**
   * Get burn history
   */
  getBurnHistory(): BurnRecord[] {
    return this.burnRecords;
  }

  /**
   * Save burn history to file
   */
  private saveBurnHistory(): void {
    const historyPath = path.join(__dirname, "../burn-history.json");
    fs.writeFileSync(historyPath, JSON.stringify({
      stats: this.getBurnStats(),
      records: this.burnRecords
    }, null, 2));
  }

  /**
   * Load burn history from file
   */
  loadBurnHistory(): void {
    const historyPath = path.join(__dirname, "../burn-history.json");

    if (fs.existsSync(historyPath)) {
      const data = JSON.parse(fs.readFileSync(historyPath, "utf-8"));
      this.burnRecords = data.records || [];
      this.totalActuationSeconds = data.stats?.totalActuationSeconds || 0;
      this.totalTokensBurned = ethers.parseEther(data.stats?.totalTokensBurned || "0");

      console.log("📂 Loaded burn history:");
      console.log(`   Total burns: ${this.burnRecords.length}`);
      console.log(`   Total tokens burned: ${ethers.formatEther(this.totalTokensBurned)} $RPO`);
    }
  }

  /**
   * Check if user has sufficient tokens
   */
  async checkBalance(): Promise<{ balance: string; hasEnough: boolean }> {
    const signerAddress = await this.signer.getAddress();
    const balance = await this.contract.balanceOf(signerAddress);
    const balanceFormatted = ethers.formatEther(balance);

    return {
      balance: balanceFormatted,
      hasEnough: parseFloat(balanceFormatted) >= this.minTokensRequired
    };
  }
}

// Example usage
if (require.main === module) {
  async function demo() {
    console.log("🚀 MotorHandPro Token Burn Integration Demo\n");

    const integration = new MotorTokenBurnIntegration();

    // Load previous history
    integration.loadBurnHistory();

    // Check balance
    const { balance, hasEnough } = await integration.checkBalance();
    console.log(`\n💰 Current Balance: ${balance} $RPO`);
    console.log(`   Sufficient for operations: ${hasEnough ? "✅ Yes" : "❌ No"}\n`);

    // Simulate actuator events
    console.log("📊 Simulating actuator operations...\n");

    // Simulate 3.6 seconds of actuation (should trigger burn as per deployment script comment)
    await integration.recordActuation({
      timestamp: Date.now(),
      duration: 1.2,
      actuatorId: "motor_hand_finger_1",
      energyUsed: 0.5
    });

    await integration.recordActuation({
      timestamp: Date.now(),
      duration: 1.2,
      actuatorId: "motor_hand_finger_2",
      energyUsed: 0.5
    });

    await integration.recordActuation({
      timestamp: Date.now(),
      duration: 1.2,
      actuatorId: "motor_hand_finger_3",
      energyUsed: 0.5
    });

    // Display stats
    const stats = integration.getBurnStats();
    console.log("\n📈 Burn Statistics:");
    console.log("━".repeat(60));
    console.log(`Total Actuation Time: ${stats.totalActuationSeconds}s`);
    console.log(`Total Tokens Burned: ${stats.totalTokensBurned} $RPO`);
    console.log(`Average Rate: ${stats.averageTokensPerSecond.toFixed(4)} $RPO/s`);
    console.log(`Burn Count: ${stats.burnCount}`);
    console.log(`Accumulated (unburnned): ${stats.accumulatedUnburned.toFixed(4)} $RPO`);
    console.log("━".repeat(60));
  }

  demo().catch(console.error);
}
