import { ethers } from "hardhat";

async function main() {
  console.log("\n🚀 DEPLOYING RECURSIVE PLANCK OPERATOR ($RPO)...\n");
  console.log("━".repeat(60));

  const [deployer] = await ethers.getSigners();
  console.log("Deploying from:", deployer.address);
  console.log("Balance:", ethers.formatEther(await ethers.provider.getBalance(deployer.address)), "HBAR");
  console.log("━".repeat(60) + "\n");

  // IMMEDIATE RELEASE ADDRESS - Receives 35% of total supply
  const IMMEDIATE_RELEASE_ADDRESS = "0x27aa333e759b64fd4bb4eeedac8eaaaf107580ed";

  // Token allocation addresses (load from environment or use defaults)
  const FOUNDER_ADDRESS = process.env.FOUNDER || process.env.FOUNDER_ADDRESS || deployer.address;
  const TREASURY_ADDRESS = process.env.TREASURY || process.env.TREASURY_ADDRESS || deployer.address;
  const COMMUNITY_ADDRESS = process.env.COMMUNITY || process.env.COMMUNITY_ADDRESS || deployer.address;
  const LEGAL_ADDRESS = process.env.LEGAL || process.env.LEGAL_ADDRESS || deployer.address;
  const DEVELOPMENT_ADDRESS = process.env.DEVELOPMENT || process.env.DEVELOPMENT_ADDRESS || deployer.address;

  console.log("📍 TOKEN ALLOCATION ADDRESSES:");
  console.log("━".repeat(60));
  console.log("Immediate Release (35% split):");
  console.log("  ├─ Founder (17.5B):", FOUNDER_ADDRESS);
  console.log("  └─ Specified (17.5B):", IMMEDIATE_RELEASE_ADDRESS);
  console.log("Founder/Team Vesting (25%):", FOUNDER_ADDRESS);
  console.log("Treasury (50%):", TREASURY_ADDRESS);
  console.log("Community (8.3%):", COMMUNITY_ADDRESS);
  console.log("Legal Fund (8.3%):", LEGAL_ADDRESS);
  console.log("Development (8.4%):", DEVELOPMENT_ADDRESS);
  console.log("━".repeat(60) + "\n");

  const RPO = await ethers.getContractFactory("RecursivePlanckOperator");
  console.log("⏳ Deploying 100,000,000,000 $RPO with allocation structure...");

  const rpo = await RPO.deploy(
    IMMEDIATE_RELEASE_ADDRESS,
    FOUNDER_ADDRESS,
    TREASURY_ADDRESS,
    COMMUNITY_ADDRESS,
    LEGAL_ADDRESS,
    DEVELOPMENT_ADDRESS
  );

  await rpo.waitForDeployment();

  const address = await rpo.getAddress();

  const network = await ethers.provider.getNetwork();
  const networkName = network.chainId === 295n ? "mainnet" : "testnet";

  console.log(`\n✅ $RPO IS LIVE ON HEDERA ${networkName.toUpperCase()}`);
  console.log("━".repeat(60));
  console.log("Contract:", address);
  console.log(`HashScan → https://hashscan.io/${networkName}/contract/${address}`);

  console.log("\n📊 TOKEN ALLOCATION (100 Billion $RPO)");
  console.log("━".repeat(60));
  console.log("💰 Immediate Release: 35B (35%) - SPLIT:");
  console.log("   ├─ Founder: 17.5B →", FOUNDER_ADDRESS.substring(0, 10) + "...");
  console.log("   └─ Specified: 17.5B →", IMMEDIATE_RELEASE_ADDRESS.substring(0, 10) + "...");
  console.log("   Execute with: executeImmediateRelease()");
  console.log();
  console.log("📅 Vesting Allocations:");
  console.log("   Founder/Team: 25B (25%) - 1yr cliff + 1.5yr vest");
  console.log("   Treasury: 50B (50%) - Governance controlled");
  console.log("   Community: 8.3B (8.3%) - 20% immediate + 1yr vest");
  console.log("   Legal Fund: 8.3B (8.3%) - 6mo cliff + 6mo vest");
  console.log("   Development: 8.4B (8.4%) - 2yr linear vest");
  console.log("━".repeat(60));

  console.log("\n🎯 NEXT STEPS:");
  console.log("━".repeat(60));
  console.log("1. Call executeImmediateRelease() to release 35B split:");
  console.log("   ├─ 17.5B to founder:", FOUNDER_ADDRESS.substring(0, 10) + "...");
  console.log("   └─ 17.5B to specified:", IMMEDIATE_RELEASE_ADDRESS.substring(0, 10) + "...");
  console.log("2. Call distributeTreasury() to release 50B treasury allocation");
  console.log("3. Vesting beneficiaries can claim tokens via release functions:");
  console.log("   - releaseFounderTokens(address)");
  console.log("   - releaseCommunityTokens(address)");
  console.log("   - releaseLegalTokens(address)");
  console.log("   - releaseDevelopmentTokens(address)");
  console.log("4. Run your GoTrax. Burn the first 3,600 $RPO.");
  console.log("\n⚡ The recursion has begun.");
  console.log("━".repeat(60));

  // Automatically execute immediate release after deployment
  console.log("\n⚡ Executing immediate release of 35B tokens (split)...");
  const tx = await rpo.executeImmediateRelease();
  await tx.wait();
  console.log("✅ Immediate release executed!");
  console.log(`   ├─ Founder (${FOUNDER_ADDRESS.substring(0, 10)}...) received 17,500,000,000 $RPO`);
  console.log(`   └─ Specified (${IMMEDIATE_RELEASE_ADDRESS.substring(0, 10)}...) received 17,500,000,000 $RPO`);
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
