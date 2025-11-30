// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "@openzeppelin/contracts/token/ERC20/extensions/ERC20Burnable.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

/**
 * @title RecursivePlanckOperator ($RPO)
 * @dev ERC20 Token with vesting schedules and immediate release functionality
 * Total Supply: 100,000,000,000 (100 Billion) $RPO
 *
 * Allocation:
 * - 35% (35B) Immediate Release split: 17.5B to founder, 17.5B to specified address
 * - 65% (65B) Distributed across vesting schedules:
 *   - Founder/Team: 25% with 1yr cliff + 1.5yr vest
 *   - Treasury: 50% governance controlled
 *   - Community: 8.3% with 20% immediate + 1yr vest
 *   - Legal Fund: 8.3% with 6mo cliff + 6mo vest
 *   - Development: 8.4% with 2yr linear vest
 */
contract RecursivePlanckOperator is ERC20, ERC20Burnable, Ownable {
    uint256 public constant TOTAL_SUPPLY = 100_000_000_000 * 10**18; // 100 Billion tokens
    uint256 public constant IMMEDIATE_RELEASE_PERCENT = 35; // 35%
    uint256 public constant IMMEDIATE_RELEASE_AMOUNT = (TOTAL_SUPPLY * IMMEDIATE_RELEASE_PERCENT) / 100;

    // Vesting allocation percentages (of total supply)
    uint256 public constant FOUNDER_PERCENT = 25;
    uint256 public constant TREASURY_PERCENT = 50;
    uint256 public constant COMMUNITY_PERCENT = 83; // 8.3% * 10
    uint256 public constant LEGAL_PERCENT = 83;     // 8.3% * 10
    uint256 public constant DEVELOPMENT_PERCENT = 84; // 8.4% * 10

    // Vesting schedules
    struct VestingSchedule {
        uint256 totalAmount;
        uint256 releasedAmount;
        uint256 startTime;
        uint256 cliffDuration;
        uint256 vestingDuration;
        uint256 immediateReleasePercent; // For community allocation
    }

    mapping(address => VestingSchedule) public founderVesting;
    mapping(address => VestingSchedule) public communityVesting;
    mapping(address => VestingSchedule) public legalVesting;
    mapping(address => VestingSchedule) public developmentVesting;

    address public treasuryAddress;
    bool public treasuryDistributed;

    address public immediateReleaseAddress;
    address public founderImmediateReleaseAddress;
    bool public immediateReleaseExecuted;

    event TokensReleased(address indexed beneficiary, uint256 amount, string vestingType);
    event TreasuryDistributed(address indexed treasury, uint256 amount);
    event ImmediateReleaseExecuted(address indexed recipient, uint256 amount);

    /**
     * @dev Constructor that sets up all allocations
     * @param _immediateReleaseAddr Address to receive 17.5B immediate release (half of 35%)
     * @param _founderAddress Address for founder/team allocation (also receives 17.5B immediate)
     * @param _treasuryAddress Address for treasury allocation
     * @param _communityAddress Address for community allocation
     * @param _legalAddress Address for legal fund allocation
     * @param _developmentAddress Address for development allocation
     */
    constructor(
        address _immediateReleaseAddr,
        address _founderAddress,
        address _treasuryAddress,
        address _communityAddress,
        address _legalAddress,
        address _developmentAddress
    ) ERC20("Recursive Planck Operator", "RPO") Ownable(msg.sender) {
        require(_immediateReleaseAddr != address(0), "Invalid immediate release address");
        require(_founderAddress != address(0), "Invalid founder address");
        require(_treasuryAddress != address(0), "Invalid treasury address");
        require(_communityAddress != address(0), "Invalid community address");
        require(_legalAddress != address(0), "Invalid legal address");
        require(_developmentAddress != address(0), "Invalid development address");

        immediateReleaseAddress = _immediateReleaseAddr;
        founderImmediateReleaseAddress = _founderAddress;
        treasuryAddress = _treasuryAddress;

        // Mint total supply to contract
        _mint(address(this), TOTAL_SUPPLY);

        uint256 deploymentTime = block.timestamp;

        // Set up vesting schedules
        // Founder/Team: 25% with 1yr cliff + 1.5yr vest
        founderVesting[_founderAddress] = VestingSchedule({
            totalAmount: (TOTAL_SUPPLY * FOUNDER_PERCENT) / 100,
            releasedAmount: 0,
            startTime: deploymentTime,
            cliffDuration: 365 days,
            vestingDuration: 365 days + 180 days, // 1.5 years
            immediateReleasePercent: 0
        });

        // Community: 8.3% with 20% immediate + 1yr vest
        communityVesting[_communityAddress] = VestingSchedule({
            totalAmount: (TOTAL_SUPPLY * COMMUNITY_PERCENT) / 1000,
            releasedAmount: 0,
            startTime: deploymentTime,
            cliffDuration: 0,
            vestingDuration: 365 days,
            immediateReleasePercent: 20
        });

        // Legal Fund: 8.3% with 6mo cliff + 6mo vest
        legalVesting[_legalAddress] = VestingSchedule({
            totalAmount: (TOTAL_SUPPLY * LEGAL_PERCENT) / 1000,
            releasedAmount: 0,
            startTime: deploymentTime,
            cliffDuration: 180 days,
            vestingDuration: 180 days,
            immediateReleasePercent: 0
        });

        // Development: 8.4% with 2yr linear vest
        developmentVesting[_developmentAddress] = VestingSchedule({
            totalAmount: (TOTAL_SUPPLY * DEVELOPMENT_PERCENT) / 1000,
            releasedAmount: 0,
            startTime: deploymentTime,
            cliffDuration: 0,
            vestingDuration: 730 days, // 2 years
            immediateReleasePercent: 0
        });
    }

    /**
     * @dev Execute the immediate release of 35% split between two addresses
     * 17.5B to founder address, 17.5B to specified address
     * Can only be called once by owner
     */
    function executeImmediateRelease() external onlyOwner {
        require(!immediateReleaseExecuted, "Immediate release already executed");
        immediateReleaseExecuted = true;

        uint256 halfAmount = IMMEDIATE_RELEASE_AMOUNT / 2;

        // Send 17.5B to founder
        _transfer(address(this), founderImmediateReleaseAddress, halfAmount);
        emit ImmediateReleaseExecuted(founderImmediateReleaseAddress, halfAmount);

        // Send 17.5B to specified address
        _transfer(address(this), immediateReleaseAddress, halfAmount);
        emit ImmediateReleaseExecuted(immediateReleaseAddress, halfAmount);
    }

    /**
     * @dev Distribute treasury allocation (50% of total supply)
     * Can only be called once by owner
     */
    function distributeTreasury() external onlyOwner {
        require(!treasuryDistributed, "Treasury already distributed");
        treasuryDistributed = true;

        uint256 treasuryAmount = (TOTAL_SUPPLY * TREASURY_PERCENT) / 100;
        _transfer(address(this), treasuryAddress, treasuryAmount);
        emit TreasuryDistributed(treasuryAddress, treasuryAmount);
    }

    /**
     * @dev Calculate vested amount for a given schedule
     */
    function _calculateVestedAmount(VestingSchedule memory schedule) private view returns (uint256) {
        if (block.timestamp < schedule.startTime + schedule.cliffDuration) {
            // Still in cliff period
            if (schedule.immediateReleasePercent > 0) {
                // Community gets immediate release even during cliff
                return (schedule.totalAmount * schedule.immediateReleasePercent) / 100;
            }
            return 0;
        }

        uint256 timeVested = block.timestamp - schedule.startTime;

        if (timeVested >= schedule.cliffDuration + schedule.vestingDuration) {
            // Fully vested
            return schedule.totalAmount;
        }

        // Calculate vested amount
        uint256 vestedAmount = (schedule.totalAmount * timeVested) / (schedule.cliffDuration + schedule.vestingDuration);

        return vestedAmount;
    }

    /**
     * @dev Release vested tokens for founder
     */
    function releaseFounderTokens(address beneficiary) external {
        VestingSchedule storage schedule = founderVesting[beneficiary];
        require(schedule.totalAmount > 0, "No vesting schedule found");

        uint256 vestedAmount = _calculateVestedAmount(schedule);
        uint256 releasable = vestedAmount - schedule.releasedAmount;
        require(releasable > 0, "No tokens available for release");

        schedule.releasedAmount += releasable;
        _transfer(address(this), beneficiary, releasable);
        emit TokensReleased(beneficiary, releasable, "Founder");
    }

    /**
     * @dev Release vested tokens for community
     */
    function releaseCommunityTokens(address beneficiary) external {
        VestingSchedule storage schedule = communityVesting[beneficiary];
        require(schedule.totalAmount > 0, "No vesting schedule found");

        uint256 vestedAmount = _calculateVestedAmount(schedule);
        uint256 releasable = vestedAmount - schedule.releasedAmount;
        require(releasable > 0, "No tokens available for release");

        schedule.releasedAmount += releasable;
        _transfer(address(this), beneficiary, releasable);
        emit TokensReleased(beneficiary, releasable, "Community");
    }

    /**
     * @dev Release vested tokens for legal fund
     */
    function releaseLegalTokens(address beneficiary) external {
        VestingSchedule storage schedule = legalVesting[beneficiary];
        require(schedule.totalAmount > 0, "No vesting schedule found");

        uint256 vestedAmount = _calculateVestedAmount(schedule);
        uint256 releasable = vestedAmount - schedule.releasedAmount;
        require(releasable > 0, "No tokens available for release");

        schedule.releasedAmount += releasable;
        _transfer(address(this), beneficiary, releasable);
        emit TokensReleased(beneficiary, releasable, "Legal");
    }

    /**
     * @dev Release vested tokens for development
     */
    function releaseDevelopmentTokens(address beneficiary) external {
        VestingSchedule storage schedule = developmentVesting[beneficiary];
        require(schedule.totalAmount > 0, "No vesting schedule found");

        uint256 vestedAmount = _calculateVestedAmount(schedule);
        uint256 releasable = vestedAmount - schedule.releasedAmount;
        require(releasable > 0, "No tokens available for release");

        schedule.releasedAmount += releasable;
        _transfer(address(this), beneficiary, releasable);
        emit TokensReleased(beneficiary, releasable, "Development");
    }

    /**
     * @dev Get releasable amount for a beneficiary
     */
    function getReleasableAmount(address beneficiary, string memory vestingType) external view returns (uint256) {
        VestingSchedule memory schedule;

        if (keccak256(bytes(vestingType)) == keccak256(bytes("Founder"))) {
            schedule = founderVesting[beneficiary];
        } else if (keccak256(bytes(vestingType)) == keccak256(bytes("Community"))) {
            schedule = communityVesting[beneficiary];
        } else if (keccak256(bytes(vestingType)) == keccak256(bytes("Legal"))) {
            schedule = legalVesting[beneficiary];
        } else if (keccak256(bytes(vestingType)) == keccak256(bytes("Development"))) {
            schedule = developmentVesting[beneficiary];
        } else {
            return 0;
        }

        if (schedule.totalAmount == 0) return 0;

        uint256 vestedAmount = _calculateVestedAmount(schedule);
        return vestedAmount - schedule.releasedAmount;
    }
}
