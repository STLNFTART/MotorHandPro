# Branch Consolidation Plan

## Current State
**Total Branches: 36** (way too many!)

## Proposed Final Structure (4 branches)

### Keep These:
1. **main** - Production/stable code
2. **develop** - Active development branch (consolidate all features here)
3. **restore-original** - Backup of original state
4. **claude/rewrite-apl-prolog-01HVqgLRGjH79gH2SJVBFrnr** - Current APL/Prolog rewrite (will merge to develop when complete)

## Branches to Delete (32 branches)

### Old Claude Feature Branches (28):
- claude/actuator-token-burn-01QLBbocBiTULQ32EbrxMHAD
- claude/add-company-repo-links-0197RM2HDn8ECYtEvYnPPone
- claude/add-missing-documents-01YR1A2sBb4hyJJASj683xPX
- claude/add-rpo-token-address-01WkVCCpdBQyzSPeik31FpxW
- claude/add-tests-empirical-constants-017pjiVmkhws1SveL1quwwne
- claude/add-wiki-01SPzhcqge1UCyWqcaiwMgHn
- claude/clarify-primal-logic-framework-017zZnydRhSAKDgsFnSwPPAz
- claude/d-drug-safety-system-018rRJDG7HFGRoppR3YF8zJm
- claude/deploy-rpo-token-01SdxJTDSkwoG4b4eLeGDe9x
- claude/fix-skip-navigation-01H7E4941zXDorx6x42J714j
- claude/generate-max-value-019GSMm4MQyyd15LzUGWmWxe
- claude/implement-lam-framework-01S25pRJzLaUFdoVbJYwhZRN
- claude/implement-simulation-01XDCjvPrGe5XeCKQ6oZxAJQ
- claude/implement-validate-algorithms-01DLfrnYQj9EGgYzWQteCqFs
- claude/integrate-radiation-effects-01YJoxRHvzvoAuqou1kW3Pxx
- claude/nasa-data-visualization-016KXTEY4nfAPoi65hwC2FRQ
- claude/nasa-starlink-api-01WrjyTUaZvA3TpZLi67QCSG
- claude/network-simulation-cluster-01DCmdhEKQV66866pymadSTC
- claude/node-red-integration-01AmUXEAxYnDvktLT2HBLhgY
- claude/primallang-transpilation-01Mei8ASx3H7UKbk6FDpUfhw
- claude/quantum-resistant-crypto-kernel-011CV3mpkif8bZQjGEUtYF5c
- claude/regulatory-api-integration-015CEcokqTc3i6waB6ib4S2N
- claude/run-all-branches-max-output-01RnbrcthoKHEQ8fXcwrMsNt
- claude/run-all-iterations-data-vis-01SVxJhgrYD4xFmNB5a5GG5h
- claude/run-full-tests-018tNx5zkS9Lc3DTkWuU3sLY
- claude/run-r-script-011ZmFB249vghDR5R1YGMmxe
- claude/setup-hedera-testnet-018bQHdjMhHtcHoZdHzJ79LT
- claude/test-satellite-integration-01UaNQjhYRRZwqodZpzcUiuo
- claude/unified-results-framework-01AqMEy8x8ruuHXhChvVa9Xz
- claude/update-earth-radius-wgs84-013kdrMxarbhNsGSLKcUGgyW

### Old Copilot Branches (3):
- copilot/add-run-all-branches-script
- copilot/add-run-every-branch-script
- copilot/add-test-build-script

### Patch Branches (1):
- STLNFTART-patch-1
- STLNFTART-patch-2

## Deletion Commands (Run after confirming with user)

```bash
# Delete remote branches
git push origin --delete claude/actuator-token-burn-01QLBbocBiTULQ32EbrxMHAD
git push origin --delete claude/add-company-repo-links-0197RM2HDn8ECYtEvYnPPone
# ... (repeat for all 32 branches)

# Or use a loop:
for branch in \
  claude/actuator-token-burn-01QLBbocBiTULQ32EbrxMHAD \
  claude/add-company-repo-links-0197RM2HDn8ECYtEvYnPPone \
  # ... (all branches)
do
  git push origin --delete "$branch"
done
```

## Notes
- All these feature branches appear to be merged or their work is in main
- Keeping restore-original as a safety backup
- Future work: Use develop branch for ongoing features instead of many feature branches
