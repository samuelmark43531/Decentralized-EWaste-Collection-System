# Decentralized E-Waste Collection System

A blockchain-based solution for managing electronic waste collection using Stacks blockchain.

## Overview

This smart contract enables:
- Users to schedule e-waste pickups
- Collectors to accept and complete pickups
- Automatic reward distribution
- Tracking of collection statistics

## Contract Functions

### For Users
- `schedule-pickup`: Schedule new e-waste pickup with location and estimated weight
- `get-pickup`: View details of a specific pickup
- `get-user-stats`: View statistics for any user

### For Collectors
- `accept-pickup`: Accept a pending pickup request
- `complete-pickup`: Mark pickup as completed and receive rewards

### For Admin
- `set-reward-rate`: Update the reward rate per unit weight
- `transfer-admin`: Transfer admin rights to new address

## Usage

1. Schedule a pickup:
```clarity
(contract-call? .ewaste schedule-pickup "123 Main St" u10)
```

2. Accept a pickup:
```clarity
(contract-call? .ewaste accept-pickup u1)
```

3. Complete a pickup:
```clarity
(contract-call? .ewaste complete-pickup u1 u10)
```

## Rewards

Rewards are calculated based on the weight of e-waste collected. The current reward rate is 10 STX per unit weight.
