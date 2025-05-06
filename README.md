# Construction Milestone Smart Contract

A Clarity smart contract for managing construction project milestones and payments on the Stacks blockchain.

## Overview

This smart contract enables clients to create construction projects, define milestones, and release payments to contractors based on verified completion of work. The contract ensures that payments are only released when authorized verifiers confirm that milestones have been completed.

## Features

- Create construction projects with budgets
- Add milestones with specific payment amounts
- Assign independent verifiers to each milestone
- Verify milestone completion
- Release payments to contractors
- Track project and milestone status

## Contract Functions

### Read-Only Functions

- `get-project`: Retrieve project details by ID
- `get-milestone`: Retrieve milestone details by ID
- `get-project-milestones`: Get all milestone IDs for a project
- `get-milestone-details`: Get detailed information about a milestone
- `get-project-details`: Get detailed information about a project

### Public Functions

- `create-project`: Create a new construction project
- `add-milestone`: Add a milestone to an existing project
- `verify-milestone`: Mark a milestone as verified (only callable by the assigned verifier)
- `release-payment`: Release payment for a verified milestone (only callable by the client)
- `complete-project`: Mark a project as completed (only callable by the client)

## Usage Example

### Creating a Project

```clarity
(contract-call? .construction-milestone create-project "Office Building Renovation" 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u10000)
```

### Adding a Milestone

```clarity
(contract-call? .construction-milestone add-milestone u1 "Foundation completion" u2000 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG)
```

### Verifying a Milestone

```clarity
;; Called by the verifier
(contract-call? .construction-milestone verify-milestone u1)
```

### Releasing Payment

```clarity
;; Called by the client
(contract-call? .construction-milestone release-payment u1)
```

### Completing a Project

```clarity
(contract-call? .construction-milestone complete-project u1)
```

## Error Codes

- `u100`: Owner only operation
- `u101`: Resource not found
- `u102`: Resource already exists
- `u103`: Unauthorized operation
- `u104`: Milestone not in active state
- `u105`: Milestone already completed
- `u106`: Insufficient funds
- `u107`: Invalid amount
- `u108`: Project not active
- `u109`: Project already completed