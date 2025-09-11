# 🔬 Open Research Funding DAO

A decentralized autonomous organization (DAO) for funding open source and scientific research projects.

## 🎯 Features

- Create funding proposals for research projects
- Token-weighted voting system
- Automatic fund distribution upon proposal approval
- Transparent voting and proposal tracking

## 🛠 Technical Details

The smart contract implements:
- Proposal creation with minimum funding threshold
- Token-based voting mechanism
- Automatic execution of approved proposals
- Vote tracking and proposal status monitoring

## 📝 Usage

### Creating a Proposal
```clarity
(contract-call? .orf submit-proposal "Research Project XYZ" u1000000000 recipient-address)
```

### Voting on Proposals
```clarity
(contract-call? .orf vote u1 u50000000 true)
```

### Executing Approved Proposals
```clarity
(contract-call? .orf execute-proposal u1)
```

## 🔍 Query Functions

- `get-proposal`: Retrieve proposal details
- `get-vote`: Check individual votes

## ⚠️ Requirements

- STX token for funding
- Governance token for voting
- Clarinet for development and testing

## 🤝 Contributing

Feel free to submit issues and enhancement requests!


