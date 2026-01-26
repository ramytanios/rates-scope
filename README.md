# 📈 Rates Vanilla Scope

Vanilla option pricing library for interest rates derivatives 🚧 _Work in progress_

## JSON-RPC API

The `json-rpc/` module provides HTTP API access to the pricing library via JSON-RPC 2.0. It exposes four main methods:

- **`price`** - Price caplets, swaptions, and backward-looking caplets
- **`arbitrage`** - Check volatility arbitrage for specific tenor/expiry
- **`arbitragematrix`** - Full arbitrage matrix across all tenors/expiries
- **`volsampling`** - Volatility skew sampling with PDF calculations ⚡

## API Example

```json
{
  "jsonrpc": "2.0",
  "method": "price",
  "params": {
    "tRef": "2025-10-12",
    "payoff": {
      "rate": "LIBOR_RATE",
      "fixingAt": "2026-10-12",
      "startAt": "2026-10-14",
      "endAt": "2027-01-14",
      "paymentAt": "2027-01-14",
      "paymentCurrency": "USD",
      "strike": 0.009887915724457295,
      "discountCurve": { "currency": "USD", "name": "SINGLE_CURVE" },
      "optionType": "Call",
      "type": "Caplet"
    },
    "market": {
      "USD": {
        "rates": {
          "LIBOR_RATE": {
            "currency": "USD",
            "tenor": "3M",
            "spotLag": 2,
            "dayCounter": "Act360",
            "calendar": "NO_HOLIDAYS",
            "resetCurve": { "currency": "USD", "name": "SINGLE_CURVE" },
            "bdConvention": "ModifiedFollowing",
            "type": "Libor"
          }
        },
        "curves": {
          "SINGLE_CURVE": { "rate": "0.02", "type": "ContinuousCompounding" }
        },
        "volatility": {
          "unit": "BpPerYear",
          "cube": {
            "3M": {
              "surface": {
                "1Y": {
                  "skew": [
                    [-0.02, 100.0],
                    [0.0, 69.0],
                    [0.02, 93.0]
                  ]
                }
              }
            }
          }
        }
      }
    },
    "static": { "calendars": { "NO_HOLIDAYS": { "holidays": [] } } }
  },
  "id": 1
}
```

**Response:**

```json
{ "jsonrpc": "2.0", "result": { "price": 0.001234 }, "id": 1 }
```

## Usage 🔍

```bash
# Build project
sbt compile
# Run JSON-RPC server
sbt "json-rpc/run"
# Run tests
sbt test
```
