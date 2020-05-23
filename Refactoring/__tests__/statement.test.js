const statement = require('../ch1/statement')

const plays = {
    "hamlet": { "name": "Hamlet", "type": "tragedy" },
    "as-like": { "name": "As You Like It", "type": "comedy" },
    "othello": { "name": "Othello", "type": "tragedy" }
}

const invoices = [
    {
        "customer": "BigCo",
        "performances": [
            {
                "playID": "hamlet",
                "audience": 55
            },
            {
                "playID": "as-like",
                "audience": 35
            },
            {
                "playID": "othello",
                "audience": 40
            }
        ]
    }
]

// const expected = "Statement for BigCo\n" +
//     "Hamlet: $650.00 (55 seats)\n" +
//     "As You Like It: $580.00 (35 seats)\n" +
//     "Othello: $500.00 (40 seats)\n" +
//     "Amount owed is $1,730.00\n" +
//     "You earned 47 ccredits \n\n";

const expected = `Statement for BigCo
 Hamlet: $650.00 (55 seats)
 As You Like It: $580.00 (35 seats)
 Othello: $500.00 (40 seats)
Amount owed is $1,730.00
You earned 47 ccredits 
`

test('regression test', () => {
    expect(statement(invoices[0], plays)).toBe(expected);
});
