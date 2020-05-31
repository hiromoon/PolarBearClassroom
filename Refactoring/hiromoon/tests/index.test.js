const { expect } = require('chai');
const fs = require('fs');

const statement = require('../src/statement');
const { amountFor } = require('../src/statement');

describe('statement.js', () => {
  let invoices;
  let plays;

  beforeEach(() => {
    invoices = JSON.parse(fs.readFileSync('./data/invoices.json').toString());
    plays = JSON.parse(fs.readFileSync('./data/plays.json').toString());
  });

  describe('.statement', () => {
    it('should be BigCo output', () => {
      const expected = `Statement for BigCo
Hamlet: $650.00 (55 seats)
As You Like It: $580.00 (35 seats)
Othello: $500.00 (40 seats)
Amount owed is $1,730.00
You earned 47 credits
`;
      expect(statement(invoices[0], plays)).to.equal(expected);
    });
  });

  describe('.amountFor', () => {
    it('should be amount for hamlet', () => {
      const perf = invoices[0].performances[0];
      const play = plays.hamlet;
      expect(amountFor(perf, play)).to.equal(65000);
    });
  });
});
