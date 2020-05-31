const fs = require('fs');
const statement = require('./statement');

const invoices = JSON.parse(fs.readFileSync('./data/invoices.json').toString());
const plays = JSON.parse(fs.readFileSync('./data/plays.json').toString());

console.log(statement(invoices[0], plays));
