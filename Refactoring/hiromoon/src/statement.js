const createStatementData = require('./createStatementData');

function renderPlainText(data) {
  function usd(aNumber) {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
      minimumFractionDigits: 2,
    }).format(aNumber / 100);
  }

  let result = `Statement for ${data.customer}\n`;

  for (const perf of data.performances) {
    result += `${perf.play.name}: ${usd(perf.amount)} (${perf.audience} seats)\n`;
  }

  result += `Amount owed is ${usd(data.totalAmount)}\n`;
  result += `You earned ${data.totalVolumeCredits} credits\n`;
  return result;
}

module.exports = function statement(invoice, plays) {
  return renderPlainText(createStatementData(invoice, plays));
};
