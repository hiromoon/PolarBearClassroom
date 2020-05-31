function amountFor(aPerformance, play) {
  let result = 0;

  switch (play.type) {
    case 'tragedy':
      result = 40000;
      if (aPerformance.audience > 30) {
        result += 1000 * (aPerformance.audience - 30);
      }
      break;
    case 'comedy':
      result = 30000;
      if (aPerformance.audience > 20) {
        result += 10000 + 500 * (aPerformance.audience - 20);
      }
      result += 300 * aPerformance.audience;
      break;
    default:
      throw new Error(`unknown type: ${play.type}`);
  }
  return result;
}

module.exports = function statement(invoice, plays) {
  function playFor(aPerformance) {
    return plays[aPerformance.playID];
  }

  let totalAmount = 0;
  let volumeCredits = 0;
  let result = `Statement for ${invoice.customer}\n`;

  const { format } = new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD',
    minimumFractionDigits: 2,
  });

  for (const perf of invoice.performances) {
    const play = playFor(perf);
    const thisAmount = amountFor(perf, play);

    volumeCredits += Math.max(perf.audience - 30, 0);
    if (play.type === 'comedy') volumeCredits += Math.floor(perf.audience / 5);
    result += `${play.name}: ${format(thisAmount / 100)} (${perf.audience} seats)\n`;
    totalAmount += thisAmount;
  }
  result += `Amount owed is ${format(totalAmount / 100)}\n`;
  result += `You earned ${volumeCredits} credits\n`;
  return result;
};

module.exports.amountFor = amountFor;