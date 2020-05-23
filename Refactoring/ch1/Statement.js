//const invoices = require(`./invoices.json`)
//const plays = require(`./plays.json`) 
//console.log(statment(invoices[0], plays));

function statement(invoice, plays) {
    let totalAmount = 0;
    let volumeCredits = 0;
    let result = `Statement for ${invoice.customer}\n`;
    
    const format  = new Intl.NumberFormat("en-US", {style: "currency", currency: "USD", maximumFractionDigits: 2}).format;

    for (const perf of invoice.performances) {
        const play = plays[perf.playID];
        let thisAmount = 0;

        switch (play.type) {
            case "tragedy":
                thisAmount = 40000;
                if (perf.audience > 30) {
                    thisAmount += 1000 * (perf.audience - 30);
                }
                break;
            case "comedy":
                thisAmount = 30000;
                if (perf.audience > 20) {
                    thisAmount += 10000 + 500 * (perf.audience - 20);
                }
                thisAmount += 300 * perf.audience;
                break;
            default:
                throw new Error(`unknown type: ${play.type}`);
        }

        // ボリューム特典のポイントを加算
        volumeCredits += Math.max(perf.audience - 30, 0);
        // 喜劇のときは10人につき、さらにポイントを加算
        if ("comedy" === play.type) { volumeCredits += Math.floor(perf.audience / 5); }
        // 注文の内容を出力
        result += ` ${play.name}: ${format(thisAmount / 100)} (${perf.audience} seats)\n`;
        totalAmount += thisAmount    
    }
    result += `Amount owed is ${format(totalAmount/100)}\n`;
    result += `You earned ${volumeCredits} ccredits \n`;
    return result;
}

module.exports = statement;