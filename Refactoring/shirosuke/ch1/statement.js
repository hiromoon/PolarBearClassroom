function statement(invoice, plays) {
    const statementData = {};
    statementData.customer = invoice.customer;
    statementData.performances = invoice.performances.map(enrichPerformance);
    statementData.totalAmount = totalAmount(statementData);
    return renderPlainText(statementData, plays);

    function enrichPerformance(aPerformance) {
        const result = Object.assign({}, aPerformance);
        // TODO オブジェクト渡してない? copy必要なのでは?
        result.play =  playFor(result); 
        result.amount = amountFor(result);
        result.volumeCredits = volumeCreditsFor(result);
        return result;
    }

    function playFor(aPerformance) {
        return plays[aPerformance.playID];
    }

    function amountFor(aPerformance) {
        let result = 0;
        switch (aPerformance.play.type) {
            case "tragedy":
                result = 40000;
                if (aPerformance.audience > 30) {
                    result += 1000 * (aPerformance.audience - 30);
                }
                break;
            case "comedy":
                result = 30000;
                if (aPerformance.audience > 20) {
                    result += 10000 + 500 * (aPerformance.audience - 20);
                }
                result += 300 * aPerformance.audience;
                break;
            default:
                // FIXME ここ通ったらバグる
                throw new Error(`unknown type: ${play.type}`);
        }
        return result;
    }

    function totalAmount(data) {
        let totalAmount = 0;
        for (const perf of data.performances) {
            totalAmount += perf.amount
        }
        return totalAmount;
    }

    function volumeCreditsFor(aPerformance) {
        let result = 0;
        // ボリューム特典のポイントを加算
        result += Math.max(aPerformance.audience - 30, 0);
        // 喜劇のときは10人につき、さらにポイントを加算
        if ("comedy" === aPerformance.play.type) { result += Math.floor(aPerformance.audience / 5); }
        return result;
    }
}

function renderPlainText(data) {
    let result = `Statement for ${data.customer}\n`;

    for (const perf of data.performances) {
        // 注文の内容を出力
        result += ` ${perf.play.name}: ${usd(perf.amount)} (${perf.audience} seats)\n`;
    }

    result += `Amount owed is ${usd(data.totalAmount)}\n`;
    result += `You earned ${totalVolumeCredits()} credits \n`;
    return result; 

    function totalVolumeCredits() {
        let volumeCredits = 0;
        for (const perf of data.performances) {
            volumeCredits += perf.volumeCredits;
        }
        return volumeCredits;
    }

    function usd(aNumber) {
        return new Intl.NumberFormat("en-US", { style: "currency", currency: "USD", maximumFractionDigits: 2 }).format(aNumber / 100);
    }    
}
module.exports = statement;