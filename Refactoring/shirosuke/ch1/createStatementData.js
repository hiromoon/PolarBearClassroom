function createStatementData(invoice, plays) {
    const result = {};
    result.customer = invoice.customer;
    result.performances = invoice.performances.map(enrichPerformance);
    result.totalAmount = totalAmount(result);
    result.totalVolumeCredits = totalVolumeCredits(result);
    return result;

    function enrichPerformance(aPerformance) {
        const result = Object.assign({}, aPerformance);
        const calculator = new PerformanceCalculator(aPerformance, playFor(result));
        // TODO オブジェクト渡してない? copyした方が良いのでは?
        result.play =  calculator.play;
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
        return data.performances
            .reduce((total, p) => total + p.amount, 0);
    }

    function volumeCreditsFor(aPerformance) {
        let result = 0;
        // ボリューム特典のポイントを加算
        result += Math.max(aPerformance.audience - 30, 0);
        // 喜劇のときは10人につき、さらにポイントを加算
        if ("comedy" === aPerformance.play.type) { result += Math.floor(aPerformance.audience / 5); }
        return result;
    }

    function totalVolumeCredits(data) {
        return data.performances
            .reduce((total, p) => total + p.volumeCredits, 0);
    }
}

class PerformanceCalculator {
    constructor(aPerformance, aPlay) {
        this.performance = aPerformance;
        this.play = aPlay;
    }
}

module.exports = createStatementData;