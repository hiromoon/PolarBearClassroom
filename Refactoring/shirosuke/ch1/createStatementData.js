function createStatementData(invoice, plays) {
    const result = {};
    result.customer = invoice.customer;
    result.performances = invoice.performances.map(enrichPerformance);
    result.totalAmount = totalAmount(result);
    result.totalVolumeCredits = totalVolumeCredits(result);
    return result;

    function enrichPerformance(aPerformance) {
        const result = Object.assign({}, aPerformance);
        const calculator = new createPerformanceCalculator(aPerformance, playFor(result));
        // TODO オブジェクト渡してない? copyした方が良いのでは?
        result.play = calculator.play;
        result.amount = calculator.amount;
        result.volumeCredits = volumeCreditsFor(result);
        return result;
    }

    function createPerformanceCalculator(aPerformance, aPlay) {
        switch (aPlay.type) {
            case "tragedy": return new TragedyCalculator(aPerformance, aPlay);
            case "comedy": return new ComedyCalculator(aPerformance, aPlay);
            default:
                throw new Error(`未知の演劇の種類： ${aPlay.type}`);
        }
    }

    function playFor(aPerformance) {
        return plays[aPerformance.playID];
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
    amountFor() {
        throw Error("サブクラスの責務");
    }
}

class TragedyCalculator extends PerformanceCalculator {
    get amount() {
        let result = 40000;
        if (this.performance.audience > 30) {
            result += 1000 * (this.performance.audience - 30);
        }
        return result;
    }
}

class ComedyCalculator extends PerformanceCalculator {
    get amount() {
        let result = 30000;
        if (this.performance.audience > 20) {
            result += 10000 + 500 * (this.performance.audience - 20);
        }
        result += 300 * this.performance.audience;
        return result;
    }
}

module.exports = createStatementData;