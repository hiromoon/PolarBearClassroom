module.exports = class PerformanceCalculator {
  constructor(aPerformance, aPlay) {
    this.performance = aPerformance;
    this.play = aPlay;
  }

  get amount() {
    switch (this.play.type) {
      case 'tragedy':
        throw Error('Unexpected call: tragedy.');
      case 'comedy':
        throw Error('Unexpected call: comedy.');
      default:
        throw new Error(`unknown type: ${this.play.type}`);
    }
  }

  get volumeCredits() {
    let result = 0;
    result += Math.max(this.performance.audience - 30, 0);
    if (this.play.type === 'comedy') result += Math.floor(this.performance.audience / 5);
    return result;
  }
};
