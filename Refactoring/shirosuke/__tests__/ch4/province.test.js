const Province = require('../../ch4/province')

function sampleProvinceData() {
    return {
        name: "Asia",
        producers: [
            { name: "Byzantium",  cost: 10, production: 9 },
            { name: "Attalia",    cost: 12, production: 10 },
            { name: "Sinope",     cost: 10, production: 6 }
        ],
        demand: 30,
        price: 20
    };
}

test('province', () => {
    const asia = new Province(sampleProvinceData());
    expect(asia.shortfall).toBe(5);
});