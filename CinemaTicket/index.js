var _a, _b, _c, _d, _e;
var VisitorType = {
    general: 'general',
    member: 'member',
    senior: 'senior',
    disabledPerson: 'disabledPerson',
    college: 'college',
    highSchoolStudent: 'highSchoolStudent',
    juniorHighSchoolStudent: 'juniorHighSchoolStudent',
    elementaryStudent: 'elementaryStudent'
};
var Gender;
(function (Gender) {
    Gender[Gender["male"] = 0] = "male";
    Gender[Gender["female"] = 1] = "female";
    Gender[Gender["other"] = 2] = "other";
})(Gender || (Gender = {}));
var Visitor = /** @class */ (function () {
    function Visitor(id, age, gender, type) {
        this.id = id;
        this.age = age;
        this.gender = gender;
        this.type = type;
    }
    return Visitor;
}());
var Movie = /** @class */ (function () {
    function Movie(title, is3D) {
        this.title = title;
        this.is3D = is3D;
    }
    Movie.prototype.is3DTitle = function () {
        return this.is3D;
    };
    return Movie;
}());
var MovieRepository = /** @class */ (function () {
    function MovieRepository() {
        this.movies = [
            new Movie('title1', true),
            new Movie('title2', false),
            new Movie('title3', true),
            new Movie('title4', false),
            new Movie('title5', true),
        ];
    }
    MovieRepository.prototype.get = function (id) {
        return this.movies[id];
    };
    return MovieRepository;
}());
var MovieDayPriceTable = (_a = {},
    _a[VisitorType.general] = 1800,
    _a[VisitorType.member] = 1000,
    _a[VisitorType.senior] = 1100,
    _a[VisitorType.disabledPerson] = 1000,
    _a[VisitorType.college] = 1500,
    _a[VisitorType.highSchoolStudent] = 1000,
    _a[VisitorType.juniorHighSchoolStudent] = 1000,
    _a[VisitorType.elementaryStudent] = 1000,
    _a);
var WorkdayCommon = (_b = {},
    _b[VisitorType.general] = 1800,
    _b[VisitorType.member] = 1000,
    _b[VisitorType.senior] = 1100,
    _b[VisitorType.disabledPerson] = 1000,
    _b[VisitorType.college] = 1500,
    _b[VisitorType.highSchoolStudent] = 1000,
    _b[VisitorType.juniorHighSchoolStudent] = 1000,
    _b[VisitorType.elementaryStudent] = 1000,
    _b);
var WorkdayLateShow = (_c = {},
    _c[VisitorType.general] = 1800,
    _c[VisitorType.member] = 1000,
    _c[VisitorType.senior] = 1100,
    _c[VisitorType.disabledPerson] = 1000,
    _c[VisitorType.college] = 1500,
    _c[VisitorType.highSchoolStudent] = 1000,
    _c[VisitorType.juniorHighSchoolStudent] = 1000,
    _c[VisitorType.elementaryStudent] = 1000,
    _c);
var HolidayCommon = (_d = {},
    _d[VisitorType.general] = 1800,
    _d[VisitorType.member] = 1000,
    _d[VisitorType.senior] = 1100,
    _d[VisitorType.disabledPerson] = 1000,
    _d[VisitorType.college] = 1500,
    _d[VisitorType.highSchoolStudent] = 1000,
    _d[VisitorType.juniorHighSchoolStudent] = 1000,
    _d[VisitorType.elementaryStudent] = 1000,
    _d);
var HolidayLateShow = (_e = {},
    _e[VisitorType.general] = 1800,
    _e[VisitorType.member] = 1000,
    _e[VisitorType.senior] = 1100,
    _e[VisitorType.disabledPerson] = 1000,
    _e[VisitorType.college] = 1500,
    _e[VisitorType.highSchoolStudent] = 1000,
    _e[VisitorType.juniorHighSchoolStudent] = 1000,
    _e[VisitorType.elementaryStudent] = 1000,
    _e);
var PriceTable = /** @class */ (function () {
    function PriceTable() {
    }
    // XXX: screeningTimeの命名が糞
    PriceTable.get = function (screeningTime) {
        if (screeningTime.day() === 1) {
            return MovieDayPriceTable;
        }
        // TODO: 祝日は後で考える
        if (screeningTime.date() === 0 || screeningTime.date() === 6) {
            return screeningTime.hour() < 22 ? HolidayCommon : HolidayLateShow;
        }
        return screeningTime.hour() < 22 ? WorkdayCommon : WorkdayLateShow;
    };
    return PriceTable;
}());
var Calculator = /** @class */ (function () {
    function Calculator() {
    }
    Calculator.calc = function (movie, visitor, priceTable, discount) {
        var price = priceTable[visitor.type];
        price -= discount;
        price += (movie.is3DTitle() ? 400 : 0);
        return price;
    };
    return Calculator;
}());
var main = function () {
    var vType = VisitorType.general;
    var gender = Gender.male;
    var age = 25;
    var visitor = new Visitor('0', age, gender.male, vType);
    var movieRepository = new MovieRepository();
    var movie = movieRepository.get(1);
    var screeningTime = Date.now();
    var priceTable = PriceTable.get(screeningTime);
    var discount = 100;
    var price = Calculator.calc(movie, visitor, priceTable, discount);
    console.info('==============================Price==============================');
    console.info(price);
    console.info('============================================================');
};
main();
