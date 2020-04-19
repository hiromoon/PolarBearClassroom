"use strict";
exports.__esModule = true;
var discount_1 = require("./src/domain/models/discount");
var visitor_1 = require("./src/domain/models/visitor");
var movieRepository_1 = require("./src/domain/repositories/movieRepository");
var TicketPriceCalculationService_1 = require("./src/app/TicketPriceCalculationService");
var main = function (age, gender, visitorType, movieId) {
    var visitor = new visitor_1.Visitor('0', age, gender, visitorType);
    var movieRepository = new movieRepository_1["default"]();
    var screeningTime = new Date(Date.now());
    var ticketPriceCalculationService = new TicketPriceCalculationService_1["default"](movieRepository);
    var discount = new discount_1["default"](0);
    var price = ticketPriceCalculationService.calculate(visitor, discount, movieId, screeningTime);
    return price;
};
// const age = 25;
// const gender = Gender.male;
// const visitorType = VisitorType.member;
// const movieId = 1;
//
// main(age, gender, visitorType, movieId);
exports["default"] = main;
