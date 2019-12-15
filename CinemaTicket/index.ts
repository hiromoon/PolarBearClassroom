import Gender from './src/domain/models/gender';
import Discount from './src/domain/models/discount';
import { VisitorType, Visitor } from './src/domain/models/visitor';

import MovieRepository from './src/domain/repositories/movieRepository';
import TicketPriceCalculationService from './src/app/TicketPriceCalculationService';

const main = (age, gender, visitorType, movieId) => {
  const visitor = new Visitor('0', age, gender, visitorType);

  const movieRepository = new MovieRepository();
  const screeningTime = new Date(Date.now());


  const ticketPriceCalculationService = new TicketPriceCalculationService(
    movieRepository,
  );

  const discount = new Discount(0);

  const price = ticketPriceCalculationService.calculate(visitor, discount, movieId, screeningTime);
  return price;
};

// const age = 25;
// const gender = Gender.male;
// const visitorType = VisitorType.member;
// const movieId = 1;
//
// main(age, gender, visitorType, movieId);

export default main;
