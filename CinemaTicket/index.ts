import Gender from './src/domain/models/gender';
import Discount from './src/domain/models/discount';
import { VisitorType, Visitor } from './src/domain/models/visitor';

import MovieRepository from './src/domain/repositories/movieRepository';
import TicketPriceCalculationService from './src/app/TicketPriceCalculationService';

export default const main = (age, gender, visitorType, movieId) => {
  const visitor = new Visitor('0', age, gender, visitorType);

  const movieRepository = new MovieRepository();
  const screeningTime = new Date(Date.now());


  const ticketPriceCalculationService = new TicketPriceCalculationService(
    movieRepository,
  );

  const discount = new Discount(0);

  const price = ticketPriceCalculationService.calculate(visitor, discount, movieId, screeningTime);
  console.info('==============================Price==============================');
  console.info(screeningTime);
  console.info(screeningTime.getDay());
  console.info(price);
  console.info('============================================================');
};

const age = 25;
const gender = Gender.male;
const visitorType = VisitorType.member;
const movieId = 1;

main(age, gender, visitorType, movieId);
