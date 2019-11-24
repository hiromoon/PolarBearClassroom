import Calculator from './src/domain/models/calculator';
import Gender from './src/domain/models/gender';
import PriceTable from './src/domain/models/priceTable';
import Discount from './src/domain/models/discount';
import { VisitorType, Visitor } from './src/domain/models/visitor';

import MovieRepository from './src/domain/repositories/movieRepository';

const main = () => {
  const vType = VisitorType.general;
  const gender = Gender.male;
  const age = 25;

  const visitor = new Visitor('0', age, gender, vType);

  const movieRepository = new MovieRepository();
  const movie = movieRepository.get(1);

  const screeningTime = new Date(Date.now());

  const priceTable = PriceTable.get(screeningTime);
  const discount:Discount = { value: 100 };

  const price = Calculator.calc(movie, visitor, priceTable, discount);
  console.info('==============================Price==============================');
  console.info(price);
  console.info('============================================================');
};

main();
