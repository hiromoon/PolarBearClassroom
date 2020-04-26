import Calculator from '../domain/models/calculator';
import MovieRepository from '../domain/repositories/movieRepository';
import PriceTable from '../domain/models/priceTable';
import { Visitor } from '../domain/models/visitor';
import Discount from '../domain/models/discount';

export default class TicketPriceCalculationService {
  movieRepository: MovieRepository

  constructor(movieRepository: MovieRepository) {
    this.movieRepository = movieRepository;
  }

  calculate(visitor: Visitor, discount: Discount, movieId: number, screeningTime: Date): number {
    const movie = this.movieRepository.get(movieId);
    const priceList = PriceTable.get(screeningTime);
    const price = Calculator.calc(movie, visitor, priceList, discount);
    return price;
  }
}
