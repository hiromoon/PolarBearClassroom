import Calculator from '../domain/models/calculator';
import MovieRepository from '../domain/repositories/movieRepository';
import PriceTable from '../domain/models/priceTable';

export default class TicketPriceCalculationService {
  movieRepository: MovieRepository

  constructor(movieRepository) {
    this.movieRepository = movieRepository;
  }

  calculate(visitor, discount, movieId, screeningTime): number {
    const movie = this.movieRepository.get(movieId);
    const priceTable = PriceTable.get(screeningTime);
    const price = Calculator.calc(movie, visitor, priceTable, discount);
    return price;
  }
}
