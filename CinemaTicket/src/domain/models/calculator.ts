import Movie from './movie';
import { Visitor } from './visitor';
import PriceTable from './priceTable';
import Discount from './discount';

export default class Calculator {
  static calc(movie: Movie, visitor: Visitor, priceTable: PriceTable, discount: Discount): number {
    let price = priceTable[visitor.visitorType];
    price -= discount.value;
    price += (movie.is3DTitle() ? 400 : 0);
    return price;
  }
}
