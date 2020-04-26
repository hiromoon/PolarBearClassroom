import Movie from './movie';
import { Visitor } from './visitor';
import { PriceList } from './priceTable';
import Discount from './discount';

export default class Calculator {
  static calc(movie: Movie, visitor: Visitor, priceList: PriceList, discount: Discount): number {
    let price = priceList[visitor.visitorType];
    price -= discount.value;
    price += (movie.is3DTitle() ? 400 : 0);
    return price;
  }
}
