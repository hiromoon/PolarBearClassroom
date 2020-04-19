package domain.models

fun calc(movie: Movie, visitor: Visitor, priceList: PriceList, discount: Discount): Int {
    var price: Int = priceList.get(visitorType = visitor.visitorType)
    price -= discount.value
    price += if (movie.is3DTitle()) 400 else 0
    return price
}
