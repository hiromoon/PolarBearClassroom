package domain.models

fun calc(movie: Movie, visitor: Visitor, priceTable: PriceTable, discount: Discount) {
    var price: Int = priceTable[visitor.visitorType]
    price -= discount.value
    price += if (movie.is3DTitle()) 400 else 0
    return price
}



