package app

import domain.models.Discount
import domain.models.MovieRepository
import domain.models.Visitor
import java.time.LocalDateTime

class TicketPriceCalculationService(val movieRepository: MovieRepository) {
    fun calculate(visitor: Visitor, discount: Discount, movieId: Int, screeningTime: LocalDateTime): Int {
        val movie = movieRepository.get(movieId)
        val priceTable = PriceTable().get(screeningTime)
        val price = calc(movie, visitor, priceTable, discount)
        return price
    }
}
