import app.TicketPriceCalculationService
import domain.models.*
import java.time.LocalDateTime

fun main(args : Array<String>) {
    val visitor = Visitor("0", 27, Gender.Male, VisitorType.General)
    val movieRepository = MovieRepository()
    val screeningTime = LocalDateTime.now()

    val ticketPriceCalculationService = TicketPriceCalculationService(movieRepository)

    val discount = Discount(0)
    val price = ticketPriceCalculationService.calculate(visitor, discount, 1, screeningTime)

    println("Price: $price")
}
