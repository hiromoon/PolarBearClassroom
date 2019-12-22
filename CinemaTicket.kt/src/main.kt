fun main(args : Array<String>) {
    println("Hello Kotlin!!")
    val visitor = new Visitor('0', age, gender, visitorType)
    val movieRepository = new MovieRepository()
    val screeningTime = new DateTime()

    val ticketPriceCalculationService = new TicketPriceCalculationService(movieRepository)

    val discount = new Discount(0)
    val price = ticketPriceCalculationService.caculate(visitor, discount, movieId, screeningTime)

    println("Price: $price")
}