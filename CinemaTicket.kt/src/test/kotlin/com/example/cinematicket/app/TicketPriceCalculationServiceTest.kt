package com.example.cinematicket.app

import app.TicketPriceCalculationService
import domain.models.*
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource
import java.time.LocalDateTime

/**
 * TicketPriceCalculationService のテスト
 */
class TicketPriceCalculationServiceTest {

    @Test
    fun `一般_3Dあり`() {
        // Given
        // TODO age, gender, visitorTypeddrはパラメータ化
        val visitor = Visitor("0", 27, Gender.Male, VisitorType.General)
        val movieRepository = MovieRepository()
        val screeningTime = LocalDateTime.of(2019, 12, 28, 17, 0)
        val ticketPriceCalculationService = TicketPriceCalculationService(movieRepository)
        val discount = Discount(0)

        // When
        val price = ticketPriceCalculationService.calculate(visitor, discount, 1, screeningTime)

        // Then
        Assertions.assertEquals(2200, price)
    }
}