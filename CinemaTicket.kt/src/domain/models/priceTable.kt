package domain.models

import java.time.DayOfWeek
import java.time.LocalDateTime


class PriceTable() {
    companion object {
        const val MOVIE_DAY = 1

        val MovieDayPriceTable = PriceList(
            mapOf<VisitorType, Int>(
                VisitorType.General to 1800,
                VisitorType.Member to 1100,
                VisitorType.Senior to 1100,
                VisitorType.SeniorMember to 1000,
                VisitorType.DisabledPerson to 1000,
                VisitorType.College to 1500,
                VisitorType.HighSchoolStudent to 1000,
                VisitorType.JuniorHighSchoolStudent to 1000,
                VisitorType.ElementaryStudent to 1000
            )
        )

        val WorkdayCommon = PriceList(
            mapOf<VisitorType, Int>(
                VisitorType.General to 1800,
                VisitorType.Member to 1000,
                VisitorType.Senior to 1100,
                VisitorType.SeniorMember to 1000,
                VisitorType.DisabledPerson to 1000,
                VisitorType.College to 1500,
                VisitorType.HighSchoolStudent to 1000,
                VisitorType.JuniorHighSchoolStudent to 1000,
                VisitorType.ElementaryStudent to 1000
            )
        )

        val WorkdayLateShow = PriceList(
            mapOf<VisitorType, Int>(
                VisitorType.General to 1800,
                VisitorType.Member to 1000,
                VisitorType.Senior to 1100,
                VisitorType.SeniorMember to 1000,
                VisitorType.DisabledPerson to 1000,
                VisitorType.College to 1500,
                VisitorType.HighSchoolStudent to 1000,
                VisitorType.JuniorHighSchoolStudent to 1000,
                VisitorType.ElementaryStudent to 1000
            )
        )

        val HolidayCommon = PriceList(
            mapOf<VisitorType, Int>(
                VisitorType.General to 1800,
                VisitorType.Member to 1300,
                VisitorType.Senior to 1100,
                VisitorType.SeniorMember to 1000,
                VisitorType.DisabledPerson to 1000,
                VisitorType.College to 1500,
                VisitorType.HighSchoolStudent to 1000,
                VisitorType.JuniorHighSchoolStudent to 1000,
                VisitorType.ElementaryStudent to 1000
            )
        )

        val HolidayLateShow = PriceList(
            mapOf<VisitorType, Int>(
                VisitorType.General to 1800,
                VisitorType.Member to 1000,
                VisitorType.Senior to 1100,
                VisitorType.SeniorMember to 1000,
                VisitorType.DisabledPerson to 1000,
                VisitorType.College to 1500,
                VisitorType.HighSchoolStudent to 1000,
                VisitorType.JuniorHighSchoolStudent to 1000,
                VisitorType.ElementaryStudent to 1000
            )
        )
    }

    fun get(screeningTime: LocalDateTime): PriceList {
        if (screeningTime.dayOfMonth == MOVIE_DAY) {
            return MovieDayPriceTable
        }

        if (screeningTime.dayOfWeek == DayOfWeek.SUNDAY || screeningTime.dayOfWeek == DayOfWeek.SATURDAY) {
           return if (screeningTime.hour < 22) HolidayCommon else HolidayLateShow
        }
        return if (screeningTime.hour < 22) WorkdayCommon else WorkdayLateShow
    }
}

class PriceList(val priceList: Map<VisitorType, Int>)
