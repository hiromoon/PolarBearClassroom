enum {
  general,
  member,
  senior,
  disabledPerson,
  college,
  highSchoolStudent,
  juniorHighSchoolStudent,
  elementaryStudent,
}

enum {
  male,
  female,
  other,
}

class Visitor {
  constructor(private id:string, public age: number, public gender: Gender, public type: VisitorType) {
  }
}

class Movie {
  constructor(public title: string, private _is3DTitle: boolean) {
  }
  is3DTitle(): boolean {
    return this._is3DTitle
  }
}

const movies:[]Movie = [
  new Movie('title1', true),
  new Movie('title2', false),
  new Movie('title3', true),
  new Movie('title4', false),
  new Movie('title5', true),
]

class MovieRepository {
  get(id: number): Movie {
    return movies[id]
  }
}

type PriceTable = {[key:VisitorType]: nuber}

const MovieDayPriceTable: PriceTable = {
  VisitorType.general: 1800,
  VisitorType.member: 1000,
  VisitorType.senior: 1100,
  VisitorType.disabledPerson: 1000,
  VisitorType.college: 1500,
  VisitorType.highSchoolStudent: 1000,
  VisitorType.juniorHighSchoolStudent: 1000,
  VisitorType.elementaryStudent: 1000,
}
const WorkdayCommon: PriceTable = {
  VisitorType.general: 1800,
  VisitorType.member: 1000,
  VisitorType.senior: 1100,
  VisitorType.disabledPerson: 1000,
  VisitorType.college: 1500,
  VisitorType.highSchoolStudent: 1000,
  VisitorType.juniorHighSchoolStudent: 1000,
  VisitorType.elementaryStudent: 1000,
}
const WorkdayLateShow: PriceTable = {
  VisitorType.general: 1800,
  VisitorType.member: 1000,
  VisitorType.senior: 1100,
  VisitorType.disabledPerson: 1000,
  VisitorType.college: 1500,
  VisitorType.highSchoolStudent: 1000,
  VisitorType.juniorHighSchoolStudent: 1000,
  VisitorType.elementaryStudent: 1000,
}
const HolidayCommon: PriceTable = {
  VisitorType.general: 1800,
  VisitorType.member: 1000,
  VisitorType.senior: 1100,
  VisitorType.disabledPerson: 1000,
  VisitorType.college: 1500,
  VisitorType.highSchoolStudent: 1000,
  VisitorType.juniorHighSchoolStudent: 1000,
  VisitorType.elementaryStudent: 1000,
}
const HolidayLateShow: PriceTable = {
  VisitorType.general: 1800,
  VisitorType.member: 1000,
  VisitorType.senior: 1100,
  VisitorType.disabledPerson: 1000,
  VisitorType.college: 1500,
  VisitorType.highSchoolStudent: 1000,
  VisitorType.juniorHighSchoolStudent: 1000,
  VisitorType.elementaryStudent: 1000,
}

class PriceTable {
  // XXX: screeningTimeの命名が糞
  static get(screeningTime: DateTime) {
    if (screeningTime.day() === 1) {
      return MovieDayPriceTable
    }

    // TODO: 祝日は後で考える
    if (screeningTime.date() === 0 || screeningTime.date() === 6 ) {
      return screeningTime.hour() < 22 ? HolidayCommon : HolidayLateShow
    }

    return screeningTime.hour() < 22 ? WorkdayCommon : WorkdayLateShow
  }
}

type Discount = number

class Calculator {
  static calc(movie: Movie, visitor: Visitor, priceTable: PriceTable, discount: Discount) {
    let price = priceTable[visitor.type]
    price -= discount
    price += (movie.is3DTitle() ? 400 : 0)
    return price
  }
}

const main = () => {
  const vType = VisitorType.general
  const gender = Gender.male
  const age = 25

  const visitor = new Visitor('0', age, gender, vType)

  const movieRepository = new MovieRepository()
  const movie = MovieRepository.get(1)

  const screeningTime = Date.now()

  const priceTable = PriceTable.get(screeningTime)
  class discount:Discount = 100

  price = Calculator.calc(movie, visitor, priceTable, discount)
  console.log("==============================Price==============================")
  console.log(price)
  console.log("============================================================")
}

main()
