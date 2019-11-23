const VisitorType = {
  general: 'general',
  member: 'member',
  senior: 'senior',
  disabledPerson: 'disabledPerson',
  college: 'college',
  highSchoolStudent: 'highSchoolStudent',
  juniorHighSchoolStudent: 'juniorHighSchoolStudent',
  elementaryStudent: 'elementaryStudent',
};

enum Gender {
  male,
  female,
  other,
}

class Visitor {
  constructor(private id:string, public age: number, public gender: Gender, public type: VisitorType) {
  }
}

class Movie {
  constructor(public title: string, private is3D: boolean) {
  }

  is3DTitle(): boolean {
    return this.is3D;
  }
}


class MovieRepository {
  private movies: Movie[] = [
    new Movie('title1', true),
    new Movie('title2', false),
    new Movie('title3', true),
    new Movie('title4', false),
    new Movie('title5', true),
  ];

  get(id: number): Movie {
    return this.movies[id];
  }
}

type PriceList = {[key:VisitorType]: number}

const MovieDayPriceTable: PriceList = {
  [VisitorType.general]: 1800,
  [VisitorType.member]: 1000,
  [VisitorType.senior]: 1100,
  [VisitorType.disabledPerson]: 1000,
  [VisitorType.college]: 1500,
  [VisitorType.highSchoolStudent]: 1000,
  [VisitorType.juniorHighSchoolStudent]: 1000,
  [VisitorType.elementaryStudent]: 1000,
};
const WorkdayCommon: PriceList = {
  [VisitorType.general]: 1800,
  [VisitorType.member]: 1000,
  [VisitorType.senior]: 1100,
  [VisitorType.disabledPerson]: 1000,
  [VisitorType.college]: 1500,
  [VisitorType.highSchoolStudent]: 1000,
  [VisitorType.juniorHighSchoolStudent]: 1000,
  [VisitorType.elementaryStudent]: 1000,
};
const WorkdayLateShow: PriceList = {
  [VisitorType.general]: 1800,
  [VisitorType.member]: 1000,
  [VisitorType.senior]: 1100,
  [VisitorType.disabledPerson]: 1000,
  [VisitorType.college]: 1500,
  [VisitorType.highSchoolStudent]: 1000,
  [VisitorType.juniorHighSchoolStudent]: 1000,
  [VisitorType.elementaryStudent]: 1000,
};
const HolidayCommon: PriceList = {
  [VisitorType.general]: 1800,
  [VisitorType.member]: 1000,
  [VisitorType.senior]: 1100,
  [VisitorType.disabledPerson]: 1000,
  [VisitorType.college]: 1500,
  [VisitorType.highSchoolStudent]: 1000,
  [VisitorType.juniorHighSchoolStudent]: 1000,
  [VisitorType.elementaryStudent]: 1000,
};
const HolidayLateShow: PriceList = {
  [VisitorType.general]: 1800,
  [VisitorType.member]: 1000,
  [VisitorType.senior]: 1100,
  [VisitorType.disabledPerson]: 1000,
  [VisitorType.college]: 1500,
  [VisitorType.highSchoolStudent]: 1000,
  [VisitorType.juniorHighSchoolStudent]: 1000,
  [VisitorType.elementaryStudent]: 1000,
};

class PriceTable {
  // XXX: screeningTimeの命名が糞
  static get(screeningTime: Date) {
    if (screeningTime.day() === 1) {
      return MovieDayPriceTable;
    }

    // TODO: 祝日は後で考える
    if (screeningTime.date() === 0 || screeningTime.date() === 6) {
      return screeningTime.hour() < 22 ? HolidayCommon : HolidayLateShow;
    }

    return screeningTime.hour() < 22 ? WorkdayCommon : WorkdayLateShow;
  }
}

type Discount = number

class Calculator {
  static calc(movie: Movie, visitor: Visitor, priceTable: PriceTable, discount: Discount) {
    let price = priceTable[visitor.type];
    price -= discount;
    price += (movie.is3DTitle() ? 400 : 0);
    return price;
  }
}

const main = () => {
  const vType = VisitorType.general;
  const gender = Gender.male;
  const age = 25;

  const visitor = new Visitor('0', age, gender.male, vType);

  const movieRepository = new MovieRepository();
  const movie = movieRepository.get(1);

  const screeningTime = Date.now();

  const priceTable = PriceTable.get(screeningTime);
  const discount:Discount = 100;

  const price = Calculator.calc(movie, visitor, priceTable, discount);
  console.info('==============================Price==============================');
  console.info(price);
  console.info('============================================================');
};

main();
