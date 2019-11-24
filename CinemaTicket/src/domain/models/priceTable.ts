import { VisitorType } from './visitor';

type PriceList = {[key:string]: number}

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

export default class PriceTable {
  // XXX: screeningTimeの命名が糞
  static get(screeningTime: Date) {
    if (screeningTime.getDay() === 1) {
      return MovieDayPriceTable;
    }

    // TODO: 祝日は後で考える
    if (screeningTime.getDate() === 0 || screeningTime.getDate() === 6) {
      return screeningTime.getHours() < 22 ? HolidayCommon : HolidayLateShow;
    }

    return screeningTime.getHours() < 22 ? WorkdayCommon : WorkdayLateShow;
  }
}
