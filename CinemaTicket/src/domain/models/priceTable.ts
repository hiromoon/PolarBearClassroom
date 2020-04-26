import { VisitorType } from './visitor';

type PriceList = {[key: string]: number}

const MovieDayPriceTable: PriceList = {
  [VisitorType.general]: 1800,
  [VisitorType.member]: 1100,
  [VisitorType.senior]: 1100,
  [VisitorType.seniorMember]: 1000,
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
  [VisitorType.seniorMember]: 1000,
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
  [VisitorType.seniorMember]: 1000,
  [VisitorType.disabledPerson]: 1000,
  [VisitorType.college]: 1500,
  [VisitorType.highSchoolStudent]: 1000,
  [VisitorType.juniorHighSchoolStudent]: 1000,
  [VisitorType.elementaryStudent]: 1000,
};
const HolidayCommon: PriceList = {
  [VisitorType.general]: 1800,
  [VisitorType.member]: 1300,
  [VisitorType.senior]: 1100,
  [VisitorType.seniorMember]: 1000,
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
  [VisitorType.seniorMember]: 1000,
  [VisitorType.disabledPerson]: 1000,
  [VisitorType.college]: 1500,
  [VisitorType.highSchoolStudent]: 1000,
  [VisitorType.juniorHighSchoolStudent]: 1000,
  [VisitorType.elementaryStudent]: 1000,
};

const MovieDay = 1;

const SaturDay = 6;
const SunDay = 0;

const LateShowStartAt = 22;

export default class PriceTable {
  // XXX: screeningTimeの命名が糞
  static get(screeningTime: Date): PriceList {
    if (screeningTime.getDate() === MovieDay) {
      return MovieDayPriceTable;
    }

    // TODO: 祝日は後で考える
    if (screeningTime.getDay() === SunDay || screeningTime.getDay() === SaturDay) {
      return screeningTime.getHours() < LateShowStartAt ? HolidayCommon : HolidayLateShow;
    }

    return screeningTime.getHours() < LateShowStartAt ? WorkdayCommon : WorkdayLateShow;
  }
}
