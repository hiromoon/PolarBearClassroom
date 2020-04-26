import Gender from './gender';

export const VisitorType = {
  general: 'general',
  member: 'member',
  seniorMember: 'seniorMember',
  senior: 'senior',
  disabledPerson: 'disabledPerson',
  college: 'college',
  highSchoolStudent: 'highSchoolStudent',
  juniorHighSchoolStudent: 'juniorHighSchoolStudent',
  elementaryStudent: 'elementaryStudent',
};

export class Visitor {
    private id: string

    public age: number

    public gender: Gender

    public visitorType: string

    constructor(
      id: string,
      age: number,
      gender: Gender,
      visitorType: string,
    ) {
      this.id = id;
      this.age = age;
      this.gender = gender;
      this.visitorType = visitorType;
    }
}
