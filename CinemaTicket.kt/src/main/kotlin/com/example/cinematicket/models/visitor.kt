package domain.models

enum class Gender {
    Male, Female, Other
}

enum class VisitorType {
    General,
    Member,
    SeniorMember,
    Senior,
    DisabledPerson,
    College,
    HighSchoolStudent,
    JuniorHighSchoolStudent,
    ElementaryStudent
}

public class Visitor(var id: String, var age: Int, var gender: Gender, var visitorType: VisitorType) {
}