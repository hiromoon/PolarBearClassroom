package domain

import "golang-sample/domain"

type UserRepository interface {
	FindByID(int) domain.User
	Save(domain.User)
}
