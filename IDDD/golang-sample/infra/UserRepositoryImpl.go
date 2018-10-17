package infra

import "golang-sample/domain"

type UserRepositoryImpl struct {
	userList []*domain.User
}

func NewUserRepository() *UserRepositoryImpl {
	repo := &UserRepositoryImpl{}
	return repo
}

func (repo *UserRepositoryImpl) FindByID(userId int) *domain.User {
	var user *domain.User
	for _, u := range repo.userList {
		if u.Id == userId {
			user = u
		}
	}
	return user
}

func (repo *UserRepositoryImpl) Save(user *domain.User) {
	repo.userList = append(repo.userList, user)
}
