package app

import "golang-sample/domain"

type UserService struct {
	repo domain.UserRepository
}

func NewUserService(repo domain.UserRepository) *UserService {
	var service = &UserService{}
	service.repo = repo
	for index := 0; index < 10; index++ {
		repo.Save(*domain.NewUser(index))
	}
	return service
}

func (service *UserService) ActivateUser(userID int) {
	repo := service.repo
	user := repo.FindByID(userID)
	user.Activate()
}
