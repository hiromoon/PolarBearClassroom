package main

import (
	"golang-sample/app"
	"golang-sample/infra"
)

func main() {
	repo := infra.NewUserRepository()
	service := app.NewUserService(repo)
	service.ActivateUser(3)
}
