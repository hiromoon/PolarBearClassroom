package domain

type User struct {
	Id     int
	Active bool
}

func NewUser(id int) *User {
	user := &User{id, false}
	return user
}

func (user *User) Activate() {
	user.Active = true
}
