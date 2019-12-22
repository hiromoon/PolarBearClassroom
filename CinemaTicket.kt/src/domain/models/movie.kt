package domain.models

class Movie(val title: String, private val is3D: Boolean) {
    fun is3DTitle(): Boolean {
        return this.is3D
    }
}