package domain.models

class MovieRepository {
    companion object {
        val movies: Array<Movie> = arrayOf(
            Movie("title1", true),
            Movie("title2", true),
            Movie("title3", true),
            Movie("title4", true),
            Movie("title5", true),
            Movie("title6", true)
        )
    }

    fun get(id :Int ) :Movie{
        return movies[id];
    }
}