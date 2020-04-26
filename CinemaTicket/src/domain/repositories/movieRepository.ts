import { Movie } from '../models/movie';

export default class MovieRepository {
  private movies: Movie[] = [
    new Movie('title1', true),
    new Movie('title2', false),
    new Movie('title3', true),
    new Movie('title4', false),
    new Movie('title5', true),
  ];

  get(id: number): Movie {
    return this.movies[id];
  }
}
