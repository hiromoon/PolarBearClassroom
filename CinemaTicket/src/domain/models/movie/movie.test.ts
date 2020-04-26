import { expect } from 'chai';
import Movie from '.';

describe('Movie', () => {
  describe('#is3DTitle', () => {
    it('3DTitleの場合', () => {
      const movie = new Movie('test', true);
      const actual = movie.is3DTitle();

      expect(actual).to.eq(true);
    });
    it('3DTitleではない場合', () => {
      const movie = new Movie('test', false);
      const actual = movie.is3DTitle();

      expect(actual).to.eq(false);
    });
  });
});
