export default class Movie {
  constructor(public title: string, private is3D: boolean) {
  }

  is3DTitle(): boolean {
    return this.is3D;
  }
}
