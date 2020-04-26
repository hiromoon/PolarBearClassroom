export default class Movie {
  constructor(public title: string, private is3D: boolean) {
    this.title = title;
    this.is3D = is3D;
  }

  is3DTitle(): boolean {
    return this.is3D;
  }
}
