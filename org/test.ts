
  interface A {
    title: string;
  }

  interface B<T> {
    field: T;
  }

const MyComp = ({title, field}: A & B<number>) => {
    console.log(title, field);
}
MyComp({title: "Hello", field: 32});
