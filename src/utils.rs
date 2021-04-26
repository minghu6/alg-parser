
pub type CounterType = impl FnMut() -> usize;

pub fn gen_counter()-> CounterType {
    _gen_counter(0)
}

fn _gen_counter(init: usize) -> CounterType {
    let mut count = init;

    move || {
        let old_count = count;
        count += 1;
        old_count
    }
}


mod test {
    #[test]
    fn test_counter() {
        use super::{ gen_counter, _gen_counter };

        let mut c0counter = _gen_counter(0);

        assert_eq!(c0counter(), 0);
        assert_eq!(c0counter(), 1);
        assert_eq!(c0counter(), 2);

        let mut c5counter = _gen_counter(5);
        assert_eq!(c5counter(), 5);
        assert_eq!(c5counter(), 6);

        let mut counter = gen_counter();
        assert_eq!(counter(), 0);
        assert_eq!(counter(), 1);
    }
}