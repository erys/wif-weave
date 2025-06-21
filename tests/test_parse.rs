use wif_weave::Wif;
use wif_weave::wif::data::{ColorMetadata, WifColor};

#[test]
fn parse_simple_fiberworks_wif() {
    let (wif, errors) = Wif::load("wifs/simple_fiberworks_draft.wif").unwrap();
    assert!(errors.is_empty());
    assert_eq!(
        wif.threading()
            .unwrap()
            .to_single_sequence()
            .unwrap()
            .default_iter()
            .collect::<Vec<u32>>(),
        vec![1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4]
    );
    assert_eq!(
        wif.treadling()
            .unwrap()
            .to_single_sequence()
            .unwrap()
            .default_iter()
            .collect::<Vec<u32>>(),
        vec![1, 2, 3, 4, 3, 2, 1, 2, 3, 4, 3, 2, 1]
    );

    assert_eq!(
        wif.color_palette().unwrap().color_range().unwrap(),
        ColorMetadata(0, 999)
    );

    assert_eq!(
        wif.color_palette().unwrap().colors().unwrap().0[0].value(),
        &WifColor(999, 999, 999)
    );

    assert_eq!(
        wif.tie_up()
            .unwrap()
            .default_iter()
            .collect::<Vec<Vec<u32>>>(),
        vec![
            vec![1, 2],
            vec![2, 3],
            vec![3, 4],
            vec![1, 4],
            vec![1, 3],
            vec![2, 4]
        ]
    );

    let wif_string = wif.writes();
    assert_eq!(wif, Wif::read(wif_string).unwrap().0)
}
