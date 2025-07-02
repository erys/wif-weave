use std::collections::HashMap;
use wif_weave::Wif;
use wif_weave::wif::data::{ThreadUnit, WifColor};

#[test]
fn parse_simple_fiberworks_wif() {
    let (wif, errors) = Wif::load("wifs/simple_fiberworks_draft.wif").unwrap();
    assert_eq!(errors, HashMap::new());
    assert_eq!(
        wif.threading()
            .unwrap()
            .to_single_sequence()
            .unwrap()
            .default_iter()
            .collect::<Vec<usize>>(),
        vec![1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4]
    );
    assert_eq!(
        wif.weaving()
            .unwrap()
            .treadles()
            .unwrap()
            .as_option()
            .unwrap(),
        &4
    );
    assert_eq!(
        wif.treadling()
            .unwrap()
            .to_single_sequence()
            .unwrap()
            .default_iter()
            .collect::<Vec<usize>>(),
        vec![1, 2, 3, 4, 3, 2, 1, 2, 3, 4, 3, 2, 1]
    );

    assert_eq!(
        wif.color_palette().unwrap().color_range().unwrap(),
        &(0, 999)
    );

    assert_eq!(wif.weft().unwrap().threads().as_option().unwrap(), &13);
    assert_eq!(
        wif.weft().unwrap().units().unwrap().as_option().unwrap(),
        &ThreadUnit::Centimeters
    );
    assert_eq!(
        0.212,
        wif.warp()
            .unwrap()
            .spacing()
            .unwrap()
            .as_option()
            .unwrap()
            .clone()
            .into(),
    );

    assert_eq!(
        wif.color_palette().unwrap().colors().unwrap().0[0].value(),
        &WifColor(999, 999, 999)
    );

    assert_eq!(
        wif.tie_up()
            .unwrap()
            .default_iter()
            .collect::<Vec<Vec<usize>>>(),
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

#[test]
fn parse_color_fiberworks_wif() {
    let (wif, errors) = Wif::load("wifs/simple_fiberworks_with_colors_draft.wif").unwrap();
    assert_eq!(errors, HashMap::new());
    assert_eq!(
        wif.warp().unwrap().color_index().unwrap().as_option(),
        Some(&4)
    );

    let color_map = wif.color_palette().unwrap().colors().unwrap().to_map();
    assert_eq!(color_map.get(&4).unwrap(), &WifColor(0, 266, 999));
}
