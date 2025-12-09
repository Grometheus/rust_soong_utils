use serde::{Deserialize, Serialize};

#[derive(Deserialize, Debug)]
pub struct SoongConfig {

    path : Vec<SoongPath>
}




#[derive(Deserialize, Debug)]
pub struct SoongPath {
    path : String,

    fake_path : Option<String>


}
