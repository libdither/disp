use std::any::type_name;

use bytecheck::CheckBytes;
use rkyv::{validation::validators::DefaultValidator, Archived};
use serde::{Deserialize, Serialize};

use crate::{Hash, NativeHashtype};
