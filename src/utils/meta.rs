#[derive(Debug)]
pub struct Meta<T>(pub T);

impl<T> From<T> for Meta<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<T> std::ops::Deref for Meta<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> std::ops::DerefMut for Meta<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> PartialEq for Meta<T> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}
impl<T> Eq for Meta<T> {}

impl<T> std::hash::Hash for Meta<T> {
    fn hash<H: std::hash::Hasher>(&self, _h: &mut H) {
        // Intentionally do nothing here.
    }
}

impl<T> PartialOrd for Meta<T> {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ordering::Equal)
    }
}
impl<T> Ord for Meta<T> {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Meta<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
