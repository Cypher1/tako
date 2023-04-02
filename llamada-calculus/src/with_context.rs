pub struct WithContext<'a, Ctx, T> {
    pub ctx: &'a Ctx,
    pub val: &'a T,
    pub names: Vec<String>,
}

impl<'a, Ctx, T> WithContext<'a, Ctx, T> {
    pub fn new(ctx: &'a Ctx, val: &'a T, names: Vec<String>) -> Self {
        WithContext {
            ctx,
            val,
            names: names.to_vec(),
        }
    }

    pub fn child<U>(&self, val: &'a U, new_names: Vec<String>) -> WithContext<'a, Ctx, U> {
        let mut names = self.names.clone();
        names.extend(new_names);
        WithContext::new(self.ctx, val, names)
    }
}
