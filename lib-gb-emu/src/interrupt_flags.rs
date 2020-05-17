pub struct InterruptFlags
{
    pub vblank: bool,
    pub lcdstat: bool,
    pub timer: bool,
}

impl InterruptFlags
{
    pub fn new() -> InterruptFlags
    {
        InterruptFlags {
            vblank: false,
            lcdstat: false,
            timer: false,
        }
    }
}
