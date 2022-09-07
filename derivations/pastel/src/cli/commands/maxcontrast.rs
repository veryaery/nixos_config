use crate::commands::prelude::*;

pub struct MaxContrastCommand;

impl GenericCommand for MaxContrastCommand {
    fn run(&self, out: &mut Output, matches: &ArgMatches, config: &Config) -> Result<()> {
        let color = matches.value_of("color").expect("required argument");
        let color = ColorArgIterator::from_color_arg(config, color, &mut PrintSpectrum::Yes)?;
        
        let mut options = vec![];
        for option in ColorArgIterator::from_args(config, matches.values_of("option"))? {
            options.push(option?);
        }

        let mut max = options.first().expect("required argument");
        let max = {
            let mut max_ratio = max.contrast_ratio(&color);
            for option in options.iter().skip(1) {
                let ratio = option.contrast_ratio(&color);
                if ratio > max_ratio {
                    max_ratio = ratio;
                    max = option;
                }
            }
            max
        };

        out.show_color(config, max)?;

        Ok(())
    }
}
