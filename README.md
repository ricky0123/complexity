# life.ricky0123.com

This is a web app that allows you to run an artificial life experiment in your browser. The experiment starts by generating a population of randomly-initialized [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) programs. Then, the following process is repeated indefinitely:

1. A random pair of programs is chosen.
1. They are concatenated to form a longer program.
1. The program is run, using itself as the input/output tape, thereby modifying itself.
1. The program is split apart, thus producing two child programs.

The inventors of this experiment observed interesting phenomena, such as the complexity of the programs increasing over time.

For more information, listen to [this interview](https://www.preposterousuniverse.com/podcast/2024/08/19/286-blaise-aguera-y-arcas-on-the-emergence-of-replication-and-computation/) with one of the authors on the Mindscape podcast or read the paper: [Computational Life: How Well-formed, Self-replicating Programs Emerge from Simple Interaction](https://arxiv.org/pdf/2406.19108).

I created this project with [PureScript](https://pursuit.purescript.org/), a language I am trying to learn. Don't expect to see beautifully-written code in here just yet ;)

## Development

Clone the repository, then run `npm i` and `npm run build`. After doing this, you will be able to run `npm run dev` to start a local server and view the site in your browser.
