import {changeset} from "vega";
import embed from "vega-embed";

const spec = {
  $schema: "https://vega.github.io/schema/vega-lite/v5.json",
  width: 400,
  height: 300,
  config: {
    background: null
  },
  data: {
    name: "sim",
  },
  transform: [{
    window: [
      {
        field: "computations",
        op: "mean",
        as: "rolling_mean",
    },
    ],
    frame: [-70, 70],
  },
  ],
  mark: "point",
  encoding: {
    x: {
      field: "interactions",
      type: "quantitative",
      title: "Number of Interactions",
    },
    y: {
      type: "quantitative",
      title: "Number of Computations",
    },
  },
  layer: [
    {
      mark: { type: "point", opacity: 0.3 },
      encoding: {
        y: { field: "computations" },
      },
    },
    {
      mark: { type: "line", color: "black", size: 1 },
      encoding: {
        y: { field: "rolling_mean" },
      },
    },
    {
      mark: { type: "rule", color: "red", opacity: 0.5, size: 1 },
      encoding: {
        x: { field: "newRecord", title: "New Record" },
      },
    }
  ]
};

const _embedChartImpl = async (el) => {
  let result = await embed(el, spec);
  return result.view;
};

// https://github.com/purescript-contrib/purescript-aff/tree/a1c9c3ee4b91d806b35da1de7eadec4da396b6d0/docs#escaping-callback-hell
export const _embedChart = (id) => () => _embedChartImpl(id);

export const addDataPoint = (view) => (interactions) => (computations) => () => {
  const changeSet = changeset().insert([{ interactions, computations }]);
  view.change("sim", changeSet).run();
};

export const addNewRecord = (view) => (newRecord) => () => {
  const changeSet = changeset().insert([{ newRecord }]);
  view.change("sim", changeSet).run();
};
