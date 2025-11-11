import * as echarts from 'echarts/core';

import { LineChart } from 'echarts/charts';

import {
  TitleComponent,
  GridComponent,
  DatasetComponent,
  TransformComponent,
} from 'echarts/components';

// Features like Universal Transition and Label Layout
import { LabelLayout, UniversalTransition } from 'echarts/features';

// Import the Canvas renderer
// Note that including the CanvasRenderer or SVGRenderer is a required step
import { CanvasRenderer } from 'echarts/renderers';

// Register the required components
echarts.use([
  TitleComponent,
  LineChart,
  GridComponent,
  DatasetComponent,
  TransformComponent,
  LabelLayout,
  UniversalTransition,
  CanvasRenderer
]);

export const createLineChart = ({ title, elemId }) => () => {
  const x = new Uint32Array(1);
  const y = new Uint32Array(0);

  const myChart = echarts.init(document.getElementById(elemId));
  myChart.setOption({
    title: {
      text: title
    },
  });
  const updateFn = ({ noOps, compression }) => () => {
    myChart.setOption({
      title: {
        text: title
      },
      dataset: [
        {
          source: {
            x: noOps.interactions,
            y: noOps.noOps,
          }
        },
        {
          source: {
            x: compression.interactions,
            y: compression.compression,
          }
        },
      ],
      xAxis: { type: 'value', name: 'interactions' },
      yAxis: [
        { type: 'value', name: 'noOps' },
        { type: 'value', name: 'information' },
      ],
      series: [
        {
          name: 'No Ops',
          datasetIndex: 0,
          type: 'line',
          symbol: 'none',
          encode: { x: 'x', y: 'y' },
          yAxisIndex: 0,
        },
        {
          name: 'Information',
          datasetIndex: 1,
          type: 'line',
          symbol: 'none',
          encode: { x: 'x', y: 'y' },
          yAxisIndex: 1,
        },
      ]
    }, true)
  }
  return updateFn
}
