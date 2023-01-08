import { html, TemplateResult } from 'lit';
import '../src/tako-webui.js';

export default {
  title: 'TakoWebui',
  component: 'tako-webui',
  argTypes: {
    backgroundColor: { control: 'color' },
  },
};

interface Story<T> {
  (args: T): TemplateResult;
  args?: Partial<T>;
  argTypes?: Record<string, unknown>;
}

interface ArgTypes {
  title?: string;
  backgroundColor?: string;
}

const Template: Story<ArgTypes> = ({
  title,
  backgroundColor = 'white',
}: ArgTypes) => html`
  <tako-webui
    style="--tako-webui-background-color: ${backgroundColor}"
    .title=${title}
  ></tako-webui>
`;

export const App = Template.bind({});
App.args = {
  title: 'My app',
};
