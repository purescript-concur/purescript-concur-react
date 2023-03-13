// Demo Concur-React interop
// This file exports a JS component that is used within a concur widget
// AND this JS component itself imports and uses a Concur Widget inside
import React, { Component } from 'react';
import {helloWidgetClass} from '../../output/Test.Hello/index.js';

// React requires all component names to start with an uppercase letter
let HelloWidget = helloWidgetClass;

export default class ReactComponent extends Component {
    constructor() {
        super();
        this.state = {count: 0};
    }
    render(props) {
        let {count} = this.state;
        return (
            <div>
              <h4>This button is defined in a Concur Widget</h4>
              <HelloWidget/>
              <h4>Which is used inside a React JS component</h4>
              <button onClick={() => this.setState({count: count+1})}>
                React Button: Count is {count}
              </button>
            </div>
        );
    }
}
