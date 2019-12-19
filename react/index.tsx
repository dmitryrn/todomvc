import React, { useState, useEffect } from "react";
import ReactDOM from "react-dom";

import "./styles.css";

const Todo = ({ todo: { id, text, checked }, onCheck, onDelete }) => {
  return (
    <div style={{ border: "1px solid black" }}>
      <input type="checkbox" checked={checked} onChange={() => onCheck(id)} />
      <div>{text}</div>
      <button onClick={() => onDelete(id)}>x</button>
    </div>
  );
};

let _id = 0;
const getId = () => {
  _id += 1;
  return _id;
};

const makeTodo = text => ({
  id: getId(),
  text,
  checked: false
});

function App() {
  const [todo, setTodo] = useState("");
  const [todos, setTodos] = useState([]);
  const [visibilityFilter, setVisibilityFilter] = useState("all"); // active completed

  const onKeyPress = e => {
    if (e.key === "Enter") {
      setTodos(todos.concat(makeTodo(todo)));
      setTodo("");
    }
  };

  const onCheck = idc => {
    setTodos(
      todos.map(todo =>
        todo.id === idc ? { ...todo, checked: !todo.checked } : todo
      )
    );
  };

  const onDelete = idx => {
    setTodos(todos.filter(({ id }) => id !== idx));
  };

  const filters = {
    all: () => true,
    active: ({ checked }) => !checked,
    completed: ({ checked }) => checked
  };

  const predicate = filters[visibilityFilter];

  const todosLeft = todos.reduce(
    (acc, { checked }) => acc + (checked ? 0 : 1),
    0
  );

  const clearCompleted = () => {
    setTodos(todos.filter(({ checked }) => !checked));
  };

  const toggleChecks = () => {
    if (todos.some(({ checked }) => !checked)) {
      setTodos(todos.map(todo => ({ ...todo, checked: true })));
    } else {
      setTodos(todos.map(todo => ({ ...todo, checked: false })));
    }
  };

  return (
    <div
      style={{ display: "flex", flexDirection: "column", alignItems: "start" }}
    >
      <input
        value={todo}
        onChange={e => setTodo(e.target.value)}
        onKeyPress={onKeyPress}
      />
      <button onClick={toggleChecks}>toggle cheks</button>
      {todos.filter(predicate).map(todo => (
        <Todo key={todo.id} todo={todo} onDelete={onDelete} onCheck={onCheck} />
      ))}
      <div>
        {["all", "active", "completed"].map(f => (
          <button
            style={visibilityFilter === f ? { border: "1px solid" } : {}}
            onClick={() => setVisibilityFilter(f)}
          >
            {f}
          </button>
        ))}
      </div>
      {todosLeft} left
      <button onClick={clearCompleted}>clear completed</button>
    </div>
  );
}

const rootElement = document.getElementById("root");
ReactDOM.render(<App />, rootElement);
