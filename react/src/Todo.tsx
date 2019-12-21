import * as React from 'react'

export const Todo = ({ todo: { id, text, checked }, onCheck, onDelete }) => {
  return (
    <div style={{ border: "1px solid black" }}>
      <input type="checkbox" checked={checked} onChange={() => onCheck(id)} />
      <div>{text}</div>
      <button onClick={() => onDelete(id)}>x</button>
    </div>
  );
};
