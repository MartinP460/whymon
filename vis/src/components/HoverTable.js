import React from 'react';
import Table from '@mui/material/Table';
import TableBody from '@mui/material/TableBody';
import TableCell from '@mui/material/TableCell';
import TableContainer from '@mui/material/TableContainer';
import TableHead from '@mui/material/TableHead';
import TableRow from '@mui/material/TableRow';
import Paper from '@mui/material/Paper';
import PresentFormula from './PresentFormula';
import { monospacedStringWidth } from '../util';

export default function HoverTable({ table, subf }) {

  return (
    <div className="muiTable">
      <TableContainer component={Paper}>
        <Table size="small" aria-label="a dense table">
          <TableHead sx={{ width: 100,
                           maxWidth: 100,
                           overflow: "hidden",
                           textOverflow: "ellipsis",
                           borderStyle: "border-box" }}>
            <TableRow>
              {table.columns.map((v, i) =>
                <TableCell key={i} align="center">
                  <span style={{fontSize: '18px'}} className="editorFont">
                    {v}
                  </span>
                </TableCell>
              )}
              <TableCell key={table.columns.length + 1}
                         align="center">
                  <span style={{fontWeight: 'bold'}}>
                    Formula
                  </span>
              </TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            <TableRow>
              {table.values.map((v, i) =>
                <TableCell key={i}>
                  <span className="editorFont">
                    {v}
                  </span>
                </TableCell>
              )}
              {/* <Box /\* maxWidth={(theme) => theme.breakpoints.values.sm} *\/> */}
              <TableCell key={table.values.length + 1} align="center">
                <PresentFormula formula={subf}
                                predsWidth={monospacedStringWidth(subf)+6}
                                presentingColumn={false}
                />
              </TableCell>
              {/* </Box> */}
            </TableRow>
          </TableBody>
        </Table>
      </TableContainer>
    </div>
  );
}
