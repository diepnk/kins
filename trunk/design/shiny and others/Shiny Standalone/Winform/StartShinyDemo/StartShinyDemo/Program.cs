using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;

namespace StartShinyDemo
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.Write(System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location));
            Process scriptProc = new Process();
            scriptProc.StartInfo.FileName = @"run.vbs";
            scriptProc.StartInfo.WorkingDirectory = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location); //<---very important 
            scriptProc.StartInfo.Arguments = "//B //Nologo run.vbs";
            scriptProc.StartInfo.WindowStyle = ProcessWindowStyle.Hidden; //prevent console window from popping up
            scriptProc.Start();
            scriptProc.Close();
        }
    }
}
