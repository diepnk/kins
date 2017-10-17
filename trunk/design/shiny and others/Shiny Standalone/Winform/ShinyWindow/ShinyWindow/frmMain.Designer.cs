namespace ShinyWindow
{
    partial class frmMain
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.wbrsApp = new System.Windows.Forms.WebBrowser();
            this.SuspendLayout();
            // 
            // wbrsApp
            // 
            this.wbrsApp.Dock = System.Windows.Forms.DockStyle.Fill;
            this.wbrsApp.Location = new System.Drawing.Point(0, 0);
            this.wbrsApp.MinimumSize = new System.Drawing.Size(20, 20);
            this.wbrsApp.Name = "wbrsApp";
            this.wbrsApp.ScriptErrorsSuppressed = true;
            this.wbrsApp.Size = new System.Drawing.Size(568, 313);
            this.wbrsApp.TabIndex = 0;
            this.wbrsApp.Url = new System.Uri("http://www.vnexpress.net", System.UriKind.Absolute);
            // 
            // frmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(568, 313);
            this.Controls.Add(this.wbrsApp);
            this.Name = "frmMain";
            this.Text = "Shiny Demo";
            this.WindowState = System.Windows.Forms.FormWindowState.Maximized;
            this.Load += new System.EventHandler(this.Form1_Load);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.WebBrowser wbrsApp;
    }
}

