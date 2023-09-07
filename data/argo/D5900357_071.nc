CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_CALIB       N_LEVELS   8   	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       p2005-04-01T10:16:03Z Decoding & Creation; 2010-05-05T16:39:04Z DMQC & Calibration; 2013-09-11T17:11:32Z Timing;    
references        (http://www.argodatamgt.org/Documentation   comment       	free text      comment_on_resolution         �The profile TEMP and PSAL data resolution can be different than nominal. The data packing algorithm requires lower resolution be used to accommodate high vertical gradients. Values of 0.002, 0.004, & 0.008 are typical.     user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  conventions       Argo reference table 1     	long_name         	Data type      
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    8   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    8   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  84   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  8t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     9   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     98   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     9X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   axis      T      
_FillValue        A.�~            9|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :�   PRES               
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�     axis      Z      
_FillValue        G�O�      �  :�   PRES_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  ;�   PRES_ADJUSTED                  
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�     axis      Z      
_FillValue        G�O�      �  ;�   PRES_ADJUSTED_QC                  	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  <�   PRES_ADJUSTED_ERROR                   	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�     
_FillValue        G�O�      �  <�   TEMP               	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ;�o   
_FillValue        G�O�      �  =�   TEMP_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  >�   TEMP_ADJUSTED                  	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ;�o   
_FillValue        G�O�      �  >�   TEMP_ADJUSTED_QC                  	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  ?�   TEMP_ADJUSTED_ERROR                   	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   
_FillValue        G�O�      �  ?�   PSAL               	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ;�o   
_FillValue        G�O�      �  @�   PSAL_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  A�   PSAL_ADJUSTED                  	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ;�o   
_FillValue        G�O�      �  A�   PSAL_ADJUSTED_QC                  	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  B�   PSAL_ADJUSTED_ERROR                   	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   
_FillValue        G�O�      �  C   	PARAMETER            
   	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  C�   SCIENTIFIC_CALIB_EQUATION            
   	            	long_name         'Calibration equation for this parameter    
_FillValue                    D   SCIENTIFIC_CALIB_COEFFICIENT         
   	            	long_name         *Calibration coefficients for this equation     
_FillValue                    G   SCIENTIFIC_CALIB_COMMENT         
   	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    J   SCIENTIFIC_CALIB_DATE            
   	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  M   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    MH   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ML   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    MP   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    MT   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  MX   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    M�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    M�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    M�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        M�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        M�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        M�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    M�Argo profile    3.1 1.2 19500101000000  20050401101603  20140718134426  5900357 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   PRES            TEMP            PSAL               GA   AO  0471_22808_071                  2C  D   SOLO                            2170                            V1.0; SBE601 02Jan03            851 @ӴRӮh^8   @ӴW|o�'@4���F�b�=p��
1   ARGOS   A   A   A   Primary sampling: averaged [data averaged with equal weights into irregular pressure bins, sampled at 0.5 Hz from a SBE41CP]                                                                                                                                       @�  A  Ap  A�  A�  B  B  B4  BL  Bd  B|  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C  C  C  C  C%  C+  C1  C7  C=  CC  CI  CO  C^  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D  D  D"  D1  D@  DO  D^  Dm  D|  11111111111111111111111111111111111111111111111111111111@�  A  Ap  A�  A�  B  B  B4  BL  Bd  B|  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C  C  C  C  C%  C+  C1  C7  C=  CC  CI  CO  C^  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D  D  D"  D1  D@  DO  D^  Dm  D|  11111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��#A��#A��#A��TA��A��^A�XA���A�A�A�bA���A�;dA�JA�hsA�z�A�  A�n�A���A���A��\A�$�A��^A�M�A�r�A���A�5?A�
=A���A�9XA��7A���A���A��A��;Ak�mAH  A4��A%��AC�A�A�+A%@�ƨ@�C�@�{@���@��@�^5@���@�Q�@�Ĝ@��@�9X@�Ĝ11111111111111111111111111111111111111111111111111111111A���A���A��#A��#A��#A��TA��A��^A�XA���A�A�A�bA���A�;dA�JA�hsA�z�A�  A�n�A���A���A��\A�$�A��^A�M�A�r�A���A�5?A�
=A���A�9XA��7A���A���A��A��;Ak�mAH  A4��A%��AC�A�A�+A%@�ƨ@�C�@�{@���@��@�^5@���@�Q�@�Ĝ@��@�9X@�Ĝ11111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�jB�jB�jB�jB�jB�jB��BĜB��B��B+BK�BS�Bl�B�7B��B�^B��BB�wB�BBBB�B�FB�BhsB7LB��B�9B�BbNB9XBbB�B
"�B	ffB	=qB	Q�B	ZB	C�B	/B	Q�B	�B	��B	��B	�B	��B
VB
K�B
�B
�-B
��B
�/B
�y11111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��BBVB33BgmB�+B�hB�BŢB�#B��B	7BB��B+B?}B?}BF�B8RB��BƨB�Bz�B;dB��BǮB��B}�BZB
_;B	p�B�!B�%B��B��B�DBt�B��B��B�/B�TB�B	�B	Q�B	�\B	ǮB	��B
\B
�B
+11111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<bi�<#�(<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<DP<#�
<Q��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OW V1.0: r =0.9952(+/-0.0001), vertically averaged dS =-0.189(+/-0.004)                                                                                                                        SIO SOLO floats correct for mild pressure drift onboard the float by zeroing the pressure sensor while on the surface. Additional correction to pressure was found to be unnecessary in DMQC;          PRES_ADJ_ERR: Manufacturer sensor accuracy               No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy                        Significant salinity drift present; OW weighted least squares fit is adopted; Map Scales:[x:4/2,y:2/1.0]; Fit to T<4.5C; max_breaks=1;                                                                 PSAL_ADJ_ERR:max(OW+CTM err,SBE/other err set to 0.01psu)201005051638432010050516384320100505163843  AO  ARFM                                                                        20050401101603  IP                  G�O�G�O�G�O�                AO  ARGQ                                                                        20050401101603  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20050401101603  QCF$                G�O�G�O�G�O�0               SI  ARSQOW  V1.0CTD_for_DMQC_2010V01                                            20100505163854  IP                  G�O�G�O�G�O�                SI  ARSQSIQCV2.1                                                                20100505163854  IP                  G�O�G�O�G�O�                SI  ARCAOW                                                                      20100505163904  IP                  G�O�G�O�G�O�                SI  ARDU                                                                        20140718134426  IP                  G�O�G�O�G�O�                SI  ARSQ                                                                        20140718134426  IP                  G�O�G�O�G�O�Timing          SI  ARDU                                                                        20140718134426  IP                  G�O�G�O�G�O�                