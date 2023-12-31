CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  V   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:19Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        X  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  @    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  Ex   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  F�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  L(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  Q�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  R�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  X0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  Y�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ^�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  d8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  e�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  j�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  l@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  q�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    q�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    t�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    w�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  z�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    z�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    z�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    z�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    {    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  {   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    {D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    {T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    {X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         {h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         {l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        {p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    {tArgo profile    3.1 1.2 19500101000000  20181005190519  20181005190519  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               >A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׾��w��1   @׾�y\��@1S�E����c�7KƧ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      >A   A   A   @�  @���@���AffA>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"�C$  C%�fC'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D��D� D  D� D  D�fDfDy�D��D� D	  D	� D
  D
� DfD� D  D� D  D�fD  D� D  Dy�D��D� DfD� D  Dy�D  Dy�D��D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D�fD  Dy�D  D� D��D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(fD(� D)  D)� D*  D*� D+�D+@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���A ��A ��A@��AbffA�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B��B��B�L�BȀ B�L�B�L�B�L�B�L�B�L�B�L�B�L�B� B�L�B�L�B�L�B�L�B�� C &fC&fC�C&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC@ C&fC &fC"@ C$&fC&�C(�C*&fC,&fC.&fC0&fC2&fC4&fC6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR�CT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv&fCx&fCz&fC|&fC~&fC�3C�3C�3C�3C�  C�3C�3C�3C�3C�fC�fC�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�  C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�  C�3C�3C�  C�  C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�  C�3C�3C�3C�3C�  C�  C�3C�fC�fC�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�fC�3C�3C�3C�3C�3D 	�D ��D	�D��D	�D��D	�D��D4D��D	�D��D	�D� D D�4D4D��D		�D	��D
	�D
��D D��D	�D��D	�D� D	�D��D	�D�4D4D��D D��D	�D�4D	�D�4D4D��D	�D��D	�D��D	�D��D	�D��D4D��D	�D��D	�D��D	�D� D	�D�4D	�D��D4D��D 	�D ��D!	�D!��D"	�D"�4D#	�D#��D$	�D$��D%	�D%��D& D&��D'	�D'��D( D(��D)	�D)��D*	�D*��D+#4D+I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�r�AځAډ7Aډ7AڋDAڋDAڍPAڍPAڏ\Aڏ\Aڏ\Aڏ\Aډ7AڑhAړuAڏ\A�|�A�r�AڃA�33A��yAټjA�n�A�$�A���Aذ!A�O�A���A�\)A֗�A�XAԣ�A�XA�$�AӶFA���AэPA�"�A��A���A΋DA;wA��A̓uA�K�A�9XA�`BA��A�33A�z�A��A�S�A�~�A�`BA��jA�O�A��PA�t�A�l�A��A��HA�l�A�JA�?}A�M�A�A�A�A��mA�{A��jA��jA���A��FA�oA�JA��A��A�-A�z�A���A�9XA�ƨA���A��A�XA�=qA�M�A�~�A��A��A��hA�oA��A��TA���A�ĜA��A�&�A��TA���A�M�A�ffA��#A|ĜAx�+Au�^As��Ar��An�jAjn�Ag��AcXA^jA\�A[x�AX�RAT�AQ?}AO��AN �AM�ALjAK��AJ�AIXAC��AA��A=33A:��A:JA8z�A6��A5�A5dZA4ȴA2�A1l�A0��A/��A.�A.A�A-�#A,��A+��A)�A(��A({A'�A&n�A%�A%&�A$�!A"�A!��A!&�A =qAM�A��A+A7LA7LAZA�yA  A�hAS�A�/A��A\)AK�A�hA`BA�yA^5AXAx�A�
A�FA�A�yA`BAdZAG�A7LA;dA%AĜAjAA1'A�uA�A�A�AVA�DAZA �A�-AA	�;A�+A{A 1@�/@��D@��@�bN@�l�@�7L@���@�n�@�O�@�Q�@�V@�7@��@�z�@�b@�@���@��@��@�@�?}@�&�@�&�@�&�@�@�I�@�(�@���@�l�@�G�@�9@�bN@��;@�|�@��@��#@�@�`B@�b@�-@���@��@�G�@ݺ^@�=q@ݩ�@�O�@�X@���@ܓu@�bN@���@�+@ڇ+@ڟ�@ڰ!@�@� �@���@�ff@�\)@��@֏\@և+@��@�n�@�=q@�5?@��@�J@�$�@��#@�x�@���@ҏ\@҇+@��@�x�@�?}@�Ĝ@��@�|�@�S�@�ff@��#@�`B@��@�
=@�ȴ@�ȴ@ʗ�@�@ɡ�@�G�@���@� �@ư!@�?}@�r�@�33@��H@\@�?}@�~�@�`B@�1@���@�S�@�C�@���@���@��P@�l�@�Q�@��@�E�@�V@�1@�|�@�"�@�
=@�ȴ@��\@���@�V@�S�@�1@��7@��^@�Ĝ@�9X@���@�V@��\@�v�@�=q@�z�@��D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�r�AځAډ7Aډ7AڋDAڋDAڍPAڍPAڏ\Aڏ\Aڏ\Aڏ\Aډ7AڑhAړuAڏ\A�|�A�r�AڃA�33A��yAټjA�n�A�$�A���Aذ!A�O�A���A�\)A֗�A�XAԣ�A�XA�$�AӶFA���AэPA�"�A��A���A΋DA;wA��A̓uA�K�A�9XA�`BA��A�33A�z�A��A�S�A�~�A�`BA��jA�O�A��PA�t�A�l�A��A��HA�l�A�JA�?}A�M�A�A�A�A��mA�{A��jA��jA���A��FA�oA�JA��A��A�-A�z�A���A�9XA�ƨA���A��A�XA�=qA�M�A�~�A��A��A��hA�oA��A��TA���A�ĜA��A�&�A��TA���A�M�A�ffA��#A|ĜAx�+Au�^As��Ar��An�jAjn�Ag��AcXA^jA\�A[x�AX�RAT�AQ?}AO��AN �AM�ALjAK��AJ�AIXAC��AA��A=33A:��A:JA8z�A6��A5�A5dZA4ȴA2�A1l�A0��A/��A.�A.A�A-�#A,��A+��A)�A(��A({A'�A&n�A%�A%&�A$�!A"�A!��A!&�A =qAM�A��A+A7LA7LAZA�yA  A�hAS�A�/A��A\)AK�A�hA`BA�yA^5AXAx�A�
A�FA�A�yA`BAdZAG�A7LA;dA%AĜAjAA1'A�uA�A�A�AVA�DAZA �A�-AA	�;A�+A{A 1@�/@��D@��@�bN@�l�@�7L@���@�n�@�O�@�Q�@�V@�7@��@�z�@�b@�@���@��@��@�@�?}@�&�@�&�@�&�@�@�I�@�(�@���@�l�@�G�@�9@�bN@��;@�|�@��@��#@�@�`B@�b@�-@���@��@�G�@ݺ^@�=q@ݩ�@�O�@�X@���@ܓu@�bN@���@�+@ڇ+@ڟ�@ڰ!@�@� �@���@�ff@�\)@��@֏\@և+@��@�n�@�=q@�5?@��@�J@�$�@��#@�x�@���@ҏ\@҇+@��@�x�@�?}@�Ĝ@��@�|�@�S�@�ff@��#@�`B@��@�
=@�ȴ@�ȴ@ʗ�@�@ɡ�@�G�@���@� �@ư!@�?}@�r�@�33@��H@\@�?}@�~�@�`B@�1@���@�S�@�C�@���@���@��P@�l�@�Q�@��@�E�@�V@�1@�|�@�"�@�
=@�ȴ@��\@���@�V@�S�@�1@��7@��^@�Ĝ@�9X@���@�V@��\@�v�@�=q@�z�@��D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�jB
ƨB
�B
�fB
�B
�B
�B
�B
��B
��B
�B
�HB
�HB
�fB
�TB
�;B
�TB
�jB
�^B
�dB
��B
�#B
��B+B\BbB)�BS�B`BB~�B��B��BB�)B�B�BoB�B#�B'�B(�B+B1'B?}Br�Bm�BO�B7LB7LB8RB1'B;dB?}B=qBS�B�B�B��B��B��B�JBs�BS�B;dB/B,B�B\BB��BɺB�\BhsBN�B�B
��B
��B
�B
��B
��B
��B
x�B
XB
.B	��B	�B	�HB	�B	ĜB	��B	��B	{�B	[#B	P�B	G�B	9XB	'�B	�B	�B	{B	\B	JB	+B	B��B�#B��B��B�^B�LB�-B�!B�'B�'B�!B�B�B��B��B�B�-B�3B�'B�dB�dB�^B�FB�-B�!B�B�B�B��B��B��B��B��B�3B��B��B�XB�FB�3B�'B�FB��B��B��B�^BȴB��B��B�B�B��B�)B�mB�B�B��B	
=B	�B	�B	�B	�B	�B	�B	�B	{B	$�B	33B	.B	(�B	%�B	,B	)�B	(�B	(�B	-B	(�B	$�B	�B��B�;B��B��B��B��B��B�B�B�
B�B��B��B��B��B�
B�B�mB�B�B��B��B��B��B	B	B	B	
=B	�B	�B	�B	�B	 �B	$�B	$�B	$�B	%�B	'�B	(�B	(�B	'�B	#�B	 �B	(�B	0!B	6FB	=qB	@�B	?}B	A�B	D�B	G�B	G�B	E�B	E�B	E�B	F�B	F�B	E�B	F�B	H�B	I�B	P�B	Q�B	Q�B	VB	]/B	_;B	_;B	cTB	dZB	e`B	gmB	ffB	hsB	l�B	o�B	r�B	p�B	o�B	p�B	p�B	t�B	v�B	v�B	x�B	x�B	x�B	x�B	|�B	|�B	}�B	~�B	�B	�B	�%B	�1B	�1B	�7B	�1B	�+B	�%B	�B	�B	� B	|�B	z�B	z�B	{�B	{�B	}�B	�B	�PB	�uB	��B	��B	��B	��B	�{B	�uB	�uB	�oB	�uB	�{B	��B	��B	��B	��B	��B	�B	�'B	�'B	�B	�B	�B	�'B	�!B	�!B	�B	�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�jB
ƨB
�B
�fB
�B
�B
�B
�B
��B
��B
�B
�HB
�HB
�fB
�TB
�;B
�TB
�jB
�^B
�dB
��B
�#B
��B+B\BbB)�BS�B`BB~�B��B��BB�)B�B�BoB�B#�B'�B(�B+B1'B?}Br�Bm�BO�B7LB7LB8RB1'B;dB?}B=qBS�B�B�B��B��B��B�JBs�BS�B;dB/B,B�B\BB��BɺB�\BhsBN�B�B
��B
��B
�B
��B
��B
��B
x�B
XB
.B	��B	�B	�HB	�B	ĜB	��B	��B	{�B	[#B	P�B	G�B	9XB	'�B	�B	�B	{B	\B	JB	+B	B��B�#B��B��B�^B�LB�-B�!B�'B�'B�!B�B�B��B��B�B�-B�3B�'B�dB�dB�^B�FB�-B�!B�B�B�B��B��B��B��B��B�3B��B��B�XB�FB�3B�'B�FB��B��B��B�^BȴB��B��B�B�B��B�)B�mB�B�B��B	
=B	�B	�B	�B	�B	�B	�B	�B	{B	$�B	33B	.B	(�B	%�B	,B	)�B	(�B	(�B	-B	(�B	$�B	�B��B�;B��B��B��B��B��B�B�B�
B�B��B��B��B��B�
B�B�mB�B�B��B��B��B��B	B	B	B	
=B	�B	�B	�B	�B	 �B	$�B	$�B	$�B	%�B	'�B	(�B	(�B	'�B	#�B	 �B	(�B	0!B	6FB	=qB	@�B	?}B	A�B	D�B	G�B	G�B	E�B	E�B	E�B	F�B	F�B	E�B	F�B	H�B	I�B	P�B	Q�B	Q�B	VB	]/B	_;B	_;B	cTB	dZB	e`B	gmB	ffB	hsB	l�B	o�B	r�B	p�B	o�B	p�B	p�B	t�B	v�B	v�B	x�B	x�B	x�B	x�B	|�B	|�B	}�B	~�B	�B	�B	�%B	�1B	�1B	�7B	�1B	�+B	�%B	�B	�B	� B	|�B	z�B	z�B	{�B	{�B	}�B	�B	�PB	�uB	��B	��B	��B	��B	�{B	�uB	�uB	�oB	�uB	�{B	��B	��B	��B	��B	��B	�B	�'B	�'B	�B	�B	�B	�'B	�!B	�!B	�B	�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190519                              AO  ARCAADJP                                                                    20181005190519    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190519  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190519  QCF$                G�O�G�O�G�O�8000            