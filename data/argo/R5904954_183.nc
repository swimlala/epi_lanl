CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  ^   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:30Z creation      
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
resolution        =���   axis      Z        x  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  @H   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  E�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  G    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  L�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  R   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  Sp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  X�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ZH   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  _�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  e8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  f�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  mp   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  r�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    s   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    v   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    y   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  |   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    |D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    |H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    |L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    |P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  |T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    |�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    |�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    |�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         |�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         |�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        |�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    |�Argo profile    3.1 1.2 19500101000000  20181005191730  20181005191730  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$Ot��1   @��$�>�&@6a���o�d� ě��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A>ffA^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BO��BW��B`  Bh  Bp  Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC�fC   C"  C$�C&  C(  C)�fC,  C.  C0  C1�fC3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�fC��3C�  C��C��C�  C��3C�  C��C��C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C��C��C�  C��3C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C��C�  C��3C��3C�  C�  C��3C��3C�  C��C��C��C�  C�  C�  C�  C��3C��C�  C�  C��C��C��3C��3C�  C�  C��3C��3C��3C��C�  C��C�  C�  C�  C�  C�  C�  C��C��C��3C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C��C��3C��3C��3C�  C�  C��3C��C�  C��3C��3C�  C��3C�  C�  C��C��3C��3C�  C�  C�  C��C�  C�  C��C�  C��3C��3C��3C��3D y�D ��D� D  Dy�D  D�fD  D� DfDy�D��Dy�D  D�fDfD�fD	  D	� D	��D
� DfD��D  D� DfD� D  D� D�3Dy�D��Dy�D  D� D  Dy�D  D�fDfD�fD  D� D  D�fD  D�fDfD� D��Dy�D  D� D  D�fD  D� DfD�fDfD� D��D� D��D � D!fD!y�D"  D"�fD#fD#� D$  D$� D%  D%y�D%��D&� D'fD'� D(  D(� D)fD)� D*  D*�fD+  D+y�D,  D,�fD-  D-y�Dy��D�<{D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @C34@���@���AffA"ffA@��A`��A�33A�33A�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B1  B8��B@��BH��BP34BX34B`��Bh��Bp��Bx34B��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C @ C&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC�C�C &fC"&fC$@ C&&fC(&fC*�C,&fC.&fC0&fC2�C4�C6&fC8&fC:&fC<&fC>&fC@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN&fCP&fCR&fCT&fCV&fCX&fCZ�C\&fC^&fC`&fCb&fCd&fCf&fCh&fCj&fCl&fCn&fCp&fCr&fCt&fCv�Cx&fCz&fC|&fC~&fC�fC�fC�3C�  C�  C�3C�fC�3C�  C�  C�3C�3C�3C�  C�  C�3C�3C�3C�  C�3C�3C�  C�3C�fC�3C�3C�3C�  C�3C�3C�  C�  C�3C�fC�3C�  C�3C�3C�3C�3C�fC�3C�3C�3C�  C�  C�3C�3C�  C�3C�fC�fC�3C�3C�fC�fC�3C�  C�  C�  C�3C�3C�3C�3C�fC�  C�3C�3C�  C�  C�fC�fC�3C�3C�fC�fC�fC�  C�3C�  C�3C�3C�3C�3C�3C�3C�  C�  C�fC�3C�3C�3C�3C�  C�fC�3C�3C�3C�3C�  C�fC�fC�fC�3C�3C�fC�  C�3C�fC�fC�3C�fC�3C�3C�  C�fC�fC�3C�3C�3C�,�C�3C�3C�  C�3C�fC�fC�fD 3D �4D4D��D	�D�4D	�D� D	�D��D D�4D4D�4D	�D� D D� D		�D	��D
4D
��D D�gD	�D��D D��D	�D��D��D�4D4D�4D	�D��D	�D�4D	�D� D D� D	�D��D	�D� D	�D� D D��D4D�4D	�D��D	�D� D	�D��D D� D D��D4D��D 4D ��D! D!�4D"	�D"� D# D#��D$	�D$��D%	�D%�4D&4D&��D' D'��D(	�D(��D) D)��D*	�D*� D+	�D+�4D,	�D,� D-	�D-�4Dy��D�AHD��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A�A�A�A�ƨA�ĜA�ȴA�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���AϼjAϲ-Aϩ�Aϡ�Aϙ�Aϕ�AϑhA�z�A�hsA�dZA�bNA�dZA�\)A�G�A�33A�+A��A�
=A��A���AΗ�A�VA�;dA�1'A�$�A��yA���Aͣ�A�VA�z�Aɴ9A���AƮAĸRA�{A�bA��A��A�{A�1A���A�JA�ffA�JA�5?A�/A�ƨA��A��FA�A�A���A���A�M�A��A�VA��-A�K�A��A�VA�I�A��A�G�A�r�A�"�A�oA�$�A�A�A��jA�(�A�7LA��A��DA�~�A�&�A���A�v�A��#A��A���A�5?A��;A���A�XA�(�A�ƨA���A���A�M�A���A�r�A��A���A�33A��A���A��FA�JA�VA���A�S�A�33A��A�x�A~��A|�!A|1Az��Ax��Aw;dAu`BArn�Aq33Ao�Al�9Aj�DAgC�Ae��Ad��AcdZAa�
A`�DA_?}A^$�A]��A[�
AZv�AY;dAX�jAXI�AWK�AUx�AS�
AS&�AR��AQl�AO�TAO�AMS�AKoAH��AF�AC��AA�TAA�A@ĜA@jA?�PA?/A>�DA=�A;/A8�+A7�PA5�A5C�A4�RA3�mA2��A0��A.��A.VA.-A,�A+��A*��A*A�A(~�A%|�A#K�A"r�A!��A I�A?}A�A��A`BA�RAx�A�Al�A9XA�#A?}A��AG�A~�A"�A&�A^5A\)AO�AG�A&�AffAhsA&�A
��A
1A	��A	K�A	�A��AƨAt�A
=AA�A��A�DA^5AJA�RA(�A��A�-A�RA��A~�A��A?}@��w@�G�@��@���@��`@�@�x�@�u@�(�@�@��#@��@�"�@�J@�9@�1'@��#@���@�&�@�Z@��H@��@�?}@�bN@҇+@�X@���@�Q�@�ƨ@�\)@��H@��`@ʟ�@��@���@�1'@�
=@őh@��@�ff@��@��@���@���@�J@��u@���@��
@�(�@�b@���@��@�X@�r�@��F@�33@�v�@�@���@�ƨ@�"�@�E�@�/@��@�bN@�  @�l�@�K�@�
=@���@��@�A�@�S�@��H@���@�5?@�M�@��T@�x�@�`B@���@��7@�hs@��7@�&�@��@���@�bN@��@��;@�S�@��\@�V@��!@���@~W�@n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A�A�A�A�ƨA�ĜA�ȴA�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���AϼjAϲ-Aϩ�Aϡ�Aϙ�Aϕ�AϑhA�z�A�hsA�dZA�bNA�dZA�\)A�G�A�33A�+A��A�
=A��A���AΗ�A�VA�;dA�1'A�$�A��yA���Aͣ�A�VA�z�Aɴ9A���AƮAĸRA�{A�bA��A��A�{A�1A���A�JA�ffA�JA�5?A�/A�ƨA��A��FA�A�A���A���A�M�A��A�VA��-A�K�A��A�VA�I�A��A�G�A�r�A�"�A�oA�$�A�A�A��jA�(�A�7LA��A��DA�~�A�&�A���A�v�A��#A��A���A�5?A��;A���A�XA�(�A�ƨA���A���A�M�A���A�r�A��A���A�33A��A���A��FA�JA�VA���A�S�A�33A��A�x�A~��A|�!A|1Az��Ax��Aw;dAu`BArn�Aq33Ao�Al�9Aj�DAgC�Ae��Ad��AcdZAa�
A`�DA_?}A^$�A]��A[�
AZv�AY;dAX�jAXI�AWK�AUx�AS�
AS&�AR��AQl�AO�TAO�AMS�AKoAH��AF�AC��AA�TAA�A@ĜA@jA?�PA?/A>�DA=�A;/A8�+A7�PA5�A5C�A4�RA3�mA2��A0��A.��A.VA.-A,�A+��A*��A*A�A(~�A%|�A#K�A"r�A!��A I�A?}A�A��A`BA�RAx�A�Al�A9XA�#A?}A��AG�A~�A"�A&�A^5A\)AO�AG�A&�AffAhsA&�A
��A
1A	��A	K�A	�A��AƨAt�A
=AA�A��A�DA^5AJA�RA(�A��A�-A�RA��A~�A��A?}@��w@�G�@��@���@��`@�@�x�@�u@�(�@�@��#@��@�"�@�J@�9@�1'@��#@���@�&�@�Z@��H@��@�?}@�bN@҇+@�X@���@�Q�@�ƨ@�\)@��H@��`@ʟ�@��@���@�1'@�
=@őh@��@�ff@��@��@���@���@�J@��u@���@��
@�(�@�b@���@��@�X@�r�@��F@�33@�v�@�@���@�ƨ@�"�@�E�@�/@��@�bN@�  @�l�@�K�@�
=@���@��@�A�@�S�@��H@���@�5?@�M�@��T@�x�@�`B@���@��7@�hs@��7@�&�@��@���@�bN@��@��;@�S�@��\@�V@��!@���@~W�@n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺB��B��B��B��B��B��BɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�)B�/B�5B�;B�;B�BB�ZB�mB�yB�B�B�B�B��B��BBBB+B	7B
=BPB�B8RBH�BT�Bk�Br�Bv�B�B��B��B��B��B��B�BjBv�B�1B�JB�bB�uB�{B�uB�\B�oB�PB�7B�=B��B��B��B��B�oB�7B�Bq�BN�B/B�B�B
=BB��B�B�yB�TB��BŢB��B�oB|�B]/BXBT�BJ�B@�B=qB<jB49B+B"�B�B�BbB
��B
�yB
�)B
��B
��B
�'B
��B
��B
��B
��B
��B
�%B
t�B
n�B
dZB
R�B
G�B
;dB
&�B
�B
oB	��B	�B	�B	��B	ĜB	�dB	�-B	��B	��B	��B	��B	�PB	�B	~�B	{�B	x�B	q�B	ffB	cTB	bNB	`BB	[#B	Q�B	L�B	D�B	9XB	-B	�B	�B	JB		7B	+B	B	B��B��B��B�B�BB�)B�
B��B��B��BȴBÖB��B��B�}B�jB�FB�-B�B��B��B��B��B��B�oB�JB�1B�B�B� B|�By�Bw�Bt�Br�Bp�Bm�Bk�BiyBe`B`BB^5B\)B[#B[#BZBXBVBT�BS�BR�BR�BR�BP�BP�BQ�BR�BR�BR�BR�BR�BT�BW
BXBYBYBZB\)BZB[#BiyBw�Bp�Bp�Bo�Bm�BbNBaHBR�BT�BXBXBVBW
BXBYBW
BVBS�BQ�BN�BQ�BXBT�BT�BW
BYB\)B]/B]/B^5B]/B[#BYBXB^5B_;B`BB`BB`BBaHBbNBcTBjBp�Bm�Bn�Bl�Bk�Bl�Bp�Bs�Bs�Bq�Bp�Bp�Bp�Bo�Bo�Bo�Bo�Bq�Bq�Br�Bt�Bu�Bv�Bv�Bw�By�By�Bz�Bz�B� B�7B�PB�VB�hB��B��B��B��B�B�-B�FB�RB�dB�qB�}B��B��BÖBƨBȴBɺB��B�#B	��B
	�B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺB��B��B��B��B��B��BɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�)B�/B�5B�;B�;B�BB�ZB�mB�yB�B�B�B�B��B��BBBB+B	7B
=BPB�B8RBH�BT�Bk�Br�Bv�B�B��B��B��B��B��B�BjBv�B�1B�JB�bB�uB�{B�uB�\B�oB�PB�7B�=B��B��B��B��B�oB�7B�Bq�BN�B/B�B�B
=BB��B�B�yB�TB��BŢB��B�oB|�B]/BXBT�BJ�B@�B=qB<jB49B+B"�B�B�BbB
��B
�yB
�)B
��B
��B
�'B
��B
��B
��B
��B
��B
�%B
t�B
n�B
dZB
R�B
G�B
;dB
&�B
�B
oB	��B	�B	�B	��B	ĜB	�dB	�-B	��B	��B	��B	��B	�PB	�B	~�B	{�B	x�B	q�B	ffB	cTB	bNB	`BB	[#B	Q�B	L�B	D�B	9XB	-B	�B	�B	JB		7B	+B	B	B��B��B��B�B�BB�)B�
B��B��B��BȴBÖB��B��B�}B�jB�FB�-B�B��B��B��B��B��B�oB�JB�1B�B�B� B|�By�Bw�Bt�Br�Bp�Bm�Bk�BiyBe`B`BB^5B\)B[#B[#BZBXBVBT�BS�BR�BR�BR�BP�BP�BQ�BR�BR�BR�BR�BR�BT�BW
BXBYBYBZB\)BZB[#BiyBw�Bp�Bp�Bo�Bm�BbNBaHBR�BT�BXBXBVBW
BXBYBW
BVBS�BQ�BN�BQ�BXBT�BT�BW
BYB\)B]/B]/B^5B]/B[#BYBXB^5B_;B`BB`BB`BBaHBbNBcTBjBp�Bm�Bn�Bl�Bk�Bl�Bp�Bs�Bs�Bq�Bp�Bp�Bp�Bo�Bo�Bo�Bo�Bq�Bq�Br�Bt�Bu�Bv�Bv�Bw�By�By�Bz�Bz�B� B�7B�PB�VB�hB��B��B��B��B�B�-B�FB�RB�dB�qB�}B��B��BÖBƨBȴBɺB��B�#B	��B
	�B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191730                              AO  ARCAADJP                                                                    20181005191730    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191730  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005191730  QCF$                G�O�G�O�G�O�8000            