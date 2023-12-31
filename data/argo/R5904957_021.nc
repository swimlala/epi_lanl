CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  V   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:07Z creation      
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
_FillValue                    {tArgo profile    3.1 1.2 19500101000000  20181024140807  20181024140807  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׮����U1   @׮�^Ъ^@3xbM���c�z�G�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�33B�  B�  C   C  C  C  C  C
  C  C�fC  C�fC  C  C  C�C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C[�fC]�fC`  Cb  Cd  Cf  Ch  Ci�fCk�fCn  Cp  Cr  Ct  Cv�Cx  Cy�fC|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D��D� DfD�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� DfD� D  Dy�D   D � D!  D!� D"  D"y�D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� Dy��D�5q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B BBBB B(B0B8B@BHBPBXB`BhBpBxB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�.B�aHB�aHB�{B��{B�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C
C0�C
C0�C0�C0�CJ>C0�C0�C 0�C"0�C$0�C&J>C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV
CX0�CZ0�C\
C^
C`0�Cb0�Cd0�Cf0�Ch0�Cj
Cl
Cn0�Cp0�Cr0�Ct0�CvJ>Cx0�Cz
C|0�C~0�C�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC��C�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�%C�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC��C��C�RC�RC�RC�RC�RC�%C�RC�RC�RC�%C�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�%C�RC�RC�RC�RC�RC�RC�RC�RC�RC��C��C�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�%C�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D�D�)D�D��D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D�D��D)D�)D)D�)D)D�)D)D�)D)D�)D)D��D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D�D�)D)D��D )D �)D!)D!�)D")D"��D#)D#�)D$�D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)Dy�D�;�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AёhAёhAя\AэPAщ7AсA�~�AуA�ZA�;dA��A��A�{A�JA���A�
=A�9XAΡ�A���A��TA��A��A���A��yA���A�jA�ZA�K�A�I�A�E�A�9XA��A��A���A���A��
A�A̴9A�z�A�E�A��A�bA��A˺^AˋDA�bNA�5?A���A��
Aʺ^A�bNAɣ�A���A��HAƲ-A�A�A��A���A��#AĴ9AăA�^5A�/A��A���A�ƨA�~�A�hsA�XA� �A�G�A�9XA�jA�Q�A��wA�\)A���A�+A��A���A�hsA�K�A�%A��+A�5?A�M�A��;A�v�A� �A�$�A���A���A�z�A�z�A�ZA���A��/A�dZA�{A�-A��A�1A�&�A���A��/A��
A��^A�"�A�XA�O�A��9A��jA���A���A�\)A�~�A���A�ȴA��A�ƨA���A��yA���A���A+A}��A|JAz�DAyƨAu��An�HAhbAe�A^��AZr�AU�wAT��AT�+ATn�AQ��AN�AK�hAJbAHM�AG"�AFM�AE�FAAƨA<r�A8ĜA7;dA5��A4  A1�PA1VA0��A.5?A,�9A*=qA({A&$�A%��A%�A#�A"�A!�-A �\A�Ar�A$�Ax�A&�AJAv�A�AG�A�uA�PA��A`BA^5AO�AA7LAbNA�FAr�A�uA�A
E�A��A��AbNAbA\)A�TA
=A(�A�A"�A�RAAXA ��A n�@��#@��w@�
=@��@�=q@�X@���@� �@�1@�\)@��@�J@�V@�I�@��@���@���@�x�@��/@��@�dZ@�
=@��@�R@�+@��@��@��^@�r�@���@�&�@��@��;@�+@�^5@���@�|�@�~�@��@���@�ff@�/@��;@�@�X@���@֧�@Ձ@��@�I�@�;d@ҟ�@Ѻ^@�7L@���@�(�@�|�@�33@�v�@��#@�X@���@�j@ˍP@�
=@�=q@�hs@ȣ�@ǥ�@Ƈ+@�{@ŉ7@�Ĝ@���@+@�$�@���@��@���@�@���@�{@�@�{@��@��@��@�Q�@��@��y@�`B@���@�Ĝ@�Q�@��m@��@�ƨ@��P@�@��H@�ȴ@�@�hs@��@��D@�9X@�b@��P@���@�ff@�$�@�{@��T@��-@�X@��@�Ĝ@�bN@�b@���@�33@��!@��\@���@�7L@��`@���@��D@�A�@�b@�1@��@���@�o@�@�RT@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AёhAёhAя\AэPAщ7AсA�~�AуA�ZA�;dA��A��A�{A�JA���A�
=A�9XAΡ�A���A��TA��A��A���A��yA���A�jA�ZA�K�A�I�A�E�A�9XA��A��A���A���A��
A�A̴9A�z�A�E�A��A�bA��A˺^AˋDA�bNA�5?A���A��
Aʺ^A�bNAɣ�A���A��HAƲ-A�A�A��A���A��#AĴ9AăA�^5A�/A��A���A�ƨA�~�A�hsA�XA� �A�G�A�9XA�jA�Q�A��wA�\)A���A�+A��A���A�hsA�K�A�%A��+A�5?A�M�A��;A�v�A� �A�$�A���A���A�z�A�z�A�ZA���A��/A�dZA�{A�-A��A�1A�&�A���A��/A��
A��^A�"�A�XA�O�A��9A��jA���A���A�\)A�~�A���A�ȴA��A�ƨA���A��yA���A���A+A}��A|JAz�DAyƨAu��An�HAhbAe�A^��AZr�AU�wAT��AT�+ATn�AQ��AN�AK�hAJbAHM�AG"�AFM�AE�FAAƨA<r�A8ĜA7;dA5��A4  A1�PA1VA0��A.5?A,�9A*=qA({A&$�A%��A%�A#�A"�A!�-A �\A�Ar�A$�Ax�A&�AJAv�A�AG�A�uA�PA��A`BA^5AO�AA7LAbNA�FAr�A�uA�A
E�A��A��AbNAbA\)A�TA
=A(�A�A"�A�RAAXA ��A n�@��#@��w@�
=@��@�=q@�X@���@� �@�1@�\)@��@�J@�V@�I�@��@���@���@�x�@��/@��@�dZ@�
=@��@�R@�+@��@��@��^@�r�@���@�&�@��@��;@�+@�^5@���@�|�@�~�@��@���@�ff@�/@��;@�@�X@���@֧�@Ձ@��@�I�@�;d@ҟ�@Ѻ^@�7L@���@�(�@�|�@�33@�v�@��#@�X@���@�j@ˍP@�
=@�=q@�hs@ȣ�@ǥ�@Ƈ+@�{@ŉ7@�Ĝ@���@+@�$�@���@��@���@�@���@�{@�@�{@��@��@��@�Q�@��@��y@�`B@���@�Ĝ@�Q�@��m@��@�ƨ@��P@�@��H@�ȴ@�@�hs@��@��D@�9X@�b@��P@���@�ff@�$�@�{@��T@��-@�X@��@�Ĝ@�bN@�b@���@�33@��!@��\@���@�7L@��`@���@��D@�A�@�b@�1@��@���@�o@�@�RT@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
?}B
@�B
@�B
@�B
@�B
?}B
?}B
?}B
;dB
9XB
8RB
7LB
5?B
6FB
E�B
v�B
�B
�B
�bB
��B
�wB
��B
�B
��BDB	7B+B%BBBBVB�B)�B,B1'B49B;dBC�BN�B[#B]/BbNBjBp�Bw�By�B}�B|�B�B� B�+B�=B��B��B�BȴB��B��B��B�B�5B�`B�B�B{B�B�B�B�B2-B33BM�Bq�Bs�B�B�B}�B��B��B��B�B�?B�XB��B��B��B��B��B��B��B�bB|�BjBaHBXBYBZBM�B@�B9XB�BuBB�#B�wB�B�uBx�BjB`BBM�BA�B?}B<jB1'B
��B
��B
��B
��B
� B
t�B
m�B
_;B
B�B
6FB
)�B
�B
�B	��B	ǮB	��B	�B	[#B	@�B	'�B	"�B	!�B	�B	�B	DB��B��B�B�B�fB�;B��BB�RB�9B�'B�B��B��B��B��B��B�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B�B�!B�'B�!B�!B�-B�3B�3B�-B�'B�B�B�B�B�!B�'B�!B�?B�?B�FB�LB�RB�XB�^B�qB�wB�}B�}B�wB�qB�wB��B��BÖBŢBǮB��B��B��B��B��B��B�B�
B�B�#B�/B�;B�BB�ZB�sB�B�B�B�B��B��B��B��B	B	B	1B	JB	PB	VB	VB	\B	uB	{B	{B	�B	�B	�B	�B	�B	 �B	"�B	#�B	#�B	#�B	$�B	%�B	'�B	(�B	+B	-B	.B	.B	0!B	5?B	6FB	8RB	9XB	;dB	;dB	=qB	>wB	A�B	D�B	F�B	H�B	L�B	P�B	R�B	VB	ZB	^5B	aHB	aHB	e`B	iyB	jB	n�B	q�B	t�B	v�B
�B
'�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
?}B
@�B
@�B
@�B
@�B
?}B
?}B
?}B
;dB
9XB
8RB
7LB
5?B
6FB
E�B
v�B
�B
�B
�bB
��B
�wB
��B
�B
��BDB	7B+B%BBBBVB�B)�B,B1'B49B;dBC�BN�B[#B]/BbNBjBp�Bw�By�B}�B|�B�B� B�+B�=B��B��B�BȴB��B��B��B�B�5B�`B�B�B{B�B�B�B�B2-B33BM�Bq�Bs�B�B�B}�B��B��B��B�B�?B�XB��B��B��B��B��B��B��B�bB|�BjBaHBXBYBZBM�B@�B9XB�BuBB�#B�wB�B�uBx�BjB`BBM�BA�B?}B<jB1'B
��B
��B
��B
��B
� B
t�B
m�B
_;B
B�B
6FB
)�B
�B
�B	��B	ǮB	��B	�B	[#B	@�B	'�B	"�B	!�B	�B	�B	DB��B��B�B�B�fB�;B��BB�RB�9B�'B�B��B��B��B��B��B�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B�B�!B�'B�!B�!B�-B�3B�3B�-B�'B�B�B�B�B�!B�'B�!B�?B�?B�FB�LB�RB�XB�^B�qB�wB�}B�}B�wB�qB�wB��B��BÖBŢBǮB��B��B��B��B��B��B�B�
B�B�#B�/B�;B�BB�ZB�sB�B�B�B�B��B��B��B��B	B	B	1B	JB	PB	VB	VB	\B	uB	{B	{B	�B	�B	�B	�B	�B	 �B	"�B	#�B	#�B	#�B	$�B	%�B	'�B	(�B	+B	-B	.B	.B	0!B	5?B	6FB	8RB	9XB	;dB	;dB	=qB	>wB	A�B	D�B	F�B	H�B	L�B	P�B	R�B	VB	ZB	^5B	aHB	aHB	e`B	iyB	jB	n�B	q�B	t�B	v�B
�B
'�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.19 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140807                              AO  ARCAADJP                                                                    20181024140807    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140807  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140807  QCF$                G�O�G�O�G�O�0               