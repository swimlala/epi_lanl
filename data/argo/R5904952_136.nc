CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  8   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:35Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  >P   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ?�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  Dh   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  E�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  J�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  O`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  P�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  Ux   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  V�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  [�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  `p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  a�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  f�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  l�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    l�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    o�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    r�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  u�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    u�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    v    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    v   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    v   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  v   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    vL   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    v\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    v`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         vp   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         vt   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        vx   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    v|Argo profile    3.1 1.2 19500101000000  20181005190535  20181005190535  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��ip�w/1   @��j  �@0���`A��c�O�;dZ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @&ff@�  @�  A   AffA@  A`  A~ffA�33A���A�  A�  A�  A�  A�  B   B  B  BffB ffB(  B/��B7��B@  BH  BPffBX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D y�D  D� D  D� D  D� D��D� DfD�fDfD�fD  D�fD  Dy�D	  D	� D
  D
y�D  D� D  D� D  D� D  D�fD  D� DfD� D��D� D  D�fDfD� D  D� D  D� D��Dy�D��Dy�D  D� DfD�fDfD� Dy��D�(RD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@9��@���@ə�A��A#33AD��Ad��A���A���A�33A�ffA�ffA�ffA�ffA�ffB33B	33B33B��B!��B)33B0��B8��BA33BI33BQ��BY33Ba33Bi��Bq��By33B���B���B���B���B���B���B���B���B���B�fgB���B���B���B���B�fgB�fgB���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CfgC
L�CL�CL�CL�CL�CL�CL�CL�CL�C33CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C633C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CN33CP33CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClfgCnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC��C�&fC�33C�33C�&fC�&fC��C��C�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�33C�&fC�&fC�&fC�&fC��C�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC��C��C��C��C�&fC�&fC��C�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fD 3D ��D3D�3D3D�3D3D�3D�D�3D�D��D�D��D3D��D3D��D	3D	�3D
3D
��D3D�3D3D�3D3D�3D3D��D3D�3D�D�3D�D�3D3D��D�D�3D3D�3D3D�3D�D��D�D��D3D�3D�D��D�D�3Dy��D�1�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aҝ�Aҥ�Aҧ�AҬAҮAҰ!AҰ!AҰ!AҰ!AҰ!AҰ!AҮAҮAҰ!AҲ-AҰ!AҮAҰ!AҰ!AҲ-AҰ!AҴ9AҶFAҾwA���AҾwAҾwA�ȴA�ƨA�ĜA�ƨA�ƨA���A���A���A��mA��A���A�
=A�
=A�A�A��`A�A�A�S�A�"�A��A���A�p�A���A�C�A�+A�9XA��FA��A��A�dZA�VA��!A��A�  A��!A�(�A��A��+A�A�t�A���A���A��#A���A�+A��jA�ȴA��A��A�|�A���A�hsA�JA���A��uA�S�A�XA���A�ȴA��!A�33A�-A��-A�`BA��A�S�A�1'A�9XA�
=A�n�A��^A�C�A��hA�VA�bNA�dZA��hA��-A�;dA��A���A��A��-A�A�I�A�1A���A~�AwXAo7LAk��Ah�+Ac�PAb�DAahsA`E�A]�TA[;dAY`BAV�!AU\)AS��ARȴAQ��AO�PAKG�AI��AHffAF�+AB��AA�PAA&�A@r�A=�-A7��A6Q�A4��A4M�A3`BA1�FA1K�A/��A/"�A-XA,�jA,{A*��A*jA)A(�RA(  A&��A&z�A%��A#��A"��A!hsA 9XA/A9XAG�AM�A7LA�AA�A�
A�A�+A��A��Ax�Ar�A�wAv�Al�A^5A�A+A~�A��Ap�A��A-A\)A	�mA	VA^5A�TAhsAȴA�#AhsAp�A�A�/Ar�A$�A�`AA�A�PA33A �`A M�@��@�j@��@��;@�|�@�@��@��@�\)@�@�1'@�P@�!@���@�Z@�R@�Ĝ@�u@��@�@�|�@�+@߶F@ݲ-@��/@�t�@�O�@؃@؛�@�V@١�@ف@؋D@��@�V@��#@�?}@ԛ�@��
@�ff@�hs@�/@���@�r�@���@�
=@��@�&�@�;d@�{@�O�@ȴ9@ȓu@�bN@�b@ǅ@�C�@�K�@�+@��#@őh@�j@�A�@�1'@�A�@���@å�@�dZ@���@�@+@+@\@�~�@�5?@���@�&�@�r�@���@�"�@�ȴ@��+@�=q@���@�x�@�p�@��@�x�@���@��D@��D@��@��D@�j@���@�\)@�"�@���@��@���@x@g1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aҝ�Aҥ�Aҧ�AҬAҮAҰ!AҰ!AҰ!AҰ!AҰ!AҰ!AҮAҮAҰ!AҲ-AҰ!AҮAҰ!AҰ!AҲ-AҰ!AҴ9AҶFAҾwA���AҾwAҾwA�ȴA�ƨA�ĜA�ƨA�ƨA���A���A���A��mA��A���A�
=A�
=A�A�A��`A�A�A�S�A�"�A��A���A�p�A���A�C�A�+A�9XA��FA��A��A�dZA�VA��!A��A�  A��!A�(�A��A��+A�A�t�A���A���A��#A���A�+A��jA�ȴA��A��A�|�A���A�hsA�JA���A��uA�S�A�XA���A�ȴA��!A�33A�-A��-A�`BA��A�S�A�1'A�9XA�
=A�n�A��^A�C�A��hA�VA�bNA�dZA��hA��-A�;dA��A���A��A��-A�A�I�A�1A���A~�AwXAo7LAk��Ah�+Ac�PAb�DAahsA`E�A]�TA[;dAY`BAV�!AU\)AS��ARȴAQ��AO�PAKG�AI��AHffAF�+AB��AA�PAA&�A@r�A=�-A7��A6Q�A4��A4M�A3`BA1�FA1K�A/��A/"�A-XA,�jA,{A*��A*jA)A(�RA(  A&��A&z�A%��A#��A"��A!hsA 9XA/A9XAG�AM�A7LA�AA�A�
A�A�+A��A��Ax�Ar�A�wAv�Al�A^5A�A+A~�A��Ap�A��A-A\)A	�mA	VA^5A�TAhsAȴA�#AhsAp�A�A�/Ar�A$�A�`AA�A�PA33A �`A M�@��@�j@��@��;@�|�@�@��@��@�\)@�@�1'@�P@�!@���@�Z@�R@�Ĝ@�u@��@�@�|�@�+@߶F@ݲ-@��/@�t�@�O�@؃@؛�@�V@١�@ف@؋D@��@�V@��#@�?}@ԛ�@��
@�ff@�hs@�/@���@�r�@���@�
=@��@�&�@�;d@�{@�O�@ȴ9@ȓu@�bN@�b@ǅ@�C�@�K�@�+@��#@őh@�j@�A�@�1'@�A�@���@å�@�dZ@���@�@+@+@\@�~�@�5?@���@�&�@�r�@���@�"�@�ȴ@��+@�=q@���@�x�@�p�@��@�x�@���@��D@��D@��@��D@�j@���@�\)@�"�@���@��@���@x@g1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��BB
=BhB�B�B�B\BDB%B
�B
B
�^B
�dB
ǮB
��B
�)B
�;B
�mB
�BVB�RB�TB�B	7BbB�B�B �B0!B9XB>wB@�B6FB/B"�B!�B49BT�BhsBhsBB�B,B1'BH�BJ�BF�B1'BoB1B  B��B��B��B�RB��B�JB}�Bq�BdZBP�BH�BA�B5?B�B
�B
�}B
��B
�+B
[#B
F�B
2-B
hB
B	��B	�B	�`B	�#B	�B	v�B	C�B	:^B	-B	bB	1B��B��B�yB�B��B��B�dB�9B�!B�B��B��B�hB�PB�=B�7B�+B�%B�B�B�VB�hB��B��B��B��B��B��B��B�9B�FB�^B��BĜBɺB��B��B�B�B�B�)B�#B�
B�
B��B��B��B��B��B��B��B��B��B��B��BȴBƨBÖB�}B�dB�LB�9B�'B�!B�B��B�B��B��B��B��B�'B�XB�}B��B�}B��BǮBȴB�B�)B�#B�B��B��B�#B�)B�B�
B�BB�`B�B��B	%B	B	  B��B��B��B�B�B�yB�NB�5B�B�B�B�
B��B��B��B�B�B�B�B�
B�B�HB�mB�B��B	B	B	B	B	B	B		7B	VB	�B	�B	{B	uB	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	'�B	+B	,B	,B	.B	.B	-B	49B	33B	33B	33B	8RB	=qB	?}B	D�B	F�B	F�B	F�B	F�B	F�B	G�B	H�B	J�B	J�B	N�B	S�B	W
B	YB	[#B	]/B	]/B	]/B	^5B	^5B	bNB	gmB	jB	m�B	p�B	q�B	s�B	v�B	w�B	x�B	}�B	� B
sB
%`B
0�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��BB
=BhB�B�B�B\BDB%B
�B
B
�^B
�dB
ǮB
��B
�)B
�;B
�mB
�BVB�RB�TB�B	7BbB�B�B �B0!B9XB>wB@�B6FB/B"�B!�B49BT�BhsBhsBB�B,B1'BH�BJ�BF�B1'BoB1B  B��B��B��B�RB��B�JB}�Bq�BdZBP�BH�BA�B5?B�B
�B
�}B
��B
�+B
[#B
F�B
2-B
hB
B	��B	�B	�`B	�#B	�B	v�B	C�B	:^B	-B	bB	1B��B��B�yB�B��B��B�dB�9B�!B�B��B��B�hB�PB�=B�7B�+B�%B�B�B�VB�hB��B��B��B��B��B��B��B�9B�FB�^B��BĜBɺB��B��B�B�B�B�)B�#B�
B�
B��B��B��B��B��B��B��B��B��B��B��BȴBƨBÖB�}B�dB�LB�9B�'B�!B�B��B�B��B��B��B��B�'B�XB�}B��B�}B��BǮBȴB�B�)B�#B�B��B��B�#B�)B�B�
B�BB�`B�B��B	%B	B	  B��B��B��B�B�B�yB�NB�5B�B�B�B�
B��B��B��B�B�B�B�B�
B�B�HB�mB�B��B	B	B	B	B	B	B		7B	VB	�B	�B	{B	uB	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	'�B	+B	,B	,B	.B	.B	-B	49B	33B	33B	33B	8RB	=qB	?}B	D�B	F�B	F�B	F�B	F�B	F�B	G�B	H�B	J�B	J�B	N�B	S�B	W
B	YB	[#B	]/B	]/B	]/B	^5B	^5B	bNB	gmB	jB	m�B	p�B	q�B	s�B	v�B	w�B	x�B	}�B	� B
sB
%`B
0�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190535                              AO  ARCAADJP                                                                    20181005190535    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190535  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190535  QCF$                G�O�G�O�G�O�8000            