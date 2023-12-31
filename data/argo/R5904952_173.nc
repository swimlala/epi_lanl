CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:44Z creation      
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
resolution        =���   axis      Z        H  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ?�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  AL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  G�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  I(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  Op   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  U�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  WL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  _(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ep   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  k�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  mL   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  s�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  u(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  {p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    {�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ~�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20181005190544  20181005190544  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�����1   @�䪙���@1B��`A��c����o1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�33A��A!��AA��A`  A�  A�  A���A���A�  A���A�  A�  B ffB  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH�CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp  Cr  Ct  Cu�fCw�fCz  C|  C~�C��C��C��C��C��C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C��C��C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D�fDfD� D��D�fD  Dy�D��D� DfD�fD   D � D!  D!� D"fD"�fD#fD#�fD$  D$� D$��D%� D%��D&� D3  D3� D4  D4� D5  D5y�D5��D6� D7  D7�fD8fD8� D8��D9y�D9��D:� D;  D;� D;��D<y�D<��D=�fD>  D>� D?  D?� D@fD@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH� DU  DU�fDV  DVy�DV��DW� DXfDX� DY  DY� DY��DZy�D[  D[� D[��D\� D]fD]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dl��Dmy�Dn  Dn� Do  Do� Do��Dpy�Dp��Dq� Dr  Dr� Dr��Ds� Dt  Dt� DufDu� Dv  Dv� Dw  Dw� Dw�3Dy��D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@˅AA%AEAd(�A�{A�{A��HA��HA�{A��HA�{A�{Bp�B	
=B��B��B��B��B�Q�B��B��B��BąBȅB̸RBЅBԅB؅B܅B�Q�B�B�B�B��B�B��B��C B�C(�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CF\)CH\)CJB�CLB�CN\)CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^(�C`B�CbB�CdB�CfB�Ch\)Cj\)ClB�CnB�CpB�CrB�CtB�Cv(�Cx(�CzB�C|B�C~\)C�.C�.C�.C�.C�.C�{C�{C�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�{C�!HC�!HC�.C�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�.C�.C�.C�.C�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�.C�.C�.C�!HC�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�.C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�!HC�{C�!HC�!HC�{C�{C�!HC�!HC�!HC�!HC�!HD 
=D ��D�D��D�D�
D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D
D��D�D�
D
D��D
>D�
D�D�>D
>D��D
D�
D �D ��D!�D!��D"
D"�
D#
D#�
D$�D$��D%
>D%��D&
>D&��D3�D3��D4�D4��D5�D5�>D6
>D6��D7�D7�
D8
D8��D9
>D9�>D:
>D:��D;�D;��D<
>D<�>D=
>D=�
D>�D>��D?�D?��D@
D@�
DA�DA��DB�DB��DC�DC��DD�DD��DE�DE�>DF�DF��DG�DG��DH�DH��DU�DU�
DV�DV�>DW
>DW��DX
DX��DY�DY��DZ
>DZ�>D[�D[��D\
>D\��D]
D]��D^�D^�
D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De
>De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl�>Dm
>Dm�>Dn�Dn��Do�Do��Dp
>Dp�>Dq
>Dq��Dr�Dr��Ds
>Ds��Dt�Dt��Du
Du��Dv�Dv��Dw�Dw��Dw��Dy��D�&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ĜA���A�A�ĜA�ƨA�ƨA�ƨAͺ^A;wA;wA�ȴA���A���A���A���A���A�ZA��HA�bNAɶFA�x�A�?}A�ƨA���A�7LA�bNA���A�JA�O�A�{A�p�A���A���A���A�XA���A�7LA�ffA���A��A��RA��PA��A���A��RA���A��A�dZA�A���A�I�A���A��A�bNA��!A���A�{A���A��-A�&�A���A��;A�+A���A�{A�A�C�A�t�A��`A�/A��FA��uA�|�A���A��`A�1'A�G�A�JA�A�bA���A��hA�&�A�^5A�x�A��hA�;dA}x�Az~�Aw��Ar�9Ak�Af1Ac"�Ab��A_�^A\jAY�ARĜAO�^AM"�AJȴAH��AF�/AB�yAAVA>��A>�/A;VA9�
A9+A8Q�A7/A6Q�A5�A4A3�7A3`BA3?}A2-A/�mA-&�A,z�A+�A*�DA*  A)K�A'�FA%�#A%�A$�9A$1'A"��A!�^AA�mAM�A�AjA�;A�An�AO�A1AO�A~�A��A7LA�
A
�A	��A�A=qA�^AI�A"�A~�A-A�A��A�AS�AG�AoA��A�/A�RA�Ao@���@��j@�9X@�;d@�
=@�O�@�t�@��h@�b@�l�@�!@�@�t�@��@@�{@��@�|�@���@�-@�^@��@�/@�Z@�o@�$�@�O�@� �@㝲@��@��@��`@߶F@��@�`B@ܣ�@��@�S�@�@�^5@٩�@��@ج@�r�@�9X@���@�;d@�{@ԃ@ӥ�@��y@���@ҏ\@�hs@�`B@�I�@ϥ�@�o@͙�@̬@�A�@��@�  @��@�C�@ʗ�@��@ɡ�@�hs@�I�@�b@��
@ǝ�@�|�@�S�@�"�@�ȴ@Ɵ�@�~�@�V@�=q@�$�@��@�{@���@��@ă@ēu@�l�@���@�~�@�=q@�&�@�Z@�b@�O�@�z�@�I�@�1'@���@�
=@�=q@�{@�{@��@�X@�?}@���@�b@��m@���@�$�@���@�`B@�&�@��@�
=@��H@��T@�V@�V@�Ĝ@��m@���@��@�33@�"�@��@�M�@��T@��T@���@�@��@�&�@��`@�z�@��m@��w@���@�dZ@�K�@��y@��y@��@��@���@�~�@�V@�5?@���@�X@��9@�z�@�9X@��
@�ƨ@�ƨ@���@�C�@�o@���@�^5@���@�X@���@�Ĝ@��u@�z�@�1'@�b@�  @��;@��w@�t�@�S�@�o@��\@�=q@�J@��@�@���@��@�?}@��@�%@��9@�j@�1@�t�@�
=@��@��H@���@��!@�~�@�M�@�{@���@���@�hs@�?}@��@���@��/@��@�1'@�  @��@�l�@�;d@�+@�@��y@��R@�E�@�{@��@�x�@�G�@�Ĝ@�Z@��@��@��@���@��@�dZ@�K�@��@�8�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ĜA���A�A�ĜA�ƨA�ƨA�ƨAͺ^A;wA;wA�ȴA���A���A���A���A���A�ZA��HA�bNAɶFA�x�A�?}A�ƨA���A�7LA�bNA���A�JA�O�A�{A�p�A���A���A���A�XA���A�7LA�ffA���A��A��RA��PA��A���A��RA���A��A�dZA�A���A�I�A���A��A�bNA��!A���A�{A���A��-A�&�A���A��;A�+A���A�{A�A�C�A�t�A��`A�/A��FA��uA�|�A���A��`A�1'A�G�A�JA�A�bA���A��hA�&�A�^5A�x�A��hA�;dA}x�Az~�Aw��Ar�9Ak�Af1Ac"�Ab��A_�^A\jAY�ARĜAO�^AM"�AJȴAH��AF�/AB�yAAVA>��A>�/A;VA9�
A9+A8Q�A7/A6Q�A5�A4A3�7A3`BA3?}A2-A/�mA-&�A,z�A+�A*�DA*  A)K�A'�FA%�#A%�A$�9A$1'A"��A!�^AA�mAM�A�AjA�;A�An�AO�A1AO�A~�A��A7LA�
A
�A	��A�A=qA�^AI�A"�A~�A-A�A��A�AS�AG�AoA��A�/A�RA�Ao@���@��j@�9X@�;d@�
=@�O�@�t�@��h@�b@�l�@�!@�@�t�@��@@�{@��@�|�@���@�-@�^@��@�/@�Z@�o@�$�@�O�@� �@㝲@��@��@��`@߶F@��@�`B@ܣ�@��@�S�@�@�^5@٩�@��@ج@�r�@�9X@���@�;d@�{@ԃ@ӥ�@��y@���@ҏ\@�hs@�`B@�I�@ϥ�@�o@͙�@̬@�A�@��@�  @��@�C�@ʗ�@��@ɡ�@�hs@�I�@�b@��
@ǝ�@�|�@�S�@�"�@�ȴ@Ɵ�@�~�@�V@�=q@�$�@��@�{@���@��@ă@ēu@�l�@���@�~�@�=q@�&�@�Z@�b@�O�@�z�@�I�@�1'@���@�
=@�=q@�{@�{@��@�X@�?}@���@�b@��m@���@�$�@���@�`B@�&�@��@�
=@��H@��T@�V@�V@�Ĝ@��m@���@��@�33@�"�@��@�M�@��T@��T@���@�@��@�&�@��`@�z�@��m@��w@���@�dZ@�K�@��y@��y@��@��@���@�~�@�V@�5?@���@�X@��9@�z�@�9X@��
@�ƨ@�ƨ@���@�C�@�o@���@�^5@���@�X@���@�Ĝ@��u@�z�@�1'@�b@�  @��;@��w@�t�@�S�@�o@��\@�=q@�J@��@�@���@��@�?}@��@�%@��9@�j@�1@�t�@�
=@��@��H@���@��!@�~�@�M�@�{@���@���@�hs@�?}@��@���@��/@��@�1'@�  @��@�l�@�;d@�+@�@��y@��R@�E�@�{@��@�x�@�G�@�Ĝ@�Z@��@��@��@���@��@�dZ@�K�@��@�8�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�?B�?B�?B�?B�?B�?B�?B�FB�FB�FB�LB�LB�LB�FB�LB�FB
e`B
}�B
�RB
�B
��B{B�BJB1B0!B`BB}�B��B�LB�wBɺB�#B�mB�B��B+BuB�B&�B.B0!B5?B6FB<jB=qBC�BG�BF�BD�BA�B5?B>wB<jB8RB.B#�B�BVB	7B+BB�B�B��B�}B��B�VB�BffBN�B?}B=qB7LB$�B
=B
�B
��B
�jB
��B
}�B
[#B
D�B
&�B
  B	�B	�)B	B	�B	��B	x�B	H�B	)�B	�B	uB	\B	B�B��B�dBƨBǮBĜB�jB�B��B��B�?B�RB�LB�FB�?B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�'B�'B�'B�B�B�3B�?B�?B�?B�?B�FB�RB�^B�dB�dB�dB�dB�^B�dB�dB�qB�qB�}BƨB��BȴB��B��B��B��B��B��B��B��B�B�B�#B�)B�/B�5B�/B�/B�/B�;B�ZB�`B�ZB�`B�`B�sB�yB�B�B�B�B��B��B��B��B	  B	B	B	B	%B		7B	bB	uB	uB	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	'�B	'�B	'�B	'�B	'�B	'�B	+B	/B	2-B	5?B	;dB	=qB	=qB	>wB	>wB	>wB	?}B	B�B	D�B	F�B	G�B	H�B	I�B	I�B	K�B	L�B	O�B	Q�B	XB	ZB	^5B	`BB	aHB	cTB	hsB	jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	ƨB	ŢB	ĜB	ƨB	ǮB	ƨB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�HB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
1B
1B
1B
	7B
	7B

=B
DB
JB
JB
PB
PB
PB
PB
PB
oB
�B
$�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�?B�?B�?B�?B�?B�?B�?B�FB�FB�FB�LB�LB�LB�FB�LB�FB
e`B
}�B
�RB
�B
��B{B�BJB1B0!B`BB}�B��B�LB�wBɺB�#B�mB�B��B+BuB�B&�B.B0!B5?B6FB<jB=qBC�BG�BF�BD�BA�B5?B>wB<jB8RB.B#�B�BVB	7B+BB�B�B��B�}B��B�VB�BffBN�B?}B=qB7LB$�B
=B
�B
��B
�jB
��B
}�B
[#B
D�B
&�B
  B	�B	�)B	B	�B	��B	x�B	H�B	)�B	�B	uB	\B	B�B��B�dBƨBǮBĜB�jB�B��B��B�?B�RB�LB�FB�?B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�'B�'B�'B�B�B�3B�?B�?B�?B�?B�FB�RB�^B�dB�dB�dB�dB�^B�dB�dB�qB�qB�}BƨB��BȴB��B��B��B��B��B��B��B��B�B�B�#B�)B�/B�5B�/B�/B�/B�;B�ZB�`B�ZB�`B�`B�sB�yB�B�B�B�B��B��B��B��B	  B	B	B	B	%B		7B	bB	uB	uB	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	'�B	'�B	'�B	'�B	'�B	'�B	+B	/B	2-B	5?B	;dB	=qB	=qB	>wB	>wB	>wB	?}B	B�B	D�B	F�B	G�B	H�B	I�B	I�B	K�B	L�B	O�B	Q�B	XB	ZB	^5B	`BB	aHB	cTB	hsB	jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	ƨB	ŢB	ĜB	ƨB	ǮB	ƨB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�HB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
1B
1B
1B
	7B
	7B

=B
DB
JB
JB
PB
PB
PB
PB
PB
oB
�B
$�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190544                              AO  ARCAADJP                                                                    20181005190544    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190544  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190544  QCF$                G�O�G�O�G�O�8000            