CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:11Z AOML 3.0 creation; 2016-05-31T19:14:26Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230511  20160531121426  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               
A   AO  4051_7090_010                   2C  D   APEX                            5368                            041511                          846 @�K�ѿ�1   @�KL��@3]p��
=�d�hr�!1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    
A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDys3D��D�S3D�l�D�� D� D�L�D�|�D��fD�3D�<�D��fD��fD���D�33Dڜ�D�� D���D�<�D�vfD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9��C;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dt` Dyl�D�gD�P D�i�D���D��D�I�D�y�D��3D� D�9�D��3D��3D��gD�0 Dڙ�D��D���D�9�D�s3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aв-AиRAк^Aк^AиRAжFAиRAк^AиRAмjAоwAк^Aд9Aв-AЮA�|�A�`BA�G�A�C�A�1'A� �A��A� �A��A�JA�A��A��#AϾwA��yA��`A�ȴA��#A�|�A�(�A��;A�ZA�=qA���A��A�jA�VA��A°!A���A���A��yA��A�hsA���A�ƨA�A�A���A���A���A�I�A���A��hA�I�A�A�A�
=A�A��A���A��TA��A�A�A�\)A�5?A��TA���A�O�A��;A���A���A�\)A� �A��A��;A���A�M�A�G�A��+A�t�A���A���A��A��+A�5?A��A���A�M�A��/A�XA�/A� �A�33A�A��A��A�"�A��+A�|�A��RA�ȴA�;dA�-A���A�O�A�=qA�S�A���A�\)A��`A�XA��A{�AvE�As�
Ao�#An{Al�\Ak��Aj��Ai�Ag�Ad��Ab�`A`�A`�!A`��A`��A`�+A`9XA_��A_��A_�A\�\AZ-AXVAW�wAWt�AV��AT��AR9XAO�hAM�7AL{AJ��AH��AH-AG%AF1AD�ABVAAt�A?��A>9XA=��A<VA:�A8�A7oA6�`A5t�A4I�A3A1�#A0��A.ĜA-+A,��A,�uA+�A+t�A*��A(��A';dA&��A&M�A$�RA#�A#��A#�PA"ĜA!7LA I�AdZA�+A��A�AAr�Ax�A
=A33A��A�7A��AbA�#AI�A�mA
��A
n�A	�;A	dZA�A�A�uAt�A�AO�A��A��Az�AA ��@�v�@��@��@���@���@�l�@�@���@�=q@�G�@�1'@��@�V@�9X@���@��@�C�@�"�@��@�`B@蛦@��
@��@��@�F@���@���@��@�h@�z�@�+@ޏ\@�X@�1'@ۅ@�@�v�@��@؛�@���@��@Չ7@�dZ@�G�@���@Л�@Ͼw@�"�@�n�@���@�p�@�G�@���@�  @�C�@ʟ�@���@��@�Z@ǍP@��@�$�@�x�@���@�9X@öF@�5?@�hs@���@���@��m@�+@�v�@�-@���@�%@�@�@��7@�`B@��@�A�@�+@��H@�=q@�G�@��D@�Q�@��w@�ȴ@�=q@��#@���@��h@��h@�hs@�%@�(�@�|�@�
=@��R@�E�@��T@�hs@���@��@���@��@�33@�ȴ@�~�@�5?@�{@��@�O�@�X@�V@��D@�Z@�9X@��@���@��P@�l�@�S�@�;d@�+@��@��!@��+@�-@���@�hs@�%@��/@���@��@�bN@�I�@��;@�t�@�\)@�o@��@���@�V@�{@���@�x�@�/@�V@��j@�z�@�9X@�1@��w@�C�@�C�@�l�@���@�@��-@�`B@��@��/@���@�I�@���@��;@��w@���@�l�@�;d@��y@���@�~�@�M�@�{@���@�%@�r�@��@��;@��F@�|�@�K�@�+@��@���@�V@�5?@�J@��T@���@�O�@�&�@���@��/@���@�I�@��@�b@��m@�\)@���@���@���@��+@�V@�@��@���@��-@���@�O�@���@��9@��@�bN@�A�@�(�@��m@���@�|�@�"�@��@���@��!@�~�@�$�@�{@���@���@��@�p�@�O�@�&�@���@���@��@��@��@��`@��`@���@���@��j@��D@��@�j@�(�@���@���@�ƨ@��@�l�@�+@�o@���@�n�@�=q@�J@�@��T@��7@�O�@�7L@�/@��@��u@�J@l�@uV@lj@c�
@Y��@Rn�@J�\@B��@<��@5`B@0A�@)&�@$�@�@�@ff@C�@bN@j@bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aв-AиRAк^Aк^AиRAжFAиRAк^AиRAмjAоwAк^Aд9Aв-AЮA�|�A�`BA�G�A�C�A�1'A� �A��A� �A��A�JA�A��A��#AϾwA��yA��`A�ȴA��#A�|�A�(�A��;A�ZA�=qA���A��A�jA�VA��A°!A���A���A��yA��A�hsA���A�ƨA�A�A���A���A���A�I�A���A��hA�I�A�A�A�
=A�A��A���A��TA��A�A�A�\)A�5?A��TA���A�O�A��;A���A���A�\)A� �A��A��;A���A�M�A�G�A��+A�t�A���A���A��A��+A�5?A��A���A�M�A��/A�XA�/A� �A�33A�A��A��A�"�A��+A�|�A��RA�ȴA�;dA�-A���A�O�A�=qA�S�A���A�\)A��`A�XA��A{�AvE�As�
Ao�#An{Al�\Ak��Aj��Ai�Ag�Ad��Ab�`A`�A`�!A`��A`��A`�+A`9XA_��A_��A_�A\�\AZ-AXVAW�wAWt�AV��AT��AR9XAO�hAM�7AL{AJ��AH��AH-AG%AF1AD�ABVAAt�A?��A>9XA=��A<VA:�A8�A7oA6�`A5t�A4I�A3A1�#A0��A.ĜA-+A,��A,�uA+�A+t�A*��A(��A';dA&��A&M�A$�RA#�A#��A#�PA"ĜA!7LA I�AdZA�+A��A�AAr�Ax�A
=A33A��A�7A��AbA�#AI�A�mA
��A
n�A	�;A	dZA�A�A�uAt�A�AO�A��A��Az�AA ��@�v�@��@��@���@���@�l�@�@���@�=q@�G�@�1'@��@�V@�9X@���@��@�C�@�"�@��@�`B@蛦@��
@��@��@�F@���@���@��@�h@�z�@�+@ޏ\@�X@�1'@ۅ@�@�v�@��@؛�@���@��@Չ7@�dZ@�G�@���@Л�@Ͼw@�"�@�n�@���@�p�@�G�@���@�  @�C�@ʟ�@���@��@�Z@ǍP@��@�$�@�x�@���@�9X@öF@�5?@�hs@���@���@��m@�+@�v�@�-@���@�%@�@�@��7@�`B@��@�A�@�+@��H@�=q@�G�@��D@�Q�@��w@�ȴ@�=q@��#@���@��h@��h@�hs@�%@�(�@�|�@�
=@��R@�E�@��T@�hs@���@��@���@��@�33@�ȴ@�~�@�5?@�{@��@�O�@�X@�V@��D@�Z@�9X@��@���@��P@�l�@�S�@�;d@�+@��@��!@��+@�-@���@�hs@�%@��/@���@��@�bN@�I�@��;@�t�@�\)@�o@��@���@�V@�{@���@�x�@�/@�V@��j@�z�@�9X@�1@��w@�C�@�C�@�l�@���@�@��-@�`B@��@��/@���@�I�@���@��;@��w@���@�l�@�;d@��y@���@�~�@�M�@�{@���@�%@�r�@��@��;@��F@�|�@�K�@�+@��@���@�V@�5?@�J@��T@���@�O�@�&�@���@��/@���@�I�@��@�b@��m@�\)@���@���@���@��+@�V@�@��@���@��-@���@�O�@���@��9@��@�bN@�A�@�(�@��m@���@�|�@�"�@��@���@��!@�~�@�$�@�{@���@���@��@�p�@�O�@�&�@���@���@��@��@��@��`@��`@���@���@��j@��D@��@�j@�(�@���@���@�ƨ@��@�l�@�+@�o@���@�n�@�=q@�J@�@��T@��7@�O�@�7L@�/@��G�O�@�J@l�@uV@lj@c�
@Y��@Rn�@J�\@B��@<��@5`B@0A�@)&�@$�@�@�@ff@C�@bN@j@bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB"�B"�B!�B"�B"�B"�B"�B"�B"�B"�B"�B"�B#�B#�B#�B'�B+B.B/B-B+B,B,B,B,B+B)�B+B8RBk�B�B�RB�B,B/B(�B2-B]/B[#BP�BH�B:^B)�B'�B �B�B�B�B�B�B%�B+B1'B6FB>wBA�BB�B@�BA�BA�BB�B@�B?}B6FB1'B'�B �B�B��B�B��B�jB�B��B��B�oB�PB�B{�Bo�B[#BO�BF�B;dB5?B/B(�B!�B�BDB��B�mB�B��BǮB�XB�B��B��B�BjBI�B:^B-B�B1B
��B
�B
�yB
�B
�}B
�?B
�B
��B
�B
y�B
R�B
/B
�B
	7B	��B	��B	�B	�mB	�5B	��B	�qB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�+B	y�B	p�B	m�B	k�B	ffB	[#B	O�B	D�B	;dB	33B	+B	!�B	�B	�B	�B	bB	DB	+B	B��B��B��B�B�mB�mB�mB�NB�#B�B��B��BȴBĜBŢBĜBB��B�dB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�7B�B{�Bv�Bq�Bl�Bk�BiyBgmBcTBbNBaHBaHBaHB`BBcTBaHB^5B\)BYBW
BcTBiyBiyBffBe`BffBhsBgmBe`BdZBe`BgmBhsBhsBjBiyBhsBgmBe`Be`BffBiyBo�Bt�Bx�B{�B}�B� B�B~�B� B� B�B�B�%B�1B�1B�7B�7B�7B�7B�DB�DB�PB�PB�JB�PB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�-B�9B�9B�LB�^B�}B��BBƨB��B��B��B��B��B�B�5B�;B�;B�NB�fB�mB�B�B��B��B��B	B	1B	
=B	JB	JB	PB	VB	hB	�B	�B	�B	�B	�B	�B	"�B	%�B	,B	0!B	5?B	7LB	8RB	9XB	<jB	=qB	=qB	>wB	?}B	@�B	D�B	G�B	G�B	H�B	I�B	K�B	L�B	L�B	M�B	M�B	P�B	Q�B	R�B	T�B	XB	[#B	^5B	`BB	bNB	e`B	hsB	iyB	k�B	m�B	n�B	q�B	r�B	u�B	w�B	z�B	}�B	~�B	�B	�B	�B	�+B	�JB	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�?B	�RB	�dB	�qB	�wB	�wB	��B	��B	��B	B	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
+B
DB
PB
�B
!�B
'�B
/B
6FB
;dB
B�B
H�B
M�B
S�B
XB
_;B
cTB
hsB
k�B
q�B
s�B
v�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B"�B"�B!�B"�B"�B"�B"�B"�B"�B"�B"�B"�B#�B#�B#�B'�B+	B.B/$B-B+	B,B,B,B,B+	B*B+B8WBk�B�B�[B��B,B/%B)B28B];B[.BP�BH�B:gB*B'�B �B�B�B�B�B�B%�B+B13B6UB>�BA�BB�B@�BA�BA�BB�B@�B?�B6TB1/B'�B �B�B��B�B��B�uB�B��B��B�xB�ZB�-B{�Bo�B[.BO�BF�B;nB5GB/$B)B!�B�BNB��B�vB�B��BǼB�bB�B��B��B�Bj�BI�B:hB-B�B;B
��B
�B
�B
�"B
��B
�MB
�'B
��B
�/B
y�B
SB
/*B
�B
	IB	�B	��B	�B	�B	�GB	��B	��B	�1B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�?B	y�B	p�B	m�B	k�B	f|B	[:B	O�B	D�B	;}B	3KB	+B	!�B	�B	�B	�B	B	_B	GB	#B�B�B��B��B�B�B�B�kB�?B�-B�B��B��BĽB��BĽB«B��B��B�RB�.B�,B�B�B�B�B�B�	B��B��B��B��B��B��B��B�XB�8B|Bv�Bq�Bl�Bk�Bi�Bg�BcxBbqBalBalBajB`hBcwBajB^WB\KBY8BW+BcwBi�Bi�Bf�Be�Bf�Bh�Bg�Be�Bd|Be�Bg�Bh�Bh�Bj�Bi�Bh�Bg�Be�Be�Bf�Bi�Bo�Bt�Bx�B|	B~B�"B�(BB�"B�#B�(B�-B�EB�QB�RB�ZB�XB�YB�XB�fB�cB�qB�tB�kB�pB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�$B�)B�-B�<B�HB�KB�ZB�ZB�kB�}B��B��B®B��B��B��B��B��B�B�<B�SB�XB�WB�nB�B�B�B��B��B��B�B	:B	LB	
WB	eB	eB	kB	qB	�B	�B	�B	�B	�B	�B	�B	"�B	& B	,"B	0;B	5YB	7fB	8lB	9pB	<�B	=�B	=�B	>�B	?�B	@�B	D�B	G�B	G�B	H�B	I�B	K�B	L�B	L�B	M�B	M�B	Q B	RB	SB	UB	X'B	[=B	^NB	`YB	bgB	eyB	h�B	i�B	k�B	m�B	n�B	q�B	r�B	u�B	w�B	z�B	~
B	B	�#B	�)B	�4B	�BB	�`B	�tB	�{B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�0B	�8B	�?B	�DB	�IB	�RB	�gB	�|B	��B	��B	��B	��B	��B	��B	¥B	ıB	ƽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�&B	�+B	�1B	�7B	�=B	�=B	�EB	�FB	�PB	�VB	�^B	�^B	�[B	�_B	�dB	�mB	�uB	�vB	�}B	�zB	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
 B
 B
B
 B
%B
&B
&B
&B
0B
9B
9B
:B
?G�O�B
bB
�B
!�B
(B
/.B
6YB
;wB
B�B
H�B
M�B
T	B
X#B
_MB
ceB
h�B
k�B
q�B
s�B
v�B
z�B
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
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
<#�
<#�
<#�
G�O�<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214262016053112142620160531121426  AO  ARCAADJP                                                                    20140721230511    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230511  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230511  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121426  IP                  G�O�G�O�G�O�                