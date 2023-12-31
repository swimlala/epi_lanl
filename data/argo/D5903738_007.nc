CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:06:38Z AOML 3.0 creation; 2016-05-31T21:48:58Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230638  20160531144858  5903738 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4053_7107_007                   2C  D   APEX                            5370                            041511                          846 @�<'�y��1   @�<(Tb�@8�z�G��b�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy� D�fD�@ D��3D�� D�fD�C3D�&fD�� D�3D�9�D��fD���D�  D�33Dڜ�D��D���D�0 D�fD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�  A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC  C	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��DtY�Dy��D�3D�<�D�� D���D�3D�@ D�#3D���D�  D�6gD��3D�ɚD���D�0 Dڙ�D๚D���D�,�D�3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�  A�  A���A���A���A���A�A�
=A�JA�VA�VA�VA�VA�bA�bA�JA�VA�bA�{A�bA�oA��A��A��A�1A��A�E�A��RA��-A�M�A�JA�  A��A�\)A��mA���A��!A���A��DA�v�A�S�A�(�A��/A�ƨA��PA�l�A�XA�33A�9XA�&�A�A��yA�ȴA��PA�bNA�{A��mA��
A��\A�7LA��A��9A�oA��jA��A�hsA��!A�I�A���A�1'A���A�t�A�\)A�JA���A��7A�r�A�1'A���A��DA�oA��FA�bNA��;A���A��-A�XA���A�33A��9A���A���A�z�A���A��A���A��`A���A��TA���A��A�S�A�Q�A�9XA�ĜA��mA���A��+A�7LA���A�\)A�jA���A�K�A��A�A�S�A}XA|$�AzE�Av��At��Ar�DAo��Ao&�Am?}Ak�7Ah��AhbAi�Ah��Af�HAeƨAdE�Ab��A`�A_
=A]t�A\ȴA\ffAZ��AYS�AX��AXv�AW�;AV��AV�AU��AT��ASVAQ��AP��AO��AO|�AOdZAN9XAKO�AH��AH1AF�DAD��AC�-ABA@A>�jA<(�A:�A:��A:1'A9�^A8�A8bNA7��A5��A4�`A3hsA2E�A29XA1l�A0�/A0�9A0=qA/ƨA/oA.�HA.=qA,�A+�A+\)A*��A*^5A)��A(v�A'`BA&M�A%|�A$��A$  A"�\A!��A!XA �DA?}A��AK�AZAXA�A�HA�AVA�9A=qAx�A1'AdZA�+A��A��AS�A�uAƨA�jA��A�7AȴA��A�TA�A
~�A	��A	�A{A�
A�A1A1'A/AjA�TA ��@��H@���@�?}@��@���@��@��@�@�/@�
=@�?}@��T@�  @�K�@�o@���@��#@�9X@���@���@�9@�+@��#@�Q�@�ff@�&�@�Ĝ@�Ĝ@�E�@��@�\)@�\)@ѡ�@�Q�@�-@�9X@���@�r�@�;d@�n�@�@�x�@�7L@�%@�?}@�X@���@�Z@�1@�V@���@�K�@���@���@�7L@��D@�b@�C�@���@�Ĝ@��D@��P@�;d@��@�-@�?}@�%@��@���@�;d@�=q@��7@��9@�b@���@�\)@��@�M�@�J@��-@�%@��D@��@�S�@��y@���@���@���@�1'@��
@�l�@��!@��^@�`B@�Z@��m@�dZ@�
=@�V@���@�V@���@�z�@�r�@�j@�j@�Q�@�1@��m@���@�"�@���@�n�@���@� �@��m@���@�K�@�;d@�
=@���@�-@�@�?}@�&�@�7L@�7L@���@��`@��`@��@��9@���@��u@��j@��9@�j@� �@�t�@�33@��!@�^5@��^@��`@��
@��@���@��R@��R@��!@�n�@���@�G�@��9@���@��D@�j@��@��F@���@���@��F@�|�@�;d@�@��@��!@���@��+@�~�@�n�@�E�@�J@��T@��^@���@�p�@�hs@�`B@�&�@�%@��`@��9@��u@��@��@�C�@�C�@�C�@�+@�o@��y@��!@�ȴ@��H@��H@��H@��@��!@��!@��R@�ȴ@��@���@���@�V@�$�@��#@���@�hs@�p�@�p�@�p�@�O�@�`B@�O�@�/@�%@��`@��j@��@��D@�r�@�Q�@�  @��
@���@�K�@�;d@�;d@�;d@�;d@�o@�v�@�M�@�5?@�ff@�n�@�=q@��T@���@��h@�/@��@�Ĝ@���@�r�@�v�@~5?@v�y@l�@d1@^V@Vv�@QG�@J-@D(�@>��@8Ĝ@3��@-��@)��@#�m@ A�@@1'@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�  A�  A���A���A���A���A�A�
=A�JA�VA�VA�VA�VA�bA�bA�JA�VA�bA�{A�bA�oA��A��A��A�1A��A�E�A��RA��-A�M�A�JA�  A��A�\)A��mA���A��!A���A��DA�v�A�S�A�(�A��/A�ƨA��PA�l�A�XA�33A�9XA�&�A�A��yA�ȴA��PA�bNA�{A��mA��
A��\A�7LA��A��9A�oA��jA��A�hsA��!A�I�A���A�1'A���A�t�A�\)A�JA���A��7A�r�A�1'A���A��DA�oA��FA�bNA��;A���A��-A�XA���A�33A��9A���A���A�z�A���A��A���A��`A���A��TA���A��A�S�A�Q�A�9XA�ĜA��mA���A��+A�7LA���A�\)A�jA���A�K�A��A�A�S�A}XA|$�AzE�Av��At��Ar�DAo��Ao&�Am?}Ak�7Ah��AhbAi�Ah��Af�HAeƨAdE�Ab��A`�A_
=A]t�A\ȴA\ffAZ��AYS�AX��AXv�AW�;AV��AV�AU��AT��ASVAQ��AP��AO��AO|�AOdZAN9XAKO�AH��AH1AF�DAD��AC�-ABA@A>�jA<(�A:�A:��A:1'A9�^A8�A8bNA7��A5��A4�`A3hsA2E�A29XA1l�A0�/A0�9A0=qA/ƨA/oA.�HA.=qA,�A+�A+\)A*��A*^5A)��A(v�A'`BA&M�A%|�A$��A$  A"�\A!��A!XA �DA?}A��AK�AZAXA�A�HA�AVA�9A=qAx�A1'AdZA�+A��A��AS�A�uAƨA�jA��A�7AȴA��A�TA�A
~�A	��A	�A{A�
A�A1A1'A/AjA�TA ��@��H@���@�?}@��@���@��@��@�@�/@�
=@�?}@��T@�  @�K�@�o@���@��#@�9X@���@���@�9@�+@��#@�Q�@�ff@�&�@�Ĝ@�Ĝ@�E�@��@�\)@�\)@ѡ�@�Q�@�-@�9X@���@�r�@�;d@�n�@�@�x�@�7L@�%@�?}@�X@���@�Z@�1@�V@���@�K�@���@���@�7L@��D@�b@�C�@���@�Ĝ@��D@��P@�;d@��@�-@�?}@�%@��@���@�;d@�=q@��7@��9@�b@���@�\)@��@�M�@�J@��-@�%@��D@��@�S�@��y@���@���@���@�1'@��
@�l�@��!@��^@�`B@�Z@��m@�dZ@�
=@�V@���@�V@���@�z�@�r�@�j@�j@�Q�@�1@��m@���@�"�@���@�n�@���@� �@��m@���@�K�@�;d@�
=@���@�-@�@�?}@�&�@�7L@�7L@���@��`@��`@��@��9@���@��u@��j@��9@�j@� �@�t�@�33@��!@�^5@��^@��`@��
@��@���@��R@��R@��!@�n�@���@�G�@��9@���@��D@�j@��@��F@���@���@��F@�|�@�;d@�@��@��!@���@��+@�~�@�n�@�E�@�J@��T@��^@���@�p�@�hs@�`B@�&�@�%@��`@��9@��u@��@��@�C�@�C�@�C�@�+@�o@��y@��!@�ȴ@��H@��H@��H@��@��!@��!@��R@�ȴ@��@���@���@�V@�$�@��#@���@�hs@�p�@�p�@�p�@�O�@�`B@�O�@�/@�%@��`@��j@��@��D@�r�@�Q�@�  @��
@���@�K�@�;d@�;d@�;d@�;d@�o@�v�@�M�@�5?@�ff@�n�@�=q@��T@���@��h@�/@��@�Ĝ@���@�r�@�v�@~5?@v�y@l�@d1@^V@Vv�@QG�@J-@D(�@>��@8Ĝ@3��@-��@)��@#�m@ A�@@1'@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B�B�B �B!�B!�B!�BoB"�B$�B'�B,BP�Bv�B�B|�Bx�B{�B|�B� B�%B�%B�7B�DB�JB�VB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�PB�=B�Br�Bk�BjBk�Be`Be`B_;BZB^5B^5B\)BYBVBT�BS�BO�BJ�BF�B@�B:^B33B%�BJB�B��BÖB�FB��B�B{�B]/B1B��B�!B��B�PB�DB�Bw�BffB<jB
��B
�B
�5B
�dB
�9B
�B
�B
��B
�\B
~�B
^5B
@�B
.B
�B
B	��B	�`B	ȴB	�^B	��B	��B	�oB	�+B	{�B	jB	r�B	��B	��B	��B	�{B	�DB	�B	z�B	q�B	jB	gmB	cTB	]/B	T�B	Q�B	P�B	M�B	H�B	F�B	D�B	>wB	6FB	0!B	,B	'�B	%�B	#�B	�B	DB	B��B��B�B�sB�BB�B�B��BǮBƨBŢBÖBB��B�wB�jB�dB�dB��BȴBɺBǮBƨBĜBĜBĜBB��B�qB�dB�^B�LB�9B�'B�B��B��B��B��B��B��B��B�oB�PB�1B�B� B|�Bx�Bs�Bq�Bn�Bl�Bk�BiyBffBbNB_;B]/B]/B\)B[#BXBT�BP�BL�BJ�BK�BI�BM�BK�BC�BC�BC�BD�BK�BI�BF�B;dB7LB6FB5?B1'B.B)�B%�B$�B"�B#�B"�B!�B �B!�B"�B!�B!�B!�B!�B �B �B!�B!�B!�B"�B!�B�B�BuBoBuBoBVBVBbBoB�B�B�B{B{B�B�B�B�B�B �B$�B(�B+B,B,B-B-B(�B(�B.B1'B0!B2-B33B0!B,B,B1'B49B8RB<jBA�BC�BD�BF�BM�BP�BT�BT�BVBW
BZB\)B^5B`BBaHBcTBgmBhsBl�Bm�Bn�Bo�Br�Bw�Bz�B{�B{�B|�B}�B~�B~�B� B�B�B�+B�1B�=B�VB�bB�hB�hB�oB��B��B��B��B��B��B��B��B�B�B�!B�3B�FB�dB�qB�}BĜBȴB��B��B�
B�B�#B�NB�fB�mB�sB�B�B�B��B��B��B	B		7B	
=B	JB	VB	PB	bB	�B	�B	�B	�B	�B	!�B	"�B	'�B	(�B	,B	/B	2-B	:^B	?}B	C�B	K�B	R�B	T�B	W
B	XB	]/B	`BB	cTB	cTB	e`B	gmB	jB	p�B	s�B	t�B	x�B	{�B	~�B	�B	�B	�%B	�7B	�DB	�JB	�PB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�'B	�3B	�?B	�FB	�XB	��B	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�5B	�5B	�BB	�NB	�NB	�ZB	�`B	�`B	�fB	�sB	�B	��B
JB
�B
 �B
%�B
/B
49B
;dB
@�B
D�B
K�B
O�B
T�B
YB
^5B
bNB
e`B
jB
n�B
s�11111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B�B�B �B!�B!�B!�G�O�B"�B$�B'�B,BP�Bv�B�B|�Bx�B{�B|�B�B�6B�5B�FB�SB�WB�gB�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�zB�^B�JB�Br�Bk�Bj�Bk�BenBemB_LBZ(B^AB^FB\6BY%BVBUBTBO�BJ�BF�B@�B:lB3?B%�BVB�B�BâB�PB��B�B{�B]5B<B��B�0B��B�[B�MB�*Bw�BfnB<wB
�B
�B
�@B
�rB
�EB
�"B
�B
��B
�lB
B
^FB
@�B
.'B
�B
*B	��B	�qB	��B	�sB	�B	��B	��B	�AB	{�B	j�B	r�B	��B	��B	��B	��B	�]B	�$B	z�B	q�B	j�B	g�B	ckB	]EB	UB	RB	P�B	M�B	H�B	F�B	D�B	>�B	6_B	0<B	,#B	(	B	%�B	#�B	�B	]B	)B�B��B�B�B�`B�6B�"B��B��B��B��BõB°B��B��B��B��B��B��B��B��B��B��BļBļBļB­B��B��B��B�~B�lB�XB�GB�'B�B��B��B��B��B��B��B��B�tB�RB�3B�!B}Bx�Bs�Bq�Bn�Bl�Bk�Bi�Bf�BbqB__B]QB]RB\KB[HBX2BUBQBL�BJ�BK�BI�BM�BK�BC�BC�BC�BD�BK�BI�BF�B;�B7qB6jB5cB1MB.9B*!B&B%B"�B#�B"�B!�B �B!�B"�B!�B!�B!�B!�B �B �B!�B!�B!�B"�B!�B�B�B�B�B�B{BbBbBnB�B�B�B�B�B�B�B�B�B�B�B �B%B)B+)B,.B,.B-5B-0B)B)B.:B1NB0EB2SB3WB0GB,-B,+B1MB4`B8xB<�BA�BC�BD�BF�BM�BQ	BUBU"BV)BW-BZ>B\MB^YB`dBajBcvBg�Bh�Bl�Bm�Bn�Bo�Br�Bw�B{B|	B|
B}B~BBB�#B�&B�3B�LB�TB�^B�wB��B��B��B��B��B��B��B��B��B��B��B�	B�(B�)B�@B�PB�eB��B��B��BĻB��B��B�B�'B�4B�@B�mB�B�B�B�B��B��B��B��B�B	5B		SB	
VB	eB	sB	kB	}B	�B	�B	�B	�B	�B	!�B	"�B	(
B	)B	,"B	/3B	2IB	:xB	?�B	C�B	K�B	SB	UB	W!B	X)B	]GB	`[B	cnB	cpB	evB	g�B	j�B	p�B	s�B	t�B	x�B	| B	B	�)B	�1B	�:B	�NB	�[B	�`B	�gB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�"B	�+B	�*B	�/B	�0B	�=B	�>B	�IB	�QB	�\B	�mB	��B	ĳB	ŶB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�$B	�+B	�1B	�=B	�KB	�JB	�WB	�`B	�`B	�pB	�tB	�uB	�{B	�B	��B	��B
^B
�B
 �B
%�B
//B
4KB
;wB
@�B
D�B
K�B
O�B
UB
Y+B
^GB
b`B
erB
j�B
n�B
s�11111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311448582016053114485820160531144858  AO  ARCAADJP                                                                    20140721230638    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230638  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230638  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531144858  IP                  G�O�G�O�G�O�                