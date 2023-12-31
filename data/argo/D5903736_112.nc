CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-13T09:15:43Z AOML 3.0 creation; 2016-05-31T19:14:43Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150513091543  20160531121443  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               pA   AO  4051_7090_112                   2C  D   APEX                            5368                            041511                          846 @�POS_�1   @�PO�^�@4�V�u�dJz�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    pA   A   A   @�ff@�33@���A   A>ffA`  A�  A�  A���A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBg��Bo��Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dys3D�3D�C3D��fD�ɚD�fD�VfD��D���D�  D�0 D��3D�� D��D�I�Dڙ�D��3D� D�33D�l�D�vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�  @���AffA<��A^ffA~ffA�33A�  A�33A�33A�33A�  A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B`  Bg34Bo34Bw34B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC  C�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,� D-  D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��DtfgDyl�D� D�@ D��3D��gD�3D�S3D�gD�ɚD���D�,�D�� D���D�	�D�FgDږgD�� D��D�0 D�i�D�s311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A�r�A�t�A�t�A�x�Aя\AёhAхAсAуA�z�AхAя\AѓuAя\AэPA�t�A�^5A�ZA�VA�M�A�"�A��A��
A�ȴAв-AЏ\AЉ7A�VA���A���A˝�A���A��Aɝ�Aɣ�Aə�A�;dA��AȋDA�E�AǕ�A��`A��AčPA�M�A���A�M�A�/A��A��A²-A�5?A��A���A��A���A���A�&�A���A��A���A���A�t�A�"�A�O�A�x�A� �A���A��A��HA�33A�%A��FA���A��TA��
A��A�hsA���A��A�ƨA�?}A��A�hsA��DA�?}A��A��hA�+A��7A��/A�^5A�G�A�;dA�bNA�G�A�I�A���A��9A��7A��uA�S�A�1'A��A�G�A��HA�jA�oA���A�~�A��A��+A��+A���A���A��mA��`A�XA��A��A���A��7A�?}A��A~ĜA{�7AzE�Az  Ay��AxĜAv�At1'ArjAo��AlVAj��Aj5?Ai/Ag��Af�HAc�AaA[�AZ�AY�^AY/AV�AT5?AP��AO7LAM��ALM�AKXAJv�AH�uAF�AFM�AD��AA��A@�jA>ĜA=�-A;/A8��A7�
A6ĜA3�A2-A1?}A/dZA.�A-ƨA-t�A, �A++A*�\A)l�A'��A&��A&ffA&�A#K�A"=qA!�#A!S�A ^5A�DA`BA �A��A�uA�
A�+A�A1'A%AjA��A��A��AC�A�#At�A��AK�A	�-A�DAE�A��Ar�A`BA�\A{A?}A�RA/@��w@���@�?}@���@��@��;@���@�  @�+@�=q@�@�r�@��;@��@�@��@��@ꟾ@���@�p�@��;@�Q�@�@�33@��@�!@�R@�!@��@��@�33@��@���@���@ڟ�@�x�@ؓu@�S�@ְ!@�X@�V@ԣ�@Ӿw@�+@�ȴ@�X@�  @�n�@��@��@˶F@���@�=q@ɉ7@�1'@��@�M�@��@�z�@��y@�V@���@�O�@���@��9@��m@�\)@��!@��^@���@��D@�9X@�l�@��R@���@��\@�-@��#@��@�A�@� �@�(�@�1'@�1'@��@���@�dZ@�"�@���@��+@�E�@��T@���@�?}@�V@��m@�t�@�V@��@��@��T@��#@��@���@�&�@��u@�j@�I�@�1@��@�o@���@�M�@��@�@���@�p�@�%@��j@���@�z�@�A�@�1@�ƨ@��@�~�@�=q@��#@�`B@�7L@��/@��j@��u@�r�@�A�@���@��@�K�@�+@��@���@�M�@�=q@�J@��@��#@���@�p�@�Ĝ@�Q�@��
@���@�S�@�S�@�l�@�"�@���@��@�ȴ@�V@��@��-@��/@�j@�I�@��m@��w@���@���@���@�l�@��@��y@��R@��!@�ȴ@��@��@�K�@�|�@�  @��@�bN@�(�@��m@��F@�S�@�@��\@�n�@�E�@�$�@���@���@��7@�/@�7L@�7L@�&�@��@���@��u@���@�r�@��
@���@��@�\)@���@��!@�n�@�-@�{@���@���@��7@��-@�?}@��j@�j@�b@��m@�t�@��@��@��R@�ff@�M�@�{@���@�`B@���@��@�r�@�j@�Q�@�  @�S�@��R@�~�@�-@�@���@��@���@��-@�p�@��@��@�l�@�;d@�o@���@�-@�$�@�-@�V@�=q@��@���@�hs@��@��/@��9@�9X@�t�@�@���@�E�@�-@�{@���@�;d@w\)@ol�@ep�@^{@T��@L1@DI�@>$�@8�u@2M�@,�@'
=@ �`@��@$�@S�@�@?}@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�|�A�r�A�t�A�t�A�x�Aя\AёhAхAсAуA�z�AхAя\AѓuAя\AэPA�t�A�^5A�ZA�VA�M�A�"�A��A��
A�ȴAв-AЏ\AЉ7A�VA���A���A˝�A���A��Aɝ�Aɣ�Aə�A�;dA��AȋDA�E�AǕ�A��`A��AčPA�M�A���A�M�A�/A��A��A²-A�5?A��A���A��A���A���A�&�A���A��A���A���A�t�A�"�A�O�A�x�A� �A���A��A��HA�33A�%A��FA���A��TA��
A��A�hsA���A��A�ƨA�?}A��A�hsA��DA�?}A��A��hA�+A��7A��/A�^5A�G�A�;dA�bNA�G�A�I�A���A��9A��7A��uA�S�A�1'A��A�G�A��HA�jA�oA���A�~�A��A��+A��+A���A���A��mA��`A�XA��A��A���A��7A�?}A��A~ĜA{�7AzE�Az  Ay��AxĜAv�At1'ArjAo��AlVAj��Aj5?Ai/Ag��Af�HAc�AaA[�AZ�AY�^AY/AV�AT5?AP��AO7LAM��ALM�AKXAJv�AH�uAF�AFM�AD��AA��A@�jA>ĜA=�-A;/A8��A7�
A6ĜA3�A2-A1?}A/dZA.�A-ƨA-t�A, �A++A*�\A)l�A'��A&��A&ffA&�A#K�A"=qA!�#A!S�A ^5A�DA`BA �A��A�uA�
A�+A�A1'A%AjA��A��A��AC�A�#At�A��AK�A	�-A�DAE�A��Ar�A`BA�\A{A?}A�RA/@��w@���@�?}@���@��@��;@���@�  @�+@�=q@�@�r�@��;@��@�@��@��@ꟾ@���@�p�@��;@�Q�@�@�33@��@�!@�R@�!@��@��@�33@��@���@���@ڟ�@�x�@ؓu@�S�@ְ!@�X@�V@ԣ�@Ӿw@�+@�ȴ@�X@�  @�n�@��@��@˶F@���@�=q@ɉ7@�1'@��@�M�@��@�z�@��y@�V@���@�O�@���@��9@��m@�\)@��!@��^@���@��D@�9X@�l�@��R@���@��\@�-@��#@��@�A�@� �@�(�@�1'@�1'@��@���@�dZ@�"�@���@��+@�E�@��T@���@�?}@�V@��m@�t�@�V@��@��@��T@��#@��@���@�&�@��u@�j@�I�@�1@��@�o@���@�M�@��@�@���@�p�@�%@��j@���@�z�@�A�@�1@�ƨ@��@�~�@�=q@��#@�`B@�7L@��/@��j@��u@�r�@�A�@���@��@�K�@�+@��@���@�M�@�=q@�J@��@��#@���@�p�@�Ĝ@�Q�@��
@���@�S�@�S�@�l�@�"�@���@��@�ȴ@�V@��@��-@��/@�j@�I�@��m@��w@���@���@���@�l�@��@��y@��R@��!@�ȴ@��@��@�K�@�|�@�  @��@�bN@�(�@��m@��F@�S�@�@��\@�n�@�E�@�$�@���@���@��7@�/@�7L@�7L@�&�@��@���@��u@���@�r�@��
@���@��@�\)@���@��!@�n�@�-@�{@���@���@��7@��-@�?}@��j@�j@�b@��m@�t�@��@��@��R@�ff@�M�@�{@���@�`B@���@��@�r�@�j@�Q�@�  @�S�@��R@�~�@�-@�@���@��@���@��-@�p�@��@��@�l�@�;d@�o@���@�-@�$�@�-@�V@�=q@��@���@�hs@��@��/@��9@�9X@�t�@�@���@�E�@�-@�{@���@�;d@w\)@ol�@ep�@^{@T��@L1@DI�@>$�@8�u@2M�@,�@'
=@ �`@��@$�@S�@�@?}@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	o�B	o�B	o�B	m�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	p�B	p�B	q�B	s�B	�B	�hB	��B	��B	��B	��B	�fB
P�B
�VB
ÖB�B5?BJ�BcTBt�B�bB�dB��BbB�B,BC�BN�B]/BhsB|�B�wB+BJBVBJB��B�B�B-B:^B>wBC�B:^BG�B9XB{B��B"�BPB�B��B��B'�B?}B+B9XB;dBB�BB�B%�B2-B7LB0!B#�B�BJBB
=BJB �BS�B^5Be`BXBE�B"�BB�HB��BhsBD�B �BJB��B�TB�5B�B��B��B�PB�B�B{�Bs�BR�B@�B0!B!�B{BB
�mB
��B
��B
�B
�oB
� B
l�B
bNB
ZB
F�B
;dB
8RB
33B
)�B
�B
\B	��B	�fB	��B	ĜB	��B	�^B	�!B	��B	�bB	z�B	`BB	VB	R�B	L�B	@�B	0!B	�B	�B	bB	B	%B	B��B��B�B�B�mB�`B�HB�/B�
B��B��BǮB��B�qB�XB�3B�B�B�B��B��B��B��B�uB�bB�VB�=B�B�B�B}�Bz�Bz�Bw�Bt�Bs�Br�Bq�Bp�Bo�Br�By�B{�B}�B}�B|�Bz�Bu�Bu�Bt�Bt�Bs�Bt�Bt�Bs�Bs�Bq�Bq�Br�Bs�Bq�Bp�Bn�Bk�BffBbNBcTBbNBcTBffBiyBk�Bk�BjBl�Bl�Bl�Bl�Bm�Bp�Bo�Bm�BjBhsBiyBjBl�Bl�Bl�Bl�Bk�BjBjBk�Bl�Bn�Bp�Br�Bu�Bx�By�B�B�B�B�+B�7B�DB�hB�uB��B��B��B��B��B��B�B�B�'B�3B�9B�XB��BÖBŢBȴBɺBɺB��B��B�B�/B�ZB�fB�sB�B�B�B��B��B��B	%B	VB	oB	uB	uB	{B	�B	�B	�B	!�B	'�B	+B	-B	/B	2-B	6FB	6FB	:^B	<jB	@�B	@�B	?}B	@�B	B�B	G�B	G�B	J�B	L�B	M�B	M�B	N�B	O�B	P�B	VB	XB	[#B	[#B	\)B	^5B	`BB	bNB	cTB	dZB	e`B	e`B	ffB	k�B	m�B	n�B	o�B	q�B	r�B	t�B	u�B	u�B	u�B	u�B	w�B	z�B	{�B	{�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�1B	�=B	�DB	�PB	�\B	�bB	�hB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�?B	�LB	�dB	�dB	�dB	�^B	�XB	�XB	�XB	�RB	�XB	�^B	�dB	�dB	��B	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�
B	�B	�#B	�#B	�)B	�BB	�TB	�ZB	�`B	�fB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
  B
  B
  B
  B
  B
B
B
B
B
B
%B
1B
�B
�B
#�B
)�B
/B
49B
:^B
@�B
G�B
L�B
S�B
ZB
_;B
e`B
jB
o�B
r�B
t�B
x�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	o�B	o�B	o�B	m�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	o�B	p�B	p�B	q�B	s�B	�B	�zB	��B	��B	��B	��B	�vB
P�B
�bB
ãB�B5JBJ�Bc\Bt�B�kB�lB�BhB�B,BC�BN�B]7Bh~B|�B��B6BSBcBRB�B�B�B-B:iB>�BC�B:jBG�B9eB�B��B"�B\B�B��B��B'�B?�B+B9bB;nBB�BB�B%�B2:B7]B0-B#�B�BVBB
GBWB �BTB^CBemBXBE�B"�BB�VB��Bh{BD�B �BRB��B�YB�AB� B��B��B�[B�,B�B{�Bs�BR�B@�B0*B!�B�B*B
�zB
��B
��B
�B
�|B
�B
l�B
bcB
Z,B
F�B
;vB
8`B
3CB
*B
�B
nB	�B	�{B	��B	įB	��B	�rB	�5B	��B	�zB	z�B	`YB	VB	SB	L�B	@�B	0<B	�B	�B	~B	*B	AB	(B�B��B��B�B�B�}B�dB�LB�(B�B��B��B��B��B�wB�QB�;B�4B�(B��B��B��B��B��B��B�vB�^B�>B�5B�(B~B{B{Bw�Bt�Bs�Br�Bq�Bp�Bo�Br�By�B|B~B~B}B{Bu�Bu�Bt�Bt�Bs�Bt�Bt�Bs�Bs�Bq�Bq�Br�Bs�Bq�Bp�Bn�Bk�Bf�BbrBcyBbqBcwBf�Bi�Bk�Bk�Bj�Bl�Bl�Bl�Bl�Bm�Bp�Bo�Bm�Bj�Bh�Bi�Bj�Bl�Bl�Bl�Bl�Bk�Bj�Bj�Bk�Bl�Bn�Bp�Br�Bu�Bx�By�B�(B�/B�5B�MB�ZB�eB��B��B��B��B��B�B�B�B�#B�4B�FB�PB�ZB�yB��BôB��B��B��B��B��B�B�"B�NB�xB�B�B�B��B��B��B��B�B	@B	rB	�B	�B	�B	�B	�B	�B	�B	!�B	(	B	+B	-&B	/6B	2GB	6^B	6aB	:uB	<�B	@�B	@�B	?�B	@�B	B�B	G�B	G�B	J�B	L�B	M�B	M�B	N�B	O�B	P�B	VB	X(B	[;B	[;B	\CB	^LB	`ZB	bfB	cmB	dqB	ezB	exB	f~B	k�B	m�B	n�B	o�B	q�B	r�B	t�B	u�B	u�B	u�B	u�B	w�B	z�B	{�B	|B	~
B	�B	�B	�B	�"B	�,B	�,B	�*B	�4B	�FB	�TB	�[B	�gB	�tB	�zB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�%B	�IB	�WB	�bB	�{B	�{B	�zB	�uB	�mB	�lB	�mB	�gB	�mB	�uB	�zB	�{B	��B	ƾB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�*B	�8B	�9B	�>B	�WB	�hB	�pB	�vB	�zB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B
B
!B
B
B
B
 B
 B
 B
 B
 B
!B
'B
+B
4B
3B
:B
FB
�B
�B
#�B
*
B
/.B
4JB
:qB
@�B
G�B
L�B
T
B
Z.B
_MB
esB
j�B
o�B
r�B
t�B
x�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214432016053112144320160531121443  AO  ARCAADJP                                                                    20150513091543    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150513091543  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150513091543  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121443  IP                  G�O�G�O�G�O�                