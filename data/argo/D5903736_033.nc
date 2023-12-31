CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:23Z AOML 3.0 creation; 2016-05-31T19:14:30Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230523  20160531121430  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               !A   AO  4051_7090_033                   2C  D   APEX                            5368                            041511                          846 @օ�G��1   @օ����@3�bM���d�l�C��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    !A   B   B   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy` D�fD�FfD�i�D��3D�  D�@ D��fD���D�	�D�I�D�p D��3D��D�Y�D�l�D�� D�3D�)�D�s3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DC� DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��DyY�D�3D�C3D�fgD�� D���D�<�D��3D�ٚD�gD�FgD�l�D�� D��D�VgD�i�D���D�  D�&gD�p D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aƴ9AƩ�AƧ�AƧ�Aƥ�AƑhAƓuAƓuAƓuAƓuAƓuAƑhAƍPAƍPAƍPAƍPAƏ\AƏ\AƋDAƉ7AƇ+AƉ7AƉ7AƉ7AƉ7AƉ7AƇ+AƃAƅAƇ+AƉ7AƉ7AƋDAƋDAƅAƃAƅAƅAƅAƅAƃAƃAƅAƅA�~�A�r�A�l�A�ZA�ZA�\)A�\)A���A�ZAăAËDA��A�K�A���A�VA�x�A��^A���A� �A���A�XA��RA��RA���A�M�A��A��PA�VA���A�1'A�33A�7LA���A��7A�dZA��
A��A��A��HA���A�t�A�$�A��A��^A��A�z�A�p�A���A�`BA��A�ZA���A�1'A���A�{A�  A�&�A���A�33A�dZA�M�A�%A��!A�  A�S�A��7A���A�-A�M�A�ffA���A���A�n�A��PA�x�A�ȴA�M�A�%A|��Ax5?Au?}Aq`BAnbNAm7LAl^5Ak�#Aj�`Af�uAd9XAbM�Aa�A^�yA]��A[��AX��AWAWdZAV��AU�FAT��AS7LAP�AL��AK��AH�/AG��AF�RAE|�ADr�AA\)A?dZA>��A=�hA;%A8��A8�A7��A7O�A6E�A3�mA0(�A-�A,A�A+��A+%A*A)S�A)"�A(�A'�A';dA& �A%p�A#�PA!�AƨAffA�/A&�AZA=qA  A�AQ�A�-A�A7LAA�A�^A�AoA�Al�A�A�A�A"�A�A
�9A	��A	�A�RA�#A��A�\A��Al�A7LA(�AVA�A�+AjA�@��;@��@�O�@���@�
=@�=q@���@�w@�-@��@�\@�7@�  @�u@�ȴ@�J@�h@��/@�@�F@�o@��y@��@⟾@�^5@���@���@�5?@��T@ݙ�@�?}@܋D@�@�V@ّh@�?}@� �@�@�%@�z�@��
@�K�@�ȴ@�V@щ7@���@�t�@�@�O�@̴9@� �@�E�@ɉ7@�j@���@�t�@�v�@�G�@��/@Ĭ@�Z@���@Å@��@��@��@�p�@�Q�@��w@���@�;d@�v�@�G�@��D@��@���@� �@�S�@�V@�@��#@�/@��@�b@�S�@�C�@�+@��H@��-@��9@�9X@�t�@��@��@��@�?}@�%@��/@�Ĝ@�bN@��@��y@��H@�$�@��h@�A�@���@�n�@���@�|�@��@��@�%@���@�(�@�  @�ƨ@��@��;@��w@�33@�"�@�^5@��@�%@���@���@��D@���@��/@��@��m@��@���@���@�p�@�`B@��@���@�bN@�I�@�I�@�1'@�b@��m@�K�@��y@�ȴ@���@���@���@�E�@���@��h@�O�@�7L@�7L@���@���@��9@�r�@�9X@��@��@�C�@��y@���@���@���@�ȴ@���@�n�@��T@��-@�/@�%@��j@��u@�I�@� �@��;@��F@���@�33@��R@���@��!@��+@�v�@�ff@�M�@��@��@��^@��@�`B@�7L@��@���@��/@��9@�j@�b@��F@�l�@�C�@��@�@��@���@��+@�^5@�-@��@���@���@��7@�p�@�X@���@��j@��u@�j@�  @���@�|�@�dZ@�S�@�;d@�+@�@���@��!@�n�@�-@�J@��@���@�O�@���@��@��/@��@�j@�1'@�b@�b@�1@��;@���@��w@��F@���@�t�@�t�@�;d@��@���@�~�@�E�@�=q@�J@��@��^@�x�@�/@���@���@� �@��
@�;d@��@}��@v��@lI�@cdZ@[��@S�@K�
@EO�@?�@:��@5/@.V@'�P@!��@�@A�@�@�R@
�@
=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aƴ9AƩ�AƧ�AƧ�Aƥ�AƑhAƓuAƓuAƓuAƓuAƓuAƑhAƍPAƍPAƍPAƍPAƏ\AƏ\AƋDAƉ7AƇ+AƉ7AƉ7AƉ7AƉ7AƉ7AƇ+AƃAƅAƇ+AƉ7AƉ7AƋDAƋDAƅAƃAƅAƅAƅAƅAƃAƃAƅAƅA�~�A�r�A�l�A�ZA�ZA�\)A�\)A���A�ZAăAËDA��A�K�A���A�VA�x�A��^A���A� �A���A�XA��RA��RA���A�M�A��A��PA�VA���A�1'A�33A�7LA���A��7A�dZA��
A��A��A��HA���A�t�A�$�A��A��^A��A�z�A�p�A���A�`BA��A�ZA���A�1'A���A�{A�  A�&�A���A�33A�dZA�M�A�%A��!A�  A�S�A��7A���A�-A�M�A�ffA���A���A�n�A��PA�x�A�ȴA�M�A�%A|��Ax5?Au?}Aq`BAnbNAm7LAl^5Ak�#Aj�`Af�uAd9XAbM�Aa�A^�yA]��A[��AX��AWAWdZAV��AU�FAT��AS7LAP�AL��AK��AH�/AG��AF�RAE|�ADr�AA\)A?dZA>��A=�hA;%A8��A8�A7��A7O�A6E�A3�mA0(�A-�A,A�A+��A+%A*A)S�A)"�A(�A'�A';dA& �A%p�A#�PA!�AƨAffA�/A&�AZA=qA  A�AQ�A�-A�A7LAA�A�^A�AoA�Al�A�A�A�A"�A�A
�9A	��A	�A�RA�#A��A�\A��Al�A7LA(�AVA�A�+AjA�@��;@��@�O�@���@�
=@�=q@���@�w@�-@��@�\@�7@�  @�u@�ȴ@�J@�h@��/@�@�F@�o@��y@��@⟾@�^5@���@���@�5?@��T@ݙ�@�?}@܋D@�@�V@ّh@�?}@� �@�@�%@�z�@��
@�K�@�ȴ@�V@щ7@���@�t�@�@�O�@̴9@� �@�E�@ɉ7@�j@���@�t�@�v�@�G�@��/@Ĭ@�Z@���@Å@��@��@��@�p�@�Q�@��w@���@�;d@�v�@�G�@��D@��@���@� �@�S�@�V@�@��#@�/@��@�b@�S�@�C�@�+@��H@��-@��9@�9X@�t�@��@��@��@�?}@�%@��/@�Ĝ@�bN@��@��y@��H@�$�@��h@�A�@���@�n�@���@�|�@��@��@�%@���@�(�@�  @�ƨ@��@��;@��w@�33@�"�@�^5@��@�%@���@���@��D@���@��/@��@��m@��@���@���@�p�@�`B@��@���@�bN@�I�@�I�@�1'@�b@��m@�K�@��y@�ȴ@���@���@���@�E�@���@��h@�O�@�7L@�7L@���@���@��9@�r�@�9X@��@��@�C�@��y@���@���@���@�ȴ@���@�n�@��T@��-@�/@�%@��j@��u@�I�@� �@��;@��F@���@�33@��R@���@��!@��+@�v�@�ff@�M�@��@��@��^@��@�`B@�7L@��@���@��/@��9@�j@�b@��F@�l�@�C�@��@�@��@���@��+@�^5@�-@��@���@���@��7@�p�@�X@���@��j@��u@�j@�  @���@�|�@�dZ@�S�@�;d@�+@�@���@��!@�n�@�-@�J@��@���@�O�@���@��@��/@��@�j@�1'@�b@�b@�1@��;@���@��w@��F@���@�t�@�t�@�;d@��@���@�~�@�E�@�=q@�J@��@��^@�x�@�/@���@���@� �@��
G�O�@��@}��@v��@lI�@cdZ@[��@S�@K�
@EO�@?�@:��@5/@.V@'�P@!��@�@A�@�@�R@
�@
=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB2-B2-B2-B2-B2-B2-B2-B2-B1'B1'B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B33B33B33B33B33B33B33B49B49B49B33B33B49B33B49B49B5?B5?B5?B5?B5?B5?B5?B5?B6FB7LB7LB6FB6FB6FB5?B5?B49B1'B0!B1'B7LBE�BXB_;BK�B�BB��B�yB�TB�BB��B�qB�9B�FB�XB�FB�-B�wB��BɺBǮBÖB�FB��B��B��B��B��B��B�JB�+By�BaHBZB]/B[#BE�B6FB+B$�B�BB��B�B�B�NB��B��B��B�uBw�BiyBZBF�B,BDB
�)B
�dB
��B
�7B
l�B
[#B
P�B
G�B
49B
�B
  B	�yB	��B	�qB	�XB	�9B	�B	��B	�1B	}�B	t�B	k�B	]/B	YB	Q�B	D�B	C�B	@�B	=qB	8RB	49B	)�B	�B	B	B��B�B�yB�NB�)B�B��B��BȴB��B�jB�XB�LB�9B�!B�-B�B��B��B�B�'B�!B�B�B�B��B��B��B��B��B�{B�hB�bB�PB�7B�%B�B�B�B~�B|�Bz�By�Bx�Bu�Bu�Bt�Br�Bp�Bn�Bn�Bm�Bl�Bk�Bk�BjBjBiyBiyBiyBhsBhsBgmBffBgmBhsBhsBhsBhsBhsBhsBffBe`BcTBbNBaHBbNBdZBdZBe`Be`BdZBdZBgmBffBffBffBe`BffBffBhsBiyBjBk�Bl�Bm�Bo�Bo�Bn�Bn�Bn�Bo�Bo�Bn�Bm�Bk�Bp�Bv�By�B}�B�B�%B�+B�1B�7B�DB�VB�hB�uB�{B�{B��B��B��B��B��B��B�B�B�B�!B�-B�-B�9B�^B�jB�jB�wB�wB�wB�}B�}B��BBŢBǮB��B��B�B�)B�/B�NB�mB�B�B�B�B�B�B�B��B��B	B	%B		7B	
=B	DB	PB	PB	\B	uB	�B	�B	 �B	(�B	/B	-B	(�B	&�B	#�B	$�B	7LB	;dB	:^B	9XB	9XB	:^B	;dB	>wB	C�B	J�B	Q�B	S�B	XB	\)B	`BB	aHB	bNB	hsB	k�B	m�B	r�B	t�B	t�B	u�B	v�B	v�B	w�B	y�B	|�B	�B	�B	�B	�B	�%B	�=B	�PB	�VB	�VB	�VB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�?B	�LB	�RB	�jB	�qB	�wB	�wB	�wB	�wB	�wB	�qB	�qB	�qB	�}B	��B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�BB	�NB	�NB	�TB	�TB	�ZB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
oB
+B
\B
�B
�B
'�B
/B
7LB
=qB
B�B
G�B
J�B
O�B
W
B
]/B
bNB
gmB
l�B
q�B
u�B
x�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B2:B26B26B26B29B24B26B26B13B15B29B26B29B26B29B29B24B26B26B29B3=B3?B3?B3?B3?B3?B3?B4FB4HB4FB3?B3?B4KB3=B4FB4FB5IB5IB5IB5KB5NB5NB5IB5IB6QB7[B7XB6QB6QB6QB5MB5KB4GB15B0-B16B7WBE�BXB_FBK�B�BB��B�B�`B�LB��B�|B�EB�TB�_B�QB�8B��B��B��BǸBÞB�SB��B� B�B��B��B��B�VB�6By�BaOBZ%B]9B[-BE�B6QB+
B$�B�B$B��B�B�B�ZB� B��B�B�Bw�Bi�BZ*BF�B,BOB
�8B
�pB
��B
�GB
l�B
[.B
P�B
G�B
4JB
�B
 B	�B	��B	��B	�kB	�NB	�0B	��B	�HB	~	B	t�B	k�B	]HB	Y1B	RB	D�B	C�B	@�B	=�B	8mB	4QB	*B	�B	;B	,B��B�B�B�lB�EB�/B�B��B��B��B��B�vB�jB�YB�@B�LB�:B�B��B�2B�FB�?B�4B�/B�'B�B�B��B��B��B��B��B��B�vB�YB�GB�@B�:B�/BB}B{By�Bx�Bu�Bu�Bt�Br�Bp�Bn�Bn�Bm�Bl�Bk�Bk�Bj�Bj�Bi�Bi�Bi�Bh�Bh�Bg�Bf�Bg�Bh�Bh�Bh�Bh�Bh�Bh�Bf�Be�BcxBbpBakBbqBd{Bd{Be�Be�Bd}Bd|Bg�Bf�Bf�Bf�Be�Bf�Bf�Bh�Bi�Bj�Bk�Bl�Bm�Bo�Bo�Bn�Bn�Bn�Bo�Bo�Bn�Bm�Bk�Bp�Bv�By�B~B�/B�FB�MB�SB�WB�dB�wB��B��B��B��B��B��B��B��B��B�B�%B�3B�;B�AB�LB�MB�ZB�~B��B��B��B��B��B��B��B��B°B��B��B��B�B�.B�HB�JB�kB�B�B�B��B�B�B�B��B��B�B	"B	@B		QB	
WB	`B	kB	kB	xB	�B	�B	�B	 �B	)B	/4B	-(B	)B	'B	#�B	$�B	7fB	;}B	:vB	9qB	9rB	:yB	;�B	>�B	C�B	J�B	RB	TB	X(B	\AB	`\B	a_B	bgB	h�B	k�B	m�B	r�B	t�B	t�B	u�B	v�B	v�B	w�B	y�B	}B	�%B	�(B	�1B	�6B	�;B	�UB	�fB	�mB	�oB	�oB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�$B	�2B	�0B	�7B	�>B	�DB	�DB	�HB	�SB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	¦B	ëB	ĲB	ĲB	ĲB	ŸB	ŵB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�%B	�#B	�+B	�0B	�1B	�6B	�CB	�HB	�QB	�XB	�cB	�aB	�iB	�jB	�lB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
 B
 B
B
G�O�B
AB
nB
�B
�B
(B
/.B
7`B
=�B
B�B
G�B
J�B
O�B
WB
]AB
b_B
gB
l�B
q�B
u�B
x�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214302016053112143020160531121430  AO  ARCAADJP                                                                    20140721230523    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230523  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230523  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121430  IP                  G�O�G�O�G�O�                