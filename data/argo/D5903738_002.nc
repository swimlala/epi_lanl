CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:06:35Z AOML 3.0 creation; 2016-05-31T21:48:57Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230635  20160531144857  5903738 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4053_7107_002                   2C  D   APEX                            5370                            041511                          846 @�/iw��1   @�/jο�@7��Q��c-�"��`1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D�fD�Y�D��3D��fD���D�\�D�� D��3D�  D�VfD��3Dǰ D��fD�L�D�y�D� D���D�Y�D�fD�ɚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D4  D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dtl�Dy�gD�3D�VgD�� D��3D���D�Y�D���D�� D��D�S3D�� DǬ�D��3D�I�D�vgD��D���D�VgD�3D��g11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�I�A�I�A�K�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�C�A�=qA�?}A�?}A�E�A�I�A�C�A�E�A�Q�A�VA�XA�XA�XA�ZA�VA�Q�A�O�A�Q�A�S�A�S�A�Q�A�Q�A�XA�S�A�Q�A�S�A�VA�M�A�A�A�=qA�?}A�A�A�5?A�+A��A�{A��#A��hA���A��FA� �A��HA�%A���A�M�A���A���A�ĜA���A��\A���A�r�A�VA�v�A��`A�Q�A��-A�(�A��7A��hA�l�A��
A��;A��9A�1A�oA�I�A�ffA���A��A��
A���A�-A��A��
A���A�A�A��A�`BA��uA��#A��jA�  A��A��7A��HA��RA�?}A��yA��PA�+A�-A}�A{|�Ay�mAwAt^5Ar��ArI�AqVAp5?An��Al��Ai��Af�Ab�A`�jA^�A\^5AZ�9AZ�AY��AX�AT�+AP1AM�-AJ��AI?}AG��AD��AAt�A@�yA@A�A?;dA>��A>M�A=ƨA=l�A=%A<��A<��A;�;A;?}A8��A4ffA2��A0VA/p�A.��A.bA-"�A.  A,�`A,��A,5?A+33A*�\A)�A)�A*bA*A)33A(9XA%�A$�uA#��A$�yA%oA%"�A"ffA!O�A�mA�AVA�7AXA��AA�A��A��AdZA�/A�AhsAS�A��A�AdZA7LAz�AG�A�A��An�A��A;dAE�A�hAl�AK�A
��A	�^A��A/A�Az�A$�A�^A~�Al�A�`AAC�@�l�@���@�"�@�O�@��9@���@�~�@���@�X@�hs@�V@�@��#@�$�@���@웦@�b@�@�K�@�z�@�V@�z�@��@�^5@�@�h@ߝ�@�v�@ݩ�@�z�@ە�@�;d@�"�@�E�@�?}@�Q�@�v�@�Ĝ@�b@�dZ@ѩ�@�"�@�=q@��@�bN@�33@�-@��@ȋD@��@Ƈ+@�hs@ě�@ă@î@�33@å�@�j@�r�@��@��T@�x�@�&�@��`@�1@��@�J@���@�|�@�+@�"�@���@���@�|�@��R@���@���@�hs@��9@�@���@�n�@��
@�p�@��`@�r�@�(�@��@���@��!@�G�@��@��D@�I�@�|�@���@���@�n�@�-@�G�@���@��/@��u@�bN@�z�@�9X@�ƨ@�dZ@�K�@�C�@�"�@��@���@��\@�n�@�5?@���@�33@�o@�@���@��H@���@���@��^@���@�p�@�A�@���@�l�@�\)@�"�@�
=@�o@�o@��@��!@�ff@���@���@���@�9X@��@���@�l�@�K�@��!@���@�~�@�^5@�=q@��T@��T@��@�$�@�M�@�^5@�~�@��\@�=q@�$�@�@��7@�X@�7L@��@���@��;@�|�@�K�@��y@��@���@�V@�E�@�5?@�5?@�{@��T@���@���@�/@��@��/@�Z@���@��w@��@���@���@�|�@�l�@�S�@�"�@�ȴ@��+@�=q@�$�@�@��@��@�X@�O�@�V@�z�@��D@�bN@�1@���@��P@�t�@�dZ@�K�@�33@���@��R@�~�@�V@�-@��@�{@�@��#@��-@��h@���@�7L@���@�z�@�1'@��9@��u@���@�|�@�"�@��@�
=@�
=@�@���@�n�@��@��#@��@�&�@�V@��`@���@���@���@�j@� �@��w@��F@��w@���@���@�l�@�"�@�
=@�K�@��P@��P@���@�t�@�33@��@�^5@�M�@�5?@�$�@�$�@�{@��7@�&�@�;@{ƨ@q��@l�@c�
@]/@W�w@M�h@G�@A��@=V@6V@/K�@*�H@&5?@!7L@�@��@Z@v�@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�I�A�I�A�K�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�C�A�=qA�?}A�?}A�E�A�I�A�C�A�E�A�Q�A�VA�XA�XA�XA�ZA�VA�Q�A�O�A�Q�A�S�A�S�A�Q�A�Q�A�XA�S�A�Q�A�S�A�VA�M�A�A�A�=qA�?}A�A�A�5?A�+A��A�{A��#A��hA���A��FA� �A��HA�%A���A�M�A���A���A�ĜA���A��\A���A�r�A�VA�v�A��`A�Q�A��-A�(�A��7A��hA�l�A��
A��;A��9A�1A�oA�I�A�ffA���A��A��
A���A�-A��A��
A���A�A�A��A�`BA��uA��#A��jA�  A��A��7A��HA��RA�?}A��yA��PA�+A�-A}�A{|�Ay�mAwAt^5Ar��ArI�AqVAp5?An��Al��Ai��Af�Ab�A`�jA^�A\^5AZ�9AZ�AY��AX�AT�+AP1AM�-AJ��AI?}AG��AD��AAt�A@�yA@A�A?;dA>��A>M�A=ƨA=l�A=%A<��A<��A;�;A;?}A8��A4ffA2��A0VA/p�A.��A.bA-"�A.  A,�`A,��A,5?A+33A*�\A)�A)�A*bA*A)33A(9XA%�A$�uA#��A$�yA%oA%"�A"ffA!O�A�mA�AVA�7AXA��AA�A��A��AdZA�/A�AhsAS�A��A�AdZA7LAz�AG�A�A��An�A��A;dAE�A�hAl�AK�A
��A	�^A��A/A�Az�A$�A�^A~�Al�A�`AAC�@�l�@���@�"�@�O�@��9@���@�~�@���@�X@�hs@�V@�@��#@�$�@���@웦@�b@�@�K�@�z�@�V@�z�@��@�^5@�@�h@ߝ�@�v�@ݩ�@�z�@ە�@�;d@�"�@�E�@�?}@�Q�@�v�@�Ĝ@�b@�dZ@ѩ�@�"�@�=q@��@�bN@�33@�-@��@ȋD@��@Ƈ+@�hs@ě�@ă@î@�33@å�@�j@�r�@��@��T@�x�@�&�@��`@�1@��@�J@���@�|�@�+@�"�@���@���@�|�@��R@���@���@�hs@��9@�@���@�n�@��
@�p�@��`@�r�@�(�@��@���@��!@�G�@��@��D@�I�@�|�@���@���@�n�@�-@�G�@���@��/@��u@�bN@�z�@�9X@�ƨ@�dZ@�K�@�C�@�"�@��@���@��\@�n�@�5?@���@�33@�o@�@���@��H@���@���@��^@���@�p�@�A�@���@�l�@�\)@�"�@�
=@�o@�o@��@��!@�ff@���@���@���@�9X@��@���@�l�@�K�@��!@���@�~�@�^5@�=q@��T@��T@��@�$�@�M�@�^5@�~�@��\@�=q@�$�@�@��7@�X@�7L@��@���@��;@�|�@�K�@��y@��@���@�V@�E�@�5?@�5?@�{@��T@���@���@�/@��@��/@�Z@���@��w@��@���@���@�|�@�l�@�S�@�"�@�ȴ@��+@�=q@�$�@�@��@��@�X@�O�@�V@�z�@��D@�bN@�1@���@��P@�t�@�dZ@�K�@�33@���@��R@�~�@�V@�-@��@�{@�@��#@��-@��h@���@�7L@���@�z�@�1'@��9@��u@���@�|�@�"�@��@�
=@�
=@�@���@�n�@��@��#@��@�&�@�V@��`@���@���@���@�j@� �@��w@��F@��w@���@���@�l�@�"�@�
=@�K�@��P@��P@���@�t�@�33@��@�^5@�M�@�5?@�$�@�$�@�{@��7@�&�@�;@{ƨ@q��@l�@c�
@]/@W�w@M�h@G�@A��@=V@6V@/K�@*�H@&5?@!7L@�@��@Z@v�@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BJ�BI�BI�BI�BH�BH�BG�BF�BC�B>wB6FB2-B(�B�B��B�mB�;B�B��B��B�FB�B��B��B�DB�1B�B{�B~�Bz�BdZBT�BP�BC�B!�B��B�B�/B��B�LB��B�JB�+B�Bw�BaHBK�B6FB/B&�B�B
=B
�B
�)B
��B
�^B
�B
��B
y�B
]/B
I�B
@�B
6FB
�B
1B	��B	�B	�NB	��B	ǮB	��B	�RB	�'B	��B	�hB	x�B	aHB	I�B	=qB	49B	%�B	�B	uB	bB	%B�/B�B�\Bq�B_;BO�B8RB$�B#�B�B�B�B�B!�B&�B1'B49B<jB@�B?}B:^B �B�B�B�B�B"�B-BI�B^5BiyBp�Bp�Bw�By�B|�B�1B�DB�=B�BhsBYBVBl�Bw�B� Bm�BffB[#BJ�B@�BI�BO�BL�BH�BG�BE�BB�B?}B=qBB�BD�BF�BG�BF�BE�BD�BE�BC�B@�B?}B>wB=qB;dB:^B:^B9XB7LB5?B33B1'B0!B/B/B-B)�B&�B$�B#�B!�B �B �B�B�B�B�B�B�B�B�B�BuBhBuB{B�B�B�B�B�B�B�B�B�B�B�B{B�B�B�B�B�B�B{BoBbBPBJBDB
=B1B	7B1B1B1B	7B	7B	7B	7B	7B
=BJBuB�B�B�B%�B49B49B5?B;dBE�BK�BS�BVBXBYB_;BaHBdZBgmBgmBhsBk�Bk�Bm�BjBffBcTBaHBbNBaHB^5B]/B_;BaHBdZBhsBhsBffBgmBjBk�Bm�Bo�Bp�Bq�Bs�Bs�By�B|�B}�B�B�B�+B�JB�hB�{B��B��B��B��B��B��B�B�!B�RB��BĜBƨBǮBȴB��B��B�B�
B�B�ZB�B�B�B�B��B��B��B��B��B	  B	B	%B	B	B	B	B	%B	%B	%B		7B	VB	hB	uB	{B	�B	�B	%�B	.B	2-B	6FB	8RB	9XB	=qB	A�B	H�B	J�B	K�B	M�B	N�B	R�B	T�B	W
B	[#B	\)B	]/B	^5B	_;B	`BB	aHB	bNB	dZB	e`B	ffB	jB	jB	l�B	o�B	r�B	s�B	s�B	s�B	t�B	t�B	u�B	w�B	z�B	}�B	�B	�B	�B	�B	�7B	�PB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�3B	�3B	�?B	�LB	�RB	�XB	�^B	�^B	�^B	�jB	��B	��B	B	��B	��B	��B	��B	��B	��B	��B	��B	ÖB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�)B	�BB	�HB	�NB	�NB	�HB	�HB	�NB	�NB	�ZB	�`B	�fB	�sB	�mB	�sB	�B	��B
B
VB
�B
�B
'�B
1'B
6FB
=qB
B�B
I�B
O�B
VB
[#B
`BB
cTB
hsB
l�B
q�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BJ�BI�BI�BI�BH�BH�BG�BF�BC�B>�B6UB27B(�B�B�B�wB�GB�"B��B��B�PB�B��B��B�KB�;B�B{�BBz�BddBU	BP�BC�B!�B��B�B�7B��B�SB��B�SB�3B�Bw�BaRBK�B6PB/'B&�B�B
KB
�B
�8B
��B
�lB
�!B
��B
y�B
]>B
I�B
@�B
6TB
�B
CB	��B	��B	�`B	�B	��B	��B	�fB	�>B	��B	�}B	x�B	a`B	I�B	=�B	4UB	%�B	�B	�B	}B	AB�LB�,B�|Bq�B__BPB8wB$�B#�B�B�B�B�B!�B&�B14B4EB<�B@�B?�B:iB �B�B�B�B�B"�B-BI�B^YBi�Bp�Bp�Bw�By�B}B�TB�gB�_B�(Bh�BY:BV)Bl�Bw�B�"Bm�Bf�B[HBJ�B@�BI�BPBL�BH�BG�BE�BB�B?�B=�BB�BD�BF�BG�BF�BE�BD�BE�BC�B@�B?�B>�B=�B;�B:�B:�B9dB7qB5dB3ZB1KB0JB/?B/&B-3B*#B&�B$�B#�B!�B �B �B�B�B�B�B�B�B�B�B�B�BwB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BmB\BVBgB
HBWB	DB>BXB>B	]B	BB	^B	CB	DB
HBXB�B�B�B�B&B4_B4_B5cB;�BE�BK�BTBV(BX4BY:B_^BamBd}Bg�Bg�Bh�Bk�Bk�Bm�Bj�Bf�BcwBajBbqBalB^WB]RB_]BalBd}Bh�Bh�Bf�Bg�Bj�Bk�Bm�Bo�Bp�Bq�Bs�Bs�By�B}B~B�*B�5B�LB�iB��B��B��B��B��B��B�B�B�)B�?B�qB��BĻB��B��B��B��B�B�"B�)B�4B�uB��B�B�B��B��B��B��B�B�
B	 B	:B	BB	<B	:B	9B	:B	@B	AB	@B		SB	pB	�B	�B	�B	�B	�B	%�B	.-B	2JB	6aB	8lB	9qB	=�B	A�B	H�B	J�B	K�B	M�B	N�B	SB	UB	W"B	[;B	\BB	]GB	^LB	_TB	`ZB	a`B	bfB	dtB	euB	f~B	j�B	j�B	l�B	o�B	r�B	s�B	s�B	s�B	t�B	t�B	u�B	w�B	z�B	~B	�B	�*B	�0B	�6B	�OB	�fB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�1B	�;B	�BB	�JB	�KB	�RB	�aB	�iB	�lB	�sB	�sB	�sB	��B	��B	��B	£B	��B	��B	��B	��B	��B	��B	��B	��B	éB	ŴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�&B	�*B	�=B	�WB	�[B	�cB	�`B	�[B	�YB	�cB	�bB	�oB	�vB	�{B	�B	�B	�B	��B	��B
2B
jB
�B
�B
(B
1:B
6WB
=�B
B�B
I�B
O�B
VB
[4B
`RB
ceB
h�B
l�B
q�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311448572016053114485720160531144857  AO  ARCAADJP                                                                    20140721230635    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230635  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230635  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531144857  IP                  G�O�G�O�G�O�                