CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:12:40Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7T   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    88   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8<   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8D   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8H   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8P   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8X   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8`   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8d   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8l   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9l   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9x   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^L   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  g�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  yl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130103213  20190523124442  1514_5041_006                   2C  D   APEX                            2041                            062805                          846 @��zF)��1   @��zF)��@6̋C���c:M���1   GPS     Primary sampling: averaged [2dbar-bin averaged]                                                                                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A���A�  A��#A���A˴9A˙�AˑhAˍPAˇ+A˃AˁA˃A�|�A�z�A�t�A�p�A�r�A�p�A�p�A�p�A�l�A�l�A�l�A�jA�hsA�dZA�bNA�`BA�\)A�S�Aʴ9A��
A�VA��7A���A�z�A�VA��\A��A��
A� �A��A�$�A��A��A�ffA���A���A��A��A��A�E�A���A���A��DA�x�A�VA��^A�1A�M�A��A��A�A�M�A�jA���A��RA���A�t�A��;A�33A��/A��DA�"�A�C�A��A�;dA��A���A�x�A��^A��A���A���A���A�jA�"�A��A���A�E�A�ȴA��A��HA�=qA��hA���A�p�A���A��A�33A��A�9XA���A�;dA���A���A�dZA��`A�A�~�A��A��A���A��A~��A}VAzAx  Av  At9XAqp�AnȴAmK�Ak��AjVAidZAh�Ag`BAfbNAdAcAa��A`$�A^Q�A]VA\=qAZ��AY�
AX�jAWl�AV1'AT��AR��ARAP$�ANM�ANJAL��AKO�AJ(�AH��AHAGƨAGC�AFVAE�TAE|�AD��AC�;AC|�AC+AB5?AA�wAA?}A@ffA@ �A>bNA<A�A:r�A8��A7��A6�A5��A5oA3��A3O�A2�\A0��A/A/VA-�mA-p�A+|�A*�A*��A*bNA*$�A)�A)�A(^5A'XA%�TA$�A$  A"��A"9XA!�A!p�A!A �A�`A  A�!A1'A�hA��AhsA(�A~�AA?}A1'A`BAXA�RA1A��A��A/AM�At�A
�uA	��A��AƨAA��AbA�A��A1A`BA��A�-A A�@�J@���@�o@��+@�Q�@�=q@���@�;d@�u@�o@�5?@�p�@��@�I�@�P@���@���@��@�"�@�!@��@�v�@�G�@�^5@܋D@�5?@���@��@���@�&�@�Z@ӍP@�M�@�/@��`@Гu@Ϯ@Χ�@�{@�x�@�|�@�V@�@��@�Z@ǍP@ƸR@���@���@�b@�dZ@��@+@�V@�^5@�A�@���@��@��@��@��@�~�@�7L@��D@�|�@���@���@�O�@��u@��;@���@�33@�-@�7L@���@��F@���@���@�O�@�/@��@���@�r�@���@��j@�(�@���@��w@���@�l�@�
=@�33@���@�z�@���@�I�@� �@��
@�"�@��@��@�bN@���@�\)@���@�$�@�hs@���@���@��F@�|�@�S�@�"�@�+@�l�@��@��!@���@��@�ȴ@���@���@�ff@�7L@��@�r�@�z�@���@�r�@��P@��@�o@�ff@���@�`B@��j@�  @�+@��@��@�G�@�&�@��`@���@�Q�@��;@���@���@��@���@��F@��u@�%@��7@���@�@�n�@�V@�@�p�@�7L@��@���@��`@��@�?}@�%@��/@��j@�Q�@�9X@�(�@��@�  @��m@��
@���@��@��P@�|�@�dZ@�K�@�+@�o@���@��R@���@�~�@�^5@��@�@�`B@�G�@�/@���@��@���@���@��D@�bN@�9X@� �@�  @���@�dZ@�"�@���@��@���@�^5@�M�@�M�@�=q@�@���@���@���@��7@�X@�V@���@��`@��j@���@�z�@�I�@�1@��;@�ƨ@�|�@�;d@�"�@���@��@�v�@�-@���@�?}@��/@���@���@� �@��;@�dZ@�
=@��@��R@�5?@��#@��^@�p�@�/@���@���@���@��D@��@�r�@�Z@�A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A���A�  A��#A���A˴9A˙�AˑhAˍPAˇ+A˃AˁA˃A�|�A�z�A�t�A�p�A�r�A�p�A�p�A�p�A�l�A�l�A�l�A�jA�hsA�dZA�bNA�`BA�\)A�S�Aʴ9A��
A�VA��7A���A�z�A�VA��\A��A��
A� �A��A�$�A��A��A�ffA���A���A��A��A��A�E�A���A���A��DA�x�A�VA��^A�1A�M�A��A��A�A�M�A�jA���A��RA���A�t�A��;A�33A��/A��DA�"�A�C�A��A�;dA��A���A�x�A��^A��A���A���A���A�jA�"�A��A���A�E�A�ȴA��A��HA�=qA��hA���A�p�A���A��A�33A��A�9XA���A�;dA���A���A�dZA��`A�A�~�A��A��A���A��A~��A}VAzAx  Av  At9XAqp�AnȴAmK�Ak��AjVAidZAh�Ag`BAfbNAdAcAa��A`$�A^Q�A]VA\=qAZ��AY�
AX�jAWl�AV1'AT��AR��ARAP$�ANM�ANJAL��AKO�AJ(�AH��AHAGƨAGC�AFVAE�TAE|�AD��AC�;AC|�AC+AB5?AA�wAA?}A@ffA@ �A>bNA<A�A:r�A8��A7��A6�A5��A5oA3��A3O�A2�\A0��A/A/VA-�mA-p�A+|�A*�A*��A*bNA*$�A)�A)�A(^5A'XA%�TA$�A$  A"��A"9XA!�A!p�A!A �A�`A  A�!A1'A�hA��AhsA(�A~�AA?}A1'A`BAXA�RA1A��A��A/AM�At�A
�uA	��A��AƨAA��AbA�A��A1A`BA��A�-A A�@�J@���@�o@��+@�Q�@�=q@���@�;d@�u@�o@�5?@�p�@��@�I�@�P@���@���@��@�"�@�!@��@�v�@�G�@�^5@܋D@�5?@���@��@���@�&�@�Z@ӍP@�M�@�/@��`@Гu@Ϯ@Χ�@�{@�x�@�|�@�V@�@��@�Z@ǍP@ƸR@���@���@�b@�dZ@��@+@�V@�^5@�A�@���@��@��@��@��@�~�@�7L@��D@�|�@���@���@�O�@��u@��;@���@�33@�-@�7L@���@��F@���@���@�O�@�/@��@���@�r�@���@��j@�(�@���@��w@���@�l�@�
=@�33@���@�z�@���@�I�@� �@��
@�"�@��@��@�bN@���@�\)@���@�$�@�hs@���@���@��F@�|�@�S�@�"�@�+@�l�@��@��!@���@��@�ȴ@���@���@�ff@�7L@��@�r�@�z�@���@�r�@��P@��@�o@�ff@���@�`B@��j@�  @�+@��@��@�G�@�&�@��`@���@�Q�@��;@���@���@��@���@��F@��u@�%@��7@���@�@�n�@�V@�@�p�@�7L@��@���@��`@��@�?}@�%@��/@��j@�Q�@�9X@�(�@��@�  @��m@��
@���@��@��P@�|�@�dZ@�K�@�+@�o@���@��R@���@�~�@�^5@��@�@�`B@�G�@�/@���@��@���@���@��D@�bN@�9X@� �@�  @���@�dZ@�"�@���@��@���@�^5@�M�@�M�@�=q@�@���@���@���@��7@�X@�V@���@��`@��j@���@�z�@�I�@�1@��;@�ƨ@�|�@�;d@�"�@���@��@�v�@�-@���@�?}@��/@���@���@� �@��;@�dZ@�
=@��@��R@�5?@��#@��^@�p�@�/@���@���@���@��D@��@�r�@�Z@�A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB1'B0!B0!B0!B2-B1'B2-B33B33B33B33B33B2-B2-B2-B2-B2-B2-B2-B33B33B33B33B33B49B49B49B49B49B49B49B(�B��B�B�B�B�B�B��B��B��B��B��B��B��B  B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�TB�B��B��BɺBB�wB�qB�9B�3B�B��B��B�PB�+B~�Bn�BYB>wB5?B1'B-B�B
=B��B��B��B�HBŢB�RB��B�+Bn�BZB?}B%�B{BPB+B
�B
�B
ƨB
�}B
�3B
��B
�\B
�3B
��B
�oB
� B
n�B
^5B
O�B
B�B
/B
�B
uB
B	��B	�ZB	�B	��B	ǮB	��B	�wB	�9B	�B	��B	��B	�oB	�PB	�B	}�B	z�B	v�B	r�B	p�B	k�B	_;B	VB	J�B	A�B	>wB	49B	-B	(�B	$�B	�B	�B	�B	�B	�B	hB	VB	JB	JB	
=B	1B	B	B	  B	  B��B��B�B�NB�B��BȴBĜB�}B�qB�dB��B��B�XB�'B�B�B��B��B��B��B��B��B��B��B��B�oB�\B�PB�JB�+B�%B�B�B� B{�Bx�Bw�Bt�Bs�Bq�Bq�Bo�Bm�BjBiyBn�Bm�Be`B]/B\)B\)BW
BYB\)BP�BR�BM�BN�BL�BH�BF�BD�BB�B?}B<jB:^B8RB9XB33B0!B/B-B+B)�B&�B$�B#�B!�B �B�B�B�B�B�B�B�B�B�B�B�B �B�B�B �B�B�B�B�B�B�B�B�B�B�B�B{B�B�B�BuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B"�B$�B%�B&�B&�B(�B)�B)�B+B,B,B/B1'B2-B49B6FB9XB:^B:^B:^B9XB9XB>wBD�BE�BF�BH�BL�BP�BYBaHBjBv�Bz�B�B�B�%B�%B�+B�=B�JB�VB�bB��B��B��B��B��B�B�'B�-B�9B�dBǮB��B�
B�B�5B�BB�ZB�fB�mB�B�B�B��B��B��B	B	+B	
=B	PB	PB	PB	bB	oB	oB	uB	�B	�B	�B	 �B	$�B	%�B	+B	1'B	6FB	<jB	>wB	A�B	M�B	W
B	bNB	dZB	jB	s�B	{�B	}�B	}�B	}�B	~�B	� B	�B	�B	�+B	�1B	�7B	�=B	�PB	�VB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�!B	�'B	�-B	�-B	�3B	�3B	�FB	�LB	�RB	�XB	�^B	�qB	�}B	��B	��B	��B	ĜB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�5B	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B1'B0!B1'B0!B2-B2-B2-B33B33B33B49B33B2-B2-B2-B2-B2-B2-B2-B33B33B33B33B49B49B49B49B49B49B49B6FB33B��B�B�B�B�B��B��B��B��B��B��B��B  BBB  BB  B��B��B��B��B��B��B��B��B��B��B�B�B�B�fB�/B��B��B��BĜB��B��B�FB�?B�!B�B��B�\B�=B�Bs�BaHBA�B6FB2-B2-B%�BhB��B��B��B�sB��B�wB�B�VBr�B^5BC�B)�B�B\BJB
�B
�B
ɺB
��B
�XB
��B
�\B
�FB
�B
��B
�B
s�B
cTB
S�B
I�B
49B
#�B
�B

=B	��B	�yB	�/B	�B	��B	ÖB	B	�LB	�?B	��B	��B	��B	�oB	�1B	�B	~�B	y�B	u�B	s�B	m�B	aHB	ZB	L�B	E�B	B�B	5?B	0!B	,B	'�B	!�B	�B	�B	�B	�B	oB	\B	PB	\B	DB		7B	+B	B	B	B��B��B�B�mB�5B��B��BƨB��B��B�qBÖBĜB�jB�3B�-B�B�B��B��B��B��B��B��B��B��B��B�oB�bB�bB�7B�+B�%B�B�B� B{�B{�Bv�Bu�Bt�Bu�Bs�Bq�Bl�Bk�Bp�Bo�BiyB^5B^5B_;BYB[#B^5BR�BT�BO�BQ�BO�BJ�BG�BE�BD�BA�B>wB=qB:^B<jB7LB49B2-B.B,B-B)�B&�B%�B%�B"�B �B�B�B�B�B�B�B �B�B�B�B"�B�B�B"�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B#�B"�B$�B%�B'�B(�B(�B)�B+B+B,B,B.B0!B2-B49B6FB8RB:^B:^B:^B:^B:^B<jBA�BE�BF�BG�BI�BL�BP�BYB`BBiyBv�B{�B�B�%B�+B�1B�7B�DB�PB�VB�hB��B��B��B��B��B�B�'B�3B�?B�dBȴB��B�
B�B�5B�BB�ZB�mB�yB�B�B��B��B��B��B	B	+B	DB	VB	VB	VB	hB	uB	uB	{B	�B	�B	�B	!�B	%�B	&�B	,B	1'B	6FB	<jB	>wB	@�B	L�B	VB	bNB	dZB	jB	s�B	|�B	~�B	~�B	}�B	~�B	� B	�B	�B	�+B	�1B	�7B	�=B	�PB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�-B	�-B	�3B	�9B	�FB	�RB	�RB	�XB	�^B	�qB	�}B	��B	��B	B	ŢB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181657222011121816572220111218165722  AO  ARGQ                                                                        20111130103213  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130103213  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165722  IP                  G�O�G�O�G�O�                