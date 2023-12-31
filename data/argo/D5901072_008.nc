CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:41Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143544  20190522121828  1728_5048_008                   2C  D   APEX                            2142                            040306                          846 @�>�~ܠ1   @�>�`��@3Y������c��1'1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @,��@�  @���A   A   AA��A`  A�  A�  A�  A�  A�  A�  A���A�  B   BffB  B  B   B(  B0ffB8  B@  BH  BO��BX  B`ffBhffBpffBx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�33B�33B�  B���B�  B�33B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  C   C�fC  C  C  C
  C  C�C�C  C  C�C�C�C�C  C�fC"  C$  C&  C(  C*�C,  C.  C0�C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH�CJ�CL  CN  CP  CR  CS�fCV  CX  CY�fC\  C^�C`  Ca�fCd  Cf�Ch�Cj  Cl  Cm�fCp  Cr�Ct  Cv  Cx  Cz  C|  C~�C��C��C��C��C��C��C��C�  C��3C��3C��3C�  C�  C�  C��3C��3C�  C��C��C��C�  C�  C�  C�  C�  C��C��C�  C��3C�  C��C��C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C��C��C��C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C��C�  C��3C��3C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C��C�  C�  D   D � D  D�fD  Dy�D  D� D  D�fDfD�fDfD�fD  Dy�D  D� D	fD	�fD
  D
� D  D� DfD� D  D�fD  D� D  D� D  D�fD  Dy�D��D� DfD�fDfD�fD  D� D  D�fDfD�fD  Dy�D��D� D  D� D  D�fD  Dy�D  D� D  Dy�D  D�fD fD � D!  D!� D"  D"� D"��D#y�D#��D$� D%  D%� D&  D&y�D&��D'y�D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D-��D.� D/  D/� D0  D0� D0��D1� D2fD2� D2��D3� D4fD4� D5  D5y�D5��D6� D7  D7� D8  D8� D9  D9� D9��D:� D;  D;� D<  D<� D=  D=�fD>  D>y�D>��D?� D@  D@� DA  DA� DB  DBy�DC  DC� DC��DD� DE  DE� DE��DF� DG  DG� DH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDXfDX� DY  DYy�DZ  DZ�fD[  D[� D\  D\� D]  D]y�D]��D^� D_fD_�fD`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Di  Di�fDj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Duy�Du��Dv� Dv�fDy�fD�	�D�L�D���D���D�� D�<�D�\�D��3D��D�,�D���DǼ�D��fD�3D�Y�D���D�� D�  D�L�D�Vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @&ff@y��@���@���AffA@  A^ffA~ffA�33A�33A�33A�33A�33A�  A�33A�33B  B��B��B��B'��B0  B7��B?��BG��BO33BW��B`  Bh  Bp  Bw��B33B���B���B���B���B���B���B���B���B���B���B�  B���B���B�  B�  B�  B���BǙ�B���B�  B���B���B�  B���B���B���B���B�  B���B���B���B���C��C�fC�fC�fC	�fC�fC  C  C�fC�fC  C  C  C  C�fC��C!�fC#�fC%�fC'�fC*  C+�fC-�fC0  C1�fC3�fC5�fC7�fC:  C;�fC=�fC?�fCA�fCC�fCE�fCH  CJ  CK�fCM�fCO�fCQ�fCS��CU�fCW�fCY��C[�fC^  C_�fCa��Cc�fCf  Ch  Ci�fCk�fCm��Co�fCr  Cs�fCu�fCw�fCy�fC{�fC~  C�  C�  C�  C�  C�  C�  C�  C��3C��fC��fC��fC��3C��3C��3C��fC��fC��3C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C��3C��fC��3C�  C�  C��3C��3C��fC��3C��3C��fC��3C��3C��fC��3C�  C��3C��fC��fC��3C��3C��3C��3C��3C�  C�  C�  C��3C��fC��fC��fC��fC��fC��3C��3C��3C��3C�  C��3C��fC��fC��fC��fC��fC��fC��fC��3C��3C��3C��3C��3C��3C��3C��fC��fC��3C�  C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C�  C�  C�  C��3C��3C��3D y�D ��D� D��Ds3D��Dy�D��D� D  D� D  D� D��Ds3D��Dy�D	  D	� D	��D
y�D
��Dy�D  Dy�D��D� D��Dy�D��Dy�D��D� D��Ds3D�3Dy�D  D� D  D� D��Dy�D��D� D  D� D��Ds3D�3Dy�D��Dy�D��D� D��Ds3D��Dy�D��Ds3D��D� D   D y�D ��D!y�D!��D"y�D"�3D#s3D#�3D$y�D$��D%y�D%��D&s3D&�3D's3D'��D(y�D(��D)y�D)��D*y�D+  D+y�D+��D,y�D,��D-y�D-�3D.y�D.��D/y�D/��D0y�D0�3D1y�D2  D2y�D2�3D3y�D4  D4y�D4��D5s3D5�3D6y�D6��D7y�D7��D8y�D8��D9y�D9�3D:y�D:��D;y�D;��D<y�D<��D=� D=��D>s3D>�3D?y�D?��D@y�D@��DAy�DA��DBs3DB��DCy�DC�3DDy�DD��DEy�DE�3DFy�DF��DGy�DG��DHs3DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DP� DP��DQy�DR  DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DW� DX  DXy�DX��DYs3DY��DZ� DZ��D[y�D[��D\y�D\��D]s3D]�3D^y�D_  D_� D_��D`y�D`��Day�Da�3Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgs3Dg��Dhy�Dh��Di� Di��Djs3Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Dus3Du�3Dvy�Dv� Dy� D�fD�I�D��fD���D���D�9�D�Y�D�� D�	�D�)�D��fDǹ�D��3D� D�VfD�ɚD���D��D�I�D�S311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aى7Aه+A�z�A�Q�A�&�A�A��A�A�A�bA���A��`A���A���A�ĜAּjAְ!A։7A�^5A�O�A�E�A�=qA�5?A� �A�A���A՝�A�ZA� �A�=qA�-A�A���AԼjA�O�A�|�A��mAҶFAғuA�n�A�-A�+A�XA�?}A��A���A˛�A�|�A��HA�\)A���A�
=A��wA�=qA��;A��/A�r�A��DA�A�?}A�7LA���A�p�A���A���A�A�A��mA��jA�oA�+A��\A�~�A�z�A�n�A�"�A�VA��jA��^A�E�A�dZA��A�Q�A�n�A��A�|�A��9A�7LA���A�E�A�n�A�VA�^5A���A�VA��A��A�l�A�&�A���A�{A��A���A�jA���A��A��A�l�A���A� �A��DA�&�A�ĜA~ĜAz��Ax�uAudZAo�Aj-Af$�A`��A]?}AW�FAT1AP�RAM�7AKC�AJ�uAI\)AG�AE�mAC��AB��ABr�AA�hA@ �A;?}A:1'A9?}A6�A4��A3/A1
=A/��A/A.9XA,�jA*JA)l�A(ȴA'�A&�9A%�A%"�A"��A n�AbNA%AE�A�FAK�A��A-A&�A1'A�AS�A�!A`BA=qA&�AM�A|�A��A�A�RAƨA�yAz�A��A
��A	�
A�uA��Al�A��A�
AO�A%A�\A�FA �jA �@���@��@�1'@��@���@��/@�bN@�  @��@�7L@�j@��@�"�@�!@���@�?}@��/@��;@�"�@�+@�^@�D@�"�@柾@��T@��@���@�;d@�v�@�z�@ޏ\@�J@�@��@ܣ�@�9X@� �@�  @ۮ@�ȴ@�5?@�{@��#@�O�@��/@�j@���@���@�/@ԓu@ԃ@ԋD@�r�@�9X@��
@ҸR@�J@�/@�bN@�1@�ƨ@�C�@�n�@͡�@�1'@�dZ@ʟ�@���@ə�@�?}@��`@ȃ@�Q�@�b@��m@�t�@���@�^5@��@��@��#@Ų-@Ł@��`@Ý�@��H@�V@�x�@��@�(�@���@��@�n�@�E�@�5?@���@�p�@��@��j@���@�r�@��@��P@��R@���@��@��/@�(�@��F@�C�@��\@���@��-@�O�@��@�V@��/@��@�Q�@���@�ƨ@�S�@��H@��^@��@�Ĝ@��u@�z�@�bN@�A�@�9X@�9X@�1'@�9X@�1'@�(�@�  @��@���@��@�S�@��H@�=q@��^@�hs@�V@���@��D@� �@���@��F@��F@��F@��w@��w@��w@���@�;d@���@��\@�V@�E�@�-@��@�$�@�$�@�G�@��D@��;@��P@�K�@�+@�o@���@��y@���@�n�@�5?@�G�@��@�ƨ@���@���@��@���@�C�@��y@���@���@��\@�-@��@��@��D@�A�@�1@��m@��F@��@�\)@�
=@��+@�@��T@���@��^@�`B@���@��u@�1'@��;@��F@���@���@���@��P@��@�t�@�;d@���@�5?@��7@�?}@��@��/@�z�@�bN@�I�@�9X@� �@��@��
@��F@�t�@�33@���@��@��@�&�@�V@��`@��D@�Q�@� �@�  @���@���@�|�@�t�@�dZ@�dZ@�dZ@�C�@�"�@���@���@�~�@�E�@��@���@��#@��^@��@��@��9@�z�@�(�@��@�ƨ@��w@��w@��w@�ƨ@��w@���@�l�@�
=@��!@�ff@�@��#@��-@��@�&�@���@���@���@�r�@�Q�@�I�@�A�@��@�|�@�+@��y@�v�@�E�@��@���@��#@���@�`B@���@�ƨ@��@{t�@t9X@k��@d�j@^��@X�9@O�P@F��@Ahs@;��@7|�@1�@*n�@$�/@��@��@��@-@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aى7Aه+A�z�A�Q�A�&�A�A��A�A�A�bA���A��`A���A���A�ĜAּjAְ!A։7A�^5A�O�A�E�A�=qA�5?A� �A�A���A՝�A�ZA� �A�=qA�-A�A���AԼjA�O�A�|�A��mAҶFAғuA�n�A�-A�+A�XA�?}A��A���A˛�A�|�A��HA�\)A���A�
=A��wA�=qA��;A��/A�r�A��DA�A�?}A�7LA���A�p�A���A���A�A�A��mA��jA�oA�+A��\A�~�A�z�A�n�A�"�A�VA��jA��^A�E�A�dZA��A�Q�A�n�A��A�|�A��9A�7LA���A�E�A�n�A�VA�^5A���A�VA��A��A�l�A�&�A���A�{A��A���A�jA���A��A��A�l�A���A� �A��DA�&�A�ĜA~ĜAz��Ax�uAudZAo�Aj-Af$�A`��A]?}AW�FAT1AP�RAM�7AKC�AJ�uAI\)AG�AE�mAC��AB��ABr�AA�hA@ �A;?}A:1'A9?}A6�A4��A3/A1
=A/��A/A.9XA,�jA*JA)l�A(ȴA'�A&�9A%�A%"�A"��A n�AbNA%AE�A�FAK�A��A-A&�A1'A�AS�A�!A`BA=qA&�AM�A|�A��A�A�RAƨA�yAz�A��A
��A	�
A�uA��Al�A��A�
AO�A%A�\A�FA �jA �@���@��@�1'@��@���@��/@�bN@�  @��@�7L@�j@��@�"�@�!@���@�?}@��/@��;@�"�@�+@�^@�D@�"�@柾@��T@��@���@�;d@�v�@�z�@ޏ\@�J@�@��@ܣ�@�9X@� �@�  @ۮ@�ȴ@�5?@�{@��#@�O�@��/@�j@���@���@�/@ԓu@ԃ@ԋD@�r�@�9X@��
@ҸR@�J@�/@�bN@�1@�ƨ@�C�@�n�@͡�@�1'@�dZ@ʟ�@���@ə�@�?}@��`@ȃ@�Q�@�b@��m@�t�@���@�^5@��@��@��#@Ų-@Ł@��`@Ý�@��H@�V@�x�@��@�(�@���@��@�n�@�E�@�5?@���@�p�@��@��j@���@�r�@��@��P@��R@���@��@��/@�(�@��F@�C�@��\@���@��-@�O�@��@�V@��/@��@�Q�@���@�ƨ@�S�@��H@��^@��@�Ĝ@��u@�z�@�bN@�A�@�9X@�9X@�1'@�9X@�1'@�(�@�  @��@���@��@�S�@��H@�=q@��^@�hs@�V@���@��D@� �@���@��F@��F@��F@��w@��w@��w@���@�;d@���@��\@�V@�E�@�-@��@�$�@�$�@�G�@��D@��;@��P@�K�@�+@�o@���@��y@���@�n�@�5?@�G�@��@�ƨ@���@���@��@���@�C�@��y@���@���@��\@�-@��@��@��D@�A�@�1@��m@��F@��@�\)@�
=@��+@�@��T@���@��^@�`B@���@��u@�1'@��;@��F@���@���@���@��P@��@�t�@�;d@���@�5?@��7@�?}@��@��/@�z�@�bN@�I�@�9X@� �@��@��
@��F@�t�@�33@���@��@��@�&�@�V@��`@��D@�Q�@� �@�  @���@���@�|�@�t�@�dZ@�dZ@�dZ@�C�@�"�@���@���@�~�@�E�@��@���@��#@��^@��@��@��9@�z�@�(�@��@�ƨ@��w@��w@��w@�ƨ@��w@���@�l�@�
=@��!@�ff@�@��#@��-@��@�&�@���@���@���@�r�@�Q�@�I�@�A�@��@�|�@�+@��y@�v�@�E�@��@���@��#@���@�`B@���@�ƨ@��@{t�@t9X@k��@d�j@^��@X�9@O�P@F��@Ahs@;��@7|�@1�@*n�@$�/@��@��@��@-@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B�uB�PB�+B� B~�B~�B}�B|�B|�B|�Bz�By�Bu�Br�Bq�Bp�Bp�Bo�Bm�Bk�BgmBcTB^5B]/Bn�Br�Bo�BhsBk�BjB`BBP�BP�BP�BS�BXBgmBo�By�B�%B�DB�PB�JB�uB��BbB�B/B49BD�BJ�BP�BVBZBdZB�B�{B��B��B��B��B��B��B�B�B�B�B�B�B�B�9B�-B�3B�B�=Bl�BP�B33B�B��B�B��B��B��B��B�BB�mBɺB�dB�9B��B��B�B|�Bu�Be`BA�B&�BhB
��B
��B
�XB
��B
��B
�B
YB
=qB
�B

=B	�B	��B	�+B	bNB	<jB	�B��B��B��B��B��B�)B�B�B�fB�5B�B�B��B��BĜB�qB�FB�!B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�JB�1B�1B�7B�VB�JB�JB�=B�=B�+B�B�%B�1B�%B�%B�B�%B�B�B�B�B� B�B�B�+B�7B�=B�=B�7B�JB�=B�JB�VB�VB�oB�hB�hB�hB��B��B�{B�oB�VB�7B�1B�+B�B�+B�%B�B�B�B�B�B�B�+B�DB�DB�VB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�-B�-B�'B�?B�LB�LB�dB�wB�}B��BÖBŢBĜBȴB��B��B��B��B��B��B�B�
B�
B�B�B�#B�NB�mB�B��B��B��B��B��B	  B	B	B	B	B	B	1B		7B	
=B	JB	JB	JB	JB	bB	�B	�B	�B	$�B	)�B	.B	1'B	7LB	9XB	9XB	9XB	;dB	?}B	C�B	D�B	D�B	E�B	G�B	J�B	P�B	VB	YB	\)B	_;B	aHB	bNB	cTB	bNB	aHB	aHB	cTB	ffB	hsB	hsB	gmB	gmB	hsB	iyB	l�B	l�B	m�B	m�B	m�B	n�B	o�B	p�B	p�B	p�B	p�B	p�B	p�B	p�B	q�B	u�B	u�B	v�B	w�B	z�B	~�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�DB	�JB	�PB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�?B	�LB	�^B	�jB	�wB	��B	��B	B	B	ÖB	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	ȴB	ȴB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	�B	�B	�#B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�NB	�NB	�TB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�fB	�mB	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
	7B
uB
�B
�B
'�B
+B
0!B
5?B
=qB
D�B
H�B
M�B
Q�B
W
B
^5B
cTB
iyB
l�B
r�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B�oB�PB�B� B� B~�B|�B|�B|�B{�Bz�Bv�Br�Bq�Bp�Bp�Bp�Bn�Bm�BiyBe`B_;B\)Bo�Bt�Bq�BjBo�Bq�BdZBR�BQ�BR�BXBcTBy�B�B�B�7B�PB�hB��B��B��B"�B+B8RBC�BQ�B`BBp�Bp�Bk�Bn�B�7B��B��B��B��B��B�B�B�?B�9B�!B�B�B�-B�FB�qB��BȴB��B��Bv�B]/BB�B2-B1B�;B�B��B�B�;B�B��B�BƨBÖB�RB��B�JB�7B�7B�BVB?}B,BbB
�sB
��B
�RB
�-B
��B
o�B
^5B
7LB
�B
VB	��B	�B	�B	aHB	5?B	�B�B�ZB�ZB�/B�TB��B��B�B�B�HB�/B�/B�/B�HBǮB��B��B�^B�B�B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B�oB�bB�VB�\B�PB�PB�JB�JB�JB�PB�VB�bB�JB�JB�DB�DB�7B�1B�7B�PB�VB�hB�oB�oB��B��B��B�{B�{B��B�{B��B��B��B��B��B��B��B��B�bB�PB�+B�=B�DB�7B�%B�B�B�%B�+B�=B�VB�\B�hB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�'B�'B�3B�3B�9B�9B�LB�RB�RB�qB��B��BÖBǮB��BƨBȴB��B��B��B��B��B�
B�B�B�B�B�)B�5B�fB�B�B��B��B��B��B��B	B	B	B	B	B	B	1B		7B	DB	JB	PB	VB	JB	bB	�B	�B	!�B	'�B	)�B	0!B	33B	7LB	:^B	:^B	:^B	=qB	A�B	C�B	E�B	D�B	E�B	I�B	J�B	P�B	VB	YB	^5B	aHB	cTB	bNB	cTB	bNB	bNB	aHB	cTB	gmB	iyB	iyB	hsB	gmB	jB	iyB	o�B	n�B	n�B	m�B	m�B	n�B	o�B	p�B	p�B	p�B	p�B	p�B	p�B	p�B	q�B	u�B	v�B	w�B	y�B	z�B	~�B	�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�DB	�JB	�PB	�VB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�FB	�?B	�LB	�jB	�qB	�}B	��B	B	ÖB	B	ĜB	ĜB	ÖB	ŢB	ǮB	ǮB	ǮB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�;B	�BB	�;B	�HB	�NB	�TB	�ZB	�`B	�ZB	�TB	�NB	�NB	�NB	�ZB	�`B	�fB	�sB	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B

=B
uB
�B
�B
'�B
+B
0!B
5?B
=qB
D�B
H�B
M�B
Q�B
W
B
^5B
cTB
iyB
l�B
r�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�t�<�C�<#�
<#�
<#�
<#�
<#�
<�9X='�<�t�<D��<#�
<u<T��<�1<��<���<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<�1<�h<u<#�
<D��<u<�9X<�C�<#�
<#�
<#�
<D��<u<e`B<��
<�o<49X<u<u<u<#�
<D��<���<�/<���<ě�<���<ě�<�1<���<u<���<�<�9X<��<���<�1<�/=,1=\)=o=\)<�=+<���<�j<�1<e`B<#�
<49X<e`B<D��<D��<#�
<#�
<#�
<�C�<�`B<#�
<49X<�C�<�o<49X<e`B<#�
<#�
<#�
<D��<�o<#�
<#�
<#�
<#�
<#�
<49X<���<�o<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451552012011014515520120110145155  AO  ARGQ                                                                        20111130143544  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143544  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145155  IP                  G�O�G�O�G�O�                