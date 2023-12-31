CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:13Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               OA   AO  20111130140926  20190522121826  1727_5046_079                   2C  D   APEX                            2143                            040306                          846 @�{«�{�1   @�{�Q�o�@8�1&�y�d`A�7L1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD�fD  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D"��D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D0��D1� D2  D2� D3  D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dss3Dy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]��C_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D  D� D��Ds3D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"s3D"�3D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0�3D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5�3D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@s3D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DL  DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU�3DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`s3D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��De� De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsl�Dy� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��yAɧ�A�r�A�S�A� �A�A��yA���A�AȺ^AȰ!AȬAȥ�Aȥ�Aȟ�Aț�AȑhAȉ7A�p�A�=qAǃAƉ7A��AîA�
=A�JA���A���A���A���A�;dA�
=A�t�A�A�A���A��FA���A�I�A��A�"�A���A�G�A�\)A�v�A�(�A��9A�S�A�M�A��^A�VA���A�+A�ƨA���A�ZA��uA��FA���A�|�A�
=A��FA�;dA��A��A�hsA��A� �A��9A��A�M�A�1A�`BA�l�A��/A�&�A��uA�ZA�A�bA�l�A��`A��FA�33A���A��;A��TA��A�O�A�ƨA�t�A��A��;A���A�C�A�hsA��A��A�ĜA�ffA���A��A��+A��!A�K�A�9XA��A�dZA�33A�O�A�XA��FA��A}A|��AzI�Axv�Aw�7Au�
At��Ar�jAq;dAn�Al��Aj��Ah�`AfbNAb�9A`�9A_"�A^�RA]��A\��A\n�AZ��AX1'AW�AU�wAS�-AR$�AQ�-AP��AO��ANĜAM��AM|�ALĜAL-AK��AK"�AIAH�AH��AGAGC�AF�AD��AC��AA�#A@jA?oA>JA=�A=/A;�mA:�/A9��A8�/A81'A7�hA6~�A5|�A4�A4I�A4{A3XA2�A2$�A1�A/�FA/S�A.�A-t�A-G�A,��A, �A,JA,JA+��A*��A)`BA(bNA&��A&M�A%��A%+A$��A$1'A#��A#`BA"M�A ffA?}A(�At�A�A�A/A��A7LA��A�A�^A+A�A�TA�HA�-AC�AE�A��A�A�A��A^5AJAXA%A�!AjA�A�A
�DA	��A��AI�A�PAv�AC�AAx�A��AZA �`A A�@�
=@��`@� �@�$�@�?}@�  @���@��@��7@�Z@�@��;@��@�@� �@�+@�@�@��@�r�@�S�@�G�@�\)@�M�@�p�@�bN@�\)@��#@ۮ@ڏ\@ٙ�@��`@ش9@�t�@�I�@��H@��`@ΰ!@��@��`@�j@��;@˝�@�l�@ɡ�@�n�@�`B@ģ�@� �@�V@�1'@�;d@��+@���@�G�@���@��m@�@���@���@�hs@�&�@�1@�|�@��@��\@�E�@��T@�&�@�r�@�b@��y@���@���@�bN@���@��@�{@�p�@�?}@�j@�b@��^@�/@��@���@��F@���@���@�+@��
@�b@��@�33@�5?@��@��H@���@�M�@�=q@�$�@�$�@�$�@��@���@��^@��h@�O�@�%@��@��9@�A�@��;@�dZ@�o@��@��\@�`B@�&�@��@�|�@���@�v�@�v�@��\@�n�@���@��@��@��/@���@�z�@��@���@�@���@�\)@��@�`B@��/@���@���@�M�@�5?@�5?@��#@��h@�7L@�G�@��m@�r�@�Ĝ@��@��9@�z�@�r�@��D@��@��@��u@�r�@�z�@�b@��w@��@��@���@��@�n�@�V@�M�@�=q@��T@�x�@���@�z�@�bN@� �@�33@��h@�=q@�{@���@��#@�x�@�&�@��@�?}@��9@���@�l�@�\)@�o@�33@��H@��R@���@���@�^5@�7L@�&�@���@���@���@�Q�@���@�S�@�+@�o@��!@��@���@��^@�X@�Q�@���@���@���@�|�@�|�@�t�@���@�~�@���@��@���@��D@�t�@��y@��!@���@�M�@�{@���@���@�x�@�p�@�hs@�O�@���@���@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��yAɧ�A�r�A�S�A� �A�A��yA���A�AȺ^AȰ!AȬAȥ�Aȥ�Aȟ�Aț�AȑhAȉ7A�p�A�=qAǃAƉ7A��AîA�
=A�JA���A���A���A���A�;dA�
=A�t�A�A�A���A��FA���A�I�A��A�"�A���A�G�A�\)A�v�A�(�A��9A�S�A�M�A��^A�VA���A�+A�ƨA���A�ZA��uA��FA���A�|�A�
=A��FA�;dA��A��A�hsA��A� �A��9A��A�M�A�1A�`BA�l�A��/A�&�A��uA�ZA�A�bA�l�A��`A��FA�33A���A��;A��TA��A�O�A�ƨA�t�A��A��;A���A�C�A�hsA��A��A�ĜA�ffA���A��A��+A��!A�K�A�9XA��A�dZA�33A�O�A�XA��FA��A}A|��AzI�Axv�Aw�7Au�
At��Ar�jAq;dAn�Al��Aj��Ah�`AfbNAb�9A`�9A_"�A^�RA]��A\��A\n�AZ��AX1'AW�AU�wAS�-AR$�AQ�-AP��AO��ANĜAM��AM|�ALĜAL-AK��AK"�AIAH�AH��AGAGC�AF�AD��AC��AA�#A@jA?oA>JA=�A=/A;�mA:�/A9��A8�/A81'A7�hA6~�A5|�A4�A4I�A4{A3XA2�A2$�A1�A/�FA/S�A.�A-t�A-G�A,��A, �A,JA,JA+��A*��A)`BA(bNA&��A&M�A%��A%+A$��A$1'A#��A#`BA"M�A ffA?}A(�At�A�A�A/A��A7LA��A�A�^A+A�A�TA�HA�-AC�AE�A��A�A�A��A^5AJAXA%A�!AjA�A�A
�DA	��A��AI�A�PAv�AC�AAx�A��AZA �`A A�@�
=@��`@� �@�$�@�?}@�  @���@��@��7@�Z@�@��;@��@�@� �@�+@�@�@��@�r�@�S�@�G�@�\)@�M�@�p�@�bN@�\)@��#@ۮ@ڏ\@ٙ�@��`@ش9@�t�@�I�@��H@��`@ΰ!@��@��`@�j@��;@˝�@�l�@ɡ�@�n�@�`B@ģ�@� �@�V@�1'@�;d@��+@���@�G�@���@��m@�@���@���@�hs@�&�@�1@�|�@��@��\@�E�@��T@�&�@�r�@�b@��y@���@���@�bN@���@��@�{@�p�@�?}@�j@�b@��^@�/@��@���@��F@���@���@�+@��
@�b@��@�33@�5?@��@��H@���@�M�@�=q@�$�@�$�@�$�@��@���@��^@��h@�O�@�%@��@��9@�A�@��;@�dZ@�o@��@��\@�`B@�&�@��@�|�@���@�v�@�v�@��\@�n�@���@��@��@��/@���@�z�@��@���@�@���@�\)@��@�`B@��/@���@���@�M�@�5?@�5?@��#@��h@�7L@�G�@��m@�r�@�Ĝ@��@��9@�z�@�r�@��D@��@��@��u@�r�@�z�@�b@��w@��@��@���@��@�n�@�V@�M�@�=q@��T@�x�@���@�z�@�bN@� �@�33@��h@�=q@�{@���@��#@�x�@�&�@��@�?}@��9@���@�l�@�\)@�o@�33@��H@��R@���@���@�^5@�7L@�&�@���@���@���@�Q�@���@�S�@�+@�o@��!@��@���@��^@�X@�Q�@���@���@���@�|�@�|�@�t�@���@�~�@���@��@���@��D@�t�@��y@��!@���@�M�@�{@���@���@�x�@�p�@�hs@�O�@���@���@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBaHBbNBaHBaHBaHBaHBaHBcTBdZBdZBcTBe`Be`Be`Be`Be`Be`Be`Be`BdZBaHB\)BZBXBW
BT�BP�BL�BH�BC�B49B�B	7B��B�B�B�sB�HB�;B�)B�B��B�
B��B��BǮBɺB��BȴB��B�dB�wB��B��B��B�wB�RB�!B��B��B��B��B��B�7B|�By�Bt�Bn�BiyB`BBYBT�BK�B>wB7LB,B"�BVB�B�/BƨB�jB��B�hB|�Bn�B`BBQ�BB�B;dB6FB2-B/B,B$�B�B�BuBVB
��B
�B
�yB
�HB
�B
��B
��B
��B
�}B
�3B
��B
��B
�+B
|�B
p�B
hsB
[#B
Q�B
K�B
B�B
;dB
0!B
%�B
�B
hB
+B	��B	�B	�;B	��B	��B	��B	ɺB	ĜB	�}B	�?B	��B	��B	�{B	�=B	�B	�B	{�B	u�B	p�B	l�B	iyB	e`B	cTB	aHB	^5B	[#B	S�B	Q�B	M�B	K�B	H�B	@�B	8RB	.B	&�B	�B	�B	�B	�B	hB	PB	1B	B	B��B��B��B��B��B��B�B�B�B�mB�TB�HB�5B�B�B�B��B��B��B��B��BǮBB�qB�dB�XB�LB�?B�3B�-B�B��B��B��B��B��B��B�oB�PB�JB�%B�%B�%B�%B�B�B�B�B� B}�Bz�Bv�Bq�Bp�Bp�Bo�Bm�Bm�Bl�Bk�BjBiyBhsBgmBe`BcTBaHB_;B\)BZBXBW
BVBS�BQ�BP�BN�BM�BL�BL�BK�BJ�BH�BH�BG�BE�BD�BD�BC�BA�BA�B@�B?}B@�B@�B?}B?}B>wB=qB>wB=qB<jB;dB:^B:^B9XB9XB9XB8RB5?B7LB6FB5?B6FB6FB7LB8RB9XB:^B9XB8RB;dB<jB<jB;dB<jBB�BC�BC�BD�BG�BI�BK�BM�BP�BS�BVB[#B^5B^5B^5BaHBcTBe`BffBhsBhsBk�Bk�BjBiyBgmBiyBl�Bn�Br�Br�Bv�Bu�Bw�B}�B�B�bB��B��B�B�-B�FB�LB�LB�XBBĜB��B��B��B��B��B�B�5B�;B�;B�BB�BB�HB�HB�BB�BB�NB�fB�sB�B�B�yB�yB�sB�B�B�B�B�B�B�B�B��B��B��B��B	  B	B		7B	PB	�B	 �B	'�B	)�B	$�B	#�B	$�B	(�B	/B	/B	33B	5?B	:^B	I�B	K�B	N�B	T�B	[#B	[#B	\)B	_;B	gmB	k�B	k�B	m�B	n�B	n�B	o�B	r�B	s�B	x�B	z�B	|�B	|�B	|�B	}�B	� B	�B	�B	�B	�B	�B	~�B	|�B	�B	�%B	�%B	�B	�B	�%B	�1B	�=B	�=B	�DB	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BaHBcTBbNBbNBbNBbNBaHBcTBdZBdZBcTBe`Be`Be`Be`Be`Be`Be`BffBe`Be`BaHBaHB_;BZBZBVBN�BK�BJ�B:^B'�BbB��B�B�B�B�TB�TB�5B�#B�
B�)B�
B��BɺB��B��B��BĜB�qB��BB��BBB�jB�3B�B��B��B��B��B�\B}�B|�Bw�Bp�Bm�BcTBZBXBP�BA�B;dB/B)�B�B�B�fBɺBÖB�-B��B�Bt�BffB\)BE�B=qB8RB49B0!B/B)�B�B�B{B{B
��B
�B
�B
�`B
�B
��B
��B
��B
ŢB
�RB
�B
��B
�JB
�B
t�B
o�B
`BB
T�B
P�B
F�B
A�B
5?B
-B
 �B
�B
PB
%B	��B	�fB	�B	��B	��B	��B	ƨB	ƨB	�qB	�B	��B	��B	�\B	�%B	�+B	� B	x�B	r�B	m�B	k�B	gmB	dZB	cTB	aHB	]/B	T�B	S�B	N�B	L�B	M�B	D�B	=qB	2-B	+B	!�B	�B	�B	�B	{B	bB	DB	%B	B	B��B��B��B��B��B�B�B�B�B�`B�TB�TB�#B�B�B��B��B��B��B��BɺBƨB�wB�qB�^B�RB�LB�9B�9B�'B�B��B��B��B��B��B��B�hB�VB�1B�1B�+B�1B�%B�B�1B�%B�B�B|�B|�Bt�Bq�Br�Bp�Bo�Bn�Bm�Bl�Bl�Bl�BjBiyBhsBe`BcTBbNB_;B^5BZBYBXBXBS�BR�BQ�BN�BO�BN�BM�BL�BJ�BI�BI�BJ�BG�BD�BC�BD�BA�BB�BA�B@�BA�BA�BC�BB�B?}B?}B?}B>wB=qB=qB<jB;dB:^B:^B:^B:^B9XB9XB9XB7LB8RB8RB9XB:^B;dB<jB=qB=qB=qB=qB>wB@�BD�BD�BD�BE�BG�BK�BM�BO�BQ�BT�BW
B]/B_;B_;B_;BbNBdZBffBgmBiyBjBm�Bm�Bk�Bk�BiyBk�Bm�Bn�Br�Bs�Bz�Bv�Bx�B~�B�B�VB��B��B��B�-B�FB�RB�XB�RBBŢB��B��B��B��B��B�#B�5B�;B�;B�HB�BB�HB�NB�HB�HB�TB�mB�yB�B�B�yB�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	%B		7B	JB	oB	�B	'�B	-B	%�B	$�B	$�B	(�B	0!B	0!B	49B	5?B	5?B	H�B	J�B	M�B	W
B	\)B	[#B	\)B	^5B	gmB	l�B	k�B	m�B	n�B	o�B	o�B	r�B	s�B	y�B	{�B	|�B	|�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	{�B	�B	�%B	�%B	�%B	�%B	�%B	�1B	�DB	�JB	�DB	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447012012010314470120120103144701  AO  ARGQ                                                                        20111130140926  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140926  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144701  IP                  G�O�G�O�G�O�                