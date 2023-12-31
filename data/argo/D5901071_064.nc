CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:09Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               @A   AO  20111130140550  20190522121826  1727_5046_064                   2C  D   APEX                            2143                            040306                          846 @�h`P��1   @�h`�5@
@7ffffff�c�M���1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd�Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;�fD<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dny�Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dxٚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�ff@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�  A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC^  C_�fCa�fCd  Cf  Cg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Ds3D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Ds3D�3Dy�D��Dy�D��D� D  Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%� D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;� D<  D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAs3DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DL  DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPs3DP��DQy�DR  DRy�DR��DSy�DS��DTy�DT��DUy�DV  DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd�3Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm�3Dns3Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Dx�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\)A�XA�jA�p�A�t�A�z�A�|�A�~�A��A��A��A�~�A��A��A��A��A��7A��PA�|�A��A��A��HA��hA�$�A��yA��/A��A�hsA� �A���A��A��A��A��;A���A���A���A��A���A���A��uA��PA��DA��7A��A�z�A�r�A�t�A�n�A�ffA�p�A�p�A�n�A�`BA�O�A�M�A�XA�I�A�5?A��A�
=A�  A���A���A�C�A�bA��A���A�E�A�O�A�1A���A�ffA�5?A��A��
A�`BA�;dA�1'A�
=A���A�v�A�{A�dZA��A�1A�1'A���A���A��A��wA�-A���A�
=A��jA�Q�A��
A��A���A�ĜA��/A� �A��;A�K�A�`BA��A�  A���A���A�$�A�7LA�ĜA��A�A�A���A��A�K�A��;A���A�?}A��A�jA��PA��!A�ȴA���A��yA�K�A��A��yA�9XA��mA�Q�A��A��A�A��9A~�/A}��A|E�Az��Ax�!Ar��Ao%AkK�Aj�Ai%Af�`Ad�AdA�AdQ�Ad1Act�Ab�Aa��Aa��A_��A]|�A\��A[XAZ�AW�mAV�AU�hATE�AS��AP�AOl�AO�AN{AK�mAJ��AI�-AH��AG�AE��AD�yADr�AC��AC��AB=qAAdZA>��A=�A<A�A;\)A:��A:bA8�A6E�A4ȴA3�A3t�A2�RA1l�A1K�A0�/A0ZA/A.�9A.bA-�TA,I�A+&�A*^5A*$�A)��A(�`A(�!A(ffA'��A&�A%
=A#S�A!��A!�A ĜA 1A��A��A
=A5?AdZAĜA��A5?A�TA�^AoA�A%AĜAVA�#A�A`BA�uA{AC�A1'A��An�A?}A�hA
�A
$�A	33A�A��A�TAK�Av�A��A��A\)A"�A��AI�A�;A�A �\A VA I�A �@��R@��@��@�A�@��#@��R@�P@���@�@�-@�G�@�z�@�A�@��@�K�@�ƨ@�@�`B@�@��@�ff@�5?@�X@ޏ\@���@�;d@ڰ!@�V@�n�@�9X@�33@�Z@�Ĝ@ج@���@�/@�&�@���@�(�@�
=@Ұ!@�J@�/@�bN@�1'@�K�@�`B@�^5@ͩ�@˥�@ʗ�@�x�@�9X@Ɨ�@�C�@�E�@��@�@°!@�9X@�1@���@�9X@�l�@��7@��@��H@�J@�`B@�1@��P@�ȴ@��+@��#@�E�@��^@� �@��@�ȴ@���@��R@�M�@��@���@��u@��@�
=@��w@��F@��@�^5@��@�O�@���@��D@�(�@���@��;@�^5@�I�@�t�@�^5@��@�J@�5?@���@�dZ@���@��P@���@�ff@�=q@�$�@�O�@�1@���@�v�@�=q@�E�@���@��`@�|�@���@�ff@�E�@��#@�V@�9X@��
@�hs@�z�@�Ĝ@� �@��
@��y@���@�;d@�t�@�$�@�1'@��@��@���@�ff@��@��7@���@��u@�j@�1'@�Z@�j@�r�@��w@�dZ@�C�@�33@�33@�+@�o@�5?@�J@��@��@���@���@��@�z�@�r�@�bN@�I�@��m@�t�@�o@��!@��\@�-@���@��h@�X@��@�Ĝ@��u@�bN@�1@���@��w@���@���@��@�dZ@�33@�
=@��H@���@��+@�^5@�-@�$�@�$�@�$�@�J@��T@�x�@�O�@�X@�O�@�G�@�G�@�7L@�/@��@��@��@��@�V@�%@��@���@�z�@�Q�@�A�@�{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�\)A�XA�jA�p�A�t�A�z�A�|�A�~�A��A��A��A�~�A��A��A��A��A��7A��PA�|�A��A��A��HA��hA�$�A��yA��/A��A�hsA� �A���A��A��A��A��;A���A���A���A��A���A���A��uA��PA��DA��7A��A�z�A�r�A�t�A�n�A�ffA�p�A�p�A�n�A�`BA�O�A�M�A�XA�I�A�5?A��A�
=A�  A���A���A�C�A�bA��A���A�E�A�O�A�1A���A�ffA�5?A��A��
A�`BA�;dA�1'A�
=A���A�v�A�{A�dZA��A�1A�1'A���A���A��A��wA�-A���A�
=A��jA�Q�A��
A��A���A�ĜA��/A� �A��;A�K�A�`BA��A�  A���A���A�$�A�7LA�ĜA��A�A�A���A��A�K�A��;A���A�?}A��A�jA��PA��!A�ȴA���A��yA�K�A��A��yA�9XA��mA�Q�A��A��A�A��9A~�/A}��A|E�Az��Ax�!Ar��Ao%AkK�Aj�Ai%Af�`Ad�AdA�AdQ�Ad1Act�Ab�Aa��Aa��A_��A]|�A\��A[XAZ�AW�mAV�AU�hATE�AS��AP�AOl�AO�AN{AK�mAJ��AI�-AH��AG�AE��AD�yADr�AC��AC��AB=qAAdZA>��A=�A<A�A;\)A:��A:bA8�A6E�A4ȴA3�A3t�A2�RA1l�A1K�A0�/A0ZA/A.�9A.bA-�TA,I�A+&�A*^5A*$�A)��A(�`A(�!A(ffA'��A&�A%
=A#S�A!��A!�A ĜA 1A��A��A
=A5?AdZAĜA��A5?A�TA�^AoA�A%AĜAVA�#A�A`BA�uA{AC�A1'A��An�A?}A�hA
�A
$�A	33A�A��A�TAK�Av�A��A��A\)A"�A��AI�A�;A�A �\A VA I�A �@��R@��@��@�A�@��#@��R@�P@���@�@�-@�G�@�z�@�A�@��@�K�@�ƨ@�@�`B@�@��@�ff@�5?@�X@ޏ\@���@�;d@ڰ!@�V@�n�@�9X@�33@�Z@�Ĝ@ج@���@�/@�&�@���@�(�@�
=@Ұ!@�J@�/@�bN@�1'@�K�@�`B@�^5@ͩ�@˥�@ʗ�@�x�@�9X@Ɨ�@�C�@�E�@��@�@°!@�9X@�1@���@�9X@�l�@��7@��@��H@�J@�`B@�1@��P@�ȴ@��+@��#@�E�@��^@� �@��@�ȴ@���@��R@�M�@��@���@��u@��@�
=@��w@��F@��@�^5@��@�O�@���@��D@�(�@���@��;@�^5@�I�@�t�@�^5@��@�J@�5?@���@�dZ@���@��P@���@�ff@�=q@�$�@�O�@�1@���@�v�@�=q@�E�@���@��`@�|�@���@�ff@�E�@��#@�V@�9X@��
@�hs@�z�@�Ĝ@� �@��
@��y@���@�;d@�t�@�$�@�1'@��@��@���@�ff@��@��7@���@��u@�j@�1'@�Z@�j@�r�@��w@�dZ@�C�@�33@�33@�+@�o@�5?@�J@��@��@���@���@��@�z�@�r�@�bN@�I�@��m@�t�@�o@��!@��\@�-@���@��h@�X@��@�Ĝ@��u@�bN@�1@���@��w@���@���@��@�dZ@�33@�
=@��H@���@��+@�^5@�-@�$�@�$�@�$�@�J@��T@�x�@�O�@�X@�O�@�G�@�G�@�7L@�/@��@��@��@��@�V@�%@��@���@�z�@�Q�@�A�@�{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�7B�1B�1B�1B�1B�+B�+B�+B�1B�1B�+B�%B�+B�%B�%B�1B��B��BÖB;dBP�BR�B^5Bk�Bq�Br�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�1B�DB�PB�JB�=B�DB�bB�oB�oB�uB��B��B��B��B�B�XBB��B�HB�B�B�TB�B�B�B�B��B��B��B��B��B�B�B�mB�;B�B��B��BǮB�wB�LB�9B�'B��B��B��B��B�PB�B{�Bt�Bp�Bn�BiyBcTB[#BE�BE�BA�B33B"�B��B��B�^B�B�B��B��B�bB{�Bp�BjBH�B�BVB
��B
�B
��B
ǮB
�B
��B
�bB
�1B
�B
q�B
ffB
S�B
C�B
:^B
33B
&�B
\B	�B	�9B	��B	�oB	�+B	z�B	t�B	t�B	x�B	}�B	�B	� B	�B	�\B	�JB	�B	{�B	s�B	iyB	]/B	S�B	O�B	G�B	A�B	49B	-B	)�B	'�B	 �B	�B	uB	
=B	B��B��B��B��B��B��B�B�yB�NB�/B�#B�B�B��BĜBBB�}B�^B�RB�^B�RB�LB�?B�9B�9B�9B�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�oB�oB�uB�{B��B��B�{B�oB�uB�{B�uB�oB�uB�oB�hB�VB�\B�PB�DB�+B�1B�DB�DB�7B�+B�%B�+B�%B�B�B�B�B� B~�B~�B� B�B�B� B� B� B~�B� B�B� B� B}�Bw�Bq�Bq�Bt�Bv�Bx�By�B|�B�%B�B�+B�VB�VB�7B�B� Bz�Bx�Br�Bm�Bp�Bo�Bo�Bv�B�Bx�By�B�B�hB��B��B��B��B��B�!B�RB�^B�XB�RB�RB�^B�XB�9B�dB�XB�!B�B�B��B�!B��B��B�B�'B�^B��B��B��B��B��B��BɺB��B��B��BȴBƨB��B�RB�LB�XB�^B�^B�jB�jB�}BB��B�B�
B�B�
B�B�;B�ZB�`B�fB�sB�yB�yB�sB�sB�yB�yB�yB�B�B�B�B�B��B��B	+B	PB	PB	VB	VB	VB	VB	bB	bB	bB	hB	oB	�B	�B	{B	oB	hB	hB	hB	bB	\B	\B	PB	
=B	DB	hB	�B	�B	�B	�B	"�B	%�B	$�B	"�B	(�B	1'B	5?B	5?B	49B	7LB	;dB	>wB	?}B	B�B	H�B	I�B	J�B	L�B	O�B	P�B	VB	]/B	^5B	aHB	e`B	ffB	gmB	hsB	iyB	jB	k�B	l�B	m�B	m�B	n�B	p�B	u�B	v�B	z�B	}�B	�B	�1B	�1B	�=B	�PB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�'B	�'B	�'B	�'B	�-B	�-B	�3B	�3B	�3B	�3B	�3B	�3B	�9B	�?B	�XB	�dB	�jB	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�7B�1B�1B�1B�1B�+B�+B�+B�1B�1B�+B�%B�+B�%B�%B�1B��B��BŢB<jBQ�BT�B`BBl�Bq�Bs�By�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�1B�DB�PB�JB�=B�DB�bB�oB�oB�uB��B��B��B��B�B�XBÖB��B�`B�B�#B�ZB�B�B�B��B��B��B��B��B��B�B�B�B�TB�B��B��B��BŢB�^B�FB�FB�!B��B��B��B�hB�7B� Bw�Bq�Bq�Bm�BgmBbNBG�BJ�BI�B8RB0!B
=B�5B�wB�!B�B�B��B��B� Bq�Bu�BZB �BuB  B
�/B
��B
��B
�?B
��B
�oB
�JB
�+B
u�B
l�B
ZB
F�B
>wB
7LB
-B
�B	�NB	�wB	��B	��B	�PB	�B	v�B	t�B	y�B	� B	�B	�B	�B	��B	�uB	�B	� B	w�B	p�B	cTB	VB	S�B	J�B	J�B	8RB	.B	,B	-B	"�B	!�B	�B	VB	%B��B��B��B��B��B��B��B�B�ZB�BB�)B�#B�B�
BɺBŢBĜBB�wB�XB�jB�^B�XB�XB�FB�?B�^B�LB�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�{B�uB�uB�bB�oB�bB�\B�7B�JB�bB�PB�JB�=B�1B�7B�+B�+B�+B�+B�%B�B� B�B�B�B�B�B�B� B� B�B�B�B�B�B}�Bw�Br�Bt�Bw�Bz�B{�B}�B�JB�1B�%B�\B�uB�bB�%B� Bz�Bz�Bw�Bq�Bs�Bo�Bo�Bv�B�By�Bx�B�B�hB��B��B��B��B��B�-B�XB�dB�^B�XB�XB�jB�jB�3B�jB�jB�-B�B�B�B�FB�B��B�B�B�RB��B��B��B��B��B��BɺB��B��B��BɺBȴBǮB�XB�LB�^B�jB�dB�qB�jB�}BÖB��B�B�B�
B�B�B�;B�`B�fB�mB�sB�B�B�yB�yB�yB�B�B�B�B�B�B�B�B��B	+B	PB	\B	VB	VB	VB	bB	uB	oB	hB	oB	oB	�B	�B	�B	oB	oB	hB	oB	oB	hB	bB	hB	JB	DB	oB	�B	�B	�B	�B	"�B	(�B	(�B	$�B	(�B	1'B	5?B	6FB	5?B	8RB	<jB	>wB	?}B	B�B	H�B	I�B	K�B	M�B	O�B	P�B	VB	]/B	^5B	cTB	e`B	ffB	gmB	iyB	jB	jB	k�B	l�B	m�B	m�B	o�B	q�B	v�B	w�B	z�B	~�B	�B	�1B	�7B	�=B	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�'B	�'B	�'B	�'B	�-B	�-B	�3B	�3B	�3B	�3B	�3B	�3B	�9B	�FB	�XB	�dB	�jB	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446562012010314465620120103144656  AO  ARGQ                                                                        20111130140550  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140550  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144656  IP                  G�O�G�O�G�O�                