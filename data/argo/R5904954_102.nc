CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:11Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I`   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R<   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  YP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       [   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       k   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  r   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191711  20181005191711  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               fA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @���T��1   @�����}@4��1'�dHz�G�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      fA   A   B   @9��@y��@���A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB'33B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C  C  C   C"  C$  C&  C(�C*  C,  C.  C/�fC2  C4  C6  C8�C:�C<�C>  C@  CB�CD�CF  Cv�Cx�Cz�C|  C~  C�  C�  C��3C�  C�  C�  C��3C�  C��3C��3C�  C��C��3C��3C�  C��C��C�  C��C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C��C�  C��C�  C��3C�  C��C��C�  C��C��C�  C��3C��3C�  C�  C�  C��3C��3C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C��3C��3C�  C�  C��3C�  C��3C�  C��C�  C��3C�  C��C��C�  C�  C�  C��C��3C��3C��3C��fC�  C��C�  C�  C�  C�  C�  C��3C�  C��C�  C��C��C�  C�  C��C�  C��3C��C�  C�  D   D � D  Dy�D  Dy�D��Dy�D  D� D��Dy�D  Dy�D  D�fDfD�fD	  D	� D
  D
� D
��D� DfD� D  D�fDfD�fDfD� D��Dy�D��D� D  Dy�D  D�fDfD� D��Dy�D  D�fDfD�fD  D� D  D� D  D� D  Dy�D��Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#�fD$  D$y�D%  D%�fD&  D&y�D'  D'� D(  D(� D)  D)y�D)��D*� D+  D+� D+��D,y�D-  D-� D.fD.� D.��D/� D/��D0� D1  D1� D2  D2y�D2��D3� D4fD4�fD5  D5y�D5��D6� D7fD7�fD8  D8� D8�3D9� D:fD:� D;  D;�fD<fD<� D=fD=� D=��D>y�D?  D?� D@fD@�fDAfDA�fDBfDB� DB��DCy�DC��DD� DE  DE�fDF  DF� DGfDG� DG��DH� DIfDI� DJfDJ�fDK  DKs3DK��DLy�DL��DM� DM��DNy�DO  DO�fDPfDP� DP��DQy�DR  DR� DR��DSy�DT  DT� DU  DU�fDVfDV��DWfDW� DX  DX� DY  DYy�DY��DZ� D[  D[� D\  D\� D\��D]y�D^  D^y�D^��D_y�D_��D`y�Da  Da�fDbfDb� Dc  Dc�fDd�Dd�fDefDe��Df  Df� Dg  Dgs3Dg��Dh�fDi  Di� Di��Djy�Dk  Dk� Dl  Dl� Dl��Dm� Dn  Dny�Do  Do� Dp  Dp� Dq  Dq� DrfDry�Ds  Ds� DtfDt� Du  Du� Dv  Dv�fDv��Dwy�Dw� Dy��D�L)D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @:�H@z�H@�p�A Q�A Q�A@Q�A`Q�A�(�A���A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B z�B'G�B/�B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
=B��
B��
B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B��
B�
=B�
=B�
=B�
=CCC C"C$C&C(�C*C,C.C/�C2C4C6C8�C:�C<�C>C@CB�CD�CFCv�Cx�Cz�C|C~C��C��C���C��C��C��C���C��C���C���C��C�\C���C���C��C�\C�\C��C�\C��C��C��C��C�\C���C��C��C��C��C��C�\C��C�\C��C���C��C�\C�\C��C�\C�\C��C���C���C��C��C��C���C���C��C��C���C���C���C���C���C��C��C��C��C���C��C��C��C��C��C��C�)C�)C��C��C��C�\C��C��C��C��C�\C��C��C�\C�\C��C��C��C���C���C��C��C���C��C���C��C�\C��C���C��C�\C�\C��C��C��C�\C���C���C���C���C��C�\C��C��C��C��C��C���C��C�\C��C�\C�\C��C��C�)C��C���C�\C��C��D HD �HDHDz�DHDz�D��Dz�DHD�HD��Dz�DHDz�DHD��D�D��D	HD	�HD
HD
�HD
��D�HD�D�HDHD��D�D��D�D�HD��Dz�D��D�HDHDz�DHD��D�D�HD��Dz�DHD��D�D��DHD�HDHD�HDHD�HDHDz�D��Dz�DHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD"��D#��D$HD$z�D%HD%��D&HD&z�D'HD'�HD(HD(�HD)HD)z�D)��D*�HD+HD+�HD+��D,z�D-HD-�HD.�D.�HD.��D/�HD/��D0�HD1HD1�HD2HD2z�D2��D3�HD4�D4��D5HD5z�D5��D6�HD7�D7��D8HD8�HD8�{D9�HD:�D:�HD;HD;��D<�D<�HD=�D=�HD=��D>z�D?HD?�HD@�D@��DA�DA��DB�DB�HDB��DCz�DC��DD�HDEHDE��DFHDF�HDG�DG�HDG��DH�HDI�DI�HDJ�DJ��DKHDKt{DK��DLz�DL��DM�HDM��DNz�DOHDO��DP�DP�HDP��DQz�DRHDR�HDR��DSz�DTHDT�HDUHDU��DV�DV�DW�DW�HDXHDX�HDYHDYz�DY��DZ�HD[HD[�HD\HD\�HD\��D]z�D^HD^z�D^��D_z�D_��D`z�DaHDa��Db�Db�HDcHDc��DdDd��De�De�DfHDf�HDgHDgt{Dg��Dh��DiHDi�HDi��Djz�DkHDk�HDlHDl�HDl��Dm�HDnHDnz�DoHDo�HDpHDp�HDqHDq�HDr�Drz�DsHDs�HDt�Dt�HDuHDu�HDvHDv��Dv��Dwz�Dw�HDy�4D�L�D��g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aݺ^Aݺ^Aݺ^Aݺ^AݮA݁A�~�A�|�A���Aܺ^A܅A�x�A�p�A�n�A�n�A�l�A�hsA�hsA�jA�\)A�ffA�
=A͸RA̩�A˰!Aʧ�A��mA�z�A�  Aď\A�
=A��A�p�A�&�A��A� �A�hsA�oA��A�ffA�{A�VA�?}A��`A��A�z�A�{A�
=A��9A���A�M�A���A���A�1A�x�A�r�A��mA�  A�&�A�x�A�Q�A���A�I�A��#A��A���A�/A��+A�~�A�l�A�\)A�Q�A��A|jAz �AwXAD�\AAp�A?��A=�TA=�A<=qA:�A7�mA4I�A2M�A0A�A/\)A/+A.ȴA.�!A-��A,�!A+�mA*�9A(ZA&VA%�^A$�+A#��A"�jA!�hA!�A ��A�wA��A��A��A�hA�A\)A�An�A�A�A(�Ax�An�A�yAjA��A+AE�A��A33AZA33AĜA~�A�A?}A��A/AM�AAA
�9A
A�A	��A	�^A	VAp�A��A��AZA1AhsA�RA{A�FAZAx�AV@��;@���@���@���@��@���@�9X@���@���@�@���@�@�-@�?}@� �@���@�@��@�!@�E�@���@�p�@�x�@�@�x�@�X@���@���@�K�@�7@��
@�v�@�@���@�ƨ@�"�@ާ�@ݩ�@���@��@ش9@��m@�t�@�;d@��@���@�/@�(�@�\)@�n�@ѩ�@��@�z�@�(�@��m@�|�@�33@θR@��#@�/@�Z@ʗ�@ɡ�@�O�@�?}@�G�@�`B@�%@��;@��@�v�@�V@��@�$�@��@���@ř�@�p�@��`@ēu@�r�@� �@ÍP@��y@�v�@�ff@�{@��@�O�@��@�Ĝ@�bN@��F@�C�@��y@�-@��-@�`B@��u@�I�@���@���@�o@�V@��#@�X@���@�Z@�b@��m@��@�
=@���@�=q@���@��@�dZ@�l�@�b@���@���@�@��+@���@���@� �@�$�@��#@�@�Q�@��@���@�V@�%@�G�@�hs@��9@��9@���@��#@��m@�/@�bN@���@�l�@�;d@��@��\@���@���@��R@��R@�@��D@��!@�-@���@�I�@���@���@��/@�z�@�A�@���@�`B@���@���@�{@��@���@�`B@�hs@�x�@�p�@�`B@�/@��@���@��D@� �@�b@��@��
@�ƨ@��w@��w@��w@�ƨ@��@��@�S�@�@��H@��\@�5?@�{@���@�@��h@�hs@�&�@���@���@��u@��@�Q�@��;@��w@���@�\)@�+@�
=@�@��H@��!@�v�@���@��@��@���@���@�&�@�A�@�  @��;@��F@���@�l�@�;d@��y@�^5@�J@��@���@�?}@���@���@��@�1'@��w@���@�|�@�\)@�C�@�o@��@��@��@���@��R@�v�@�M�@��@�G�@���@��@�r�@� �@�1@���@�@��\@�M�@�=q@�{@���@��T@���@�%@��j@��D@�A�@�I�@�Z@�Z@�bN@�bN@��@�  @���@��m@�1@���@���@�t�@�;d@��H@��+@�E�@�@��@���@��h@�X@�7L@���@��/@�e�@o�*@`%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aݺ^Aݺ^Aݺ^Aݺ^AݮA݁A�~�A�|�A���Aܺ^A܅A�x�A�p�A�n�A�n�A�l�A�hsA�hsA�jA�\)A�ffA�
=A͸RA̩�A˰!Aʧ�A��mA�z�A�  Aď\A�
=A��A�p�A�&�A��A� �A�hsA�oA��A�ffA�{A�VA�?}A��`A��A�z�A�{A�
=A��9A���A�M�A���A���A�1A�x�A�r�A��mA�  A�&�A�x�A�Q�A���A�I�A��#A��A���A�/A��+A�~�A�l�A�\)A�Q�A��A|jAz �AwXAD�\AAp�A?��A=�TA=�A<=qA:�A7�mA4I�A2M�A0A�A/\)A/+A.ȴA.�!A-��A,�!A+�mA*�9A(ZA&VA%�^A$�+A#��A"�jA!�hA!�A ��A�wA��A��A��A�hA�A\)A�An�A�A�A(�Ax�An�A�yAjA��A+AE�A��A33AZA33AĜA~�A�A?}A��A/AM�AAA
�9A
A�A	��A	�^A	VAp�A��A��AZA1AhsA�RA{A�FAZAx�AV@��;@���@���@���@��@���@�9X@���@���@�@���@�@�-@�?}@� �@���@�@��@�!@�E�@���@�p�@�x�@�@�x�@�X@���@���@�K�@�7@��
@�v�@�@���@�ƨ@�"�@ާ�@ݩ�@���@��@ش9@��m@�t�@�;d@��@���@�/@�(�@�\)@�n�@ѩ�@��@�z�@�(�@��m@�|�@�33@θR@��#@�/@�Z@ʗ�@ɡ�@�O�@�?}@�G�@�`B@�%@��;@��@�v�@�V@��@�$�@��@���@ř�@�p�@��`@ēu@�r�@� �@ÍP@��y@�v�@�ff@�{@��@�O�@��@�Ĝ@�bN@��F@�C�@��y@�-@��-@�`B@��u@�I�@���@���@�o@�V@��#@�X@���@�Z@�b@��m@��@�
=@���@�=q@���@��@�dZ@�l�@�b@���@���@�@��+@���@���@� �@�$�@��#@�@�Q�@��@���@�V@�%@�G�@�hs@��9@��9@���@��#@��m@�/@�bN@���@�l�@�;d@��@��\@���@���@��R@��R@�@��D@��!@�-@���@�I�@���@���@��/@�z�@�A�@���@�`B@���@���@�{@��@���@�`B@�hs@�x�@�p�@�`B@�/@��@���@��D@� �@�b@��@��
@�ƨ@��w@��w@��w@�ƨ@��@��@�S�@�@��H@��\@�5?@�{@���@�@��h@�hs@�&�@���@���@��u@��@�Q�@��;@��w@���@�\)@�+@�
=@�@��H@��!@�v�@���@��@��@���@���@�&�@�A�@�  @��;@��F@���@�l�@�;d@��y@�^5@�J@��@���@�?}@���@���@��@�1'@��w@���@�|�@�\)@�C�@�o@��@��@��@���@��R@�v�@�M�@��@�G�@���@��@�r�@� �@�1@���@�@��\@�M�@�=q@�{@���@��T@���@�%@��j@��D@�A�@�I�@�Z@�Z@�bN@�bN@��@�  @���@��m@�1@���@���@�t�@�;d@��H@��+@�E�@�@��@���@��h@�X@�7L@���@��/@�e�@o�*@`%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BF�BG�BG�BF�BG�BG�BG�BF�BI�BI�BH�BG�BG�BH�BH�BH�BH�BH�BI�BI�B?}BuB
=BbB�B�B-B9XBH�BVBaHBq�B� B�DB�\B�hB��B��B��B��B�B�B�XB�dB�FB�9B��B��B��B��B��B��B��?�(�BF�B;dB33B'�B�BB
�mB
��B
ȴB
��B
�9B
��B
�DB
s�B
\)B
R�B
VB
W
B
T�B
<jB
(�Bt�B�/B��B��BǮBŢB��B�jB�9B�B��B��B��B��B��B��B��B�uB�bB�PB�=B�1B�B�B�B�B� B~�B� B�B�B�B�B�B�B� B�B~�B{�Bu�Bt�Bs�Bv�Bt�Bs�Bs�Bs�Bu�Bu�Bu�Bx�B}�B� B�B�B�B�+B�PB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�DB�1B�B�B�1B�1B�+B�+B�+B�DB�B�7B�PB�PB�PB�DB�7B�%B�B�B�B�B�%B�DB�hB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�-B�9B�9B�?B�?B�FB�LB�XB�XB�^B�jB�qB�}B��B��BBBÖBƨBȴBɺB��B��B��B��B��B��B��B��B�B�B�B�B�#B�)B�BB�HB�HB�TB�TB�ZB�`B�mB�mB�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	B	%B	1B	JB	\B	hB	{B	�B	�B	�B	�B	�B	 �B	�B	�B	{B	PB	uB	�B	PB		7B	\B	�B	�B	�B	�B	�B	�B	�B	.B	5?B	6FB	7LB	9XB	<jB	?}B	>wB	8RB	?}B	N�B	]/B	e`B	hsB	gmB	ffB	gmB	jB	m�B	q�B	r�B	t�B	w�B	v�B	t�B	p�B	q�B	o�B	p�B	r�B	o�B	m�B	m�B	o�B	w�B	~�B	�B	�1B	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�XB	�^B	�dB	�wB	��B	B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�#B	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B
^B
 222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   BF�BG�BG�BF�BG�BG�BG�BF�BI�BI�BH�BG�BG�BH�BH�BH�BH�BH�BI�BI�B?}BuB
=BbB�B�B-B9XBH�BVBaHBq�B� B�DB�\B�hB��B��B��B��B�B�B�XB�dB�FB�9B��B��B��B��B��B��B��?�(�BF�B;dB33B'�B�BB
�mB
��B
ȴB
��B
�9B
��B
�DB
s�B
\)B
R�B
VB
W
B
T�B
<jB
(�Bt�B�/B��B��BǮBŢB��B�jB�9B�B��B��B��B��B��B��B��B�uB�bB�PB�=B�1B�B�B�B�B� B~�B� B�B�B�B�B�B�B� B�B~�B{�Bu�Bt�Bs�Bv�Bt�Bs�Bs�Bs�Bu�Bu�Bu�Bx�B}�B� B�B�B�B�+B�PB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�DB�1B�B�B�1B�1B�+B�+B�+B�DB�B�7B�PB�PB�PB�DB�7B�%B�B�B�B�B�%B�DB�hB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�-B�9B�9B�?B�?B�FB�LB�XB�XB�^B�jB�qB�}B��B��BBBÖBƨBȴBɺB��B��B��B��B��B��B��B��B�B�B�B�B�#B�)B�BB�HB�HB�TB�TB�ZB�`B�mB�mB�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	B	%B	1B	JB	\B	hB	{B	�B	�B	�B	�B	�B	 �B	�B	�B	{B	PB	uB	�B	PB		7B	\B	�B	�B	�B	�B	�B	�B	�B	.B	5?B	6FB	7LB	9XB	<jB	?}B	>wB	8RB	?}B	N�B	]/B	e`B	hsB	gmB	ffB	gmB	jB	m�B	q�B	r�B	t�B	w�B	v�B	t�B	p�B	q�B	o�B	p�B	r�B	o�B	m�B	m�B	o�B	w�B	~�B	�B	�1B	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�XB	�^B	�dB	�wB	��B	B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�#B	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B
^B
 222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191711                              AO  ARCAADJP                                                                    20181005191711    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191711  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191711  QCF$                G�O�G�O�G�O�8000            