CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2020-11-30T00:39:51Z creation;2020-11-30T00:39:52Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `X   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ܠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  20201130003951  20201130005329  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               `A   JA                                  2B  A   APEX                            7906                            051216                          846 @�K2b	�Z1   @�K4@y\�@3š����dn��n�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A   AffA>ffA`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C?�fCB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cc�fCe�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D��D� D  D� D  D� D  D� D  D� D   D �fD!fD!� D"  D"� D#  D#� D$  D$�fD%  D%�fD&fD&�fD'  D'� D(  D(� D)  D)� D)��D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dx��Dyy�Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�C3Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D���D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�<�D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�C3Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D���D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D��3D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�C3D� D��D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D��D�  D�@ D�� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�C3D�3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�FfD�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Tz�@�p�@�p�A�RA%�AE�Af�RA�\)A��\A�\)A�\)A�\)A�\)A�\)A�\)B�B	�B�B�B!G�B)�B1�B9�BA�BI�BQ�BY�BbzBjzBq�By�B��
B��
B��
B���B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B�
=B��
B��
B�
=Bأ�B��
B��
B��
B��
B��
B��
B��
B�
=B�=pC Q�Ck�Ck�Ck�Ck�C
k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C k�C"�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8�C:k�C<k�C>k�C@Q�CBk�CDk�CF�CHk�CJk�CLk�CNk�CPk�CRk�CTk�CVk�CXQ�CZk�C\k�C^k�C`k�Cbk�CdQ�CfQ�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�B�C�5�C�5�C�5�C�B�C�5�C�5�C�5�C�5�C�5�C�5�C�B�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�B�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�B�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D!GD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D{D�{D{D��D�D��D�D��D�D��D�D��D �D �GD!!GD!��D"�D"��D#�D#��D$�D$�GD%�D%�GD&!GD&�GD'�D'��D(�D(��D)�D)��D*{D*��D+�D+�GD,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR!GDR�GDS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db!GDb��Dc�Dc��Dd�Dd��De�De�{Df�Df��Dg�Dg�GDh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy{Dy�{Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��>D��qD�qD�MqD��qD��qD�qD�MqD���D��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�
>D�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��>D�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD��D�MqD��qD��qD�qD�MqD��qD��qD��D�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�J>D��>D��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�
>D�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD���D��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�
>D�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��>D��>D�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�J>D��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqDqD��qD�qD�MqDÍqD��qD�qD�MqDčqD��qD�qD�P�DōqD��qD�qD�MqDƍqD��qD�qD�MqDǍqD��qD�qD�MqDȍqD��qD�qD�MqDɍqD��qD�qD�MqDʍqD��qD�
>D�MqDˍqD��qD�qD�MqD̍qD��qD�qD�MqD͍qD��qD�qD�J>D΍qD��qD�qD�MqDύqD��qD�qD�MqDЍqD��qD�qD�MqDэqD��qD�qD�MqDҍqD��qD�qD�MqDӍqD��qD�qD�MqDԍqD��qD�qD�MqDՐ�D��qD�qD�P�D֍qD��qD�qD�MqD׍qD��qD�qD�MqD؍qD��qD�qD�MqDٍqD��qD�qD�MqDڍqD��qD�
>D�MqDۍqD��qD�qD�MqD܍qD��qD�qD�MqDݍqD�ФD�qD�MqDލqD��qD�qD�MqDߍqD��qD�qD�MqD��qD��qD�qD�MqD�qD��qD�qD�P�D�qD��>D�qD�MqD�qD��qD�qD�MqD䐤D�ФD�qD�MqD�qD��qD�qD�MqD�qD��>D�qD�MqD�qD��>D�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�>D��qD�qD�MqD�qD��>D�qD�MqD��qD��qD��D�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD��D�P�D���D��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�P�D��qD��qD�qD�S�D�s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�5?A�5?A�7LA�9XA�A�A�K�A�K�A�K�A�M�A�M�A�O�A�O�A�Q�A�Q�A�Q�A�S�A�S�A�VA�VA�XA�XA�ZA�ZA�ZA�\)A�^5A�^5A�`BA�`BA�`BA�bNA�bNA�dZA�dZA�ffA�ffA�ffA�ffA�hsA�hsA�jA�jA�l�A�l�A�l�A�l�A�n�A�p�A�ffA�E�A�(�A�5?A��yAѸRAЗ�A�(�A�M�A��;AάA�bA�r�A�=qAȓuA�ĜAüjAº^A��`A�{A�A��DA��-A��HA�ƨA�M�A�1A��\A��A��jA��7A��yA�`BA��A�VA�M�A�t�A�p�A��+A�JA��9A��DA�hsA�(�A��A�dZA��+A�`BA�r�A��A�G�A��\A�=qA�K�A�/A�
=A��jA�$�A�A�A���A�C�A�5?A��A�\)A�1A��A�t�A���A�A��PA��9A���A�I�A��A���A��A�A�XA�n�A�z�A�?}A��A��TA�E�A��9A��!A~��A|I�Az�HAw��At��Aq��Ao33AmC�Aj$�Ag�mAfJAa�mAa7LA`�9A_�A^~�A\�/AX=qAVz�AU\)AT��AR�RAQ�API�AOC�AK�PAG�;ADVAA?}A=�A;S�A9�A7��A5K�A2�`A1��A0Q�A/C�A-C�A+�A(�9A(JA'XA&ffA$��A"z�A VA $�A �A�TAn�A��A�+A/A�\A1'A�A�7A{A&�A�HA��A�A5?A{A  A�TA�-Ax�A+A��A5?A�\A��A��A`BAffA~�AE�AbA�A	�-A��AȴA�uA�A\)A�A��A�A��A�A�#A~�A  A�#A|�A ��A �@�5?@��;@��h@���@��R@�G�@�33@�&�@��m@��@�\@���@�b@�"�@�-@�j@� �@畁@�-@��
@�P@�P@�{@��@ޏ\@�G�@�r�@�+@�=q@��;@�M�@�@��@�I�@��m@�|�@ҏ\@�Ĝ@�1'@��;@ϥ�@�t�@�dZ@�E�@��#@͉7@̬@��
@˅@˕�@�K�@�v�@ɲ-@�O�@���@ǥ�@��@��@�X@��@�j@Ý�@�V@��T@�x�@�Q�@���@��@�hs@��@���@�|�@�l�@�K�@��H@�V@�$�@��@��T@�x�@�V@���@�I�@�(�@���@�o@�5?@��-@�&�@�%@��@��D@�Q�@�1'@��@��F@�C�@��!@�^5@�@��^@��@���@��@�j@�  @��@���@�  @��@�Q�@��@��@�33@�C�@�Z@��;@�l�@�"�@���@���@�v�@���@���@�p�@��@���@�(�@�(�@�b@���@�33@�+@���@��\@��@��@��@�j@�Q�@�(�@���@���@��@�o@�@�n�@�V@�=q@�{@��h@�`B@�`B@�7L@��@��@���@���@�bN@�1'@�b@�ƨ@�t�@�C�@�o@�ȴ@���@�^5@�5?@�{@��@�@���@��h@�G�@��/@�(�@��F@�o@��!@��!@��R@��+@�n�@�^5@�^5@�M�@��#@�@�p�@��@�%@���@�Q�@��@���@�\)@���@�ȴ@��!@��\@��+@�n�@�E�@��@�p�@��@�V@�%@�z�@��
@��@���@���@�S�@��@�@���@��#@���@���@���@��h@�hs@�V@���@��m@���@���@��@�dZ@�"�@���@�v�@�E�@��^@�`B@�?}@��@��`@���@�bN@�1'@�  @���@�\)@�;d@��@���@�ȴ@��!@�ff@���@�X@���@��@�Z@�1@��;@��w@�S�@���@�~�@�E�@���@��#@���@�?}@�%@���@�j@�A�@� �@�1@��;@��F@���@�l�@��@��R@�v�@�ff@�E�@�E�@�-@��T@���@�hs@�/@�&�@��@���@�bN@�(�@��@�b@�  @��m@��w@�l�@�"�@�
=@��@��H@��H@��@�ȴ@���@�=q@�$�@���@�G�@���@�Ĝ@���@�`B@�X@�/@�%@���@���@�A�@�@~ȴ@~V@~$�@}@}V@{t�@z~�@z^5@z=q@y�^@yG�@x��@xQ�@w��@w
=@v�R@vv�@v5?@u�T@u�-@u�-@u`B@t�@tj@s��@sC�@r�H@r�!@q�7@qX@q��@q�^@p��@p�`@p�`@p�@p1'@ol�@o
=@nȴ@n��@n�+@n{@m�-@mp�@l�@l�@lz�@lI�@l1@k��@j�H@jM�@j�@ihs@h��@h��@h  @gl�@f��@f��@fff@f$�@e��@ep�@e/@dZ@d�@c��@cS�@b��@b��@b~�@b-@bJ@a��@a��@aG�@`��@`��@`Q�@_\)@^�@^ȴ@^��@^5?@]��@]p�@]O�@]/@\��@\z�@\j@\Z@\9X@[��@[�
@[C�@[@Z��@Z=q@Y��@Y��@Y%@X��@Xr�@XA�@W��@W|�@W+@W
=@V��@VV@V{@U�@U�h@U`B@U/@T�@TZ@T(�@TI�@TI�@S��@S�@So@R�H@R��@R�\@R^5@Q��@Q�^@Q�7@Qhs@Q&�@P��@P�@Pr�@P1'@O�w@O�P@O+@Nff@N$�@M��@M?}@L�@L�@Lj@L(�@K��@Kƨ@K33@J��@J~�@I�@I��@I�@H�9@Hr�@H  @G�;@G�w@G�P@G|�@G;d@Fȴ@FE�@F{@E�@E�-@Ep�@E/@D�/@D�@D�@C��@Cƨ@CC�@B�!@B-@A�@A��@A7L@@�@?�;@?�@?l�@?K�@>��@>ȴ@>��@>V@>5?@=�@=�h@=`B@=V@<�@<�D@<9X@;��@;�
@;dZ@:�@:�!@:^5@9�^@9�@8Ĝ@8Q�@7�w@7��@7l�@7
=@6ff@6E�@6@6@5�@5@5��@5�h@5O�@5�@4�j@4z�@41@3�F@3�@3t�@3C�@3"�@2��@2��@2~�@2n�@2n�@2=q@2�@1��@1�@1�^@1X@1%@0��@0bN@/�@/�;@/�@/|�@/;d@.��@.�@.�R@.V@-�T@-�h@-p�@-�@,��@,�@,��@,I�@+��@+t�@+o@+@*�@*�H@*�!@*�@)�@)�^@)7L@(��@(�@(bN@(  @'��@'|�@'\)@'+@&��@&��@&ȴ@&v�@&ff@&5?@&{@%�-@%�h@%p�@%�@$��@$�D@$z�@$Z@$9X@#��@#�F@#dZ@#"�@"��@"�!@"��@"^5@"-@!��@!X@!&�@ �`@ �9@ �u@ bN@ b@�@��@�P@K�@�@�y@��@��@ff@�@@��@O�@�@�@V@��@�j@�D@Z@I�@�m@�F@t�@C�@"�@�H@n�@=q@�@��@�^@X@&�@��@�9@�u@bN@A�@  @�@�P@|�@l�@K�@
=@
=@ȴ@��@ff@$�@@��@�-@�@�@p�@O�@�@��@�/@�j@��@z�@Z@9X@9X@�@�m@ƨ@��@S�@�@�!@��@~�@^5@=q@=q@�@J@�#@��@x�@hs@G�@7L@�@��@��@�u@r�@Q�@ �@�;@��@��@�P@�P@K�@�@�@�R@��@��@��@�+@ff@V@E�@5?@$�@@��@�h@`B@O�@?}@��@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�5?A�5?A�7LA�9XA�A�A�K�A�K�A�K�A�M�A�M�A�O�A�O�A�Q�A�Q�A�Q�A�S�A�S�A�VA�VA�XA�XA�ZA�ZA�ZA�\)A�^5A�^5A�`BA�`BA�`BA�bNA�bNA�dZA�dZA�ffA�ffA�ffA�ffA�hsA�hsA�jA�jA�l�A�l�A�l�A�l�A�n�A�p�A�ffA�E�A�(�A�5?A��yAѸRAЗ�A�(�A�M�A��;AάA�bA�r�A�=qAȓuA�ĜAüjAº^A��`A�{A�A��DA��-A��HA�ƨA�M�A�1A��\A��A��jA��7A��yA�`BA��A�VA�M�A�t�A�p�A��+A�JA��9A��DA�hsA�(�A��A�dZA��+A�`BA�r�A��A�G�A��\A�=qA�K�A�/A�
=A��jA�$�A�A�A���A�C�A�5?A��A�\)A�1A��A�t�A���A�A��PA��9A���A�I�A��A���A��A�A�XA�n�A�z�A�?}A��A��TA�E�A��9A��!A~��A|I�Az�HAw��At��Aq��Ao33AmC�Aj$�Ag�mAfJAa�mAa7LA`�9A_�A^~�A\�/AX=qAVz�AU\)AT��AR�RAQ�API�AOC�AK�PAG�;ADVAA?}A=�A;S�A9�A7��A5K�A2�`A1��A0Q�A/C�A-C�A+�A(�9A(JA'XA&ffA$��A"z�A VA $�A �A�TAn�A��A�+A/A�\A1'A�A�7A{A&�A�HA��A�A5?A{A  A�TA�-Ax�A+A��A5?A�\A��A��A`BAffA~�AE�AbA�A	�-A��AȴA�uA�A\)A�A��A�A��A�A�#A~�A  A�#A|�A ��A �@�5?@��;@��h@���@��R@�G�@�33@�&�@��m@��@�\@���@�b@�"�@�-@�j@� �@畁@�-@��
@�P@�P@�{@��@ޏ\@�G�@�r�@�+@�=q@��;@�M�@�@��@�I�@��m@�|�@ҏ\@�Ĝ@�1'@��;@ϥ�@�t�@�dZ@�E�@��#@͉7@̬@��
@˅@˕�@�K�@�v�@ɲ-@�O�@���@ǥ�@��@��@�X@��@�j@Ý�@�V@��T@�x�@�Q�@���@��@�hs@��@���@�|�@�l�@�K�@��H@�V@�$�@��@��T@�x�@�V@���@�I�@�(�@���@�o@�5?@��-@�&�@�%@��@��D@�Q�@�1'@��@��F@�C�@��!@�^5@�@��^@��@���@��@�j@�  @��@���@�  @��@�Q�@��@��@�33@�C�@�Z@��;@�l�@�"�@���@���@�v�@���@���@�p�@��@���@�(�@�(�@�b@���@�33@�+@���@��\@��@��@��@�j@�Q�@�(�@���@���@��@�o@�@�n�@�V@�=q@�{@��h@�`B@�`B@�7L@��@��@���@���@�bN@�1'@�b@�ƨ@�t�@�C�@�o@�ȴ@���@�^5@�5?@�{@��@�@���@��h@�G�@��/@�(�@��F@�o@��!@��!@��R@��+@�n�@�^5@�^5@�M�@��#@�@�p�@��@�%@���@�Q�@��@���@�\)@���@�ȴ@��!@��\@��+@�n�@�E�@��@�p�@��@�V@�%@�z�@��
@��@���@���@�S�@��@�@���@��#@���@���@���@��h@�hs@�V@���@��m@���@���@��@�dZ@�"�@���@�v�@�E�@��^@�`B@�?}@��@��`@���@�bN@�1'@�  @���@�\)@�;d@��@���@�ȴ@��!@�ff@���@�X@���@��@�Z@�1@��;@��w@�S�@���@�~�@�E�@���@��#@���@�?}@�%@���@�j@�A�@� �@�1@��;@��F@���@�l�@��@��R@�v�@�ff@�E�@�E�@�-@��T@���@�hs@�/@�&�@��@���@�bN@�(�@��@�b@�  @��m@��w@�l�@�"�@�
=@��@��H@��H@��@�ȴ@���@�=q@�$�@���@�G�@���@�Ĝ@���@�`B@�X@�/@�%@���@���@�A�@�@~ȴ@~V@~$�@}@}V@{t�@z~�@z^5@z=q@y�^@yG�@x��@xQ�@w��@w
=@v�R@vv�@v5?@u�T@u�-@u�-@u`B@t�@tj@s��@sC�@r�H@r�!@q�7@qX@q��@q�^@p��@p�`@p�`@p�@p1'@ol�@o
=@nȴ@n��@n�+@n{@m�-@mp�@l�@l�@lz�@lI�@l1@k��@j�H@jM�@j�@ihs@h��@h��@h  @gl�@f��@f��@fff@f$�@e��@ep�@e/@dZ@d�@c��@cS�@b��@b��@b~�@b-@bJ@a��@a��@aG�@`��@`��@`Q�@_\)@^�@^ȴ@^��@^5?@]��@]p�@]O�@]/@\��@\z�@\j@\Z@\9X@[��@[�
@[C�@[@Z��@Z=q@Y��@Y��@Y%@X��@Xr�@XA�@W��@W|�@W+@W
=@V��@VV@V{@U�@U�h@U`B@U/@T�@TZ@T(�@TI�@TI�@S��@S�@So@R�H@R��@R�\@R^5@Q��@Q�^@Q�7@Qhs@Q&�@P��@P�@Pr�@P1'@O�w@O�P@O+@Nff@N$�@M��@M?}@L�@L�@Lj@L(�@K��@Kƨ@K33@J��@J~�@I�@I��@I�@H�9@Hr�@H  @G�;@G�w@G�P@G|�@G;d@Fȴ@FE�@F{@E�@E�-@Ep�@E/@D�/@D�@D�@C��@Cƨ@CC�@B�!@B-@A�@A��@A7L@@�@?�;@?�@?l�@?K�@>��@>ȴ@>��@>V@>5?@=�@=�h@=`B@=V@<�@<�D@<9X@;��@;�
@;dZ@:�@:�!@:^5@9�^@9�@8Ĝ@8Q�@7�w@7��@7l�@7
=@6ff@6E�@6@6@5�@5@5��@5�h@5O�@5�@4�j@4z�@41@3�F@3�@3t�@3C�@3"�@2��@2��@2~�@2n�@2n�@2=q@2�@1��@1�@1�^@1X@1%@0��@0bN@/�@/�;@/�@/|�@/;d@.��@.�@.�R@.V@-�T@-�h@-p�@-�@,��@,�@,��@,I�@+��@+t�@+o@+@*�@*�H@*�!@*�@)�@)�^@)7L@(��@(�@(bN@(  @'��@'|�@'\)@'+@&��@&��@&ȴ@&v�@&ff@&5?@&{@%�-@%�h@%p�@%�@$��@$�D@$z�@$Z@$9X@#��@#�F@#dZ@#"�@"��@"�!@"��@"^5@"-@!��@!X@!&�@ �`@ �9@ �u@ bN@ b@�@��@�P@K�@�@�y@��@��@ff@�@@��@O�@�@�@V@��@�j@�D@Z@I�@�m@�F@t�@C�@"�@�H@n�@=q@�@��@�^@X@&�@��@�9@�u@bN@A�@  @�@�P@|�@l�@K�@
=@
=@ȴ@��@ff@$�@@��@�-@�@�@p�@O�@�@��@�/@�j@��@z�@Z@9X@9X@�@�m@ƨ@��@S�@�@�!@��@~�@^5@=q@=q@�@J@�#@��@x�@hs@G�@7L@�@��@��@�u@r�@Q�@ �@�;@��@��@�P@�P@K�@�@�@�R@��@��@��@�+@ff@V@E�@5?@$�@@��@�h@`B@O�@?}@��@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺBoBDBB
�B
�B
�B
��B
��BhB#�B33B)�B!�B"�B]/Bl�B|�B��B��B�RB�^B�jB��BǮBɺB��B��B��B��B��B��B�
B�B�TB�B�B�B�B�B�B�B��B��B�B�yB�yB�fB�#B�
B�B��BɺB�wB�9B�B��B��B�DB�B~�Bw�Bm�BhsBYBE�B=qB1'B)�B&�B!�B�BbB	7B
��B
�B
�B
�HB
ɺB
�FB
��B
{�B
gmB
VB
K�B
:^B
#�B
uB	��B	�B	�HB	��B	ÖB	��B	��B	��B	��B	��B	�DB	s�B	e`B	]/B	W
B	M�B	A�B	;dB	49B	&�B	uB	B��B�B�B�fB�fB�TB�;B�5B�/B�/B�B�
BĜBB�}BÖB�XB�FB�B�B�B�'B�3B�B�B��B��B�B�!B�^B�^B�?B�LB�^B�jB�qB�wB�wB�wB�wB�qB�qB�qB�qB�FB�B��B��B��B��B�B�!B�3B�9B�3B�3B�3B�-B�'B�B�B�B�B�B�B�B�B�B��B��B��B�B�B�B�B�B�B�B��B��B�B�B�3B�'B�-B�!B�B�B�?B�9B�'B�?B�^B�XB�FB�FB�FB�LB�FB�9B�LB�LB�FB�LB�LB�LB�RB�XB�qB�wB��BɺB��B��B��B��B��B��B��B�B�B�NB�yB�B�B�B�B��B��B��B��B	  B	B	B	B	B	+B	
=B	bB	oB	�B	�B	�B	�B	�B	�B	!�B	#�B	%�B	&�B	+B	/B	2-B	7LB	:^B	B�B	F�B	H�B	I�B	J�B	J�B	M�B	O�B	Q�B	R�B	S�B	VB	W
B	YB	\)B	_;B	e`B	ffB	ffB	gmB	iyB	jB	jB	k�B	o�B	q�B	v�B	v�B	|�B	�B	�B	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�?B	�?B	�?B	�LB	�dB	�dB	�jB	�qB	�wB	�}B	��B	ǮB	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�BB	�BB	�BB	�HB	�HB	�TB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
B
B
B
B
B
B
B
%B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B

=B

=B
JB
JB
JB
PB
PB
\B
bB
bB
hB
hB
oB
oB
uB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
 �B
�B
�B
�B
�B
"�B
$�B
$�B
$�B
$�B
$�B
#�B
#�B
#�B
"�B
$�B
$�B
$�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
&�B
(�B
(�B
'�B
)�B
+B
+B
,B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
49B
49B
49B
5?B
49B
5?B
6FB
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
:^B
9XB
9XB
:^B
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
>wB
>wB
>wB
@�B
@�B
@�B
A�B
B�B
B�B
B�B
D�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺBoBDBB
�B
�B
�B
��B
��BhB#�B33B)�B!�B"�B]/Bl�B|�B��B��B�RB�^B�jB��BǮBɺB��B��B��B��B��B��B�
B�B�TB�B�B�B�B�B�B�B��B��B�B�yB�yB�fB�#B�
B�B��BɺB�wB�9B�B��B��B�DB�B~�Bw�Bm�BhsBYBE�B=qB1'B)�B&�B!�B�BbB	7B
��B
�B
�B
�HB
ɺB
�FB
��B
{�B
gmB
VB
K�B
:^B
#�B
uB	��B	�B	�HB	��B	ÖB	��B	��B	��B	��B	��B	�DB	s�B	e`B	]/B	W
B	M�B	A�B	;dB	49B	&�B	uB	B��B�B�B�fB�fB�TB�;B�5B�/B�/B�B�
BĜBB�}BÖB�XB�FB�B�B�B�'B�3B�B�B��B��B�B�!B�^B�^B�?B�LB�^B�jB�qB�wB�wB�wB�wB�qB�qB�qB�qB�FB�B��B��B��B��B�B�!B�3B�9B�3B�3B�3B�-B�'B�B�B�B�B�B�B�B�B�B��B��B��B�B�B�B�B�B�B�B��B��B�B�B�3B�'B�-B�!B�B�B�?B�9B�'B�?B�^B�XB�FB�FB�FB�LB�FB�9B�LB�LB�FB�LB�LB�LB�RB�XB�qB�wB��BɺB��B��B��B��B��B��B��B�B�B�NB�yB�B�B�B�B��B��B��B��B	  B	B	B	B	B	+B	
=B	bB	oB	�B	�B	�B	�B	�B	�B	!�B	#�B	%�B	&�B	+B	/B	2-B	7LB	:^B	B�B	F�B	H�B	I�B	J�B	J�B	M�B	O�B	Q�B	R�B	S�B	VB	W
B	YB	\)B	_;B	e`B	ffB	ffB	gmB	iyB	jB	jB	k�B	o�B	q�B	v�B	v�B	|�B	�B	�B	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�?B	�?B	�?B	�LB	�dB	�dB	�jB	�qB	�wB	�}B	��B	ǮB	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�BB	�BB	�BB	�HB	�HB	�TB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
B
B
B
B
B
B
B
%B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B

=B

=B
JB
JB
JB
PB
PB
\B
bB
bB
hB
hB
oB
oB
uB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
 �B
�B
�B
�B
�B
"�B
$�B
$�B
$�B
$�B
$�B
#�B
#�B
#�B
"�B
$�B
$�B
$�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
&�B
(�B
(�B
'�B
)�B
+B
+B
,B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
49B
49B
49B
5?B
49B
5?B
6FB
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
:^B
9XB
9XB
:^B
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
>wB
>wB
>wB
@�B
@�B
@�B
A�B
B�B
B�B
B�B
D�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20201130093938  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20201130003951  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20201130003951  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20201130003951  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20201130003952  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20201130003952  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20201130003952  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20201130003952  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20201130003952  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201130003952                      G�O�G�O�G�O�                JA  ARUP                                                                        20201130005329                      G�O�G�O�G�O�                