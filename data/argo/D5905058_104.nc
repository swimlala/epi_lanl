CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-18T00:36:45Z creation;2018-11-18T00:36:48Z conversion to V3.1;2019-12-23T06:11:50Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �X   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181118003645  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               hA   JA  I2_0675_104                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ؑuʆB 1   @ؑv��-�@6�^5?}�cK�K]�d1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�ffB�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D���D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @[�@�@�A�HA&�HAF�HAf�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB	�RB�B�RB!�RB)�RB1�RB9�RBA�RBI�RBQ�RBY�RBa�RBi�RBq�RBz�B�B�B��)B��)B���B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C nCnCnCnCnC
nCnCnCnCnCnCnCnCnCnCnC nC"nC$nC&nC(nC*nC,nC.nC0��C2nC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCHnCJnCLnCNnCPnCRnCTnCVnCXnCZnC\nC^nC`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D�ʏD��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�P�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D�D���D��D�M�DÍ�D���D��D�M�Dč�D���D��D�M�Dō�D���D�
�D�M�Dƍ�D���D��D�M�DǍ�D���D��D�M�Dȍ�D���D��D�M�Dɍ�D���D��D�M�Dʍ�D���D��D�M�Dˍ�D���D��D�M�D̍�D���D��D�M�D͍�D���D��D�M�D΍�D���D��D�M�Dύ�D���D��D�M�DЍ�D���D��D�M�Dэ�D���D��D�M�Dҍ�D���D��D�M�DӍ�D���D��D�M�Dԍ�D���D��D�M�DՍ�D���D��D�M�D֍�D���D��D�M�D׍�D���D��D�M�D؍�D���D��D�M�Dٍ�D���D��D�J�Dڍ�D���D��D�M�Dۍ�D���D��D�M�D܍�D���D��D�M�Dݍ�D���D��D�M�Dލ�D���D��D�M�Dߍ�D���D��D�M�D���D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D���D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�5?A�5?A�5?A�7LA�7LA�9XA�9XA�;dA�;dA�1'A�5?A�33A�&�A��A�{A��A�33A���Aδ9A΍PAΙ�AΟ�AΕ�A�z�A�ffA�ZA�K�A��A���Aͺ^AͬA͝�A�G�A̴9A˓uA��A��A�K�A�-A�r�A�~�A�=qA��+A��FA�r�A��uA���A�+A��RA�oA�E�A�O�A�-A���A��A�O�A� �A�%A��#A��jA�=qA�VA���A���A�$�A�p�A���A��A�5?A���A���A��RA���A��+A�E�A��A�E�A���A��A�&�A���A�$�A�$�A�  A�"�A�ȴA���A��jA���A���A�/A��yA�ffA���A��\A���A�  A�bNA���A���A�A}��A{�AyAv�RAs\)ArĜAq�7Ao�Am�Ak�Ai�Ag33Ad�`Aa�;A`ĜA_��A^�DA[x�AY33AV�jAUC�ATffAR��AQS�AOK�AM��AMXAMAK��AJ �AI�AH�`AG�AEhsAC�hAA|�A?�A>��A=�FA;�A:�\A9��A9�7A81'A7?}A6ĜA6VA4�!A3�A2�9A2  A0z�A/\)A-ƨA-+A,��A,��A,Q�A+�
A*�!A*-A*-A*ZA*�DA*�\A)ƨA(�\A(=qA&~�A$��A#��A#XA!�wA ~�A�hA�RAv�A^5A�FAbA��A�A�
A��AhsA��A�A�7A�A�Av�A�^A��AG�A��A�yA�mA�hA
�A
9XA	�TA	+A��A��An�A;dA=qA�FAhsA�`A��A^5AbAO�A ��A �DA 5?@���@�%@�ƨ@��@��j@���@�S�@���@���@�G�@���@�Z@�"�@��@�J@�j@�v�@� �@�w@���@�;d@�=q@���@߶F@�@�5?@��@���@١�@�r�@�@��@��`@�\)@�=q@���@��@У�@��;@���@�~�@͉7@�;d@ə�@��/@��@Ƨ�@��T@őh@��/@�l�@�@�E�@�@��7@��D@��P@���@��-@�V@�Z@���@��^@�X@���@�Ĝ@��@��!@�J@���@�@��^@��7@��j@�9X@�33@��+@�v�@�n�@�V@�X@��/@�Q�@��@���@��P@�;d@���@�{@��@�|�@�v�@�I�@���@���@���@�^5@���@�7L@���@�Q�@��
@�+@���@���@��R@���@�V@��@��-@��@�G�@�V@��`@��/@�Q�@��
@�l�@�+@���@��y@���@��!@��y@�V@�-@�$�@��@��@�X@�O�@�X@�?}@�&�@�V@���@�Z@�9X@�b@��@���@�\)@��y@�n�@��@�X@���@��@�j@�(�@��
@��@��@�l�@�\)@���@��!@���@�ff@��@�x�@�O�@�?}@��`@��@��@���@���@���@��w@��F@�ƨ@���@�dZ@��H@���@�~�@��H@��@��@�"�@�33@��H@��!@�V@�V@�V@�-@�{@���@���@�X@��`@��@�(�@���@�
=@��@�l�@�+@�ȴ@���@��@��+@���@���@�~�@�-@��@�@�@�@��@���@���@���@��7@�`B@�?}@��@���@���@��j@�z�@� �@�  @��m@��w@��P@�t�@�+@���@��y@��R@�ff@��@��@��T@���@�?}@��@���@��@�bN@�Z@�b@��@��@���@�l�@��@��H@��!@�=q@��@��T@���@��7@�`B@�?}@�&�@���@���@��u@�r�@�A�@�b@���@��m@��P@�33@�
=@�ȴ@��+@�V@�E�@�J@���@��^@��h@�X@�V@���@�Ĝ@��@�j@�Q�@� �@��@K�@;d@+@~�y@~ȴ@~�R@~�R@~�+@}��@}O�@|�D@|I�@{ƨ@{C�@{@z�!@z^5@y�@yG�@x�`@x�9@xb@w�P@w|�@w
=@v@u�h@t�@t�@s��@s��@s33@s@r~�@rJ@q��@p��@pbN@p1'@o�;@o�@nV@m�@m�@l�/@l�@lZ@l1@k��@j��@j�\@j~�@j~�@j�\@j=q@i�@i��@iG�@i%@hr�@g��@gK�@f��@fȴ@fv�@fE�@f@e�-@e`B@eV@eV@d��@d�@d�@c��@c�@c"�@b�H@b��@b�\@bM�@a�@a�^@aX@a%@`�u@`b@_|�@^��@^�@^�+@^5?@]��@]p�@]?}@]�@]V@\��@\I�@\(�@[�m@[�F@[�@[dZ@[33@[@Z�!@Z^5@Y�@Y�^@Yx�@YX@Y7L@X�@W�;@W��@Wl�@Wl�@W�@Vȴ@Vff@VV@V$�@U�@U��@T��@T�j@T�@T�j@T�j@T�j@T�@T�@T��@T�D@TI�@S��@SdZ@S"�@R�@R�!@R^5@R�@Q��@Qhs@Q7L@Q�@P��@P��@P �@O�;@O�P@O;d@N�y@N�R@N�+@M�T@MV@L�/@LZ@K��@Kt�@K33@J��@JM�@I��@I��@Ihs@IG�@I&�@I%@H�`@H�9@H��@H�u@HA�@G�;@G�P@G+@F��@Fff@F@E�T@E��@E�-@E��@E��@D�/@D��@D(�@C�
@Cƨ@C�F@CS�@Co@B�@B-@BJ@A��@A��@Ax�@@�`@@�9@@�@@1'@@b@?�w@?�@?��@?��@?|�@?;d@?
=@>�y@>ȴ@>v�@>{@=@=�h@=/@<�@<��@<�D@;�m@;�
@;�F@;t�@;"�@:�@:�\@9�@9��@9�7@9�@8�`@8Ĝ@8��@8r�@8Q�@8Q�@8 �@7��@7�@7�P@7l�@7;d@6�@6v�@5@5�h@5�@5O�@5/@4��@4��@4�D@4I�@3�m@3S�@3@2�H@2��@2^5@2�@2J@2�@2J@1�@1x�@0�u@/�@/�w@/�P@/|�@/\)@/
=@.ȴ@.��@.��@.��@.ff@.5?@.5?@-�@-@-�h@-?}@-/@-V@,�/@,��@,j@,(�@+�m@+t�@+33@+o@*�H@*�!@*~�@*�@)�@)��@)�7@)&�@(Ĝ@(�u@(1'@'��@'��@'�P@'|�@';d@&�y@&�+@%�@%�@%�T@%@%�h@%�@%?}@$�/@$��@$��@$�j@$Z@#��@#ƨ@#��@#dZ@#S�@#"�@#@"�@"�!@"^5@"^5@"�@!�^@!X@!%@ �u@ bN@ 1'@   @�@�@��@�@��@l�@K�@�@��@E�@{@@p�@?}@V@�/@��@��@z�@I�@1@��@t�@dZ@dZ@S�@"�@o@�@�\@^5@=q@�@�^@x�@&�@%@��@��@�9@��@�@1'@  @�w@�@�P@\)@K�@+@��@�@�@ȴ@ȴ@��@ff@v�@E�@$�@@�T@�-@�@p�@O�@V@��@�@�D@Z@�@�
@dZ@33@"�@�@��@�\@n�@J@��@��@��@�7@x�@&�@��@�@bN@1'@  @�;@��@��@l�@
=@�y@��@��@V@$�@{@{@{@��@p�@�/@�@�@�j@�j@z�@1@�m@�F@�@S�@C�@33@"�@
�!@
��@
�\@
~�@
~�@
n�@
^5@
=q@
�@	��@	��@	�@	��@	��@	x�@	G�@��@��@�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�5?A�5?A�5?A�7LA�7LA�9XA�9XA�;dA�;dA�1'A�5?A�33A�&�A��A�{A��A�33A���Aδ9A΍PAΙ�AΟ�AΕ�A�z�A�ffA�ZA�K�A��A���Aͺ^AͬA͝�A�G�A̴9A˓uA��A��A�K�A�-A�r�A�~�A�=qA��+A��FA�r�A��uA���A�+A��RA�oA�E�A�O�A�-A���A��A�O�A� �A�%A��#A��jA�=qA�VA���A���A�$�A�p�A���A��A�5?A���A���A��RA���A��+A�E�A��A�E�A���A��A�&�A���A�$�A�$�A�  A�"�A�ȴA���A��jA���A���A�/A��yA�ffA���A��\A���A�  A�bNA���A���A�A}��A{�AyAv�RAs\)ArĜAq�7Ao�Am�Ak�Ai�Ag33Ad�`Aa�;A`ĜA_��A^�DA[x�AY33AV�jAUC�ATffAR��AQS�AOK�AM��AMXAMAK��AJ �AI�AH�`AG�AEhsAC�hAA|�A?�A>��A=�FA;�A:�\A9��A9�7A81'A7?}A6ĜA6VA4�!A3�A2�9A2  A0z�A/\)A-ƨA-+A,��A,��A,Q�A+�
A*�!A*-A*-A*ZA*�DA*�\A)ƨA(�\A(=qA&~�A$��A#��A#XA!�wA ~�A�hA�RAv�A^5A�FAbA��A�A�
A��AhsA��A�A�7A�A�Av�A�^A��AG�A��A�yA�mA�hA
�A
9XA	�TA	+A��A��An�A;dA=qA�FAhsA�`A��A^5AbAO�A ��A �DA 5?@���@�%@�ƨ@��@��j@���@�S�@���@���@�G�@���@�Z@�"�@��@�J@�j@�v�@� �@�w@���@�;d@�=q@���@߶F@�@�5?@��@���@١�@�r�@�@��@��`@�\)@�=q@���@��@У�@��;@���@�~�@͉7@�;d@ə�@��/@��@Ƨ�@��T@őh@��/@�l�@�@�E�@�@��7@��D@��P@���@��-@�V@�Z@���@��^@�X@���@�Ĝ@��@��!@�J@���@�@��^@��7@��j@�9X@�33@��+@�v�@�n�@�V@�X@��/@�Q�@��@���@��P@�;d@���@�{@��@�|�@�v�@�I�@���@���@���@�^5@���@�7L@���@�Q�@��
@�+@���@���@��R@���@�V@��@��-@��@�G�@�V@��`@��/@�Q�@��
@�l�@�+@���@��y@���@��!@��y@�V@�-@�$�@��@��@�X@�O�@�X@�?}@�&�@�V@���@�Z@�9X@�b@��@���@�\)@��y@�n�@��@�X@���@��@�j@�(�@��
@��@��@�l�@�\)@���@��!@���@�ff@��@�x�@�O�@�?}@��`@��@��@���@���@���@��w@��F@�ƨ@���@�dZ@��H@���@�~�@��H@��@��@�"�@�33@��H@��!@�V@�V@�V@�-@�{@���@���@�X@��`@��@�(�@���@�
=@��@�l�@�+@�ȴ@���@��@��+@���@���@�~�@�-@��@�@�@�@��@���@���@���@��7@�`B@�?}@��@���@���@��j@�z�@� �@�  @��m@��w@��P@�t�@�+@���@��y@��R@�ff@��@��@��T@���@�?}@��@���@��@�bN@�Z@�b@��@��@���@�l�@��@��H@��!@�=q@��@��T@���@��7@�`B@�?}@�&�@���@���@��u@�r�@�A�@�b@���@��m@��P@�33@�
=@�ȴ@��+@�V@�E�@�J@���@��^@��h@�X@�V@���@�Ĝ@��@�j@�Q�@� �@��@K�@;d@+@~�y@~ȴ@~�R@~�R@~�+@}��@}O�@|�D@|I�@{ƨ@{C�@{@z�!@z^5@y�@yG�@x�`@x�9@xb@w�P@w|�@w
=@v@u�h@t�@t�@s��@s��@s33@s@r~�@rJ@q��@p��@pbN@p1'@o�;@o�@nV@m�@m�@l�/@l�@lZ@l1@k��@j��@j�\@j~�@j~�@j�\@j=q@i�@i��@iG�@i%@hr�@g��@gK�@f��@fȴ@fv�@fE�@f@e�-@e`B@eV@eV@d��@d�@d�@c��@c�@c"�@b�H@b��@b�\@bM�@a�@a�^@aX@a%@`�u@`b@_|�@^��@^�@^�+@^5?@]��@]p�@]?}@]�@]V@\��@\I�@\(�@[�m@[�F@[�@[dZ@[33@[@Z�!@Z^5@Y�@Y�^@Yx�@YX@Y7L@X�@W�;@W��@Wl�@Wl�@W�@Vȴ@Vff@VV@V$�@U�@U��@T��@T�j@T�@T�j@T�j@T�j@T�@T�@T��@T�D@TI�@S��@SdZ@S"�@R�@R�!@R^5@R�@Q��@Qhs@Q7L@Q�@P��@P��@P �@O�;@O�P@O;d@N�y@N�R@N�+@M�T@MV@L�/@LZ@K��@Kt�@K33@J��@JM�@I��@I��@Ihs@IG�@I&�@I%@H�`@H�9@H��@H�u@HA�@G�;@G�P@G+@F��@Fff@F@E�T@E��@E�-@E��@E��@D�/@D��@D(�@C�
@Cƨ@C�F@CS�@Co@B�@B-@BJ@A��@A��@Ax�@@�`@@�9@@�@@1'@@b@?�w@?�@?��@?��@?|�@?;d@?
=@>�y@>ȴ@>v�@>{@=@=�h@=/@<�@<��@<�D@;�m@;�
@;�F@;t�@;"�@:�@:�\@9�@9��@9�7@9�@8�`@8Ĝ@8��@8r�@8Q�@8Q�@8 �@7��@7�@7�P@7l�@7;d@6�@6v�@5@5�h@5�@5O�@5/@4��@4��@4�D@4I�@3�m@3S�@3@2�H@2��@2^5@2�@2J@2�@2J@1�@1x�@0�u@/�@/�w@/�P@/|�@/\)@/
=@.ȴ@.��@.��@.��@.ff@.5?@.5?@-�@-@-�h@-?}@-/@-V@,�/@,��@,j@,(�@+�m@+t�@+33@+o@*�H@*�!@*~�@*�@)�@)��@)�7@)&�@(Ĝ@(�u@(1'@'��@'��@'�P@'|�@';d@&�y@&�+@%�@%�@%�T@%@%�h@%�@%?}@$�/@$��@$��@$�j@$Z@#��@#ƨ@#��@#dZ@#S�@#"�@#@"�@"�!@"^5@"^5@"�@!�^@!X@!%@ �u@ bN@ 1'@   @�@�@��@�@��@l�@K�@�@��@E�@{@@p�@?}@V@�/@��@��@z�@I�@1@��@t�@dZ@dZ@S�@"�@o@�@�\@^5@=q@�@�^@x�@&�@%@��@��@�9@��@�@1'@  @�w@�@�P@\)@K�@+@��@�@�@ȴ@ȴ@��@ff@v�@E�@$�@@�T@�-@�@p�@O�@V@��@�@�D@Z@�@�
@dZ@33@"�@�@��@�\@n�@J@��@��@��@�7@x�@&�@��@�@bN@1'@  @�;@��@��@l�@
=@�y@��@��@V@$�@{@{@{@��@p�@�/@�@�@�j@�j@z�@1@�m@�F@�@S�@C�@33@"�@
�!@
��@
�\@
~�@
~�@
n�@
^5@
=q@
�@	��@	��@	�@	��@	��@	x�@	G�@��@��@�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�
B�
B�
B�)B�TB�B+B?}B>wB49B-B0!B2-B1'B.B)�B'�B$�B�BhBJB
=BVB�B2-BM�BgmB��B��B�B�B��B��B��B��B��B��B�{B�PB�DB�%B�B�B�B~�B|�B}�B~�B� B�B{�Bu�Bs�Bp�Bn�BhsBcTB]/B[#BXBS�BK�BH�BG�BF�BC�B?}B8RB0!B&�B�BoB��B�B�!B�uBp�BK�B'�B+B
�B
�mB
�HB
�)B
�5B
�NB
ĜB
��B
��B
�+B
|�B
iyB
ZB
I�B
9XB
"�B
JB
B	��B	�B	�
B	��B	�XB	�B	��B	�B	z�B	u�B	l�B	[#B	O�B	B�B	6FB	0!B	$�B	�B	uB		7B	%B	B��B�B�sB�sB�`B�NB�B��B�dB�LB�-B��B��B��B��B��B��B��B�uB�VB�1B�B�+B|�B{�Bv�Bt�Bu�Bw�B{�Bz�Bu�Bs�Bs�Bu�Bw�B{�B~�B{�Bz�Bw�Bq�Bl�BjBgmBcTBaHB_;B]/B\)B\)BXBW
BVBQ�BO�BM�BJ�BJ�BH�BG�BE�BC�BA�B>wB<jB9XB7LB6FB49B49B33B2-B2-B/B/B.B.B,B+B+B)�B(�B&�B%�B%�B#�B#�B#�B!�B�B�B�B�B �B �B �B!�B!�B!�B �B!�B#�B%�B%�B'�B'�B&�B'�B+B)�B,B+B,B,B,B/B/B0!B/B0!B0!B2-B33B5?B5?B6FB7LB6FB8RB8RB;dB<jB=qB=qBA�BB�BB�BD�BH�BJ�BK�BK�BM�BO�BR�BT�BW
BXBZB_;BbNBcTBdZBdZBgmBk�Bl�Bn�Bo�Bo�Bp�Br�Bs�Bw�B{�B|�B|�B}�B�B�B�+B�+B�1B�1B�1B�7B�=B�PB��B��B��B�B�B�9B�FB�XB�}BÖBȴB��B��B�B�B�
B�B�#B�HB�ZB�fB�yB�B�B�B��B��B	B	%B	DB	PB	\B	\B	�B	�B	�B	�B	�B	�B	#�B	&�B	+B	0!B	0!B	1'B	33B	7LB	8RB	9XB	<jB	>wB	@�B	B�B	D�B	D�B	E�B	G�B	L�B	N�B	O�B	P�B	Q�B	T�B	W
B	W
B	ZB	[#B	[#B	[#B	[#B	[#B	\)B	_;B	cTB	ffB	ffB	gmB	gmB	hsB	jB	jB	l�B	m�B	n�B	o�B	q�B	q�B	w�B	}�B	~�B	�B	�B	�B	�B	�B	�+B	�7B	�JB	�PB	�bB	�hB	�oB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�9B	�?B	�FB	�LB	�XB	�XB	�^B	�dB	�dB	�jB	�qB	�}B	�}B	�}B	��B	��B	��B	B	B	ÖB	ĜB	ƨB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�;B	�BB	�HB	�NB	�TB	�TB	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B

=B

=B

=B

=B

=B

=B

=B
JB
JB
PB
PB
PB
VB
VB
VB
VB
\B
bB
bB
hB
hB
hB
oB
oB
oB
oB
uB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
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
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
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
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
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
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
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
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
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
o�B
p�B
p�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B�B�:B�qBB?cB>]B4B,�B0B2B0�B-�B)�B'�B$�B�BNB0B
#B"BkB2BM�BgRB��B��B��B��B��B��B��B�qB�kB�yB�aB�B�B�B��B��B��B~�B|�B}�B~�B�B��B{�Bu�Bs�BpoBncBhXBc B\�B[	BW�BS�BK�BH�BGzBF�BC{B?HB8B/�B&�B�BTB��B��B��B�[Bp�BK�B'�BB
�}B
�8B
�-B
��B
�B
�B
�gB
��B
�kB
��B
|�B
iDB
Y�B
I�B
9$B
"�B
B
�B	��B	�WB	��B	ʦB	�$B	��B	�qB	��B	z�B	u�B	lWB	Z�B	O�B	B[B	6B	/�B	$�B	sB	@B		B	�B	�B��B�oB�>B�>B�,B�4B��B̘B�0B�B��B��B��B�~B�qB�YB�MB�SB�@B�"B��B��B�B|�B{�Bv�Bt�Bu�Bw�B{�Bz�Bu�Bs�Bs�Bu�Bw�B{�B~�B{�Bz�Bw�BqvBlWBjKBg8Bc BaB_B\�B[�B[�BW�BV�BU�BQ�BO�BM�BJ�BJ�BH�BGzBEmBCaBAUB>BB<6B9$B72B6B4B4B2�B1�B1�B.�B.�B-�B-�B+�B*�B*�B)�B(�B&�B%�B%�B#�B#�B#�B!�B�B�B�B�B �B �B �B!�B!�B!�B �B!�B#�B%�B%�B'�B'�B&�B'�B*�B)�B+�B*�B+�B+�B+�B.�B.�B/�B.�B/�B/�B1�B2�B5B5B5�B7B6B8B8B;0B<6B=<B=<BAUBB[BBABDMBH�BJ�BK�BK�BM�BO�BR�BT�BV�BW�BY�B_BbBc Bd&Bd&Bg8BkQBlWBncBoiBoiBpoBr|Bs�Bw�B{�B|�B|�B}�B��B��B��B��B��B��B��B�B�	B�B�SB�qB��B��B��B��B�B�$B�HB�aB�fB˒BбBյBյB��B��B��B�B�&B�2B�DB�]B�iB�oB��B��B	�B	�B	
�B	B	(B	(B	SB	_B	EB	eB	qB	�B	#�B	&�B	*�B	/�B	/�B	0�B	2�B	7B	8B	9$B	<6B	>BB	@OB	BAB	DgB	DgB	EmB	GzB	L�B	N�B	O�B	P�B	Q�B	T�B	V�B	V�B	Y�B	Z�B	Z�B	Z�B	Z�B	Z�B	[�B	^�B	c B	f2B	f2B	g8B	g8B	h$B	jKB	j0B	lWB	m]B	ncB	oOB	qvB	qvB	w�B	}�B	~�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�.B	�4B	�:B	�4B	�:B	�FB	�SB	�_B	�eB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�$B	�*B	�B	�0B	�6B	�<B	�.B	�HB	�HB	�4B	�OB	�;B	�[B	�[B	�aB	�gB	�tB	�fB	ȀB	ȀB	ɆB	̘B	̘B	͟B	ΥB	ϑB	ϫB	ѷB	ҽB	��B	��B	��B	��B	ּB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	� B	� B	�,B	�2B	�B	�8B	�$B	�*B	�*B	�0B	�QB	�QB	�QB	�WB	�cB	�cB	�IB	�iB	�oB	�oB	�vB	�|B	�hB	�|B	�B	�B	�B	�B	�B	�B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	�B

	B

	B

	B
	�B

	B

	B
B
B
B
B
B
"B
B
"B
"B
(B
B
.B
4B
B
B
:B
:B
 B
:B
@B
:B
:B
@B
@B
FB
FB
FB
MB
MB
MB
SB
9B
?B
?B
eB
eB
QB
QB
kB
qB
qB
xB
xB
xB
xB
xB
dB
xB
~B
dB
~B
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
!|B
"�B
"�B
"�B
#�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
.�B
.�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
2�B
2�B
2�B
4B
4B
4B
4B
4B
4B
4�B
5B
4�B
6B
6B
5�B
6B
5�B
6B
7B
6�B
7B
8B
8B
9	B
9	B
9	B
9$B
9$B
9$B
:*B
:*B
:*B
:*B
:*B
:*B
;0B
;0B
;B
<B
<6B
<6B
=<B
=<B
=<B
=<B
>BB
>BB
>(B
>BB
?.B
?.B
?HB
@OB
@4B
@OB
A;B
A;B
AUB
AUB
AUB
AUB
AUB
AUB
B[B
B[B
BAB
B[B
CGB
CaB
CaB
DgB
DgB
DgB
DgB
DMB
EmB
DgB
DgB
DgB
DgB
ESB
EmB
EmB
ESB
FYB
GzB
G_B
GzB
GzB
GzB
H�B
I�B
I�B
J�B
J�B
J�B
JrB
J�B
J�B
K�B
J�B
K�B
K�B
K�B
K�B
KxB
K�B
KxB
L�B
L�B
L~B
L~B
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
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
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
S�B
S�B
S�B
S�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
^B
^B
^B
^B
^B
_B
_B
_B
_B
`B
`B
`B
_�B
`B
`B
`�B
aB
aB
aB
aB
aB
aB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
c B
cB
c B
c B
c B
cB
c B
cB
d&B
dB
d&B
d&B
dB
d&B
e,B
e,B
e,B
fB
f2B
f2B
f2B
f2B
f2B
g8B
g8B
g8B
g8B
gB
g8B
gB
g8B
h>B
h>B
h>B
i*B
i*B
i*B
iDB
iDB
jKB
jKB
jKB
jKB
kQB
k6B
k6B
kQB
k6B
kQB
kQB
kQB
kQB
kQB
kQB
k6B
lWB
m]B
m]B
ncB
ncB
ncB
nIB
nIB
ncB
ncB
nIB
ncB
ncB
oOB
oiB
oiB
oiB
oiB
oiB
oiB
oiB
oiB
poB
poB
poB
po111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.43(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811230039272018112300392720181123003927201811240031582018112400315820181124003158JA  ARFMdecpA19c                                                                20181118093613  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181118003645  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181118003647  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181118003647  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181118003648  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181118003648  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181118003648  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181118003648  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181118003648  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181118003648                      G�O�G�O�G�O�                JA  ARUP                                                                        20181118005554                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181118153853  CV  JULD            G�O�G�O�Fċ�                JM  ARCAJMQC2.0                                                                 20181122153927  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181122153927  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181123153158  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                