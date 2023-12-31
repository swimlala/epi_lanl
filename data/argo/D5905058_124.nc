CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-02-11T06:36:30Z creation;2019-02-11T06:36:33Z conversion to V3.1;2019-12-23T06:07:08Z update;     
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20190211063630  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               |A   JA  I2_0675_124                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ئ�M�� 1   @ئ�`� @7�rGE8��c4�䎊1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  @���AffA@  A`  A�  A�  A�  A���A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ Dм�D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@�ffA	��A)��AK33Ak33A���A���A���A�ffA�ffAՙ�A噚A���B��B
��B��B��B"��B*��B2��B:��BB��BJ��BR��BZ��Bb��Bj��Br��Bz��B�ffB�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC �3C�3C�3C�3C�3C
�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C �3C"�3C$�3C&�3C(�3C*�3C,�3C.�3C0�3C2�3C4�3C6�3C8�3C:�3C<�3C>�3C@�3CB�3CD�3CF�3CH�3CJ�3CL�3CN�3CP�3CR�3CT�3CV�3CX�3CZ�3C\�3C^�3C`�3Cb�3Cd�3Cf�3Ch�3Cj�3Cl�3Cn�3Cp�3Cr�3Ct�3Cv�3Cx�3Cz�3C|�3C~�3C�Y�C�Y�C�Y�C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�L�C�Y�C�Y�C�Y�C�Y�C�Y�D 33D ��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D	,�D	��D
,�D
��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D ,�D ��D!,�D!��D",�D"��D#,�D#��D$,�D$��D%,�D%��D&,�D&��D',�D'��D(,�D(��D),�D)��D*,�D*��D+,�D+��D,,�D,��D-,�D-��D.,�D.��D/,�D/��D0,�D0��D1,�D1��D233D2��D3,�D3��D4,�D4��D5,�D5��D6,�D6��D7,�D7��D8,�D8��D9,�D9��D:,�D:��D;,�D;��D<,�D<��D=,�D=��D>,�D>��D?,�D?��D@,�D@��DA,�DA��DB,�DB��DC,�DC��DD,�DD��DE,�DE��DF,�DF��DG,�DG��DH,�DH��DI,�DI��DJ,�DJ��DK,�DK��DL,�DL��DM,�DM��DN,�DN��DO,�DO��DP,�DP��DQ,�DQ��DR,�DR��DS,�DS��DT,�DT��DU,�DU��DV,�DV��DW,�DW��DX,�DX��DY,�DY��DZ,�DZ��D[,�D[��D\,�D\��D],�D]��D^,�D^��D_,�D_��D`,�D`��Da,�Da��Db,�Db��Dc,�Dc��Dd,�Dd��De,�De��Df,�Df��Dg,�Dg��Dh,�Dh��Di,�Di��Dj,�Dj��Dk,�Dk��Dl,�Dl��Dm,�Dm��Dn,�Dn��Do,�Do��Dp,�Dp��Dq,�Dq��Dr,�Dr��Ds,�Ds��Dt,�Dt��Du,�Du��Dv,�Dv��Dw,�Dw��Dx,�Dx��Dy,�Dy��Dz,�Dz��D{,�D{��D|,�D|��D},�D}��D~,�D~��D,�D��D�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD���D��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfDfD��fD�fD�VfDÖfD��fD�fD�VfDĖfD��fD�fD�VfDŖfD��fD�fD�VfDƖfD��fD�fD�VfDǖfD��fD�fD�VfDȖfD��fD�fD�VfDɖfD��fD�fD�VfDʖfD��fD�fD�VfD˖fD��fD�fD�VfD̖fD��fD�fD�VfD͖fD��fD�fD�VfDΖfD��fD�fD�VfDϖfD��fD�fD�VfDЖfD��3D�fD�VfDіfD��fD�fD�VfDҖfD��fD�fD�VfDӖfD��fD�fD�VfDԖfD��fD�fD�VfDՖfD��fD�fD�VfD֖fD��fD�fD�VfDזfD��fD�fD�VfDؖfD��fD�fD�VfDٖfD��fD�fD�VfDږfD��fD�fD�VfDۖfD��fD�fD�VfDܖfD��fD�fD�VfDݖfD��fD�fD�VfDޖfD��fD�fD�VfDߖfD��fD�fD�VfD��fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��3D�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A���A�ȴA���A��A��HA��A��yA�%A�1A�VA�bA�JA�bA�A�%A�  A���A���A��A���A���A���A���A���A���A�JA�{A��A�{A�{A��A��A��A�"�A�33A�O�A�bNA�dZA��7A��uA���A���A���A���A��A��9A��FA��FA��9A��9A���A���A���A�dZA�=qA���A�ƨA���A���A�~�A��7A�VA�O�A�33A�ƨA�VA��A��A��A�VA�VA�Q�A��DA��/A�  A�hsA�JA��/A��A��A��A���A���A��A�G�A��uA���A�jA�$�A��A��
A�ffA���A���A�{A��hA��A��A���A�ƨA�S�A���A��7A�\)A���A��DA�bNA�/A�z�A��`A���A�r�A�v�A�l�A�|�A���A���A���A���A��`A�M�A�JA��A��A��Al�A|�9AzAwO�At�ApȴAm%Aj(�Ag�
Ae+Ab�A`ffA^�AZr�AV��AUC�ATI�AS�wAQXAOt�AM�
ALjAL1AKVAI�TAH��AG�AG"�AFn�AE�^AC�AAS�A@�yA@�A?�wA?G�A?%A>jA<  A9��A8��A7�mA5��A4��A3�TA2��A1�7A0bNA/�A.9XA-?}A+�A*�HA)�FA)O�A(��A(n�A'��A'+A&�yA&v�A%A#�A!��A!
=A �+A��A7LA��A�A��A�+A�A�TA��A?}A��An�AO�A�A�AG�A�/A~�A�wA�jA$�A�A��A�A�;A
��A	��A	oA9XA\)AbNA33AjA��A��A�wA;dA �@��D@���@�7L@��@�S�@�p�@�@��@���@�G�@�7L@��@��T@�V@�@�!@���@�V@��;@��@��@�1'@߮@ޏ\@ݑh@ܛ�@ܓu@�I�@���@��@�
=@ڇ+@�n�@��@��@��#@�x�@�7L@�"�@�V@�K�@�J@��@Ұ!@�{@϶F@Χ�@θR@���@���@�A�@˥�@��T@���@�7L@��@�E�@�v�@ɑh@�bN@ǥ�@�+@�/@ÍP@���@��;@�{@�-@�J@�?}@���@�1@���@��T@�?}@���@�dZ@��h@��@��@�
=@��R@�~�@�^5@��@���@�t�@���@���@�p�@�j@�b@��F@�^5@�7L@��j@�C�@��H@��+@��@��h@�X@�%@�V@��@���@�Ĝ@��u@�r�@�(�@��@��;@���@��w@��@�|�@��y@���@��+@�v�@��#@���@���@��7@�x�@�X@�X@�7L@�V@��`@��9@�j@�bN@�r�@�Z@�b@���@��\@��^@��@���@�@���@�J@��@�E�@��+@��\@���@�v�@��#@�7L@���@�(�@�b@�Q�@��w@���@��/@��D@�j@�Q�@��@�l�@���@��!@���@���@���@���@�X@�&�@���@��@��
@���@��@�|�@�t�@�l�@�K�@�+@�
=@��@��@�~�@�V@�$�@�{@���@���@���@�G�@��@���@���@���@�r�@�r�@�%@��@��@��@��`@���@�Ĝ@��9@��@���@��D@��D@�z�@�z�@��@��@��@��@�ȴ@���@�^5@�-@��@��@���@���@���@��7@�`B@�`B@�G�@��@��`@���@�r�@� �@��m@��;@��
@�ƨ@���@�dZ@�33@�;d@�o@��!@��+@�M�@�5?@��@�{@���@��^@���@�hs@�G�@��@��j@��9@��u@�bN@�9X@�1@���@��
@���@��P@�t�@�K�@��@��\@�n�@�=q@�{@��h@�p�@�V@���@���@�Z@�(�@��m@���@�t�@�|�@�dZ@�33@�"�@�"�@��R@���@�v�@�V@�-@��@��7@�X@�/@��@��@���@��j@���@���@�j@�9X@�b@�1@�1@�@~�@~E�@}@}/@|�@|��@|�@|�@{C�@z��@zM�@y�@y�7@yhs@x��@w�;@w|�@wl�@wK�@w;d@v��@vff@v@u@uO�@t�/@t��@t�@s�F@s33@rM�@q��@qX@q%@p��@pbN@o�;@o�@o�@ol�@o+@nv�@n$�@m@m��@mp�@mV@lj@k��@k�@ko@j�\@j-@i�^@ix�@h��@hA�@hb@g�@g��@gl�@f��@f��@fff@fff@f$�@e�-@d��@d�@d��@dZ@d1@c��@c�@c"�@b��@b��@b=q@bJ@a��@ax�@a&�@a%@`�@`  @_�@_|�@_\)@_+@_+@_
=@^�+@]�@]�@]V@\�@\�/@\�j@\j@\1@[��@["�@Z��@Z�!@Z�\@Zn�@Y�#@Yhs@YG�@Y�@XQ�@W�@W�P@WK�@W�@V�+@V@U��@U�h@U�h@U�@U`B@T�j@TI�@S��@S�@S"�@R�H@R�!@R~�@R^5@Q��@Q�7@P��@P�9@PbN@P1'@O��@O;d@O�@N�y@Nȴ@Nff@N$�@N{@M��@M@M�-@M�@MO�@M?}@M/@L�@Lz�@L(�@K��@KS�@J��@J��@J~�@J~�@J^5@JM�@I�#@IX@HĜ@H�@H  @G�@G|�@GK�@G+@G�@F�y@FV@F{@E��@E`B@E�@D��@D�@D�j@Dz�@DZ@D�@Cƨ@C��@CS�@Co@B�!@B-@A�@Ahs@A%@@�`@@�9@@�@@Q�@?�;@?|�@>�y@>ȴ@>�R@>�R@>��@>V@>{@>@>@=�@=�h@=?}@=/@=�@=V@<�/@<��@<�D@<z�@<Z@<(�@<1@;�m@;�@;33@:��@:��@:n�@:�@:J@9�^@9�7@9G�@9�@9%@8�`@8Ĝ@8Ĝ@8�9@8�9@8��@8�u@8�u@8r�@8 �@7�@7��@7\)@7�@7
=@7
=@6��@6�y@6ȴ@6�+@6V@65?@6$�@6@5�@5V@4�D@49X@3�m@3��@3�@3dZ@3C�@2��@2^5@2M�@2-@2J@1�@1��@1x�@1&�@0Ĝ@0�u@0r�@0 �@/�;@/��@/�@.ȴ@.�+@.v�@.V@.@-��@,��@,�j@,��@,z�@,Z@,(�@+�m@+�
@+��@+dZ@+33@+@*�!@*�\@*M�@*=q@*J@)�#@)��@)�7@)hs@)7L@(��@(�9@(�@(1'@(b@'�@'l�@&��@&�R@&��@&v�@%�T@%�h@%�@%`B@%V@$�j@$z�@$I�@$(�@#�
@#�@#dZ@#S�@#33@"�@"�!@"��@"�\@"M�@"-@!��@!�#@!X@ ��@ ��@ ��@ Q�@ 1'@�@�P@\)@+@�@�y@v�@V@5?@�T@@�-@�-@�-@`B@V@�/@�@Z@I�@(�@��@�m@�F@�@S�@o@�@�H@�!@n�@�@��@��@��@�7@�@�9@�@r�@Q�@b@�;@�;@��@�w@�P@+@��@�R@�+@$�@@�h@�@`B@V@�/@j@(�@�F@�@dZ@C�@33@"�@@�H@��@~�@-@J@�#@��@��@x�@G�@�@�`@��@�u@1'@�@�P@\)@K�@+@�@��@�@��@5?@$�@{@�@��@�-@�@O�@?}@/@/@�@�/@�@�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A���A�ȴA���A��A��HA��A��yA�%A�1A�VA�bA�JA�bA�A�%A�  A���A���A��A���A���A���A���A���A���A�JA�{A��A�{A�{A��A��A��A�"�A�33A�O�A�bNA�dZA��7A��uA���A���A���A���A��A��9A��FA��FA��9A��9A���A���A���A�dZA�=qA���A�ƨA���A���A�~�A��7A�VA�O�A�33A�ƨA�VA��A��A��A�VA�VA�Q�A��DA��/A�  A�hsA�JA��/A��A��A��A���A���A��A�G�A��uA���A�jA�$�A��A��
A�ffA���A���A�{A��hA��A��A���A�ƨA�S�A���A��7A�\)A���A��DA�bNA�/A�z�A��`A���A�r�A�v�A�l�A�|�A���A���A���A���A��`A�M�A�JA��A��A��Al�A|�9AzAwO�At�ApȴAm%Aj(�Ag�
Ae+Ab�A`ffA^�AZr�AV��AUC�ATI�AS�wAQXAOt�AM�
ALjAL1AKVAI�TAH��AG�AG"�AFn�AE�^AC�AAS�A@�yA@�A?�wA?G�A?%A>jA<  A9��A8��A7�mA5��A4��A3�TA2��A1�7A0bNA/�A.9XA-?}A+�A*�HA)�FA)O�A(��A(n�A'��A'+A&�yA&v�A%A#�A!��A!
=A �+A��A7LA��A�A��A�+A�A�TA��A?}A��An�AO�A�A�AG�A�/A~�A�wA�jA$�A�A��A�A�;A
��A	��A	oA9XA\)AbNA33AjA��A��A�wA;dA �@��D@���@�7L@��@�S�@�p�@�@��@���@�G�@�7L@��@��T@�V@�@�!@���@�V@��;@��@��@�1'@߮@ޏ\@ݑh@ܛ�@ܓu@�I�@���@��@�
=@ڇ+@�n�@��@��@��#@�x�@�7L@�"�@�V@�K�@�J@��@Ұ!@�{@϶F@Χ�@θR@���@���@�A�@˥�@��T@���@�7L@��@�E�@�v�@ɑh@�bN@ǥ�@�+@�/@ÍP@���@��;@�{@�-@�J@�?}@���@�1@���@��T@�?}@���@�dZ@��h@��@��@�
=@��R@�~�@�^5@��@���@�t�@���@���@�p�@�j@�b@��F@�^5@�7L@��j@�C�@��H@��+@��@��h@�X@�%@�V@��@���@�Ĝ@��u@�r�@�(�@��@��;@���@��w@��@�|�@��y@���@��+@�v�@��#@���@���@��7@�x�@�X@�X@�7L@�V@��`@��9@�j@�bN@�r�@�Z@�b@���@��\@��^@��@���@�@���@�J@��@�E�@��+@��\@���@�v�@��#@�7L@���@�(�@�b@�Q�@��w@���@��/@��D@�j@�Q�@��@�l�@���@��!@���@���@���@���@�X@�&�@���@��@��
@���@��@�|�@�t�@�l�@�K�@�+@�
=@��@��@�~�@�V@�$�@�{@���@���@���@�G�@��@���@���@���@�r�@�r�@�%@��@��@��@��`@���@�Ĝ@��9@��@���@��D@��D@�z�@�z�@��@��@��@��@�ȴ@���@�^5@�-@��@��@���@���@���@��7@�`B@�`B@�G�@��@��`@���@�r�@� �@��m@��;@��
@�ƨ@���@�dZ@�33@�;d@�o@��!@��+@�M�@�5?@��@�{@���@��^@���@�hs@�G�@��@��j@��9@��u@�bN@�9X@�1@���@��
@���@��P@�t�@�K�@��@��\@�n�@�=q@�{@��h@�p�@�V@���@���@�Z@�(�@��m@���@�t�@�|�@�dZ@�33@�"�@�"�@��R@���@�v�@�V@�-@��@��7@�X@�/@��@��@���@��j@���@���@�j@�9X@�b@�1@�1@�@~�@~E�@}@}/@|�@|��@|�@|�@{C�@z��@zM�@y�@y�7@yhs@x��@w�;@w|�@wl�@wK�@w;d@v��@vff@v@u@uO�@t�/@t��@t�@s�F@s33@rM�@q��@qX@q%@p��@pbN@o�;@o�@o�@ol�@o+@nv�@n$�@m@m��@mp�@mV@lj@k��@k�@ko@j�\@j-@i�^@ix�@h��@hA�@hb@g�@g��@gl�@f��@f��@fff@fff@f$�@e�-@d��@d�@d��@dZ@d1@c��@c�@c"�@b��@b��@b=q@bJ@a��@ax�@a&�@a%@`�@`  @_�@_|�@_\)@_+@_+@_
=@^�+@]�@]�@]V@\�@\�/@\�j@\j@\1@[��@["�@Z��@Z�!@Z�\@Zn�@Y�#@Yhs@YG�@Y�@XQ�@W�@W�P@WK�@W�@V�+@V@U��@U�h@U�h@U�@U`B@T�j@TI�@S��@S�@S"�@R�H@R�!@R~�@R^5@Q��@Q�7@P��@P�9@PbN@P1'@O��@O;d@O�@N�y@Nȴ@Nff@N$�@N{@M��@M@M�-@M�@MO�@M?}@M/@L�@Lz�@L(�@K��@KS�@J��@J��@J~�@J~�@J^5@JM�@I�#@IX@HĜ@H�@H  @G�@G|�@GK�@G+@G�@F�y@FV@F{@E��@E`B@E�@D��@D�@D�j@Dz�@DZ@D�@Cƨ@C��@CS�@Co@B�!@B-@A�@Ahs@A%@@�`@@�9@@�@@Q�@?�;@?|�@>�y@>ȴ@>�R@>�R@>��@>V@>{@>@>@=�@=�h@=?}@=/@=�@=V@<�/@<��@<�D@<z�@<Z@<(�@<1@;�m@;�@;33@:��@:��@:n�@:�@:J@9�^@9�7@9G�@9�@9%@8�`@8Ĝ@8Ĝ@8�9@8�9@8��@8�u@8�u@8r�@8 �@7�@7��@7\)@7�@7
=@7
=@6��@6�y@6ȴ@6�+@6V@65?@6$�@6@5�@5V@4�D@49X@3�m@3��@3�@3dZ@3C�@2��@2^5@2M�@2-@2J@1�@1��@1x�@1&�@0Ĝ@0�u@0r�@0 �@/�;@/��@/�@.ȴ@.�+@.v�@.V@.@-��@,��@,�j@,��@,z�@,Z@,(�@+�m@+�
@+��@+dZ@+33@+@*�!@*�\@*M�@*=q@*J@)�#@)��@)�7@)hs@)7L@(��@(�9@(�@(1'@(b@'�@'l�@&��@&�R@&��@&v�@%�T@%�h@%�@%`B@%V@$�j@$z�@$I�@$(�@#�
@#�@#dZ@#S�@#33@"�@"�!@"��@"�\@"M�@"-@!��@!�#@!X@ ��@ ��@ ��@ Q�@ 1'@�@�P@\)@+@�@�y@v�@V@5?@�T@@�-@�-@�-@`B@V@�/@�@Z@I�@(�@��@�m@�F@�@S�@o@�@�H@�!@n�@�@��@��@��@�7@�@�9@�@r�@Q�@b@�;@�;@��@�w@�P@+@��@�R@�+@$�@@�h@�@`B@V@�/@j@(�@�F@�@dZ@C�@33@"�@@�H@��@~�@-@J@�#@��@��@x�@G�@�@�`@��@�u@1'@�@�P@\)@K�@+@�@��@�@��@5?@$�@{@�@��@�-@�@O�@?}@/@/@�@�/@�@�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�'B�'B�'B�-B�3B�9B�RB�wBĜBǮB�B�mB�B��B��B  B1BbB�B �B&�B/B>wBD�BZBl�Bz�B}�B��BȴB�qBu�BQ�BZB_;B]/BdZBe`Be`BiyBhsB\)BO�BF�B@�B;dB9XB6FB33B2-B1'B0!B/B-B)�B'�B�B�BuB�B�B�B�B�BbB	7B��B�5B�^B��B�hB�Bw�By�By�Bx�Bt�Bm�BjBffB]/BP�BG�B1'B
=B
�B
�TB
��B
�uB
��B
��B
��B
�\B
k�B
W
B
M�B
D�B
1'B
�B
B	�sB	��B	�!B	�JB	�%B	w�B	_;B	N�B	<jB	'�B	B�sB�/B�B�B��BǮB�dB�FB�LBÖBƨBȴBŢBǮBĜB��B�dB�-B�B�B�B�B��B��B��B��B��B��B��B��B��B�oB�DB�B|�Bu�Bn�Be`B^5BXB\)BZBW
BW
BW
BVBS�BQ�BL�BI�BG�BG�BH�BH�BG�BL�BI�BH�BE�BB�BB�BA�B@�B?}B>wB9XB9XB7LB6FB5?B6FB49B49B49B2-B2-B1'B1'B/B/B.B.B-B,B+B)�B)�B)�B(�B(�B'�B&�B&�B%�B&�B'�B&�B&�B%�B&�B'�B(�B'�B(�B)�B)�B+B+B-B1'B49B5?B5?B5?B7LB6FB7LB9XB:^B:^B<jB=qB?}BI�BN�BO�BO�BN�BN�BK�BI�BF�BG�BO�BT�BS�BO�BT�BS�BQ�BQ�BP�BP�BQ�BR�B^5Be`Bk�Bn�Bn�Bn�Bm�BgmBdZB`BBcTBe`BffBffBe`Be`BffBiyBm�Bo�Bo�Bq�Bv�By�Bz�B� B�B�B�B�B�B�DB�PB�bB�hB�{B��B��B��B��B��B�B�B�'B�-B�3B�FB�RB�jB�wB��BBĜBƨBɺB��B��B��B��B��B��B��B�B�B�B�HB�ZB�`B�fB�mB�B�B�B��B��B��B	B	B	%B	
=B	
=B	JB	VB	PB	\B	�B	�B	�B	!�B	$�B	)�B	0!B	1'B	1'B	33B	6FB	6FB	5?B	5?B	7LB	<jB	;dB	:^B	:^B	=qB	=qB	>wB	A�B	C�B	D�B	G�B	K�B	S�B	VB	[#B	^5B	_;B	bNB	e`B	ffB	ffB	hsB	k�B	m�B	n�B	o�B	q�B	r�B	s�B	t�B	v�B	x�B	z�B	}�B	~�B	�B	�+B	�=B	�=B	�JB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�'B	�-B	�9B	�?B	�?B	�LB	�RB	�XB	�dB	�jB	�}B	�}B	�}B	��B	��B	��B	B	ĜB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�
B	�B	�)B	�)B	�/B	�5B	�5B	�;B	�;B	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
JB
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
oB
oB
oB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
%�B
&�B
&�B
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
-B
.B
.B
.B
.B
.B
/B
/B
/B
/B
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
33B
33B
49B
49B
49B
49B
49B
5?B
49B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
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
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
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
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
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
aHB
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
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
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
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�qB�qB�WB�qB�xB�jB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�BB�MB�_B��B�8B�iB��B��B��B�B.BeB �B&�B.�B>BBDMBY�BlWBz�B}�B��BȀB�"ButBQ�BY�B_B\�BdBeBe,BiDBh>B[�BO�BFtB@4B;0B9$B6B2�B1�B0�B/�B.�B,�B)�B'�B�B]B@BKBdBqB]BSBB	B��B��B�B�jB�4B��Bw�By�By�Bx�Bt�Bm]Bj0Bf2B\�BP�BG_B0�B	�B
�QB
� B
��B
�@B
��B
��B
�9B
�B
k6B
V�B
M�B
DgB
0�B
?B
 �B	�$B	ϑB	��B	��B	��B	w�B	_B	N�B	<B	'�B	�B�$B��B��BյBңB�zB�B��B��B�GB�YB�fB�SB�zB�MB�;B�B��B��B��B��B��B��B��B��B��B�pB�]B�QB�?B�EB� B��B��B|�ButBnIBeB]�BW�B[�BY�BV�BV�BV�BU�BS�BQ�BL~BIlBG_BG_BHfBHfBG_BLdBIRBHfBESBBABBABA;B@4B?.B>B9	B9	B6�B5�B4�B5�B3�B3�B3�B1�B1�B0�B0�B.�B.�B-�B-�B,�B+�B*�B)�B)�B)�B(�B(�B'�B&�B&�B%�B&�B'�B&�B&�B%zB&�B'�B(�B'�B(�B)�B)�B*�B*�B,�B0�B3�B4�B4�B4�B6�B5�B6�B9	B:B9�B<B=B?BIRBNpBOvBO�BN�BN�BK^BIRBFYBG_BO�BT�BS�BO�BT�BS�BQ�BQ�BP�BP�BQ�BR�B]�BeBkBn/Bn/Bn/BmCBgBc�B_�BcBd�BfBfBd�BeBe�BiBm)Bo5BoOBqABv`By�Bz�B�B��B��B��B��B��B��B�B�B�B�B�2B�9B�WB�pB��B��B��B��B��B��B��B��B�B�(B�B�'B�3B�YB�lB�xB�xB�xB�dB�dBΊBңBյBרB��B��B�B��B��B�B�0B�CB�;B�TB��B�rB	 �B	�B	�B		�B		�B	�B	B	B	B	2B	KB	dB	!|B	$�B	)�B	/�B	0�B	0�B	2�B	5�B	5�B	4�B	4�B	6�B	<B	;B	9�B	:B	="B	="B	>(B	A;B	CGB	DMB	GEB	KxB	S�B	U�B	Z�B	]�B	^�B	a�B	eB	e�B	e�B	h$B	k6B	mCB	n/B	o5B	qAB	rGB	shB	tTB	vzB	xlB	z�B	}�B	~�B	��B	��B	��B	��B	��B	��B	�B	� B	�B	�=B	�pB	�VB	�bB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�.B	�B	�4B	�4B	� B	�'B	�MB	�EB	�RB	�lB	�XB	�rB	�^B	̈́B	�jB	�pB	ϑB	ЗB	҉B	өB	յB	՛B	ԕB	՛B	֡B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�$B	�
B	�$B	�
B	�0B	�=B	�CB	�/B	�5B	�[B	�[B	�[B	�aB	�aB	�hB	�nB	�tB	�ZB	�tB	�ZB	�`B	�zB	�zB	�`B	�fB	�lB	�lB	�lB	��B	�lB	�rB	�xB	�xB	��B	��B	��B	�B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B

�B

�B
�B
�B
�B
�B
B
�B
�B
B
B
�B
�B
�B
B
B
B
 B
B
B
B
,B
,B
,B
B
,B
B
2B
2B
B
$B
?B
$B
?B
?B
EB
KB
1B
KB
1B
7B
7B
QB
QB
QB
WB
]B
WB
CB
dB
dB
dB
OB
OB
jB
OB
OB
pB
pB
VB
VB
pB
pB
 \B
 vB
!|B
!|B
!bB
"hB
"hB
"hB
"hB
"�B
"�B
"�B
#�B
$�B
$tB
$�B
$�B
%zB
%zB
&�B
&�B
&�B
%zB
&�B
&�B
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
,�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
4�B
3�B
4�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
8B
8B
7�B
8B
8�B
8�B
:B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
<B
<B
<B
<B
<B
="B
="B
=B
="B
=B
="B
>(B
>B
>B
>B
>B
>B
>(B
>B
>B
?B
?B
?.B
?.B
?B
@B
@B
@B
A;B
@4B
A;B
A;B
A;B
A;B
BAB
B'B
B'B
B'B
BAB
B'B
BAB
B'B
B'B
B'B
CGB
CGB
C-B
CGB
CGB
DMB
C-B
CGB
C-B
D3B
DMB
DMB
D3B
DMB
DMB
E9B
ESB
F?B
FYB
F?B
FYB
F?B
FYB
G_B
G_B
HKB
HfB
HKB
HKB
HfB
HfB
HfB
IlB
IRB
IRB
IlB
IRB
JrB
JXB
JXB
K^B
K^B
KxB
K^B
KxB
L~B
M�B
MjB
M�B
M�B
M�B
MjB
MjB
M�B
NpB
NpB
NpB
NpB
O�B
O�B
O�B
OvB
OvB
OvB
OvB
P}B
P�B
P�B
P�B
P}B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Z�B
Z�B
Z�B
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
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
cB
b�B
b�B
cB
dB
dB
c�B
dB
c�B
d�B
d�B
eB
fB
fB
e�B
e�B
e�B
fB
e�B
gB
gB
gB
gB
gB
gB
h
B
h$B
h
B
h
B
h$B
h$B
iB
i*B
iB
i*B
j0B
j0B
j0B
jB
j0B
jB
jB
kB
kB
k6B
k6B
k6B
kB
l=B
l=B
l=B
l=B
l"B
l"B
l=B
l=B
mCB
m)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.7(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201902160037442019021600374420190216003744201902170025012019021700250120190217002501JA  ARFMdecpA19c                                                                20190211153628  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190211063630  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190211063631  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190211063631  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190211063632  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190211063632  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190211063632  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190211063632  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190211063632  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190211063633                      G�O�G�O�G�O�                JA  ARUP                                                                        20190211065705                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190211154135  CV  JULD            G�O�G�O�F�6K                JM  ARCAJMQC2.0                                                                 20190215153744  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190215153744  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190216152501  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                