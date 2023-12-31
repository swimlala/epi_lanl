CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-10-29T15:37:43Z creation;2019-10-29T15:37:48Z conversion to V3.1;2023-06-29T05:50:49Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20191029153743  20230705031506  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_185                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�����
 1   @��ᬐ�@7TɅ�o�b�]c�e�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�  B���B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @fff@�ff@�ffA33A,��AK33Ak33A���A���A���A���Ař�Aՙ�A噚A���B33B
��B��B��B"��B*��B2��B:��BB��BJ��BR��BZ��Bb��Bj��Br��Bz��B���B���B�ffB�33B�ffB�33B�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC �3C�3C�3C�3C�3C
�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C �3C"�3C$�3C&�3C(�3C*�3C,��C.�3C0�3C2�3C4�3C6�3C8�3C:�3C<�3C>�3C@�3CB�3CD�3CF�3CH�3CJ�3CL�3CN�3CP�3CR�3CT�3CV�3CX�3CZ�3C\�3C^�3C`��Cb�3Cd�3Cf�3Ch�3Cj�3Cl�3Cn�3Cp�3Cr�3Ct�3Cv�3Cx�3Cz�3C|�3C~��C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�D ,�D ��D,�D��D,�D��D,�D�3D,�D��D,�D��D,�D��D,�D��D,�D��D	,�D	��D
,�D
��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D ,�D ��D!,�D!��D",�D"��D#,�D#��D$,�D$��D%,�D%��D&,�D&��D',�D'��D(,�D(��D),�D)��D*,�D*��D+,�D+��D,,�D,��D-,�D-��D.,�D.��D/,�D/��D0,�D0��D1,�D1��D2,�D2��D3,�D3��D4,�D4��D5,�D5��D6,�D6��D7,�D7��D8,�D8��D9,�D9��D:,�D:��D;,�D;��D<,�D<��D=,�D=��D>,�D>��D?,�D?��D@,�D@��DA,�DA��DB,�DB��DC,�DC��DD,�DD��DE,�DE��DF,�DF��DG,�DG��DH,�DH��DI,�DI��DJ,�DJ��DK,�DK��DL,�DL��DM,�DM��DN,�DN��DO,�DO��DP,�DP��DQ,�DQ��DR,�DR��DS,�DS��DT,�DT��DU,�DU��DV,�DV��DW,�DW��DX,�DX��DY,�DY��DZ,�DZ��D[,�D[��D\,�D\��D],�D]��D^,�D^��D_,�D_��D`,�D`��Da,�Da��Db,�Db��Dc,�Dc��Dd,�Dd��De,�De��Df,�Df��Dg,�Dg�fDh,�Dh��Di,�Di��Dj,�Dj��Dk,�Dk��Dl,�Dl��Dm,�Dm��Dn,�Dn��Do,�Do��Dp,�Dp��Dq,�Dq��Dr,�Dr��Ds,�Ds��Dt,�Dt��Du,�Du��Dv,�Dv��Dw,�Dw��Dx,�Dx��Dy,�Dy��Dz,�Dz��D{,�D{��D|,�D|��D},�D}��D~,�D~��D,�D��D�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��3D�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfDfD��fD�fD�VfDÖfD��fD�fD�VfDĖfD��fD�fD�VfDŖfD��fD�fD�VfDƖfD��fD�fD�VfDǖfD��fD�fD�VfDȖfD��fD�fD�VfDɖfD��fD�fD�VfDʖfD��fD�fD�VfD˖fD��fD�fD�VfD̖fD��fD�fD�VfD͖fD��fD�fD�VfDΖfD��fD�fD�VfDϖfD��fD�fD�VfDЖfD��fD�fD�VfDіfD��fD�fD�VfDҖfD��fD�fD�VfDӖfD��fD�fD�VfDԖfD��fD�fD�VfDՖfD��fD�fD�VfD֖fD��fD�fD�VfDזfD��fD�fD�VfDؖfD��fD�fD�VfDٖfD��fD�fD�VfDږfD��fD�fD�VfDۖfD��fD�fD�VfDܖfD��fD�fD�VfDݖfD��fD�fD�VfDޖfD��fD�fD�VfDߖfD��fD�fD�VfD��fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�3D��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD�fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�Y�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aѩ�Aѩ�AѮAѮAѸRAѺ^AѼjAѾwAѾwAѼjA���A�ƨA�ƨA�ȴA�ȴA�ȴA���A��
A��/A��HA��TA��mA��A���A���A���A���A���A��A��A��A��A��yA��`A���A��A̛�A�n�A�?}A�ffA��yA��hA�ĜA�z�A��uA��;A���A��DA�x�A��mA�E�A��A�VA���A��uA��7A��A�C�A��A���A��TA���A��-A�-A�t�A�A�G�A�{A�`BA��A�K�A��wA�C�A��A�A�"�A��A�$�A��7A��A��9A�1'A�S�A�C�A�`BA��mA�=qA���A��A��#A��A�ȴA���A���A�ĜA�+A��uA�1A�l�A��A�A�C�A��/A��FA���A��A�%A� �A��-A��AK�Az�/AxAu�FAs�;Ar��Ap�\ApAn�Aj1'Ah�AgdZAfbNAe;dAd5?Aa��A`{A]�-AX^5AWAU��AT1ARbNAQ��AP1ANZAKAJ5?AH�/AG
=AD�RAB��AA�;AA%A?�
A>1'A=;dA<�A8�/A77LA5l�A3�;A2��A1��A0jA/\)A-;dA+��A*�jA)XA(Q�A'��A%�A%�A#�#A#33A ��A�A�^A+AI�A~�AAG�AbAI�Ax�A�;A��A~�A�+A9XA�PA��A��A�A  A�^A&�AbNAA�A^5A�A��A%A�-A
��A	G�Av�AI�AbAA�A�A��AbA��A�
A�
A��A(�A �u@���@�"�A E�@�j@�dZ@���@��j@��
@��\@�Ĝ@�ff@��@�J@�^@��#@��@�$�@�J@�^@�G�@�@�hs@�
=@�S�@���@�/@��
@��
@�z�@�7L@�&�@��@�S�@��@�5?@���@�@ۥ�@�C�@ە�@�x�@�x�@ّh@�V@�5?@�x�@���@���@؋D@���@ؓu@���@�dZ@�\)@�-@�O�@ՙ�@�p�@ҟ�@��@��@��@���@���@�v�@���@���@���@˕�@�;d@���@�V@��`@�Q�@Ǿw@�
=@�5?@�X@��/@�z�@ċD@���@���@���@�O�@�X@���@���@���@�  @��w@�M�@��7@�`B@��-@��y@���@�ƨ@���@��w@�ƨ@�ƨ@���@�ƨ@�o@��T@���@��`@���@�bN@�-@�I�@�J@���@��@���@���@�l�@�;d@�@��y@���@��R@���@�5?@��@�@���@��@�O�@���@���@���@�bN@��@��@�dZ@���@�p�@��@�Ĝ@�r�@�9X@��@��@��
@�t�@��@��H@���@�v�@�^5@�M�@�5?@�%@��+@�r�@�9X@�9X@��w@�S�@�ȴ@��T@�Z@���@�+@�;d@��y@�5?@��^@���@�j@�1'@�1@�+@�~�@���@���@�p�@�/@�%@���@�V@��9@��
@��P@�t�@�l�@�S�@�@�ȴ@��\@�n�@�V@���@��^@���@��h@�`B@�7L@��@�%@��j@�r�@�I�@�1'@��m@��@�l�@�"�@��@���@��\@�^5@�5?@�@���@��T@�@�X@��@��`@�Ĝ@��u@�Z@� �@�1@��
@���@�t�@�K�@�"�@��@�V@��@���@�hs@�G�@�/@���@�Ĝ@��u@�Q�@�1@���@��
@�ƨ@��w@���@�|�@�S�@�"�@��@��R@��+@�M�@�5?@�5?@�5?@�5?@�{@��^@��h@��@�p�@�X@��j@�1@��F@��P@�dZ@�;d@�
=@���@�v�@�5?@���@���@�@���@��h@�x�@�hs@�?}@�V@��`@��D@�A�@�(�@�1@��@K�@~��@~{@}��@}�@|�/@|I�@{��@{dZ@z�@z��@z�!@z�\@z~�@z^5@zM�@zJ@y�@y��@x�`@xbN@w�;@wl�@wK�@w
=@vff@u@up�@u?}@tj@s�m@s��@sC�@r��@r~�@rJ@q�^@qG�@p��@p�@p1'@p �@o�;@o��@o��@o�@o�@o�@nv�@n{@m��@m�@m?}@m�@l�/@l�j@l��@lI�@l(�@l�@k�
@k��@kS�@k@j��@j~�@j^5@j=q@i��@h�`@h�@h1'@g�;@g�@gl�@g�@f�y@f�@f�@f��@f$�@e�T@e�-@e�@eO�@d��@d�@dZ@d9X@d1@c�F@b�@bn�@b-@a�#@a��@aG�@`��@`�9@`�@` �@_�w@_l�@^ȴ@^5?@^{@]�-@]V@\�@\z�@\9X@\1@[�F@[o@Z~�@Y�#@Yx�@YX@X��@X��@XbN@Xb@W|�@W;d@W
=@V��@Vv�@VE�@U�T@U�h@U?}@T��@T�@TI�@T9X@T9X@T(�@S�F@R��@Rn�@R-@Q��@Q�^@Q7L@P�`@P�u@PbN@PA�@P  @Ol�@Nȴ@N�+@N5?@N$�@N$�@M�T@M`B@L�@Lz�@K�m@K�F@KdZ@KC�@Ko@J��@J��@JM�@I��@I%@H�`@H�@HQ�@Hb@G�@G�P@G;d@F�y@F��@F�+@FV@E�T@E�h@EO�@D��@D��@DZ@D(�@C�
@C�m@C�m@C�
@C�F@Ct�@C"�@C@B�\@A�@A�#@A�^@Ahs@@��@@Ĝ@@�9@@�u@@r�@@1'@@  @?��@?l�@?;d@>�+@>V@=�@=�@=p�@=�@=p�@=O�@=�@<�@<�j@<��@<�D@<j@<I�@;�m@;�F@;t�@;"�@:�@:��@:�!@:�!@:�!@:�\@:M�@:�@9��@9�^@97L@8��@8��@8r�@7�;@7��@7l�@6�y@6ȴ@6�R@6�R@6��@6�+@6V@65?@65?@6@5�@4�/@4�D@4Z@4(�@3�m@3�F@3�@3S�@3o@2��@2n�@2J@1��@1�7@1�7@1�7@1hs@0�`@0bN@0A�@/�w@/�P@/l�@/\)@/�@.�R@.ff@.@-@-p�@,�j@,j@,9X@+ƨ@+C�@*��@*~�@*-@*�@)��@)�^@)hs@)X@)7L@)&�@)%@(��@(��@(�u@(�@(�@(Q�@'�;@'�P@'K�@&�y@&V@&@%��@%�h@%�h@%p�@%/@%�@%V@$��@$z�@$I�@$(�@#�m@#t�@"�H@"��@"^5@!�@!X@ ��@ �9@ Q�@ Q�@   @�w@�@��@l�@
=@ȴ@�+@E�@{@�@�-@�h@p�@�@��@j@�@�F@�@t�@"�@�@��@�!@~�@=q@�@��@�@�#@��@7L@&�@&�@%@�`@Ĝ@�9@�@bN@Q�@ �@��@�P@|�@|�@l�@l�@K�@;d@+@�@
=@
=@�y@ȴ@��@�+@E�@{@��@`B@/@��@�/@��@�j@��@z�@Z@��@�m@�m@�
@�F@��@��@�@S�@o@@�H@��@�\@=q@�@��@�#@x�@G�@&�@%@�`@�9@��@��@�u@r�@1'@b@�;@�w@�@��@�P@|�@l�@;d@+@�@ȴ@��@E�@@��@��@�h@�@��@�@z�@9X@1@�
@��@�@t�@C�@33@o@
��@
�!@
^5@
�@
J@	�#@	��@	�7@	X@	7L@	&�@	%@�`@Ĝ@Ĝ@�9@�@r�@1'@ �@  @�;@�w@��@\)@;d@�@
=@��@ȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aѩ�Aѩ�AѮAѮAѸRAѺ^AѼjAѾwAѾwAѼjA���A�ƨA�ƨA�ȴA�ȴA�ȴA���A��
A��/A��HA��TA��mA��A���A���A���A���A���A��A��A��A��A��yA��`A���A��A̛�A�n�A�?}A�ffA��yA��hA�ĜA�z�A��uA��;A���A��DA�x�A��mA�E�A��A�VA���A��uA��7A��A�C�A��A���A��TA���A��-A�-A�t�A�A�G�A�{A�`BA��A�K�A��wA�C�A��A�A�"�A��A�$�A��7A��A��9A�1'A�S�A�C�A�`BA��mA�=qA���A��A��#A��A�ȴA���A���A�ĜA�+A��uA�1A�l�A��A�A�C�A��/A��FA���A��A�%A� �A��-A��AK�Az�/AxAu�FAs�;Ar��Ap�\ApAn�Aj1'Ah�AgdZAfbNAe;dAd5?Aa��A`{A]�-AX^5AWAU��AT1ARbNAQ��AP1ANZAKAJ5?AH�/AG
=AD�RAB��AA�;AA%A?�
A>1'A=;dA<�A8�/A77LA5l�A3�;A2��A1��A0jA/\)A-;dA+��A*�jA)XA(Q�A'��A%�A%�A#�#A#33A ��A�A�^A+AI�A~�AAG�AbAI�Ax�A�;A��A~�A�+A9XA�PA��A��A�A  A�^A&�AbNAA�A^5A�A��A%A�-A
��A	G�Av�AI�AbAA�A�A��AbA��A�
A�
A��A(�A �u@���@�"�A E�@�j@�dZ@���@��j@��
@��\@�Ĝ@�ff@��@�J@�^@��#@��@�$�@�J@�^@�G�@�@�hs@�
=@�S�@���@�/@��
@��
@�z�@�7L@�&�@��@�S�@��@�5?@���@�@ۥ�@�C�@ە�@�x�@�x�@ّh@�V@�5?@�x�@���@���@؋D@���@ؓu@���@�dZ@�\)@�-@�O�@ՙ�@�p�@ҟ�@��@��@��@���@���@�v�@���@���@���@˕�@�;d@���@�V@��`@�Q�@Ǿw@�
=@�5?@�X@��/@�z�@ċD@���@���@���@�O�@�X@���@���@���@�  @��w@�M�@��7@�`B@��-@��y@���@�ƨ@���@��w@�ƨ@�ƨ@���@�ƨ@�o@��T@���@��`@���@�bN@�-@�I�@�J@���@��@���@���@�l�@�;d@�@��y@���@��R@���@�5?@��@�@���@��@�O�@���@���@���@�bN@��@��@�dZ@���@�p�@��@�Ĝ@�r�@�9X@��@��@��
@�t�@��@��H@���@�v�@�^5@�M�@�5?@�%@��+@�r�@�9X@�9X@��w@�S�@�ȴ@��T@�Z@���@�+@�;d@��y@�5?@��^@���@�j@�1'@�1@�+@�~�@���@���@�p�@�/@�%@���@�V@��9@��
@��P@�t�@�l�@�S�@�@�ȴ@��\@�n�@�V@���@��^@���@��h@�`B@�7L@��@�%@��j@�r�@�I�@�1'@��m@��@�l�@�"�@��@���@��\@�^5@�5?@�@���@��T@�@�X@��@��`@�Ĝ@��u@�Z@� �@�1@��
@���@�t�@�K�@�"�@��@�V@��@���@�hs@�G�@�/@���@�Ĝ@��u@�Q�@�1@���@��
@�ƨ@��w@���@�|�@�S�@�"�@��@��R@��+@�M�@�5?@�5?@�5?@�5?@�{@��^@��h@��@�p�@�X@��j@�1@��F@��P@�dZ@�;d@�
=@���@�v�@�5?@���@���@�@���@��h@�x�@�hs@�?}@�V@��`@��D@�A�@�(�@�1@��@K�@~��@~{@}��@}�@|�/@|I�@{��@{dZ@z�@z��@z�!@z�\@z~�@z^5@zM�@zJ@y�@y��@x�`@xbN@w�;@wl�@wK�@w
=@vff@u@up�@u?}@tj@s�m@s��@sC�@r��@r~�@rJ@q�^@qG�@p��@p�@p1'@p �@o�;@o��@o��@o�@o�@o�@nv�@n{@m��@m�@m?}@m�@l�/@l�j@l��@lI�@l(�@l�@k�
@k��@kS�@k@j��@j~�@j^5@j=q@i��@h�`@h�@h1'@g�;@g�@gl�@g�@f�y@f�@f�@f��@f$�@e�T@e�-@e�@eO�@d��@d�@dZ@d9X@d1@c�F@b�@bn�@b-@a�#@a��@aG�@`��@`�9@`�@` �@_�w@_l�@^ȴ@^5?@^{@]�-@]V@\�@\z�@\9X@\1@[�F@[o@Z~�@Y�#@Yx�@YX@X��@X��@XbN@Xb@W|�@W;d@W
=@V��@Vv�@VE�@U�T@U�h@U?}@T��@T�@TI�@T9X@T9X@T(�@S�F@R��@Rn�@R-@Q��@Q�^@Q7L@P�`@P�u@PbN@PA�@P  @Ol�@Nȴ@N�+@N5?@N$�@N$�@M�T@M`B@L�@Lz�@K�m@K�F@KdZ@KC�@Ko@J��@J��@JM�@I��@I%@H�`@H�@HQ�@Hb@G�@G�P@G;d@F�y@F��@F�+@FV@E�T@E�h@EO�@D��@D��@DZ@D(�@C�
@C�m@C�m@C�
@C�F@Ct�@C"�@C@B�\@A�@A�#@A�^@Ahs@@��@@Ĝ@@�9@@�u@@r�@@1'@@  @?��@?l�@?;d@>�+@>V@=�@=�@=p�@=�@=p�@=O�@=�@<�@<�j@<��@<�D@<j@<I�@;�m@;�F@;t�@;"�@:�@:��@:�!@:�!@:�!@:�\@:M�@:�@9��@9�^@97L@8��@8��@8r�@7�;@7��@7l�@6�y@6ȴ@6�R@6�R@6��@6�+@6V@65?@65?@6@5�@4�/@4�D@4Z@4(�@3�m@3�F@3�@3S�@3o@2��@2n�@2J@1��@1�7@1�7@1�7@1hs@0�`@0bN@0A�@/�w@/�P@/l�@/\)@/�@.�R@.ff@.@-@-p�@,�j@,j@,9X@+ƨ@+C�@*��@*~�@*-@*�@)��@)�^@)hs@)X@)7L@)&�@)%@(��@(��@(�u@(�@(�@(Q�@'�;@'�P@'K�@&�y@&V@&@%��@%�h@%�h@%p�@%/@%�@%V@$��@$z�@$I�@$(�@#�m@#t�@"�H@"��@"^5@!�@!X@ ��@ �9@ Q�@ Q�@   @�w@�@��@l�@
=@ȴ@�+@E�@{@�@�-@�h@p�@�@��@j@�@�F@�@t�@"�@�@��@�!@~�@=q@�@��@�@�#@��@7L@&�@&�@%@�`@Ĝ@�9@�@bN@Q�@ �@��@�P@|�@|�@l�@l�@K�@;d@+@�@
=@
=@�y@ȴ@��@�+@E�@{@��@`B@/@��@�/@��@�j@��@z�@Z@��@�m@�m@�
@�F@��@��@�@S�@o@@�H@��@�\@=q@�@��@�#@x�@G�@&�@%@�`@�9@��@��@�u@r�@1'@b@�;@�w@�@��@�P@|�@l�@;d@+@�@ȴ@��@E�@@��@��@�h@�@��@�@z�@9X@1@�
@��@�@t�@C�@33@o@
��@
�!@
^5@
�@
J@	�#@	��@	�7@	X@	7L@	&�@	%@�`@Ĝ@Ĝ@�9@�@r�@1'@ �@  @�;@�w@��@\)@;d@�@
=@��@ȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
��BB  BB%BDBhBuB�B�B(�B1'B2-B33B33B33B33B33B33B33B33B1'B@�BW
B~�B�bB�B�-B�?BB�)B�fB�BBBBbBoB�B�B�B-BI�BH�BS�BW
BXB]/B_;BbNBcTBdZBdZBcTBaHB`BBaHBcTB`BB_;BN�BH�BD�B?}B:^B49B/B(�B"�B�BbB	7BB��B�B�ZB�
BÖB�^B�RB�!B��B�%BhsBYB:^B#�B�BVB
��B
�HB
��B
�3B
�uB
�%B
n�B
aHB
?}B
"�B
%B	��B	�yB	�;B	��B	ŢB	�XB	��B	�DB	�%B	}�B	u�B	p�B	dZB	T�B	G�B	+B	�B	uB	PB	  B��B�B�sB�B��B��B��BÖB�RB�9B�!B�B��B��B��B��B�\B�+B�%B�B~�Bz�Bv�Bq�BhsBbNBZBQ�BP�BJ�BG�BD�BA�BA�B?}BB�BB�BB�B=qBB�B@�B<jBJ�BM�BG�B=qB>wBK�BN�BP�BP�BP�BQ�BR�BR�BVBYB[#BYB_;B`BBaHBbNB_;B[#BVBP�BR�BR�BVBbNB�Bz�Bs�Bo�Bk�Bn�Bl�BgmBaHBXB]/Br�Bs�Bw�B�+B�%B�+B�1B�DB�7B�JB��B��B��B�B�B�B�B��B��B��B�{B�=By�Bu�Br�Bs�By�B�B�7B�7B�1B�7B�DB�=B�DB��B��B�?B�'B�9B�LBÖBƨBŢBŢBǮBȴB��B��B��B�)B�BB�HB�BB�ZB�B�fB�`B�mB�mB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	B	B	B	B	B	B	B	B	B	
=B	oB	�B	"�B	#�B	$�B	&�B	)�B	,B	-B	.B	-B	-B	.B	.B	.B	)�B	#�B	�B	�B	�B	 �B	!�B	#�B	&�B	+B	,B	.B	0!B	1'B	:^B	=qB	@�B	B�B	C�B	F�B	I�B	L�B	N�B	P�B	R�B	T�B	YB	[#B	\)B	[#B	\)B	\)B	\)B	\)B	\)B	\)B	\)B	]/B	]/B	]/B	]/B	^5B	]/B	]/B	\)B	ZB	W
B	VB	YB	ZB	ZB	ZB	\)B	[#B	]/B	^5B	`BB	aHB	aHB	bNB	cTB	ffB	ffB	ffB	ffB	ffB	hsB	gmB	hsB	k�B	o�B	p�B	p�B	r�B	u�B	v�B	v�B	w�B	w�B	z�B	|�B	~�B	�B	�B	�%B	�7B	�=B	�DB	�VB	�bB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�9B	�?B	�FB	�RB	�XB	�dB	�dB	�qB	�wB	�}B	��B	��B	B	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�/B	�;B	�;B	�BB	�BB	�BB	�TB	�`B	�fB	�fB	�mB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
\B
bB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
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
,B
+B
+B
,B
,B
-B
-B
.B
-B
.B
-B
-B
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
5?B
5?B
5?B
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
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
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
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
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
A�B
A�B
B�B
B�B
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
D�B
E�B
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
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
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
N�B
N�B
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
Q�B
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
T�B
T�B
T�B
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
XB
XB
YB
YB
YB
YB
YB
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
^5B
^5B
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
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
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
l�B
m�B
m�B
m�B
m�B
m�B
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
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
��B �B
��B�B�BB4B@BSB~B(�B0�B1�B2�B2�B2�B2�B3B33B3�B5%B6�BF�B_�B��B�[B��B�3B��BƨB��B�B�B9B{B�BTBBWB+B�B0�BJ�BJ#BT�BW�BY�B_�B`�BcnBd�Be,Be�Bc�Bb�BaHBb�Bd�Bc B`�BO�BI�BEmB@�B;dB5B/�B*0B$�B�BB
XBYB�"B�B��B�B�B�0B�xB�|B�B�7Bj0B\B<PB%,B�BhB
��B
�&B
ϑB
��B
��B
�B
qAB
f�B
D3B
%�B
�B	��B	�B	�bB	�B	�1B	�B	��B	�0B	�+B	HB	wLB	sB	f�B	X�B	L�B	,�B	/B	gB	�B	 B��B��B�6B��BңB�B�:BňB�XB�?B��B��B�8B��B�B��B�NB��B�_B�mB�iB|jBy$BsMBi�Bc�B[=BSBR�BK�BIBE�BC�BBuB?�BCGBC�BD3B>(BCaBA�B<�BK�BO\BH�B=qB>wBLBO�BRBRBQ�BR�BS@BSuBV�BY�B[�BZB_�B`�Bb4Bc�B`\B\xBV�BQ BR�BR�BU�Ba�B�aB|�Bu%BpUBk�Bo�Bm�BiBbBW�B]BtnBtBxRB�1B��B��B�7B�0B�RB��B�MB�EB�vB��B�B��B�IB��B�B�=B��B��Bz�Bv+BraBsBy>B��B��B�lB�B��B�)B��B�=B�qB��B��B��B��B��B�{B��BŢBňB�zBȀB�BՁBѝB�B�B�|B��B�B�B�B�B�mB�B��B�B�B��B��B�B�B�B��B�GB��B�B��B��B��B��B��B��B�BB�cB�]B	B	B	�B	�B	�B	�B	'B	�B	3B	�B	�B		7B	�B	]B	"�B	#�B	$tB	&�B	)�B	+�B	-]B	.}B	-CB	,�B	-�B	.}B	/5B	+B	$�B	!B	�B	�B	 �B	!�B	#�B	&�B	*�B	+�B	-�B	/�B	1'B	:DB	=VB	@OB	B[B	CaB	F�B	I�B	L�B	N�B	P�B	R�B	U2B	YeB	[�B	\)B	[	B	[�B	\B	[�B	[�B	[�B	\B	\)B	]B	\�B	\�B	\�B	^B	]/B	]�B	]dB	Z�B	V�B	U�B	YB	ZB	ZQB	Z�B	\�B	[=B	]/B	^B	`'B	a|B	a|B	b�B	c:B	fLB	ffB	f�B	f�B	ffB	hXB	g8B	h>B	kQB	oiB	pUB	p�B	r�B	u�B	vzB	vzB	w�B	w�B	z�B	|�B	~�B	��B	�B	��B	�B	��B	�)B	�"B	�B	�:B	�oB	�MB	�KB	�]B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�8B	�$B	�0B	�JB	�<B	�(B	�HB	�OB	�oB	ªB	ŢB	�_B	ȴB	˒B	�~B	̘B	͟B	ϫB	��B	ѷB	өB	өB	��B	��B	ԯB	��B	��B	��B	��B	��B	�B	�	B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�\B	�TB	�FB	�B	�LB	�8B	�RB	�RB	�DB	�DB	�KB	�KB	�0B	�0B	�QB	�QB	�QB	�QB	�WB	�]B	�B	�B	�UB	�UB	�oB	�vB	�B	�B	�|B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
	B

	B
B

�B

�B
�B
�B
�B
�B
B
B
B
B
B
B
B
B
B
B
(B
B
BB
bB
NB
B
 B
:B
:B
:B
@B
&B
&B
@B
[B
@B
,B
,B
,B
,B
FB
MB
MB
MB
gB
�B
SB
YB
YB
_B
yB
EB
_B
KB
eB
B
eB
kB
�B
xB
xB
~B
~B
�B
�B
�B
�B
�B
�B
 �B
 vB
 �B
 �B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
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
+�B
*�B
*�B
+�B
+�B
,�B
,�B
-�B
,�B
-�B
,�B
,�B
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
0�B
0�B
1B
1�B
1�B
2�B
2�B
2�B
3�B
3�B
4�B
4�B
4�B
7B
6�B
7B
6�B
72B
72B
6�B
7B
72B
88B
9$B
9$B
9	B
9	B
:B
:*B
:*B
:DB
;0B
;JB
;B
;0B
;B
:�B
;B
;0B
;B
<B
<B
<B
<6B
<B
="B
=<B
=VB
>BB
>BB
>(B
?HB
?HB
?HB
?B
?.B
?HB
?HB
@4B
@4B
@OB
@OB
AUB
A;B
AUB
AoB
A;B
A;B
AUB
A;B
BAB
A;B
A;B
B'B
B[B
B[B
BAB
B[B
BuB
CaB
CaB
CGB
DgB
DgB
DgB
DgB
DgB
DgB
ESB
E�B
ESB
FYB
FtB
F?B
F?B
FtB
FtB
G�B
G_B
G�B
HfB
HfB
HfB
H�B
H�B
I�B
I�B
JrB
J�B
J�B
KxB
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
NpB
N�B
N�B
N�B
N�B
NpB
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
Q�B
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
T�B
T�B
T�B
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
W�B
W�B
X�B
X�B
X�B
X�B
X�B
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
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
^B
]�B
^B
^B
]�B
^�B
^�B
^�B
^�B
_B
_�B
_�B
_�B
_�B
`B
_�B
_�B
_�B
_�B
_�B
_�B
aB
aB
aB
`�B
`�B
aB
bB
bB
bB
cB
cB
b�B
cB
cB
c B
c B
dB
dB
c�B
dB
dB
dB
d&B
dB
dB
dB
d&B
d&B
d&B
eB
e,B
e,B
eB
e,B
e,B
f2B
f2B
fB
f2B
gB
gB
gB
g8B
g8B
g8B
gB
gB
h
B
h$B
h$B
h
B
h$B
i*B
iB
iDB
iDB
iDB
i*B
j0B
j0B
j0B
j0B
jeB
kQB
k6B
k6B
kQB
k6B
l=B
l=B
l=B
l=B
l=B
m)B
m]B
mCB
m]B
mCB
ncB
nIB
nIB
nIB
nIB
oOB
oOB
oOB
oiB
oiB
oOB
oOB
oOB
oOB
pUB
poB
pUB
pUB
poB
poB
pUB
qvB
q[B
qvB
qAB
q[B
q[B
rG111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.7(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201911040038152019110400381520191104003815202306231718402023062317184020230623171840201911050023082019110500230820191105002308  JA  ARFMdecpA19c                                                                20191030003714  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191029153743  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191029153745  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191029153746  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191029153746  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191029153746  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191029153746  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191029153746  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191029153748  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191029153748                      G�O�G�O�G�O�                JA  ARUP                                                                        20191029155412                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191029153440  CV  JULD            G�O�G�O�F�?                JM  ARCAJMQC2.0                                                                 20191103153815  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191103153815  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191104152308  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081840  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031506                      G�O�G�O�G�O�                