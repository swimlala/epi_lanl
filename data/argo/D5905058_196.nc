CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-12-15T15:38:23Z creation;2019-12-15T15:38:28Z conversion to V3.1;2023-06-29T05:50:28Z update;     
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
_FillValue                 �  ],   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191215153823  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_196                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @���ƻ�1   @����� @7�s�g��b����-�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�33@�  A   A   A@  A^ffA�  A�  A�  A�  A�33A�  A�33A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D���D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@e�@���@�A
�HA*�HAJ�HAiG�A�p�A�p�A�p�A�p�Aģ�A�p�A��A�p�B�RB
�RB�RB�RB"�RB*Q�B2�RB:�RBB�RBJ�RBR�RBZ�RBc�Bj�RBr�RBz�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B��\B�(�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�(�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�(�B�\)C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�c�C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
D +�D ��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D	+�D	��D
+�D
��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D +�D ��D!+�D!��D"+�D"��D#+�D#��D$+�D$��D%+�D%��D&+�D&��D'+�D'��D(+�D(��D)+�D)��D*+�D*��D++�D+��D,1�D,��D-+�D-��D.+�D.��D/+�D/��D0+�D0��D1+�D1��D2+�D2��D3+�D3��D41�D4��D5+�D5��D6+�D6��D7+�D7��D8+�D8��D9+�D9��D:+�D:��D;+�D;��D<+�D<��D=+�D=��D>+�D>��D?+�D?��D@+�D@��DA+�DA��DB+�DB��DC+�DC��DD+�DD��DE+�DE��DF+�DF��DG+�DG��DH+�DH��DI+�DI��DJ+�DJ��DK+�DK��DL+�DL��DM+�DM��DN+�DN��DO+�DO��DP+�DP��DQ+�DQ��DR+�DR��DS+�DS��DT+�DT��DU+�DU��DV+�DV��DW+�DW��DX+�DX��DY+�DY��DZ+�DZ��D[+�D[��D\+�D\��D]+�D]��D^+�D^��D_+�D_��D`+�D`��Da+�Da��Db+�Db��Dc+�Dc��Dd+�Dd��De+�De�Df+�Df��Dg+�Dg��Dh+�Dh��Di+�Di��Dj+�Dj��Dk+�Dk��Dl+�Dl��Dm+�Dm��Dn+�Dn��Do+�Do��Dp+�Dp��Dq+�Dq��Dr+�Dr��Ds+�Ds��Dt+�Dt��Du+�Du��Dv+�Dv��Dw+�Dw��Dx+�Dx��Dy+�Dy��Dz+�Dz��D{+�D{��D|+�D|��D}+�D}��D~+�D~��D+�D��D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D�D���D��D�U�DÕ�D���D��D�U�Dĕ�D���D��D�U�Dŕ�D���D��D�U�Dƕ�D���D��D�U�DǕ�D���D��D�U�Dȕ�D���D��D�U�Dɕ�D���D��D�U�Dʕ�D���D��D�U�D˕�D���D��D�U�D̕�D���D��D�U�D͕�D���D��D�U�DΕ�D���D��D�U�Dϕ�D���D��D�U�DЕ�D���D��D�U�Dѕ�D���D��D�U�Dҕ�D���D��D�U�Dӕ�D���D��D�U�Dԕ�D���D��D�U�DՕ�D���D��D�U�D֕�D���D��D�U�Dו�D���D��D�U�Dؕ�D���D��D�U�Dٕ�D���D��D�U�Dڕ�D���D��D�U�Dە�D���D��D�U�Dܕ�D���D��D�U�Dݕ�D���D��D�U�Dޕ�D���D��D�U�Dߕ�D���D��D�U�D���D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D�ҏD��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�;dA�;dA�;dA�C�A�G�A�G�A�A�A�=qA�;dA�?}A�I�A�I�A�G�A�I�A�E�A�G�A�G�A�M�A�5?Aͣ�A��
A̝�A�dZA��A���A��#A�ƨA�A˼jA˓uA�I�A�9XA�{A���A��yA��;A���AʼjAʬAʑhA�-A�=qA���A��!A��A�n�A�^5A�(�A��A��mA���A���A��;A�Q�A���A���A��-A��A�S�A���A�\)A���A��PA�1A���A�;dA��RA�7LA�E�A��`A��-A��hA��HA�ĜA��;A��;A�5?A�`BA�1'A��A��A��A�n�A���A�ȴA��yA���A��\A�ƨA�z�A���A�K�A��A���A��A���A�~�A�O�A�A��DA��A�1'A��-A��A�`BA�ƨA��A~�A~-A{�mAy+Au��At�!Ar��An~�Am�7Aln�Ai��AfjAdr�A_�A^��A_��A^��A\~�A[\)AY�AU�AT��AS��AR��AQO�AO�PAMXAJ��AI��AHz�AF��AC�
A@�A>�A<ĜA;�#A;l�A:�!A8�+A7&�A6�A5�A4r�A3�A2��A2bNA1��A1�A1"�A/�A.�A-��A-"�A+A*VA(�A(��A'�-A'
=A&-A%�7A$�!A$E�A#ƨA#XA"9XA!%A�9AVAz�AA�A�AAE�A��A�-AS�A+A��A�!A�mA`BA"�AjA  A�-A��A�;A7LA-A��A��AG�A
��A
E�A	`BAA�AA
=An�AoA(�AJAM�A ��@���@�?}@�b@��!@�dZ@���@���@�S�@��@��@��@�1@ꗍ@�{@���@��@��@�^5@�l�@�G�@��@��/@�l�@�n�@�G�@�V@��y@�r�@�
=@�$�@�V@�dZ@Չ7@���@�1'@�ȴ@��T@�1'@���@́@�V@�ƨ@ɑh@�(�@��y@���@�Q�@�|�@���@���@���@���@��#@��^@�X@�%@�bN@�t�@�+@��P@��@�C�@��H@�~�@��@�p�@�G�@��/@���@�x�@�5?@�E�@�G�@�Q�@��@�%@�1@�ƨ@�S�@���@��#@��`@�A�@�+@��R@�@��@�%@��/@��D@���@�&�@���@��@� �@��w@� �@��@���@�Z@���@��@��P@�1'@��;@�bN@�(�@���@��@�(�@�@��@���@�z�@��`@�hs@��@��@�`B@���@�J@�ff@��H@�\)@�dZ@�K�@��@���@�9X@�v�@�O�@��`@�z�@�1@��y@�ff@�G�@��@�Q�@�@�@�G�@�bN@�1'@���@�;d@�33@�33@�I�@��@�@��@��@�j@���@�|�@��
@���@�\)@���@��R@��+@�$�@���@���@�O�@��@�Z@��;@�o@��@�bN@��@��@�v�@�o@�@�1@�A�@��w@�|�@�l�@�S�@���@��@��u@�b@��R@�S�@���@�Ĝ@�/@�O�@���@�%@��@���@�  @���@���@�=q@���@�ff@�J@���@�O�@���@���@�C�@��H@��P@��@�E�@���@�O�@�7L@��@���@���@���@��D@�I�@�1@��;@��@���@��@�dZ@�S�@�C�@�33@�"�@��@��R@���@�v�@�E�@�@��#@���@��h@�p�@�O�@��@���@��@��j@��D@�j@�Q�@�1'@�1@���@�t�@�K�@�;d@�+@�"�@�
=@��H@���@��+@�5?@���@���@�G�@��@��@��@�Q�@�1'@�(�@� �@��;@�ƨ@��w@���@�S�@�+@�
=@�ȴ@�~�@�ff@�M�@��@��@���@�X@�G�@�/@��/@���@�z�@�j@�9X@�  @��@��@l�@~��@~v�@~$�@}�-@}`B@}`B@}O�@|�j@|��@|z�@{��@{33@{@z��@z=q@y�#@y��@y7L@x�`@xr�@w�w@w;d@w
=@v�y@vȴ@v��@v5?@up�@uV@t�D@t(�@s�m@s�F@s��@s��@st�@sdZ@sdZ@sC�@s33@r��@r~�@r�@q�#@qx�@qG�@p�`@pA�@o�w@o+@n�R@nv�@nE�@n$�@m��@m`B@l�@lz�@lZ@l(�@k��@k��@kS�@kC�@j��@jn�@j�@iX@h��@h��@hbN@g�w@gK�@f��@f�R@fV@f5?@e�@e��@e�@eO�@d��@d�/@d��@d(�@cƨ@cdZ@c33@c@b��@b��@b�@a��@a��@a��@a�7@aX@a�@`�`@`Ĝ@`r�@` �@_�@_+@^�y@^��@^E�@]@]p�@]�@\�@\�j@\�@\z�@[��@Z�@Z�\@ZM�@Y��@Y��@Yx�@Y7L@X��@Xr�@Xb@W�@W�P@W|�@W\)@W;d@W
=@Vȴ@Vv�@V5?@U�@U�h@U?}@T��@T�@T(�@S�F@S��@S@R�!@R��@R�@Qhs@Q%@PĜ@PQ�@O�;@O�P@OK�@O+@O�@N�R@N�+@Nv�@NE�@N@MO�@L�j@LI�@L1@K��@K�m@K��@KS�@K"�@J�@J��@I�@I�7@IX@H�`@H�@Hb@G��@G|�@G�@F�@Fv�@F$�@F{@E��@E�-@D�j@D9X@C�m@Cƨ@CdZ@C33@Co@B�H@B~�@A�@AX@@Ĝ@@��@@�@@bN@@Q�@@1'@?�P@?+@>�y@>ff@>@=�@=�-@=�@<�@<I�@<9X@;�m@;dZ@;o@;o@:��@:��@:��@:~�@:^5@:�@9�@9�^@9x�@8��@8�@8Q�@8A�@81'@8b@7�@7�w@7l�@7�@6��@6v�@6$�@5�h@5p�@5/@4�/@4��@4�D@4I�@3�m@3ƨ@3��@3t�@3o@2�@2��@2�\@2n�@2M�@2�@1�#@1��@1��@1x�@1&�@1%@0�`@0�9@0�@01'@0  @/�@/�;@/�@/�P@/�@.��@.��@.$�@-@-�-@-�h@-�@-`B@,�/@,j@,(�@+��@+��@+t�@+S�@+33@*�H@*n�@*=q@*J@)x�@)X@)&�@(�`@(�9@(Ĝ@(��@(Ĝ@(�@(bN@(bN@(A�@'��@'��@'�w@'��@'l�@&�y@&�+@&E�@&@%�-@%p�@%�@$�@$z�@$j@$I�@$(�@#��@#�
@#��@#dZ@#o@"��@"�!@"�\@"~�@"M�@"=q@"=q@"-@"�@!�#@!�7@!hs@!7L@!%@ ��@ Ĝ@ bN@  �@��@��@K�@ȴ@��@ff@�@�-@�@O�@?}@�@�j@��@�D@z�@9X@�@��@�
@�F@��@��@��@t�@"�@o@�H@��@n�@=q@�@�@��@�7@7L@��@��@��@�`@��@�u@bN@A�@ �@  @�@|�@\)@+@
=@ȴ@�+@ff@�@@�h@p�@`B@O�@?}@/@V@�@��@�j@�@�D@Z@I�@�
@��@t�@S�@C�@"�@�@��@��@��@^5@=q@-@�@�#@��@�7@x�@&�@��@Ĝ@��@�@�@r�@1'@1'@ �@b@�;@�w@�@|�@l�@K�@+@��@�@ȴ@��@E�@$�@�T@��@p�@�@�@�/@��@��@�j@��@j@I�@1@�
@ƨ@��@S�@C�@"�@
�@
��@
��@
~�@
n�@
=q@
=q@
=q@
-@	��@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�;dA�;dA�;dA�C�A�G�A�G�A�A�A�=qA�;dA�?}A�I�A�I�A�G�A�I�A�E�A�G�A�G�A�M�A�5?Aͣ�A��
A̝�A�dZA��A���A��#A�ƨA�A˼jA˓uA�I�A�9XA�{A���A��yA��;A���AʼjAʬAʑhA�-A�=qA���A��!A��A�n�A�^5A�(�A��A��mA���A���A��;A�Q�A���A���A��-A��A�S�A���A�\)A���A��PA�1A���A�;dA��RA�7LA�E�A��`A��-A��hA��HA�ĜA��;A��;A�5?A�`BA�1'A��A��A��A�n�A���A�ȴA��yA���A��\A�ƨA�z�A���A�K�A��A���A��A���A�~�A�O�A�A��DA��A�1'A��-A��A�`BA�ƨA��A~�A~-A{�mAy+Au��At�!Ar��An~�Am�7Aln�Ai��AfjAdr�A_�A^��A_��A^��A\~�A[\)AY�AU�AT��AS��AR��AQO�AO�PAMXAJ��AI��AHz�AF��AC�
A@�A>�A<ĜA;�#A;l�A:�!A8�+A7&�A6�A5�A4r�A3�A2��A2bNA1��A1�A1"�A/�A.�A-��A-"�A+A*VA(�A(��A'�-A'
=A&-A%�7A$�!A$E�A#ƨA#XA"9XA!%A�9AVAz�AA�A�AAE�A��A�-AS�A+A��A�!A�mA`BA"�AjA  A�-A��A�;A7LA-A��A��AG�A
��A
E�A	`BAA�AA
=An�AoA(�AJAM�A ��@���@�?}@�b@��!@�dZ@���@���@�S�@��@��@��@�1@ꗍ@�{@���@��@��@�^5@�l�@�G�@��@��/@�l�@�n�@�G�@�V@��y@�r�@�
=@�$�@�V@�dZ@Չ7@���@�1'@�ȴ@��T@�1'@���@́@�V@�ƨ@ɑh@�(�@��y@���@�Q�@�|�@���@���@���@���@��#@��^@�X@�%@�bN@�t�@�+@��P@��@�C�@��H@�~�@��@�p�@�G�@��/@���@�x�@�5?@�E�@�G�@�Q�@��@�%@�1@�ƨ@�S�@���@��#@��`@�A�@�+@��R@�@��@�%@��/@��D@���@�&�@���@��@� �@��w@� �@��@���@�Z@���@��@��P@�1'@��;@�bN@�(�@���@��@�(�@�@��@���@�z�@��`@�hs@��@��@�`B@���@�J@�ff@��H@�\)@�dZ@�K�@��@���@�9X@�v�@�O�@��`@�z�@�1@��y@�ff@�G�@��@�Q�@�@�@�G�@�bN@�1'@���@�;d@�33@�33@�I�@��@�@��@��@�j@���@�|�@��
@���@�\)@���@��R@��+@�$�@���@���@�O�@��@�Z@��;@�o@��@�bN@��@��@�v�@�o@�@�1@�A�@��w@�|�@�l�@�S�@���@��@��u@�b@��R@�S�@���@�Ĝ@�/@�O�@���@�%@��@���@�  @���@���@�=q@���@�ff@�J@���@�O�@���@���@�C�@��H@��P@��@�E�@���@�O�@�7L@��@���@���@���@��D@�I�@�1@��;@��@���@��@�dZ@�S�@�C�@�33@�"�@��@��R@���@�v�@�E�@�@��#@���@��h@�p�@�O�@��@���@��@��j@��D@�j@�Q�@�1'@�1@���@�t�@�K�@�;d@�+@�"�@�
=@��H@���@��+@�5?@���@���@�G�@��@��@��@�Q�@�1'@�(�@� �@��;@�ƨ@��w@���@�S�@�+@�
=@�ȴ@�~�@�ff@�M�@��@��@���@�X@�G�@�/@��/@���@�z�@�j@�9X@�  @��@��@l�@~��@~v�@~$�@}�-@}`B@}`B@}O�@|�j@|��@|z�@{��@{33@{@z��@z=q@y�#@y��@y7L@x�`@xr�@w�w@w;d@w
=@v�y@vȴ@v��@v5?@up�@uV@t�D@t(�@s�m@s�F@s��@s��@st�@sdZ@sdZ@sC�@s33@r��@r~�@r�@q�#@qx�@qG�@p�`@pA�@o�w@o+@n�R@nv�@nE�@n$�@m��@m`B@l�@lz�@lZ@l(�@k��@k��@kS�@kC�@j��@jn�@j�@iX@h��@h��@hbN@g�w@gK�@f��@f�R@fV@f5?@e�@e��@e�@eO�@d��@d�/@d��@d(�@cƨ@cdZ@c33@c@b��@b��@b�@a��@a��@a��@a�7@aX@a�@`�`@`Ĝ@`r�@` �@_�@_+@^�y@^��@^E�@]@]p�@]�@\�@\�j@\�@\z�@[��@Z�@Z�\@ZM�@Y��@Y��@Yx�@Y7L@X��@Xr�@Xb@W�@W�P@W|�@W\)@W;d@W
=@Vȴ@Vv�@V5?@U�@U�h@U?}@T��@T�@T(�@S�F@S��@S@R�!@R��@R�@Qhs@Q%@PĜ@PQ�@O�;@O�P@OK�@O+@O�@N�R@N�+@Nv�@NE�@N@MO�@L�j@LI�@L1@K��@K�m@K��@KS�@K"�@J�@J��@I�@I�7@IX@H�`@H�@Hb@G��@G|�@G�@F�@Fv�@F$�@F{@E��@E�-@D�j@D9X@C�m@Cƨ@CdZ@C33@Co@B�H@B~�@A�@AX@@Ĝ@@��@@�@@bN@@Q�@@1'@?�P@?+@>�y@>ff@>@=�@=�-@=�@<�@<I�@<9X@;�m@;dZ@;o@;o@:��@:��@:��@:~�@:^5@:�@9�@9�^@9x�@8��@8�@8Q�@8A�@81'@8b@7�@7�w@7l�@7�@6��@6v�@6$�@5�h@5p�@5/@4�/@4��@4�D@4I�@3�m@3ƨ@3��@3t�@3o@2�@2��@2�\@2n�@2M�@2�@1�#@1��@1��@1x�@1&�@1%@0�`@0�9@0�@01'@0  @/�@/�;@/�@/�P@/�@.��@.��@.$�@-@-�-@-�h@-�@-`B@,�/@,j@,(�@+��@+��@+t�@+S�@+33@*�H@*n�@*=q@*J@)x�@)X@)&�@(�`@(�9@(Ĝ@(��@(Ĝ@(�@(bN@(bN@(A�@'��@'��@'�w@'��@'l�@&�y@&�+@&E�@&@%�-@%p�@%�@$�@$z�@$j@$I�@$(�@#��@#�
@#��@#dZ@#o@"��@"�!@"�\@"~�@"M�@"=q@"=q@"-@"�@!�#@!�7@!hs@!7L@!%@ ��@ Ĝ@ bN@  �@��@��@K�@ȴ@��@ff@�@�-@�@O�@?}@�@�j@��@�D@z�@9X@�@��@�
@�F@��@��@��@t�@"�@o@�H@��@n�@=q@�@�@��@�7@7L@��@��@��@�`@��@�u@bN@A�@ �@  @�@|�@\)@+@
=@ȴ@�+@ff@�@@�h@p�@`B@O�@?}@/@V@�@��@�j@�@�D@Z@I�@�
@��@t�@S�@C�@"�@�@��@��@��@^5@=q@-@�@�#@��@�7@x�@&�@��@Ĝ@��@�@�@r�@1'@1'@ �@b@�;@�w@�@|�@l�@K�@+@��@�@ȴ@��@E�@$�@�T@��@p�@�@�@�/@��@��@�j@��@j@I�@1@�
@ƨ@��@S�@C�@"�@
�@
��@
��@
~�@
n�@
=q@
=q@
=q@
-@	��@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
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
��BBoBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B%�B1'B8RB0!BJB
�B
��B
��B
�B
�/B�7B��BbB{B\B��B��B�sB�`B1B�B��B	7BVB�B�B�B�B�BoB\B�B�BuBDB%BB��B��B�B�yB�#B��B�9B�oB|�Bn�BjB]/BP�B<jB�B\B
�B
ȴB
�FB
��B
��B
|�B
m�B
m�B
aHB
M�B
49B
(�B
+B
+B
�B
!�B
$�B
�B	��B	�HB	��B	ɺB	�B	��B	�hB	~�B	dZB	K�B	,B	�B	49B	2-B	.B	"�B	�B	JB	+B	B��B��B�B�HB�B��BɺB�wB�B�{B�\B�B�B}�Bz�Bu�Bq�Bo�Br�B|�B�7B�JB�JB�VB�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�PB�B� Bs�BhsBcTBZBP�BO�BO�BP�BR�BXBW
BXBW
BXBYBYBYBYBYBXBW
BR�BP�BP�BO�BM�BM�BN�BN�BL�BH�BH�BF�BA�B=qB>wBF�B?}B>wB8RB7LB5?B5?B49B33B33B33B49B2-B/B.B/B33B>wBC�BC�BB�B>wB7LB9XB9XB9XB5?B;dB?}B9XB5?B6FB6FB49B49B5?B5?B6FB8RB9XB;dB<jB=qB=qB@�BB�BB�BC�BC�BB�BB�BD�BH�BN�BO�BP�BT�BYB[#B]/B[#B[#B`BBk�Bp�Bq�Bs�Bt�Bu�Bu�Bx�By�B�B�oB�uB�oB�bB�VB�=B�1B�+B�1B�=B�=B�=B�VB�hB�uB��B��B��B��B��B��B��B��B�B�B�!B�?B�^B�wBĜBƨBɺB��B��B�;B�`B�yB�B�B��B��B��B��B��B��B	+B	oB	#�B	'�B	+B	.B	5?B	:^B	@�B	E�B	E�B	E�B	B�B	@�B	8RB	49B	2-B	2-B	2-B	1'B	1'B	0!B	.B	0!B	5?B	49B	5?B	5?B	7LB	7LB	7LB	:^B	>wB	J�B	ZB	u�B	v�B	r�B	n�B	m�B	�B	�1B	�=B	�=B	�JB	�JB	�PB	�\B	�bB	�bB	�uB	��B	��B	��B	��B	�oB	�VB	�=B	�PB	�DB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�B	�B	�B	�B	�3B	�3B	�3B	�3B	�9B	�9B	�?B	�FB	�FB	�LB	�LB	�XB	�^B	�dB	�jB	�jB	�qB	�wB	�wB	�}B	�}B	�}B	��B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�#B	�)B	�/B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
JB
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
\B
\B
bB
bB
bB
hB
hB
hB
hB
oB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
+B
)�B
+B
+B
+B
+B
+B
,B
-B
-B
-B
-B
-B
.B
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
2-B
2-B
2-B
33B
33B
49B
49B
49B
6FB
6FB
7LB
7LB
7LB
8RB
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
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
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
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
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
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
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
T�B
T�B
T�B
T�B
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
YB
YB
ZB
ZB
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
]/B
]/B
]/B
]/B
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
aHB
aHB
aHB
aHB
aHB
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
hsB
hsB
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
iyB
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
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
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
n�B
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
q�B
r�B
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
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�YB
�YB
�YB
�YB
�YB
�sB
�mB
�YB
�SB
�SB
�YB
�_B
�_B
�_B
�_B
�B
��B
�~B
��BuB�B�B�B�B�B�BmB�B�B�B�B�B�B�B�B�B�B�B;B'RB4nB>�B;B�B
�DB
��B
��B
�QB
�#B��B�B�BEB�BB��B�B��B�B!HB�}B	�B(B
B�B�B�BCB@BbB�B?B�BB1B�B �B�B�!B�QBܬB�4B�2B��B~wBoOBl�B_;BS�B?}B �B&B
�B
�rB
�RB
��B
�=B
~BB
n}B
o�B
dtB
P�B
5�B
*KB
,WB
,qB
 �B
# B
'�B
�B	�HB	��B	׍B	͟B	�}B	��B	��B	�uB	g8B	O�B	,�B	#B	5tB	4TB	/�B	%FB	�B	�B	KB	MB��B��B�'B�B�YBуB�0B��B�/B��B�hB�B��B.B}<BwLBr�BpoBs�B~(B�lB�~B��B��B�B�&B��B�B�eB�-B�pB�B�B��B�|B��B�dB�WB��B��B�B��B��B�uButBi�Be�B[�BQ�BP�BP}BQ4BR�BXEBW
BXBWsBX�BYBYKBY�BYBYBX�BX+BS�BR:BRoBP�BNVBN<BO�BO�BM�BIRBI�BG_BB�B>]B?.BHfBA B@ B9XB8B6FB6�B5B3�B3�B4B5ZB3MB0!B.�B/ B2�B>BC{BD3BDB?cB7fB9�B:B9�B5�B;�B@�B:�B5�B6�B6�B5B5B5�B5�B6�B8�B:DB<B=B=�B>(BA�BCGBCGBD�BC�BCBCGBEBI�BOBO�BP�BUBY1B[=B]IB[	BZ�B_�Bk�Bp�Bq�Bs�Bt�Bu�Bu�Bx�By>B�{B�oB��B��B��B��B��B�1B�EB�fB��B��B�rB��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B�B�BĶB�tB�lB�JB��B޸B�,B�yB�0B�}B�%B�B�+B�B�$B�B	�B	B	#B	'mB	*eB	-wB	4�B	9�B	@4B	E�B	E�B	F?B	C{B	A;B	8�B	4TB	2GB	2aB	2�B	1[B	1vB	0UB	./B	0�B	5�B	4nB	5ZB	5?B	7fB	72B	6�B	9�B	="B	H�B	XB	u�B	wLB	r�B	m�B	k�B	��B	�B	�#B	�=B	�0B	�B	�6B	�(B	�.B	�HB	�uB	��B	��B	��B	�$B	�&B	��B	�	B	�6B	��B	��B	��B	�=B	��B	��B	��B	��B	�,B	�tB	��B	��B	��B	�B	�7B	��B	� B	�B	�sB	�
B	�KB	��B	�!B	�HB	��B	�B	�;B	��B	�B	�B	�B	�UB	�IB	�)B	��B	��B	�MB	�MB	�3B	�B	��B	��B	�B	�B	�B	�B	�2B	�$B	�*B	�0B	�B	�B	�<B	�BB	�BB	�HB	�HB	�cB	�UB	�AB	�{B	āB	ňB	�tB	�tB	�_B	ȀB	ȀB	ɠB	ʌB	ʌB	ʌB	ˬB	�~B	̈́B	̈́B	οB	��B	��B	ҽB	��B	��B	��B	өB	��B	��B	��B	�
B	��B	�B	�	B	��B	�B	�B	�B	�B	�B	�B	�4B	� B	� B	�:B	�&B	�,B	�2B	�LB	�RB	�$B	�DB	�_B	�eB	�eB	�WB	�]B	�CB	�]B	�}B	�OB	�OB	�iB	�B	�oB	�[B	�[B	�vB	�|B	�|B	�B	�nB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
B
�B
�B
B
B
�B
�B
�B
B
	B
	B
	B
	B
�B
�B

	B
	�B
B
B
0B
B
JB
6B
B
6B
"B
<B
B
"B
B
(B
(B
B
.B
.B
4B
B
B
4B
:B
:B
&B
&B
&B
FB
aB
MB
2B
9B
9B
SB
SB
SB
?B
?B
sB
_B
_B
eB
eB
B
�B
kB
qB
WB
WB
WB
�B
�B
xB
xB
]B
~B
dB
dB
�B
�B
�B
�B
 �B
 vB
 vB
 �B
 �B
!�B
!�B
!|B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
*�B
)�B
*�B
*�B
*�B
*�B
+B
+�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
.�B
.�B
/�B
/�B
0B
0B
0�B
0�B
1�B
1�B
2B
2�B
2�B
3�B
3�B
4B
6+B
6B
6�B
6�B
7B
8B
7B
72B
88B
8B
9>B
9	B
9$B
9$B
9$B
9	B
:*B
;0B
;0B
;0B
;B
;0B
;0B
;0B
;JB
<6B
<B
<PB
=<B
=<B
=B
="B
>BB
>(B
>(B
>(B
?.B
@OB
@OB
A;B
AUB
B[B
B[B
BAB
BAB
B[B
B[B
CaB
CaB
CGB
C{B
CaB
C{B
DgB
DgB
DgB
ESB
EmB
EmB
EmB
FtB
FtB
FtB
FYB
GzB
G_B
G_B
G_B
HfB
HfB
H�B
H�B
HfB
HfB
H�B
I�B
IlB
I�B
I�B
I�B
J�B
J�B
JrB
JrB
J�B
J�B
K�B
K�B
K�B
K�B
L~B
L~B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
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
T�B
T�B
T�B
T�B
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
X�B
X�B
Y�B
Y�B
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
\�B
\�B
\�B
\�B
^B
^B
]�B
]�B
_B
^�B
^�B
_B
^�B
`B
`B
`B
_�B
`B
`�B
`�B
`�B
aB
aB
aB
`�B
aB
`�B
`�B
`�B
aB
bB
a�B
bB
bB
a�B
c B
c B
c B
c B
cB
cB
c B
c�B
c�B
c�B
dB
d&B
d&B
e,B
e,B
eB
e,B
e,B
eB
fB
fB
f2B
fB
fB
f2B
fB
fB
gB
gB
gB
gB
gB
g8B
h$B
h$B
h$B
h
B
h>B
h$B
h$B
hXB
iDB
iDB
iDB
j0B
jKB
jKB
jKB
j0B
jKB
j0B
iDB
j0B
jKB
jKB
k6B
k6B
kQB
k6B
kQB
kQB
kQB
l=B
l"B
l=B
lWB
mCB
mCB
mCB
m]B
m]B
m)B
m]B
mCB
m]B
ncB
nIB
nIB
ncB
nIB
nIB
oiB
oiB
oiB
oiB
oiB
poB
pUB
pUB
p;B
p;B
poB
pUB
q[B
qvB
q[B
qvB
q[B
q[B
qvB
r|B
r|B
r|B
raB
r|B
raB
r|B
shB
sMB
sMB
shB
tnB
tn1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<9+<(�z<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.68(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912210036342019122100363420191221003634202306231719342023062317193420230623171934201912220034242019122200342420191222003424  JA  ARFMdecpA19c                                                                20191216003718  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191215153823  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191215153826  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191215153826  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191215153827  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191215153827  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191215153827  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191215153827  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191215153827  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191215153828                      G�O�G�O�G�O�                JA  ARUP                                                                        20191215155443                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191215153333  CV  JULD            G�O�G�O�Fǜ�                JM  ARCAJMQC2.0                                                                 20191220153634  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191220153634  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191221153424  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081934  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                