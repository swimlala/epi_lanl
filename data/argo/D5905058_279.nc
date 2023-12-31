CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-12-04T06:40:35Z creation;2020-12-04T06:40:38Z conversion to V3.1;2023-06-29T05:47:48Z update;     
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
resolution        =���   axis      Z        d  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ih   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  MD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  �L   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۰   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20201204064035  20230705041504  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0675_279                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�LD��O�1   @�LE��O�@6z	� ��b�\��N<1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�C3D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�A
�HA*�HAJ�HAj�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB
�RB�RB�RB"�RB*�RB2�RB:�RBB�RBJ�RBR�RBZ�RBb�RBj�RBr�RBz�RB�\)B�\)B�\)B�\)B��\B��\B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�(�B�\)B�\)B�\)B�\)C �C�C�C�C�C
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
C�J=C�W
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
��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D +�D ��D!+�D!��D"+�D"��D#+�D#��D$+�D$��D%+�D%��D&+�D&��D'+�D'��D(+�D(��D)+�D)��D*+�D*��D++�D+��D,+�D,��D-+�D-��D.+�D.��D/+�D/��D0+�D0��D1+�D1��D2+�D2��D3+�D3��D4+�D4��D5+�D5��D6+�D6��D7+�D7��D8+�D8��D9+�D9��D:+�D:��D;+�D;��D<+�D<��D=+�D=��D>+�D>��D?+�D?��D@+�D@��DA+�DA��DB+�DB��DC+�DC��DD+�DD��DE+�DE��DF+�DF��DG+�DG��DH+�DH��DI+�DI��DJ+�DJ��DK+�DK��DL+�DL��DM+�DM��DN1�DN��DO+�DO��DP+�DP��DQ+�DQ��DR+�DR��DS+�DS��DT+�DT��DU+�DU��DV+�DV��DW+�DW��DX+�DX��DY+�DY��DZ+�DZ��D[+�D[��D\+�D\��D]+�D]��D^+�D^��D_+�D_��D`+�D`��Da+�Da��Db+�Db��Dc+�Dc��Dd+�Dd��De+�De��Df+�Df��Dg+�Dg��Dh+�Dh��Di+�Di��Dj+�Dj��Dk+�Dk��Dl+�Dl��Dm+�Dm��Dn+�Dn��Do+�Do��Dp+�Dp��Dq+�Dq��Dr+�Dr��Ds+�Ds��Dt+�Dt��Du+�Du��Dv+�Dv��Dw+�Dw��Dx+�Dx��Dy+�Dy��Dz+�Dz��D{+�D{��D|+�D|��D}+�D}��D~+�D~��D+�D��D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D���D���D��D�U�D�D���D��D�U�DÕ�D���D��D�U�Dĕ�D���D��D�U�Dŕ�D���D��D�U�Dƕ�D���D��D�U�DǕ�D���D��D�U�Dȕ�D���D��D�U�Dɕ�D���D��D�U�Dʕ�D���D��D�U�D˕�D���D��D�U�D̕�D���D��D�U�D͕�D���D��D�U�DΕ�D���D��D�U�Dϕ�D���D��D�U�DЕ�D���D��D�U�Dѕ�D���D��D�U�Dҕ�D���D��D�U�Dӕ�D���D��D�U�Dԕ�D���D��D�U�DՕ�D���D��D�U�D֕�D���D��D�U�Dו�D���D��D�U�Dؕ�D���D��D�U�Dٕ�D���D��D�U�Dڕ�D���D��D�U�Dە�D���D��D�U�Dܕ�D���D��D�U�Dݕ�D���D��D�U�Dޕ�D���D��D�U�Dߕ�D���D��D�U�D���D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D��D���D��D�U�D���D���D��D�U�D���D���D��D�X�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�jA�l�A�l�A�\)A�ZA�S�A�I�A�I�A�XA�\)A���A�E�A�7LA�9XA�1A��A��#A�ĜAǗ�A�|�A�t�A�bNA�A�A�7LA�(�A�&�A�&�A�&�A�(�A�(�A�+A�&�A�"�A�"�A��A��A�
=A���A��A���A�\)A��DA�1A���A�Q�A�A�A�XA�XA�  A�dZA��wA�I�A��RA�C�A���A�M�A�  A� �A�ffA��A��mA��`A�I�A�ffA��+A�|�A��;A�S�A��#A���A�n�A��7A��A���A�+A��A�-A�bA��9A���A�^5A�C�A�C�A��A�JA�&�A�C�A��DA��+A�+A���A�O�A���A�5?A���A��A�oA�x�A��wA��A%A|��A{C�Ay�Av�+Ar�jAp�An^5AkƨAj{Ag�AfI�Ae�Ac��Ab(�A`��A^ZA[�
AV�uAT��AS��AR�APZAO
=AL1'AJZAH�9AHQ�AG7LACC�ABM�AA�A@{A?VA=t�A;��A:��A:{A9|�A8��A81A6��A6$�A5��A5/A4�A3�A3�A2ĜA1�A0Q�A/x�A.JA,ȴA+�;A+dZA*�yA)��A)VA(��A&v�A%�wA%l�A$�DA#�A#VA"JA!"�A {AA?}AQ�A%AdZA�A�hA�A��A^5A�AS�A��An�A��Av�A��A�PA��A�A&�A��A|�A�A|�A
A�A	G�AĜA{A\)AbNA �A��A�jA��A{AS�A ff@��@��T@��/@�b@��H@��@��@�Q�@��@��-@�G�@�1'@�P@���@�  @�|�@�@땁@��/@��@�=q@�D@�l�@�E�@���@�1@�
=@�@ܣ�@�@���@�/@��@�b@�l�@���@ѡ�@с@�V@�A�@Ͼw@θR@�hs@�j@�1'@�ƨ@�@��@ə�@�hs@��@�bN@�ƨ@�K�@��@���@�1'@Ý�@��@�J@��;@�ƨ@��@���@�?}@�(�@��@��\@�&�@��;@���@��@��@�`B@�z�@���@���@�\)@�K�@�@���@�&�@�I�@�;d@���@�
=@��w@�@�$�@�O�@��j@���@�Q�@��P@��!@�v�@�=q@��T@���@��u@���@���@�K�@��@���@���@��h@��@���@�r�@�ƨ@��@�t�@�S�@���@�=q@���@���@�hs@��@��`@��D@�1@���@�"�@��R@�V@�$�@���@�/@��@���@���@� �@�1@���@��F@�K�@�o@��@��\@�5?@�J@��^@���@��h@�7L@��/@��u@�Q�@�1'@� �@�b@��m@�ƨ@��@�33@��@�
=@���@��y@�ȴ@�~�@�5?@��@�{@�@���@�&�@��`@��j@��D@�bN@�I�@��@�t�@�33@�
=@��@���@�v�@�E�@�-@�{@��@���@��^@��h@�X@�/@�V@��`@���@�bN@�1'@���@��F@�dZ@�C�@��@��@���@���@��+@�n�@�V@�-@�{@��T@���@�O�@�&�@�%@��@�Ĝ@�j@�I�@�9X@�9X@�1'@�1@��@��w@�|�@�;d@�"�@�
=@��H@��R@���@��+@�V@�=q@��@��@��-@��7@�x�@�hs@�X@�?}@�/@�/@��@��j@��u@�Z@�  @�ƨ@��F@���@���@�t�@�;d@��@��y@��!@�~�@�5?@�$�@�J@���@�x�@�`B@�?}@��@���@���@���@�r�@�I�@�1'@��@�  @��;@�ƨ@��F@���@���@��@�\)@���@��H@��@���@�~�@�M�@�=q@��@��T@���@���@��^@��h@�X@�&�@��@���@�r�@�A�@�b@�w@K�@~��@~E�@}��@}p�@|�j@{�F@z��@zn�@zJ@y��@yX@y�@xĜ@xA�@w��@w��@wK�@v��@v��@vff@u�@u�T@u�@t�@t�D@t9X@s�
@sS�@r�!@rn�@r�@q�^@qG�@p��@p��@pbN@p  @o�@oK�@oK�@o�@n�+@nV@n@m�@l��@l�@lj@kƨ@k��@kC�@j��@j��@j~�@j=q@i��@ihs@i�@h�`@h��@h1'@g�@gK�@f��@f��@f�+@fV@f{@e��@e`B@e�@d�/@d��@d��@d�D@d�D@d��@dz�@c��@c��@cdZ@cC�@c33@b��@b^5@b-@a�@a��@aG�@`�@` �@`  @_;d@^��@^v�@^��@^ȴ@^@^{@]��@\��@\�D@\�@[ƨ@[��@["�@Z~�@Z~�@Z=q@Y��@Y�#@Y��@Y��@Yx�@YX@X��@Xr�@X1'@Xb@X  @W�@W\)@W+@W�@V��@Vȴ@Vv�@U�T@U�-@U��@U�h@U/@T�@Tz�@T9X@T1@S�m@S�m@S��@S�@S33@R��@R�@R��@R��@Q��@Qx�@Q�7@Qx�@Q�@P�`@P�u@Pr�@PbN@PbN@O�@O|�@O+@N��@O
=@N�@M��@Mp�@MO�@MV@L�/@Lj@L(�@K�m@K��@KC�@Ko@J�@J�H@J��@J�\@J~�@J=q@I�@I��@I%@H�u@HA�@Hb@G�w@G|�@GK�@G�@F�y@F�+@F@E��@E��@E@E`B@E?}@E/@E�@EV@EV@D��@D�@DI�@D�@C�F@Co@B��@Bn�@B=q@B�@BJ@A��@A�^@Ahs@A�@@��@@r�@@1'@?�@?|�@?+@>�R@>v�@>{@=��@=�-@=��@=�@=/@<��@<�@<�j@<��@<9X@<�@;�F@;dZ@;@:~�@:�@9�^@9x�@97L@8��@8bN@8  @7l�@6�y@6v�@65?@6$�@6@5�@5@5p�@5O�@5�@5�@5V@4��@4��@4�D@4j@4I�@3�m@3�@333@2��@2�\@2M�@2-@2�@1��@1x�@1G�@1�@0��@0�@0bN@01'@0  @/�@/l�@/�@.�R@.E�@-�T@-�T@-��@-�h@-O�@-/@,��@,�D@+��@+�
@+��@+dZ@+C�@+33@*�@*~�@*M�@)�#@)��@)��@)�7@)�7@)x�@)G�@(�`@(Ĝ@(��@(r�@(A�@'�@'��@'l�@'�@&��@&�y@&�R@&�R@&��@&V@&$�@%�T@%�-@%��@%`B@%�@$��@$�j@$z�@$I�@$1@#ƨ@#��@#�@#t�@#dZ@#dZ@#S�@#C�@#"�@"�H@"~�@"=q@!��@!hs@!�@!%@ ��@ Ĝ@ ��@ r�@ Q�@   @��@\)@\)@\)@K�@�@��@v�@ff@V@E�@{@�T@@�-@��@�@?}@/@�@�@��@��@j@I�@(�@��@�m@ƨ@��@�@C�@o@�@�!@~�@~�@^5@M�@�@��@�#@hs@7L@�@��@Ĝ@�9@��@��@�u@r�@Q�@1'@�;@��@|�@K�@;d@��@ȴ@�R@��@ff@$�@�-@`B@/@�@��@�j@��@Z@1@�
@ƨ@dZ@o@�H@�!@�\@�@hs@��@�`@��@Ĝ@Q�@  @�;@�w@�@�P@|�@\)@K�@;d@+@�@��@ȴ@E�@$�@@@��@�@?}@�@V@�@�j@��@�D@z�@z�@j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�jA�l�A�l�A�\)A�ZA�S�A�I�A�I�A�XA�\)A���A�E�A�7LA�9XA�1A��A��#A�ĜAǗ�A�|�A�t�A�bNA�A�A�7LA�(�A�&�A�&�A�&�A�(�A�(�A�+A�&�A�"�A�"�A��A��A�
=A���A��A���A�\)A��DA�1A���A�Q�A�A�A�XA�XA�  A�dZA��wA�I�A��RA�C�A���A�M�A�  A� �A�ffA��A��mA��`A�I�A�ffA��+A�|�A��;A�S�A��#A���A�n�A��7A��A���A�+A��A�-A�bA��9A���A�^5A�C�A�C�A��A�JA�&�A�C�A��DA��+A�+A���A�O�A���A�5?A���A��A�oA�x�A��wA��A%A|��A{C�Ay�Av�+Ar�jAp�An^5AkƨAj{Ag�AfI�Ae�Ac��Ab(�A`��A^ZA[�
AV�uAT��AS��AR�APZAO
=AL1'AJZAH�9AHQ�AG7LACC�ABM�AA�A@{A?VA=t�A;��A:��A:{A9|�A8��A81A6��A6$�A5��A5/A4�A3�A3�A2ĜA1�A0Q�A/x�A.JA,ȴA+�;A+dZA*�yA)��A)VA(��A&v�A%�wA%l�A$�DA#�A#VA"JA!"�A {AA?}AQ�A%AdZA�A�hA�A��A^5A�AS�A��An�A��Av�A��A�PA��A�A&�A��A|�A�A|�A
A�A	G�AĜA{A\)AbNA �A��A�jA��A{AS�A ff@��@��T@��/@�b@��H@��@��@�Q�@��@��-@�G�@�1'@�P@���@�  @�|�@�@땁@��/@��@�=q@�D@�l�@�E�@���@�1@�
=@�@ܣ�@�@���@�/@��@�b@�l�@���@ѡ�@с@�V@�A�@Ͼw@θR@�hs@�j@�1'@�ƨ@�@��@ə�@�hs@��@�bN@�ƨ@�K�@��@���@�1'@Ý�@��@�J@��;@�ƨ@��@���@�?}@�(�@��@��\@�&�@��;@���@��@��@�`B@�z�@���@���@�\)@�K�@�@���@�&�@�I�@�;d@���@�
=@��w@�@�$�@�O�@��j@���@�Q�@��P@��!@�v�@�=q@��T@���@��u@���@���@�K�@��@���@���@��h@��@���@�r�@�ƨ@��@�t�@�S�@���@�=q@���@���@�hs@��@��`@��D@�1@���@�"�@��R@�V@�$�@���@�/@��@���@���@� �@�1@���@��F@�K�@�o@��@��\@�5?@�J@��^@���@��h@�7L@��/@��u@�Q�@�1'@� �@�b@��m@�ƨ@��@�33@��@�
=@���@��y@�ȴ@�~�@�5?@��@�{@�@���@�&�@��`@��j@��D@�bN@�I�@��@�t�@�33@�
=@��@���@�v�@�E�@�-@�{@��@���@��^@��h@�X@�/@�V@��`@���@�bN@�1'@���@��F@�dZ@�C�@��@��@���@���@��+@�n�@�V@�-@�{@��T@���@�O�@�&�@�%@��@�Ĝ@�j@�I�@�9X@�9X@�1'@�1@��@��w@�|�@�;d@�"�@�
=@��H@��R@���@��+@�V@�=q@��@��@��-@��7@�x�@�hs@�X@�?}@�/@�/@��@��j@��u@�Z@�  @�ƨ@��F@���@���@�t�@�;d@��@��y@��!@�~�@�5?@�$�@�J@���@�x�@�`B@�?}@��@���@���@���@�r�@�I�@�1'@��@�  @��;@�ƨ@��F@���@���@��@�\)@���@��H@��@���@�~�@�M�@�=q@��@��T@���@���@��^@��h@�X@�&�@��@���@�r�@�A�@�b@�w@K�@~��@~E�@}��@}p�@|�j@{�F@z��@zn�@zJ@y��@yX@y�@xĜ@xA�@w��@w��@wK�@v��@v��@vff@u�@u�T@u�@t�@t�D@t9X@s�
@sS�@r�!@rn�@r�@q�^@qG�@p��@p��@pbN@p  @o�@oK�@oK�@o�@n�+@nV@n@m�@l��@l�@lj@kƨ@k��@kC�@j��@j��@j~�@j=q@i��@ihs@i�@h�`@h��@h1'@g�@gK�@f��@f��@f�+@fV@f{@e��@e`B@e�@d�/@d��@d��@d�D@d�D@d��@dz�@c��@c��@cdZ@cC�@c33@b��@b^5@b-@a�@a��@aG�@`�@` �@`  @_;d@^��@^v�@^��@^ȴ@^@^{@]��@\��@\�D@\�@[ƨ@[��@["�@Z~�@Z~�@Z=q@Y��@Y�#@Y��@Y��@Yx�@YX@X��@Xr�@X1'@Xb@X  @W�@W\)@W+@W�@V��@Vȴ@Vv�@U�T@U�-@U��@U�h@U/@T�@Tz�@T9X@T1@S�m@S�m@S��@S�@S33@R��@R�@R��@R��@Q��@Qx�@Q�7@Qx�@Q�@P�`@P�u@Pr�@PbN@PbN@O�@O|�@O+@N��@O
=@N�@M��@Mp�@MO�@MV@L�/@Lj@L(�@K�m@K��@KC�@Ko@J�@J�H@J��@J�\@J~�@J=q@I�@I��@I%@H�u@HA�@Hb@G�w@G|�@GK�@G�@F�y@F�+@F@E��@E��@E@E`B@E?}@E/@E�@EV@EV@D��@D�@DI�@D�@C�F@Co@B��@Bn�@B=q@B�@BJ@A��@A�^@Ahs@A�@@��@@r�@@1'@?�@?|�@?+@>�R@>v�@>{@=��@=�-@=��@=�@=/@<��@<�@<�j@<��@<9X@<�@;�F@;dZ@;@:~�@:�@9�^@9x�@97L@8��@8bN@8  @7l�@6�y@6v�@65?@6$�@6@5�@5@5p�@5O�@5�@5�@5V@4��@4��@4�D@4j@4I�@3�m@3�@333@2��@2�\@2M�@2-@2�@1��@1x�@1G�@1�@0��@0�@0bN@01'@0  @/�@/l�@/�@.�R@.E�@-�T@-�T@-��@-�h@-O�@-/@,��@,�D@+��@+�
@+��@+dZ@+C�@+33@*�@*~�@*M�@)�#@)��@)��@)�7@)�7@)x�@)G�@(�`@(Ĝ@(��@(r�@(A�@'�@'��@'l�@'�@&��@&�y@&�R@&�R@&��@&V@&$�@%�T@%�-@%��@%`B@%�@$��@$�j@$z�@$I�@$1@#ƨ@#��@#�@#t�@#dZ@#dZ@#S�@#C�@#"�@"�H@"~�@"=q@!��@!hs@!�@!%@ ��@ Ĝ@ ��@ r�@ Q�@   @��@\)@\)@\)@K�@�@��@v�@ff@V@E�@{@�T@@�-@��@�@?}@/@�@�@��@��@j@I�@(�@��@�m@ƨ@��@�@C�@o@�@�!@~�@~�@^5@M�@�@��@�#@hs@7L@�@��@Ĝ@�9@��@��@�u@r�@Q�@1'@�;@��@|�@K�@;d@��@ȴ@�R@��@ff@$�@�-@`B@/@�@��@�j@��@Z@1@�
@ƨ@dZ@o@�H@�!@�\@�@hs@��@�`@��@Ĝ@Q�@  @�;@�w@�@�P@|�@\)@K�@;d@+@�@��@ȴ@E�@$�@@@��@�@?}@�@V@�@�j@��@�D@z�@z�@j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B  B�B=qBB�B9XB:^BB�BI�BK�BS�B^5B^5B_;B^5B]/B]/B^5BaHBbNBbNBbNBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBaHB[#BjBcTB^5B^5B^5BW
BM�BP�BQ�BO�BG�BC�B9XB=qBS�BXBW
BE�B:^B"�B�B�B �B-B49B?}B?}B;dB1'B�B�TB��B��B�-B�B��B��B��B�bB� Bk�B[#BJ�B=qB33BuBB
��B
�sB
��B
�jB
�?B
��B
��B
�B
|�B
n�B
dZB
XB
F�B
6FB
$�B
�B
%B	��B	�#B	��B	�qB	��B	��B	�\B	�B	|�B	q�B	hsB	^5B	K�B	:^B	 �B	hB	
=B	B��B�B�HB�B��BɺBƨB�3B�B��B��B��B��B�bB�PB�DB�DB�PB�=B�%B�B�B�B� B}�By�Bx�Bv�Bo�Bn�Bl�BffBdZBbNBbNBbNB^5B]/BZBT�BT�BS�BP�BP�BN�BM�BK�BI�BH�BF�BD�BC�BC�BA�B=qB<jB;dB:^B9XB7LB7LB:^B9XB;dB:^B;dB;dB<jB=qB=qB;dB8RB5?B5?B5?B49B33B5?B49B49B0!B/B,B,B/B-B-B.B33B49B33B1'B2-B9XB9XB:^B>wB?}B?}B>wB>wB>wB?}BA�B?}B@�B@�B?}B?}BB�BA�BB�BC�BE�BI�BK�BJ�BM�BR�BR�BS�BW
BW
BXBYB[#B\)B^5B`BB`BBbNBdZBhsBjBiyBk�Bl�Bp�Br�Br�Br�Br�Bt�Bu�Bz�B{�B� B�B�7B�JB�hB�oB�hB�uB��B��B��B��B��B��B��B��B��B��B�B�B�3B�XB�RB�^B�wBŢBŢBǮB��B��B��B��B�B�B�B�B�#B�/B�NB�ZB�NB�NB�NB�ZB�fB�B��B��B��B��B	B	B	B	B	%B	1B		7B	DB	JB	PB	VB	bB	�B	�B	�B	!�B	#�B	%�B	%�B	&�B	)�B	,B	-B	.B	/B	1'B	49B	7LB	9XB	=qB	A�B	C�B	G�B	H�B	J�B	N�B	Q�B	T�B	XB	ZB	[#B	\)B	]/B	^5B	aHB	e`B	ffB	gmB	gmB	hsB	iyB	m�B	p�B	q�B	q�B	t�B	u�B	y�B	{�B	}�B	~�B	�B	�B	�B	�7B	�DB	�JB	�VB	�bB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�3B	�9B	�?B	�FB	�LB	�XB	�dB	�qB	�wB	�}B	��B	��B	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
%B
%B
%B
B
%B
%B
%B
+B
+B
+B
+B
1B
1B
	7B

=B

=B
DB
DB
JB
JB
PB
VB
VB
VB
VB
VB
\B
\B
bB
bB
hB
hB
oB
uB
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
�B
�B
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
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
 �B
 �B
 �B
!�B
$�B
$�B
%�B
&�B
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
,B
,B
,B
,B
,B
-B
-B
.B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
1'B
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
5?B
6FB
6FB
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
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
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
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
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
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
F�B
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
K�B
K�B
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
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
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
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
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
[#B
[#B
[#B
[#B
[#B
[#B
\)B
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
_;B
_;B
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
cTB
cTB
cTB
cTB
dZB
dZB
cTB
dZB
dZB
dZB
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
o�B
o�B
p�B
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
r�B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B��B�B��B��B��B B�B>BBC-B9XB:^BB�BI�BK�BTB^B^5B_!B^5B]/B]B^BaBbBbBbBc Bc Bc Bc Bc Bc:Bc:BcTBc�Bd&BdZBeBo�Bf�Ba�Bb�BbNBZ�BO�BR BT{BU�BNBF�B<�BABV�B[#BZ�BG�B=�B$ZB�B	B \B,�B4nB@�B@�B<�B49B �B�B��B�B�3B�B�NB�	B��B��B��Bm�B]/BL0B?cB6�B�B�B
�8B
�QB
�mB
��B
��B
��B
��B
��B
~�B
p!B
f2B
Z�B
IB
8�B
'B
�B
	�B	��B	��B	�VB	�B	�B	��B	�B	��B	~�B	shB	jB	aB	O\B	?}B	"�B	�B	0B	�B��B�B�TBٚBΥBˬB�#B�TB��B��B�&B��B�QB�NB��B��B��B�VB�^B��B�gB��B�uB�;B~wBzxBz*BxBp�Bp!Bm�Bg8Bd�Bc Bc�Bb�B_B_;BZ�BU�BU�BT�BQ�BQ�BO�BN�BL0BJrBI�BH1BF?BD�BD�BB'B=�B<�B;�B:�B9�B7�B8RB;�B;0B<�B:�B<jB<jB=<B>�B>wB<�B9�B6+B5�B5�B5B4B5�B4�B5�B1�B0!B,�B-B0B-�B-�B.}B3�B4�B3�B1�B2�B9�B9�B:�B>�B@�B?�B>�B?}B?�B@�BBAB@ BA;BA B@B@BB�BA�BC-BDgBFtBJXBLJBL0BOBS&BS@BTaBW
BW?BX_BYeB[�B\�B^�B`BB`vBb�Bd�BhsBjBi�Bk�Bl�Bp�Bs3Bs3Br�Br�Bu%BvFB{�B{�B�B��B��B��B��B��B�B��B�yB��B�B�pB�B��B��B��B��B��B��B�iB��B��B�8B��B��B��B��B��B��B͟B��B�2B�EB��B��B�B�=BݘB�B�@B�hB��B�4B�@B�LB��B��B��B�B��B	 �B	 �B	'B	B	�B	�B		B	DB	0B	PB	VB	HB	�B	�B	�B	!�B	#�B	%�B	%�B	&�B	)�B	,B	,�B	-�B	/ B	1B	4B	72B	9XB	=VB	AoB	C{B	GzB	H�B	J�B	N�B	Q�B	T�B	W�B	Y�B	Z�B	[�B	\�B	^B	a-B	e,B	f2B	gB	gB	hXB	i_B	mwB	pUB	qvB	q�B	t�B	u�B	y�B	{�B	}�B	~�B	��B	��B	�B	�B	�)B	�B	�<B	�HB	�:B	�FB	�FB	�2B	�9B	�YB	�yB	�kB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�>B	�0B	�<B	�(B	�.B	�iB	�oB	�aB	�MB	�MB	�mB	�mB	�YB	ǔB	ȚB	ʌB	�~B	�~B	͟B	ΥB	ϫB	ϫB	бB	ѷB	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�	B	��B	�B	�B	��B	�B	�!B	�B	�B	�B	�B	� B	�&B	�B	�&B	�2B	�B	�B	�2B	�RB	�$B	�*B	�DB	�*B	�0B	�0B	�0B	�QB	�QB	�QB	�=B	�WB	�CB	�}B	�cB	�iB	�iB	�iB	�oB	��B	�vB	�vB	�B	�|B	�hB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
�B
B
�B
�B
	B

	B

#B
)B
B
�B
B
B
B
"B
B
"B
<B
B
(B
.B
HB
B
NB
:B
@B
[B
@B
,B
FB
,B
gB
gB
2B
SB
9B
sB
yB
eB
kB
WB
qB
WB
WB
WB
�B
xB
xB
]B
xB
xB
dB
IB
jB
�B
�B
�B
�B
�B
�B
pB
�B
 �B
 �B
 �B
 �B
 �B
 vB
!�B
 �B
 �B
 vB
!|B
$�B
$�B
%�B
&�B
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
+�B
+�B
+�B
+�B
+�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
/ B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
2�B
4B
4B
3�B
5B
4�B
5�B
5�B
4�B
4�B
5B
5B
6B
5�B
6B
6B
6�B
72B
7B
5�B
6B
5�B
6B
6�B
7B
8B
9$B
:*B
:*B
:B
:*B
:*B
;B
;0B
;0B
;0B
<6B
<6B
=<B
=<B
=<B
="B
=<B
>BB
>BB
>]B
?cB
?.B
?B
?.B
?HB
@4B
@4B
@B
@4B
@B
@OB
@4B
@OB
@4B
@OB
AUB
AUB
A;B
BAB
BAB
BAB
B[B
B[B
BAB
B[B
BAB
B[B
CGB
C{B
DgB
DgB
DgB
DMB
EmB
EmB
EmB
EmB
ESB
FYB
FtB
FtB
FYB
FYB
FtB
GzB
FtB
G_B
G�B
H�B
H�B
H�B
HfB
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
KxB
KxB
KxB
L�B
L�B
L~B
L~B
LdB
LdB
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
O�B
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
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
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
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
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
\�B
\�B
\�B
]�B
]�B
]�B
^B
^B
]�B
]�B
]�B
]�B
^B
]�B
]�B
]�B
]�B
]�B
^B
^B
]�B
]�B
^B
^B
]�B
]�B
]�B
_B
_B
^�B
^�B
_B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
aB
`�B
aB
aB
`�B
a�B
a�B
a�B
cB
c B
cB
b�B
dB
dB
c B
d&B
d&B
dB
e,B
eB
e,B
e,B
eB
f2B
e�B
f2B
fB
fB
fLB
g8B
gB
gB
h>B
h$B
h>B
h$B
h>B
i*B
i*B
iDB
i*B
iDB
jKB
jKB
jeB
kkB
kQB
k6B
k6B
kQB
lWB
l=B
mCB
m]B
m)B
m]B
m)B
m]B
nIB
nIB
n/B
ncB
oOB
oiB
p�B
q[B
qvB
q[B
qvB
qvB
q[B
raB
rGB
raB
r|B
r|B
raB
raB
raB
raB
ra1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.68(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202012090036162020120900361620201209003616202306231726212023062317262120230623172621202012100028092020121000280920201210002809  JA  ARFMdecpA19c                                                                20201204154030  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20201204064035  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20201204064037  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20201204064037  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20201204064038  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20201204064038  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20201204064038  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20201204064038  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20201204064038  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201204064038                      G�O�G�O�G�O�                JA  ARUP                                                                        20201204065216                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20201204153639  CV  JULD            G�O�G�O�F�b&                JM  ARCAJMQC2.0                                                                 20201208153616  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20201208153616  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20201209152809  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082621  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041504                      G�O�G�O�G�O�                