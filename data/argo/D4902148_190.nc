CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-10-30T15:37:58Z creation;2019-10-30T15:38:03Z conversion to V3.1;2022-11-21T05:28:06Z update;     
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
_FillValue                 �  ]d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ߬   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191030153758  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_190                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @���> 1   @�� y\� @;���oiD�dd�Q�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @1G�@�=q@�=qA�A!�AA�Aa�A��\A��\A��\A��\A��\AЏ\A��\A��\B G�BG�BG�BG�B G�B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`G�BhG�BpG�BxG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\+�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�=D�?
D�
D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D��
D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�qD�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D=D��=D�=D�B=DÂ=D��=D�=D�B=DĂ=D��=D�=D�B=Dł=D��=D�=D�B=DƂ=D��=D�=D�B=Dǂ=D��=D�=D�B=DȂ=D��=D�=D�B=Dɂ=D��=D�=D�B=Dʂ=D��=D�=D�B=D˂=D��=D�=D�B=D̂=D��=D�=D�B=D͂=D��=D�=D�B=D΂=D��=D�=D�B=Dς=D��=D�=D�B=DЂ=D��=D�=D�B=Dт=D��=D�=D�B=D҂=D��=D�=D�B=Dӂ=D��=D�=D�B=DԂ=D��=D�=D�B=DՂ=D��=D�=D�B=Dւ=D��=D�=D�B=Dׂ=D��=D�=D�B=D؂=D��=D�=D�B=Dق=D��=D�=D�B=Dڂ=D��=D�=D�B=Dۂ=D��=D�=D�B=D܂=D��=D�=D�B=D݂=D��=D�=D�B=Dނ=D��=D�=D�B=D߂=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�qD�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�H�D�h�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�(�A�(�A�+A�-A�-A�/A�1'A�1'A�1'A�33A�5?A�5?A�5?A�5?A�5?A�7LA�7LA�33A�33A�1'A�33A�/A�+A��A�{A��A͇+A�  A��uA�C�A�ĜA�A�A�A�A�S�A��A�&�A�ȴA���A���A�
=A��9A��
A�E�A�bA��A��A�5?A�=qA�O�A��
A�^5A�A��`A��A��-A�A��DA�C�A�A�A�l�A�$�A��wA��A��+A�ZA�XA�jA��A��A��hA�33A��-A��7A�ZA�7LA�/A�$�A��A�{A��A���A���A�ffA�/A��yA�t�A���A�5?A��A�  A��AS�A}�TA|�A{�hAz^5Ay�Ax�AwO�AvM�Av-Av$�Av(�Av$�Av �Av�Au��Au
=AtM�ApȴAn�An �Al��Ak�
AiK�Agx�Ae�Ac��Ab��AbbNAa��Aa�A`�A_VA^�+A^1'A]��A\��A[\)AZ�AY�AXffAWhsAV�/AV�AT�AT�DASdZAR��AQ�AQ|�APQ�AO�FANĜAM�AL�9AK�wAJffAIl�AH9XAG�#AGXAFȴAF5?AE��AE|�AE�AD�\AD1ACoAA��AA��A@�yA@bA?ƨA?�wA?A=�-A<��A;�wA:�`A9K�A7�TA6�!A6ZA5A4^5A3/A1��A0��A/l�A.�+A.5?A.A,�HA+�7A*jA)��A)G�A(�uA'ƨA&��A&A%�^A%�A$v�A$M�A#�TA#�FA#S�A"��A"�A"z�A!�7A v�A A�A�mA��A�HAE�A�^A�A��A�AXA��A=qA�wA�AdZA33A��Ar�A�\A�mAdZA�AȴAZA�-A33A�!AM�A�AG�A9XA��AA�AXA�9A�A
�RA-AbNA-A1A�+AoA7LA ��A ��A �+A ^5@�
=@�7L@�ƨ@���@���@�\)@�v�@��@�@�x�@�hs@�G�@��@�
=@�&�@��@�ȴ@�V@�%@�1'@�C�@�h@�Ĝ@�Ĝ@�j@�  @��@��@䛦@�\)@�E�@�ff@�%@�Z@�ƨ@ۅ@���@�$�@�V@�X@��/@�j@�1'@�b@Ӿw@ѡ�@���@�@�Ĝ@���@�@�ff@�E�@ɩ�@�X@�&�@���@ŉ7@�p�@�X@�V@�33@��@�33@�V@�(�@���@���@��w@���@���@�K�@��@��w@�+@�M�@�O�@�j@��@���@���@�5?@�`B@��@��@��@�^5@�7L@�;d@���@��#@��/@�bN@�dZ@��H@��\@�J@���@�X@�/@���@�  @��F@�l�@��@�@�-@�`B@��@��F@�\)@���@�J@�O�@�/@�%@��`@�Ĝ@�bN@���@�|�@���@��!@�v�@�p�@��@�t�@�@���@�@��7@�x�@�%@�9X@���@�C�@��@���@�-@���@�x�@�X@�?}@�%@���@�|�@�K�@��@���@��+@��@��@���@�hs@��@���@��D@�(�@�ƨ@��@�K�@��H@��\@�M�@��^@�G�@�?}@�/@��@��/@��9@��D@�Z@��
@��@�S�@�
=@�ff@��@��^@��@�O�@��@��@��/@��/@���@��u@�z�@�1'@��
@�K�@��@��!@��\@�M�@�J@��@��@�O�@���@�Ĝ@�z�@�I�@�1'@�b@��@~5?@}/@}V@|�/@|(�@{dZ@z�\@y�^@y�7@x�`@xbN@x �@w+@u��@u��@u�h@up�@uO�@t��@t��@t�j@t�D@tz�@tj@sdZ@so@r��@q��@p�`@p�u@pQ�@pA�@pA�@pA�@p1'@p �@p �@pb@pb@o�@o�w@o|�@nȴ@n��@nff@nV@nE�@n{@m��@m�h@mO�@m?}@m?}@l��@l�j@l�D@k��@kC�@k"�@j�H@j�\@j�\@j~�@jn�@j=q@i�7@i7L@h��@h �@g�w@g��@g��@gl�@f�y@f{@e`B@d�D@dZ@d1@cƨ@c��@c33@b�!@b^5@bJ@a�^@a�7@aX@a%@`�@`A�@`b@`  @_�;@_�P@_;d@^�y@^�+@^5?@]��@]��@]V@\Z@\(�@[�
@[�@[C�@Z�H@Z�@X��@X��@XQ�@XA�@W��@WK�@V��@V�y@V�@Vȴ@V$�@U`B@U/@UV@T�@T9X@T�@S�m@St�@R�H@R�!@R^5@RJ@Q�#@Q�^@QG�@PĜ@PbN@Pb@O�@O�;@O��@O�@O��@Ol�@Nv�@M�T@M�h@M�@MO�@MV@L��@LI�@L1@L�@K��@L�@L�@K�
@K��@Kt�@KC�@K33@K@J�!@Jn�@Ihs@I&�@HbN@H �@G�@G�w@Gl�@F�R@F�@F��@FE�@E�@E�-@E�@E�@Ep�@E?}@D�j@DZ@Cƨ@C33@Co@B�H@B��@B�@A�^@A7L@@�`@@Ĝ@@Ĝ@@Ĝ@@�9@@�u@@�@@A�@@b@?�@?K�@>ȴ@>�y@>��@>ff@>V@=�T@=�@=`B@=?}@<�D@;��@;��@;t�@;"�@:�@:��@:~�@:=q@9�#@9x�@9%@8��@8bN@81'@8 �@7�;@7�@7|�@7�@6�R@6��@65?@5@5?}@4��@4��@4�j@4��@4�D@4j@4I�@3t�@333@3@2n�@1�#@1�^@1�^@1��@1�7@1%@0Ĝ@0r�@0 �@0  @/�@/�P@/;d@.ȴ@.�+@.ff@.V@.E�@.$�@-��@-?}@-/@-V@,�j@,9X@,�@+ƨ@+t�@+33@*��@*�!@*^5@*=q@*J@)�#@)G�@(�`@(Ĝ@(�@(1'@'�;@'�w@'\)@'�@'
=@&��@&��@&ȴ@&$�@%�-@%?}@$�@$z�@$Z@$I�@$Z@$I�@$9X@$�@#��@#�m@#�
@#ƨ@#�F@#�F@#�F@#��@#��@#�@#S�@#33@"��@"�@!�#@!��@!�7@!hs@!7L@ ��@ �9@ ��@ ��@ �@ �@ �@ bN@ Q�@ 1'@   @�@��@�w@�P@+@
=@�@��@ff@5?@$�@$�@$�@{@��@��@�D@j@I�@�m@�@dZ@33@"�@@@@�@��@�\@^5@�@J@J@��@��@��@hs@7L@%@Ĝ@��@�@bN@Q�@1'@b@�;@\)@�@
=@�R@v�@V@5?@{@�@�@@p�@`B@�@�/@��@�D@Z@9X@1@�m@ƨ@��@�@dZ@33@o@�@�H@��@~�@^5@M�@�@�@�#@��@��@�7@X@7L@��@��@Ĝ@�@bN@  @�w@|�@\)@;d@�y@�@ȴ@ȴ@ȴ@�+@V@@��@�-@�h@O�@�@��@��@�@�/@��@�j@��@z�@I�@��@�F@��@��@��@�@t�@dZ@S�@C�@o@@
��@
�\@
~�@
M�@
=q@	�@	��@	��@	�7@	�7@	hs@	G�@	%@��@�@A�@b@�w@�P@|�@l�@;d@+@�@
=@�@��@ff@�@��@�@`B@/@�@��@�D@�D@�D@�D@z�@I�@�m@ƨ@��@t�@S�@"�@��@�!@��@n�@n�@n�@n�@-@J@�@�#@�^@��@X@�@ �`@ �`@ ��@ Ĝ@ �9@ �u@ �@ bN@ Q�@  �@ b?��;?��w?��w1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�(�A�(�A�+A�-A�-A�/A�1'A�1'A�1'A�33A�5?A�5?A�5?A�5?A�5?A�7LA�7LA�33A�33A�1'A�33A�/A�+A��A�{A��A͇+A�  A��uA�C�A�ĜA�A�A�A�A�S�A��A�&�A�ȴA���A���A�
=A��9A��
A�E�A�bA��A��A�5?A�=qA�O�A��
A�^5A�A��`A��A��-A�A��DA�C�A�A�A�l�A�$�A��wA��A��+A�ZA�XA�jA��A��A��hA�33A��-A��7A�ZA�7LA�/A�$�A��A�{A��A���A���A�ffA�/A��yA�t�A���A�5?A��A�  A��AS�A}�TA|�A{�hAz^5Ay�Ax�AwO�AvM�Av-Av$�Av(�Av$�Av �Av�Au��Au
=AtM�ApȴAn�An �Al��Ak�
AiK�Agx�Ae�Ac��Ab��AbbNAa��Aa�A`�A_VA^�+A^1'A]��A\��A[\)AZ�AY�AXffAWhsAV�/AV�AT�AT�DASdZAR��AQ�AQ|�APQ�AO�FANĜAM�AL�9AK�wAJffAIl�AH9XAG�#AGXAFȴAF5?AE��AE|�AE�AD�\AD1ACoAA��AA��A@�yA@bA?ƨA?�wA?A=�-A<��A;�wA:�`A9K�A7�TA6�!A6ZA5A4^5A3/A1��A0��A/l�A.�+A.5?A.A,�HA+�7A*jA)��A)G�A(�uA'ƨA&��A&A%�^A%�A$v�A$M�A#�TA#�FA#S�A"��A"�A"z�A!�7A v�A A�A�mA��A�HAE�A�^A�A��A�AXA��A=qA�wA�AdZA33A��Ar�A�\A�mAdZA�AȴAZA�-A33A�!AM�A�AG�A9XA��AA�AXA�9A�A
�RA-AbNA-A1A�+AoA7LA ��A ��A �+A ^5@�
=@�7L@�ƨ@���@���@�\)@�v�@��@�@�x�@�hs@�G�@��@�
=@�&�@��@�ȴ@�V@�%@�1'@�C�@�h@�Ĝ@�Ĝ@�j@�  @��@��@䛦@�\)@�E�@�ff@�%@�Z@�ƨ@ۅ@���@�$�@�V@�X@��/@�j@�1'@�b@Ӿw@ѡ�@���@�@�Ĝ@���@�@�ff@�E�@ɩ�@�X@�&�@���@ŉ7@�p�@�X@�V@�33@��@�33@�V@�(�@���@���@��w@���@���@�K�@��@��w@�+@�M�@�O�@�j@��@���@���@�5?@�`B@��@��@��@�^5@�7L@�;d@���@��#@��/@�bN@�dZ@��H@��\@�J@���@�X@�/@���@�  @��F@�l�@��@�@�-@�`B@��@��F@�\)@���@�J@�O�@�/@�%@��`@�Ĝ@�bN@���@�|�@���@��!@�v�@�p�@��@�t�@�@���@�@��7@�x�@�%@�9X@���@�C�@��@���@�-@���@�x�@�X@�?}@�%@���@�|�@�K�@��@���@��+@��@��@���@�hs@��@���@��D@�(�@�ƨ@��@�K�@��H@��\@�M�@��^@�G�@�?}@�/@��@��/@��9@��D@�Z@��
@��@�S�@�
=@�ff@��@��^@��@�O�@��@��@��/@��/@���@��u@�z�@�1'@��
@�K�@��@��!@��\@�M�@�J@��@��@�O�@���@�Ĝ@�z�@�I�@�1'@�b@��@~5?@}/@}V@|�/@|(�@{dZ@z�\@y�^@y�7@x�`@xbN@x �@w+@u��@u��@u�h@up�@uO�@t��@t��@t�j@t�D@tz�@tj@sdZ@so@r��@q��@p�`@p�u@pQ�@pA�@pA�@pA�@p1'@p �@p �@pb@pb@o�@o�w@o|�@nȴ@n��@nff@nV@nE�@n{@m��@m�h@mO�@m?}@m?}@l��@l�j@l�D@k��@kC�@k"�@j�H@j�\@j�\@j~�@jn�@j=q@i�7@i7L@h��@h �@g�w@g��@g��@gl�@f�y@f{@e`B@d�D@dZ@d1@cƨ@c��@c33@b�!@b^5@bJ@a�^@a�7@aX@a%@`�@`A�@`b@`  @_�;@_�P@_;d@^�y@^�+@^5?@]��@]��@]V@\Z@\(�@[�
@[�@[C�@Z�H@Z�@X��@X��@XQ�@XA�@W��@WK�@V��@V�y@V�@Vȴ@V$�@U`B@U/@UV@T�@T9X@T�@S�m@St�@R�H@R�!@R^5@RJ@Q�#@Q�^@QG�@PĜ@PbN@Pb@O�@O�;@O��@O�@O��@Ol�@Nv�@M�T@M�h@M�@MO�@MV@L��@LI�@L1@L�@K��@L�@L�@K�
@K��@Kt�@KC�@K33@K@J�!@Jn�@Ihs@I&�@HbN@H �@G�@G�w@Gl�@F�R@F�@F��@FE�@E�@E�-@E�@E�@Ep�@E?}@D�j@DZ@Cƨ@C33@Co@B�H@B��@B�@A�^@A7L@@�`@@Ĝ@@Ĝ@@Ĝ@@�9@@�u@@�@@A�@@b@?�@?K�@>ȴ@>�y@>��@>ff@>V@=�T@=�@=`B@=?}@<�D@;��@;��@;t�@;"�@:�@:��@:~�@:=q@9�#@9x�@9%@8��@8bN@81'@8 �@7�;@7�@7|�@7�@6�R@6��@65?@5@5?}@4��@4��@4�j@4��@4�D@4j@4I�@3t�@333@3@2n�@1�#@1�^@1�^@1��@1�7@1%@0Ĝ@0r�@0 �@0  @/�@/�P@/;d@.ȴ@.�+@.ff@.V@.E�@.$�@-��@-?}@-/@-V@,�j@,9X@,�@+ƨ@+t�@+33@*��@*�!@*^5@*=q@*J@)�#@)G�@(�`@(Ĝ@(�@(1'@'�;@'�w@'\)@'�@'
=@&��@&��@&ȴ@&$�@%�-@%?}@$�@$z�@$Z@$I�@$Z@$I�@$9X@$�@#��@#�m@#�
@#ƨ@#�F@#�F@#�F@#��@#��@#�@#S�@#33@"��@"�@!�#@!��@!�7@!hs@!7L@ ��@ �9@ ��@ ��@ �@ �@ �@ bN@ Q�@ 1'@   @�@��@�w@�P@+@
=@�@��@ff@5?@$�@$�@$�@{@��@��@�D@j@I�@�m@�@dZ@33@"�@@@@�@��@�\@^5@�@J@J@��@��@��@hs@7L@%@Ĝ@��@�@bN@Q�@1'@b@�;@\)@�@
=@�R@v�@V@5?@{@�@�@@p�@`B@�@�/@��@�D@Z@9X@1@�m@ƨ@��@�@dZ@33@o@�@�H@��@~�@^5@M�@�@�@�#@��@��@�7@X@7L@��@��@Ĝ@�@bN@  @�w@|�@\)@;d@�y@�@ȴ@ȴ@ȴ@�+@V@@��@�-@�h@O�@�@��@��@�@�/@��@�j@��@z�@I�@��@�F@��@��@��@�@t�@dZ@S�@C�@o@@
��@
�\@
~�@
M�@
=q@	�@	��@	��@	�7@	�7@	hs@	G�@	%@��@�@A�@b@�w@�P@|�@l�@;d@+@�@
=@�@��@ff@�@��@�@`B@/@�@��@�D@�D@�D@�D@z�@I�@�m@ƨ@��@t�@S�@"�@��@�!@��@n�@n�@n�@n�@-@J@�@�#@�^@��@X@�@ �`@ �`@ ��@ Ĝ@ �9@ �u@ �@ bN@ Q�@  �@ b?��;?��w?��w1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�`B�/B��B��B�9B��B��B��B��B�oB�bB�PB�1B�B{�Bw�Bo�BdZB]/BVBH�B@�B;dB6FB&�B�BVB��B��B�B�B�/B��B��BȴB�}B�FB��B��B�By�Br�BdZBR�BL�BI�BG�BE�BD�BC�B@�B33B+B�B
��B
�yB
�B
��B
ȴB
�jB
�3B
��B
��B
��B
�7B
}�B
u�B
m�B
dZB
aHB
XB
N�B
F�B
E�B
E�B
E�B
D�B
D�B
C�B
?}B
:^B
33B
�B
hB
DB
B	��B	�B	�BB	��B	��B	ƨB	ÖB	�}B	�jB	�LB	�-B	�B	�B	��B	��B	��B	�{B	�bB	�7B	�B	�B	~�B	v�B	t�B	m�B	iyB	gmB	iyB	cTB	_;B	YB	R�B	N�B	H�B	C�B	<jB	6FB	33B	0!B	-B	(�B	&�B	$�B	"�B	 �B	�B	�B	hB	\B	PB	JB	VB	PB	
=B	B��B��B�B�B�mB�BB�/B�B��B��B��BɺBŢBÖB��B��B�dB�?B�-B�'B�B�B��B��B��B��B��B��B��B�{B�uB�hB�oB�uB�oB�VB�=B�7B�+B�B�B�B~�B|�Bx�Bu�Bs�Br�Bp�Bo�Bp�Bo�Bo�Bp�Bn�BiyBgmBe`BdZBcTBbNB`BB^5B]/B\)BZBXBVBR�BP�BM�BK�BH�BE�B@�B>wB=qB;dB8RB6FB49B33B33B2-B1'B0!B.B-B,B+B)�B)�B(�B(�B(�B(�B'�B&�B%�B$�B$�B$�B#�B"�B"�B!�B!�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B#�B#�B#�B$�B$�B&�B&�B&�B+B.B.B-B-B.B2-B49B8RB9XB:^B:^B:^B9XB9XB9XB9XB<jB=qB>wB@�BB�BG�BH�BH�BI�BJ�BK�BL�BM�BN�BS�BXBXBYB[#B\)B^5B_;BaHBcTBe`BffBffBiyBk�Bn�Bo�Bp�Bp�Bs�Bu�Bx�B|�B~�B�B�B�1B�1B�7B�7B�7B�DB�VB�VB�bB�hB�hB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�3B�9B�9B�9B�9B�?B�FB�FB�LB�RB�dB�wB��BĜBŢBȴB��B��B��B�
B�
B�B�)B�5B�BB�TB�ZB�`B�fB�fB�mB�sB�yB�B�B�B�B��B��B��B	  B	B	B	B	B	B	B	+B	1B	
=B	PB	hB	�B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	&�B	'�B	(�B	)�B	,B	2-B	7LB	7LB	8RB	:^B	>wB	B�B	E�B	F�B	I�B	K�B	M�B	Q�B	T�B	T�B	VB	VB	VB	W
B	XB	XB	XB	XB	XB	[#B	\)B	]/B	bNB	dZB	ffB	gmB	hsB	hsB	k�B	l�B	o�B	s�B	t�B	t�B	v�B	x�B	z�B	~�B	� B	�B	�B	�B	�B	�%B	�7B	�=B	�DB	�JB	�PB	�VB	�VB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�3B	�9B	�?B	�FB	�FB	�FB	�LB	�XB	�^B	�dB	�jB	�qB	�wB	��B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�/B	�5B	�5B	�;B	�HB	�NB	�TB	�ZB	�ZB	�ZB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
1B
	7B
	7B

=B
DB
DB
JB
JB
PB
PB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
hB
uB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
)�B
)�B
+B
,B
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
1'B
1'B
2-B
33B
33B
33B
49B
33B
33B
49B
5?B
5?B
5?B
6FB
7LB
7LB
8RB
8RB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
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
J�B
K�B
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
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
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
XB
XB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
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
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
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
iyB
iyB
iyB
iyB
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
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�<B��B��B��B��B�tB�B�aB�=B��B��B�
B��B�WB�,B��B�B��B�B}�Bz�Bs�Bf�B_�BY�BJ�BA�B=B9	B)yB�B�B �B��B�B�B��B��B��B�XB�;B��B��B�CB�uB{JBt�BgBS�BM6BJ#BG�BE�BEBD�BBuB5B-�B�B
��B
��B
��B
�B
�=B
��B
��B
�DB
�'B
�yB
��B
HB
wLB
n�B
e,B
b�B
Y�B
O�B
F�B
E�B
E�B
E�B
D�B
D�B
DgB
@�B
<B
6�B
�B
�B
�B
�B	��B	� B	��B	ՁB	��B	�EB	āB	�iB	��B	�lB	��B	��B	��B	�*B	��B	�B	��B	��B	�XB	�B	�3B	�4B	w�B	vB	n�B	j�B	g�B	j�B	dZB	`�B	Z�B	TFB	P.B	JXB	D�B	=�B	6�B	3�B	0�B	-�B	)�B	'mB	%`B	#�B	!�B	�B	�B	 B	HB	<B	�B	�B	�B	�B	SB�cB�2B��B�;B��B�B޸B�B՛BѝB�jB�)B��B�3B�'B�B�"B��B�B��B�B�"B�>B��B�;B�~B�kB��B�B��B��B�B��B��B��B�vB��B��B��B�%B��B��B�B}�By�Bv�Bt�Bs�Bq'Bo�Bq'Bp!BpoBq�Bp�BjKBh
Be�Bd�BdBc B`�B^�B]�B\�B[=BYeBW
BT�BRBN�BM6BJ�BH�BBuB?B>(B=�B:^B88B4�B3�B3�B2�B2aB1[B/ B-�B-)B+�B*B*0B)DB)DB)B)DB(�B(>B'B&B%FB%FB$�B#�B#�B"�B"NB!�B �B!|B �B �B�B�B�B�B�B5B5BBOB�B�BOBBBBBpBVB!bB"�B#�B$ZB$tB$@B%B%FB'RB'�B(XB+�B./B.cB-�B.}B/�B3MB5tB8�B9rB:xB:xB:xB9rB9�B:^B:�B=B>(B?.BAUBCaBG�BH�BIBJrBK^BLdBMPBN�BO�BU2BXyBX�BY�B[�B\�B^�B_�Ba�Bc�Be�Bf�Bf�Bi�Bk�Bn�Bo�Bp�Bq[Bt9BvzByrB}VBcB��B��B�fB�KB�RB�lB��B��B�pB��B��B��B� B�mB�B�	B�B�VB��B�B�ZB�zB�LB�DB�0B�QB��B��B�aB�MB�TB��B��B�nB�ZB��B�zB��B��B�B��B��B��B��B�B�"B�&B�2B�?B�YB�kB�xBޞB��B�nB�tB�B�fB�B�B�B��B��B��B��B�3B�B�"B�(B	 4B	 B	GB	B	B	3B	9B	EB	�B	
rB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	'B	(
B	)B	*0B	,qB	2aB	7LB	7fB	8�B	:�B	>�B	B�B	E�B	F�B	I�B	K�B	N<B	R:B	UB	UB	VB	VB	V9B	W$B	X+B	XB	XB	X+B	XyB	[=B	\CB	]~B	b�B	dZB	f�B	g�B	hsB	hsB	k�B	l�B	o�B	s�B	t�B	t�B	v�B	x�B	{B	B	�B	�B	�B	�-B	�9B	�%B	�RB	�XB	�^B	�dB	�PB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�&B	�B	�B	�DB	�B	�)B	�5B	�;B	�UB	�GB	�MB	�TB	�ZB	�`B	�`B	�zB	��B	�rB	�xB	�dB	�jB	��B	��B	��B	ªB	ðB	ĶB	ŢB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	� B	� B	� B	�B	��B	��B	�B	�SB	�YB	�B	�B	�7B	�]B	�IB	�5B	�OB	�pB	�bB	�NB	�TB	�ZB	�ZB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�0B	�B	�"B	�B
  B
  B
;B
AB
B
B
9B
?B
EB
1B
B
KB
1B
fB
	RB
	lB

rB
^B
DB
dB
dB
�B
�B
VB
pB
\B
BB
vB
\B
\B
}B
bB
�B
}B
�B
[B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
'B
'B
&�B
'�B
'�B
(
B
(
B
(
B
($B
*B
*B
+B
,=B
-)B
.B
.B
./B
.IB
/B
/5B
0;B
0!B
0;B
1AB
1[B
2aB
3MB
33B
3MB
49B
33B
3hB
4TB
5?B
5?B
5ZB
6zB
7fB
7LB
8RB
8lB
9XB
:xB
:^B
;dB
;B
;B
;B
<�B
=qB
=�B
=qB
>wB
>�B
>�B
?�B
@iB
@�B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
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
J�B
K�B
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
OB
PB
O�B
Q B
QB
Q B
RB
Q�B
R�B
R�B
R�B
R�B
R�B
SB
SB
S�B
S�B
T�B
T�B
T�B
T�B
T�B
UB
VB
VB
W$B
W$B
W$B
X+B
XB
X+B
X+B
XB
X+B
Y1B
YB
Y1B
Z7B
Z7B
[=B
[=B
[#B
\B
\)B
\)B
]/B
]IB
]IB
^5B
^OB
^5B
_;B
_;B
_;B
_VB
_VB
_;B
`BB
`\B
`\B
`\B
`\B
aHB
abB
aHB
abB
aHB
bhB
bNB
bNB
bhB
bNB
bhB
cTB
cnB
c:B
cTB
cTB
dtB
dtB
dZB
ezB
e`B
ezB
ezB
fLB
ffB
fLB
fLB
ffB
g�B
g�B
g�B
h�B
hsB
h�B
hsB
hsB
i_B
i_B
iyB
i_B
iyB
i�B
i�B
iyB
iyB
jB
jeB
k�B
kkB
k�B
k�B
kkB
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
mwB
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
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��I<%zx<#�
<#�
<%zx<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.07(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201911100035292019111000352920191110003529202211182140522022111821405220221118214052201911110020482019111100204820191111002048  JA  ARFMdecpA19c                                                                20191031003752  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191030153758  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191030153800  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191030153800  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191030153801  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191030153801  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191030153801  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20191030153801  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20191030153801  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191030153801  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20191030153803  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191030153803                      G�O�G�O�G�O�                JA  ARUP                                                                        20191030155400                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20191030153352  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20191030153330  CV  JULD            G�O�G�O�F�@�                JM  ARCAJMQC2.0                                                                 20191109153529  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191109153529  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191110152048  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124052  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                