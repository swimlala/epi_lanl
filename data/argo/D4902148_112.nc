CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-09-10T15:35:40Z creation;2017-09-10T15:35:43Z conversion to V3.1;2019-12-18T07:28:16Z update;2022-11-21T05:32:01Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]\   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ސ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20170910153540  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               pA   JA  I1_0397_112                     2C  Dd}NAVIS_A                         0397                            ARGO 011514                     863 @�% %��1   @�% �>� @;����o�d}Vl�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111z�@o\)@��@��A�
A;�
A[�
A{�
A��A��A��A��A��A޸RA��A��B��B��B��B��B&��B.��B6�\B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�B�z�B�z�B�z�B�z�C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C��C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸D o\D �\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D	o\D	�\D
o\D
��Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D o\D �\D!o\D!�\D"o\D"�\D#o\D#�\D$o\D$�\D%o\D%�\D&o\D&�\D'o\D'�\D(o\D(�\D)o\D)�\D*o\D*�\D+o\D+�\D,o\D,�\D-o\D-�\D.o\D.�\D/o\D/�\D0o\D0�\D1o\D1�\D2o\D2�\D3o\D3�\D4o\D4�\D5o\D5�\D6o\D6�\D7o\D7�\D8o\D8�\D9o\D9�\D:o\D:�\D;o\D;�\D<o\D<�\D=o\D=�\D>o\D>�\D?o\D?�\D@o\D@�\DAo\DA�\DBo\DB�\DCo\DC�\DDo\DD�\DEo\DE�\DFo\DF�\DGo\DG�\DHo\DH�\DIo\DI�\DJo\DJ�\DKo\DK�\DLo\DL�\DMo\DM�\DNo\DN�\DOo\DO�\DPo\DP�\DQo\DQ�\DRo\DR�\DSo\DS�\DTo\DT�\DUo\DU�\DVo\DV�\DWo\DW�\DXo\DX�\DYo\DY�\DZo\DZ�\D[o\D[�\D\o\D\�\D]o\D]�\D^o\D^�\D_o\D_�\D`o\D`�\Dao\Da�\Dbo\Db�\Dco\Dc�\Ddo\Dd�\Deo\De�\Dfo\Df�\Dgo\Dg�\Dho\Dh�\Dio\Di�\Djo\Dj�\Dko\Dk�\Dlo\Dl�\Dmo\Dm�\Dno\Dn�\Doo\Do�\Dpo\Dp�\Dqo\Dq�\Dro\Dr�\Dso\Ds�\Dto\Dt�\Duo\Du�\Dvo\Dv�\Dwo\Dw�\Dxo\Dx�\Dyo\Dy�\Dzo\Dz�\D{o\D{�\D|o\D|�\D}o\D}�\D~o\D~�\Do\D�\D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D·�D���D�7�D�w�D÷�D���D�7�D�w�Dķ�D���D�7�D�w�Dŷ�D���D�7�D�w�DƷ�D���D�7�D�w�DǷ�D���D�7�D�w�Dȷ�D���D�7�D�w�Dɷ�D���D�7�D�w�Dʷ�D���D�7�D�w�D˷�D���D�7�D�w�D̷�D���D�7�D�w�Dͷ�D���D�7�D�w�Dη�D���D�7�D�w�DϷ�D���D�7�D�w�Dз�D���D�7�D�w�Dѷ�D���D�7�D�w�Dҷ�D���D�7�D�w�Dӷ�D���D�7�D�w�DԷ�D���D�7�D�w�Dշ�D���D�7�D�w�Dַ�D���D�7�D�w�D׷�D���D�7�D�w�Dط�D���D�7�D�w�Dٷ�D���D�7�D�w�Dڷ�D���D�7�D�w�D۷�D���D�7�D�w�Dܷ�D���D�7�D�w�Dݷ�D���D�7�D�w�D޷�D���D�7�D�w�D߷�D���D�7�D�w�D෮D���D�7�D�w�DᷮD���D�7�D�w�DⷮD���D�7�D�w�D㷮D���D�7�D�w�D䷮D���D�7�D�w�D差D���D�7�D�w�D淮D���D�7�D�w�D緮D���D�7�D�w�D跮D���D�7�D�w�D鷮D���D�7�D�w�D귮D���D�7�D�w�D뷮D���D�7�D�w�D췮D���D�7�D�w�D���D���D�7�D�w�DD���D�7�D�w�D﷮D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�z�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�#A�hsA�v�A�z�A�z�A�z�A�|�A�|�A�~�A�~�A�~�A�~�A�~�A݁A݁A݃A�33A��TA�=qAڧ�A�33A�|�A�VA�O�Aɕ�A��A��`AđhA���A�ĜA�VA��A�7LA��#A�S�A�&�A��A�S�A�A�n�A�G�A��A��FA�jA��wA�x�A�5?A��9A��A�ȴA��^A�9XA�A�p�A�"�A��7A�/A�5?A�v�A��DA�%A��jA�l�A��yA��A��HA�A�A���A���A�bNA�\)A��hA��A��/A��#A�1A�JA�G�A��!A��hA��A���A��uA���A��A���A��-A��hA�C�A���A�A�A��jA��A�-A�?}A��\A�"�A��A�|�A�$�A�K�A��A�XA�
=A� �A���AO�A~z�Ay�FAvbAuXAt�yAt1As�ArZAq;dAo��Am
=Ak33Ajv�Ai��Ai&�AhI�Ae�AdAc��Ab�RAa?}A^��A]��A]G�A]
=A\�jA\AZ�AZ(�AY�
AY��AYdZAX��AX~�AW�AW?}AV�AVbNAV�AU��AU�AR�AQ��AQ�APZAP5?AO�
AO��AO�hAOS�AN��ANȴANjAM�FAMdZAL$�AKAK��AKG�AJv�AJ^5AJ�DAJ=qAI;dAHȴAG�TAFn�AE��AD$�ABbA@��A@r�A@1A?��A>jA=�A<��A;�-A:�+A9��A8�yA8VA8I�A8JA7A6�A6A�A533A41A3p�A2��A2ZA0�/A0jA0$�A/A/O�A-�hA,�A+�FA*jA*$�A)�
A)l�A(��A(ZA(1'A(1'A(JA'��A'�A'%A&��A&E�A&A%�A$��A$z�A#��A#S�A"z�A!S�A �9A�TA�`A(�A��A/A�mA��AbNAx�AI�A1'Ap�A�jA�mA��A�AE�A�PA�A�jA��AI�A��A
�9A	�#A	��A	l�A	;dA�`A�+A?}AƨAdZA�-A�mA ffA   @�G�@�Z@�t�@���@��F@�~�@��@��T@���@���@�ȴ@�/@�@��@��#@�u@�@�V@��@�;d@�J@��u@��@�l�@�~�@�E�@���@��@���@� �@��@�ƨ@�33@�Ĝ@��@�v�@Гu@�1@�;d@˅@�&�@�bN@�1@��;@�t�@Ɵ�@ũ�@�A�@¸R@�z�@�|�@�S�@�@��@���@��\@�^5@�5?@��@�hs@�Ĝ@��w@��@��@���@��P@��y@���@��R@��!@��!@�-@���@�7L@���@�9X@�K�@���@�-@�&�@�ƨ@��R@�$�@��T@��-@�G�@���@��F@�+@��@��h@�;d@�
=@��H@���@�ff@��T@�`B@��j@�\)@���@�"�@�$�@�?}@��@��y@�ff@��@�hs@�G�@�V@���@�bN@�1'@��
@�dZ@��@��@���@�~�@�ff@�J@�?}@�j@��F@��`@��9@�Z@�I�@� �@�1'@�1'@�1@�33@�^5@��#@���@���@���@��^@��-@��^@���@�ȴ@�^5@��#@�O�@�G�@��@���@�$�@�{@�p�@���@�b@��F@�|�@��@�-@�^5@�@�&�@��@��w@��P@�dZ@�+@�@�ȴ@��\@�n�@�ff@�V@�@�hs@��/@�A�@�  @���@���@��F@��F@���@���@�$�@���@��@�7L@�Ĝ@��D@�r�@�(�@�;@~�R@}��@}�@|��@|�@|Z@{��@z��@y��@xĜ@w�@v�@vȴ@v��@u��@t�@t�j@t��@t�D@t9X@r�H@p��@p��@pbN@pQ�@o�@o;d@o
=@o
=@o�@o�@o�@n�@nE�@m�@m�h@mp�@m`B@mO�@m?}@m?}@m�@l�D@kt�@k33@k"�@k"�@k"�@k@j�H@j�\@j=q@j�@i�@ihs@h��@g�P@g|�@g�@h  @hA�@hA�@g|�@gl�@g\)@f�y@f��@f�+@fV@f@e��@e�-@cS�@a��@a��@a��@a�7@a�7@aX@_�@^5?@^@]�T@]@]�h@]`B@]`B@]?}@]?}@]/@]�@\��@\�@\�/@\�@\�D@\�@[dZ@Z�H@Z~�@ZJ@YG�@X��@X��@XA�@W�@W;d@U�h@T��@Tz�@Tz�@Tz�@Tz�@Tj@TZ@S��@S33@S"�@So@R��@R�!@R~�@Q�@Q��@Q�7@Q7L@Q�@P��@Pb@O�w@N��@N$�@M?}@L�/@L�j@L�@K�F@KC�@Ko@J��@J�\@J�\@Jn�@JM�@J=q@JJ@I��@Ix�@I%@H�u@HQ�@Hb@G�;@G��@G��@G�w@G��@GK�@G�@F�y@F�@Fȴ@F�R@F��@F�+@F�+@Fv�@FE�@E�T@E@E��@E`B@E�@D�@D�/@D�D@DZ@C�m@Ct�@C33@B��@A��@A��@A�^@Ahs@@Ĝ@@�@@bN@@A�@@ �@@ �@@  @@  @@ �@@1'@@ �@@b@@b@@b@?�@?�;@?�;@?�@?�@?�@?�@?�P@?|�@?l�@?l�@?\)@?K�@?�@>ȴ@>v�@=��@<��@<��@;�F@:-@9��@9G�@8��@8��@8r�@8Q�@8A�@81'@8  @7�w@7�P@6�@6$�@4Z@3��@3��@3�
@3�
@3ƨ@3�F@3ƨ@3ƨ@3ƨ@3�
@3�F@3��@3�@3C�@3"�@3@2�\@1��@1��@1��@1x�@1hs@1X@1�@0Ĝ@0r�@0Q�@0b@/��@/|�@/K�@/�@/
=@.��@.��@.�y@.�y@.�@.ȴ@.�+@.ff@.E�@.$�@.{@-��@-�h@-?}@,�D@+��@+��@*�H@*M�@*-@*J@)�#@)��@)�7@)X@(�@( �@'�w@'��@'�P@'\)@'K�@&ȴ@&�+@&5?@%@%p�@%/@%�@$�/@$�j@$z�@#�m@#@"��@"��@"^5@"-@"�@"�@"�@"J@!��@!�#@!��@!��@!hs@!%@ �9@ �@ bN@ A�@��@�P@K�@�@��@��@V@�T@?}@��@��@�D@�D@z�@z�@z�@j@j@�@�F@t�@"�@�!@~�@M�@M�@=q@=q@-@�@�@�@�@�7@X@�@��@��@��@�9@��@r�@A�@A�@1'@1'@ �@ �@  @�;@��@�P@\)@K�@+@
=@�@�@ȴ@�R@��@v�@{@��@�h@�@p�@?}@��@j@9X@�@1@�@��@��@��@��@��@��@o@@�H@�H@��@��@�\@^5@J@�@�#@�^@��@�@��@�`@��@�9@�@r�@bN@bN@Q�@1'@ �@�;@�@�P@|�@l�@l�@\)@K�@;d@
=@��@v�@V@5?@$�@@�T@�h@p�@O�@?}@�@�@z�@Z@9X@9X@(�@(�@(�@�@�m@��@dZ@
�!@
^5@
-@	��@	�@	��@	��@	��@	X@	X@	X@	&�@	�@	%@�`@Ĝ@��@bN@\)@�@ȴ@�R@��@v�@V@V@{@@�@��@�@�@z�@Z@�@C�@"�@o@@�@��@�!@��@�\@�\@n�@=q@-@J@��@�@�^@�7@X@7L@&�@�@%@%@%@�@�@%@�@%@%@ Ĝ@ �9@ �9@ ��@ �u@ �@ �@ �@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�#A�hsA�v�A�z�A�z�A�z�A�|�A�|�A�~�A�~�A�~�A�~�A�~�A݁A݁A݃A�33A��TA�=qAڧ�A�33A�|�A�VA�O�Aɕ�A��A��`AđhA���A�ĜA�VA��A�7LA��#A�S�A�&�A��A�S�A�A�n�A�G�A��A��FA�jA��wA�x�A�5?A��9A��A�ȴA��^A�9XA�A�p�A�"�A��7A�/A�5?A�v�A��DA�%A��jA�l�A��yA��A��HA�A�A���A���A�bNA�\)A��hA��A��/A��#A�1A�JA�G�A��!A��hA��A���A��uA���A��A���A��-A��hA�C�A���A�A�A��jA��A�-A�?}A��\A�"�A��A�|�A�$�A�K�A��A�XA�
=A� �A���AO�A~z�Ay�FAvbAuXAt�yAt1As�ArZAq;dAo��Am
=Ak33Ajv�Ai��Ai&�AhI�Ae�AdAc��Ab�RAa?}A^��A]��A]G�A]
=A\�jA\AZ�AZ(�AY�
AY��AYdZAX��AX~�AW�AW?}AV�AVbNAV�AU��AU�AR�AQ��AQ�APZAP5?AO�
AO��AO�hAOS�AN��ANȴANjAM�FAMdZAL$�AKAK��AKG�AJv�AJ^5AJ�DAJ=qAI;dAHȴAG�TAFn�AE��AD$�ABbA@��A@r�A@1A?��A>jA=�A<��A;�-A:�+A9��A8�yA8VA8I�A8JA7A6�A6A�A533A41A3p�A2��A2ZA0�/A0jA0$�A/A/O�A-�hA,�A+�FA*jA*$�A)�
A)l�A(��A(ZA(1'A(1'A(JA'��A'�A'%A&��A&E�A&A%�A$��A$z�A#��A#S�A"z�A!S�A �9A�TA�`A(�A��A/A�mA��AbNAx�AI�A1'Ap�A�jA�mA��A�AE�A�PA�A�jA��AI�A��A
�9A	�#A	��A	l�A	;dA�`A�+A?}AƨAdZA�-A�mA ffA   @�G�@�Z@�t�@���@��F@�~�@��@��T@���@���@�ȴ@�/@�@��@��#@�u@�@�V@��@�;d@�J@��u@��@�l�@�~�@�E�@���@��@���@� �@��@�ƨ@�33@�Ĝ@��@�v�@Гu@�1@�;d@˅@�&�@�bN@�1@��;@�t�@Ɵ�@ũ�@�A�@¸R@�z�@�|�@�S�@�@��@���@��\@�^5@�5?@��@�hs@�Ĝ@��w@��@��@���@��P@��y@���@��R@��!@��!@�-@���@�7L@���@�9X@�K�@���@�-@�&�@�ƨ@��R@�$�@��T@��-@�G�@���@��F@�+@��@��h@�;d@�
=@��H@���@�ff@��T@�`B@��j@�\)@���@�"�@�$�@�?}@��@��y@�ff@��@�hs@�G�@�V@���@�bN@�1'@��
@�dZ@��@��@���@�~�@�ff@�J@�?}@�j@��F@��`@��9@�Z@�I�@� �@�1'@�1'@�1@�33@�^5@��#@���@���@���@��^@��-@��^@���@�ȴ@�^5@��#@�O�@�G�@��@���@�$�@�{@�p�@���@�b@��F@�|�@��@�-@�^5@�@�&�@��@��w@��P@�dZ@�+@�@�ȴ@��\@�n�@�ff@�V@�@�hs@��/@�A�@�  @���@���@��F@��F@���@���@�$�@���@��@�7L@�Ĝ@��D@�r�@�(�@�;@~�R@}��@}�@|��@|�@|Z@{��@z��@y��@xĜ@w�@v�@vȴ@v��@u��@t�@t�j@t��@t�D@t9X@r�H@p��@p��@pbN@pQ�@o�@o;d@o
=@o
=@o�@o�@o�@n�@nE�@m�@m�h@mp�@m`B@mO�@m?}@m?}@m�@l�D@kt�@k33@k"�@k"�@k"�@k@j�H@j�\@j=q@j�@i�@ihs@h��@g�P@g|�@g�@h  @hA�@hA�@g|�@gl�@g\)@f�y@f��@f�+@fV@f@e��@e�-@cS�@a��@a��@a��@a�7@a�7@aX@_�@^5?@^@]�T@]@]�h@]`B@]`B@]?}@]?}@]/@]�@\��@\�@\�/@\�@\�D@\�@[dZ@Z�H@Z~�@ZJ@YG�@X��@X��@XA�@W�@W;d@U�h@T��@Tz�@Tz�@Tz�@Tz�@Tj@TZ@S��@S33@S"�@So@R��@R�!@R~�@Q�@Q��@Q�7@Q7L@Q�@P��@Pb@O�w@N��@N$�@M?}@L�/@L�j@L�@K�F@KC�@Ko@J��@J�\@J�\@Jn�@JM�@J=q@JJ@I��@Ix�@I%@H�u@HQ�@Hb@G�;@G��@G��@G�w@G��@GK�@G�@F�y@F�@Fȴ@F�R@F��@F�+@F�+@Fv�@FE�@E�T@E@E��@E`B@E�@D�@D�/@D�D@DZ@C�m@Ct�@C33@B��@A��@A��@A�^@Ahs@@Ĝ@@�@@bN@@A�@@ �@@ �@@  @@  @@ �@@1'@@ �@@b@@b@@b@?�@?�;@?�;@?�@?�@?�@?�@?�P@?|�@?l�@?l�@?\)@?K�@?�@>ȴ@>v�@=��@<��@<��@;�F@:-@9��@9G�@8��@8��@8r�@8Q�@8A�@81'@8  @7�w@7�P@6�@6$�@4Z@3��@3��@3�
@3�
@3ƨ@3�F@3ƨ@3ƨ@3ƨ@3�
@3�F@3��@3�@3C�@3"�@3@2�\@1��@1��@1��@1x�@1hs@1X@1�@0Ĝ@0r�@0Q�@0b@/��@/|�@/K�@/�@/
=@.��@.��@.�y@.�y@.�@.ȴ@.�+@.ff@.E�@.$�@.{@-��@-�h@-?}@,�D@+��@+��@*�H@*M�@*-@*J@)�#@)��@)�7@)X@(�@( �@'�w@'��@'�P@'\)@'K�@&ȴ@&�+@&5?@%@%p�@%/@%�@$�/@$�j@$z�@#�m@#@"��@"��@"^5@"-@"�@"�@"�@"J@!��@!�#@!��@!��@!hs@!%@ �9@ �@ bN@ A�@��@�P@K�@�@��@��@V@�T@?}@��@��@�D@�D@z�@z�@z�@j@j@�@�F@t�@"�@�!@~�@M�@M�@=q@=q@-@�@�@�@�@�7@X@�@��@��@��@�9@��@r�@A�@A�@1'@1'@ �@ �@  @�;@��@�P@\)@K�@+@
=@�@�@ȴ@�R@��@v�@{@��@�h@�@p�@?}@��@j@9X@�@1@�@��@��@��@��@��@��@o@@�H@�H@��@��@�\@^5@J@�@�#@�^@��@�@��@�`@��@�9@�@r�@bN@bN@Q�@1'@ �@�;@�@�P@|�@l�@l�@\)@K�@;d@
=@��@v�@V@5?@$�@@�T@�h@p�@O�@?}@�@�@z�@Z@9X@9X@(�@(�@(�@�@�m@��@dZ@
�!@
^5@
-@	��@	�@	��@	��@	��@	X@	X@	X@	&�@	�@	%@�`@Ĝ@��@bN@\)@�@ȴ@�R@��@v�@V@V@{@@�@��@�@�@z�@Z@�@C�@"�@o@@�@��@�!@��@�\@�\@n�@=q@-@J@��@�@�^@�7@X@7L@&�@�@%@%@%@�@�@%@�@%@%@ Ĝ@ �9@ �9@ ��@ �u@ �@ �@ �@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��BBBBBBBBBBBBBBBB��B��B��B��B�B�B��B��B��B�=B�1B�B�B�%B�{B��B��B��B��B�VB�B�%B�7B�7B�+B�+B�LBŢB�jBǮB��B�XB�B�dB�}B�}B�qB��B��B��B��B��B��B��B�{B�bB�JB�%B� Bw�Bl�BffBcTB`BBR�BG�B49B%�B{B�B�;B��B��B��BɺB�}B��B��B��B��B��B��B�hB�JB�+B�Bx�Bm�B`BBVBB�B2-B�BJB
�B
�B
��B
ĜB
�9B
��B
�JB
�B
_;B
B�B
=qB
9XB
2-B
)�B
#�B
�B
\B	��B	�B	�fB	�BB	�)B	��B	B	�XB	�FB	�!B	��B	��B	��B	��B	��B	�{B	�uB	�oB	�hB	�oB	�oB	�hB	�hB	�bB	�VB	�JB	�=B	�1B	�+B	�%B	�B	z�B	v�B	s�B	p�B	o�B	n�B	m�B	l�B	jB	iyB	hsB	ffB	cTB	`BB	[#B	ZB	ZB	VB	R�B	R�B	R�B	P�B	J�B	H�B	C�B	;dB	6FB	0!B	&�B	!�B	�B	�B	�B	�B	hB	VB	
=B	B	  B��B��B��B��B��B�B�B�B�`B�TB�BB�/B�B��B��B��B��BȴBÖB��B�wB�jB�dB�^B�RB�LB�FB�FB�FB�?B�9B�-B�'B�!B�B�B��B��B��B��B��B��B��B��B�uB�hB�\B�JB�1B�B|�Bz�Bv�Br�Bp�Bn�Bl�BjBiyBffBdZBbNBbNBaHB`BB^5B]/B\)B\)B[#B[#BZBXBW
BS�BQ�BO�BM�BL�BJ�BJ�BI�BG�BG�BG�BG�BG�BF�BE�BD�BC�BB�B@�B=qB<jB9XB5?B49B33B2-B1'B/B-B,B,B+B,B-B-B-B-B,B)�B)�B'�B'�B(�B'�B%�B&�B&�B%�B$�B$�B#�B"�B"�B!�B�B �B#�B#�B%�B&�B&�B'�B'�B'�B'�B'�B&�B&�B%�B%�B%�B$�B$�B%�B%�B%�B$�B$�B%�B%�B(�B+B+B,B.B0!B33B5?B5?B6FB6FB6FB6FB8RB8RB8RB=qBL�BM�BM�BM�BM�BN�BO�BP�BT�B^5Be`BhsBk�Bp�Bt�Bv�B{�B}�B~�B�B�B�7B�bB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�LB�dB�jB�jB�jB�dB�dB�jB�qB�qB�qB�qB�wBÖB��B��B��B��B��B��B��B��B��B�
B�B�B�5B�;B�BB�ZB�mB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B	  B	B	B	B	B	B	B	%B	
=B	bB	oB	�B	�B	�B	�B	�B	�B	!�B	$�B	'�B	(�B	)�B	+B	,B	-B	2-B	7LB	:^B	>wB	B�B	B�B	C�B	H�B	K�B	L�B	M�B	M�B	N�B	VB	^5B	`BB	aHB	bNB	cTB	ffB	gmB	iyB	jB	jB	k�B	l�B	o�B	q�B	s�B	s�B	t�B	t�B	t�B	t�B	u�B	x�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�=B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�9B	�?B	�FB	�LB	�RB	�XB	�XB	�^B	�^B	�^B	�^B	�dB	�dB	�jB	�jB	�wB	�}B	��B	��B	ÖB	ŢB	ǮB	ǮB	ȴB	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�)B	�HB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
DB
JB
JB
JB
VB
VB
\B
\B
\B
\B
bB
bB
\B
\B
\B
\B
bB
\B
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
oB
oB
uB
�B
�B
�B
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
!�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
+B
+B
+B
+B
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
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
33B
49B
49B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
J�B
K�B
L�B
L�B
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
e`B
ffB
ffB
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
jB
jB
jB
jB
k�B
k�B
l�B
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
p�B
p�B
p�B
p�B
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
s�B
s�B
t�B
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
v�B
v�B
v�B
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
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��BB B BB B B B B B B'B'BAB[B�B�B��B�B�fB��B�FB��B�B�~B��B�JB��B�B�B�sB��B��B�KB�+B��B��B�B�XB��B��B��B�RB��B��B�B�B�PB�"B�B�;BB��B��B�fB�DB�DB�2B�-B��B�gB��B��B��B��Bz�Bm�Bg8Bd�Bb�BU�BKDB72B)DB�B�B��B�9B�VB͟B��B��B�&B�B��B��B�+B��B��B��B��B��B{0Bo�Bb�BYeBE�B5�B#TBHB
�hB
��B
�PB
�+B
��B
�B
��B
�B
b�B
C�B
>BB
:�B
3�B
+QB
%�B
�B
�B	�B	��B	�B	�bB	��B	��B	�3B	�DB	��B	�GB	�sB	�;B	�1B	�+B	�?B	��B	��B	�&B	��B	��B	��B	� B	�:B	�4B	�BB	�B	��B	��B	��B	��B	��B	|B	w�B	t�B	qB	p!B	o B	m�B	mB	kB	i�B	iDB	gmB	d&B	a�B	[�B	ZkB	Z�B	V�B	S@B	S&B	S�B	R B	K�B	J#B	EmB	<�B	8lB	2|B	(sB	"�B	 �B	�B	)B	�B	�B	�B	�B	YB	B��B�DB�^B��B��B��B�!B�B�LB�@B�|B��B��BյB��B�&B�B�rBĜB��B�B�B�6B�JB��B��B�zB��B��B��B�B��B��B��B�OB��B��B��B�
B�FB�NB��B��B��B��B�oB��B�VB��B�3B~�B|�By>Bs�Bq�Bo�Bm�Bk6BkBi_BeFBb�Bb�BbBabB_�B^OB\�B\�B[�B[�B[WBZBX�BUMBTaBRoBO�BM�BL~BK�BJ�BIRBH�BH�BHBHBG+BF�BF?BEBC�BB'B@ B=�B:^B6`B5tB4�B3hB2�B1B.�B,�B,qB+�B,�B-�B-�B-�B-�B,�B+�B*�B)DB)DB)�B)*B(sB(�B'�B&fB%FB%zB$�B#�B$B# B BB!|B$&B$ZB&2B'RB'RB(>B(>B(XB(�B(�B'�B'�B&�B'B&�B%`B%,B&B&2B&2B%zB%�B&fB&�B)�B+�B+�B,�B/5B1[B4B5�B5�B6�B6�B6�B7B8�B8�B9�B>�BMBN"BNVBN<BNpBO�BP�BRTBV�B_�Bf2Bi_Bl�BqvButBw�B|6B~BBcB�oB��B��B��B��B��B�	B�B�!B�B�HB��B��B��B�B�yB��B�WB�oB�fB��B��B�"B�"B��B��B��B��B��B��B��B��B�{B�JB�pB�PB��B��B��B�B�gBյBרB��B�BޞB��B��B�B�
B�QB�QB��B��B��B��B��B��B��B��B��B�B�TB�>B�jB	 �B	[B	GB	{B	GB	MB	�B	�B	
�B	�B	�B	B	B	B	 B	;B	 BB	"hB	%`B	(XB	)*B	*eB	+kB	,�B	-�B	2�B	7�B	:�B	>�B	B�B	B�B	DB	IB	K�B	MB	NB	N<B	O�B	V�B	^�B	`vB	a�B	b�B	c�B	f�B	g�B	i�B	j�B	j�B	k�B	l�B	o�B	q�B	s�B	s�B	t�B	t�B	t�B	uB	v+B	yrB	�4B	� B	� B	�UB	�[B	�[B	�uB	�aB	�gB	�gB	��B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�,B	�,B	�fB	��B	�sB	�$B	�$B	�>B	�$B	�XB	��B	��B	�hB	�nB	��B	��B	��B	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�DB	�~B	�TB	�$B	�+B	�EB	�KB	�_B	�QB	ܒB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	��B	��B	� B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�2B	�B	�8B	�$B	�B	�B	�6B	�B	�B	�"B	�"B	�"B	�(B	�(B	�B	�.B
 B
 OB
 4B
 B
 OB
 4B
UB
AB
GB
GB
gB
SB
SB
SB
YB
tB
zB
�B
�B
	�B
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
}B
�B
vB
vB
vB
�B
}B
�B
�B
}B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$B
1B
�B
B
B
�B
�B
�B
�B
B
�B
 B
 B
 'B
!HB
"�B
&B
'B
'8B
'B
'B
'B
'B
'B
'B
'B
'B
'8B
'B
($B
(>B
(>B
(>B
)_B
*KB
*KB
+QB
+6B
+QB
+6B
,WB
,=B
-CB
-CB
-]B
.cB
.IB
.cB
/5B
/5B
/5B
/5B
/5B
/5B
/OB
/OB
0UB
0oB
0oB
0UB
0UB
1vB
1vB
2�B
3�B
4�B
4�B
5�B
6zB
7�B
7�B
7�B
7�B
7�B
7�B
9�B
9�B
:�B
:�B
:�B
;�B
;�B
<�B
<�B
<�B
=�B
>�B
>�B
>�B
>�B
?�B
@ B
AB
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
IB
J	B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
KB
LB
MB
MB
NB
NB
OB
N�B
OB
N�B
N�B
N�B
N�B
OB
OB
PB
PB
PB
QB
Q B
Q4B
QB
QB
R:B
R:B
RB
RB
S&B
SB
SB
S@B
S&B
S&B
S@B
T,B
TB
TFB
T,B
T,B
UB
U2B
UB
UMB
U2B
UMB
V9B
V9B
W$B
W?B
W?B
WYB
W?B
XEB
XEB
Y1B
YKB
YeB
YKB
Y1B
Y1B
Y1B
YeB
YB
Z7B
ZQB
[WB
[=B
[WB
[WB
[WB
\]B
\xB
\]B
\]B
\xB
]�B
^OB
^OB
^OB
^�B
^jB
_pB
_VB
_pB
_�B
`vB
`vB
`vB
a|B
a|B
a|B
abB
abB
a|B
abB
a|B
b�B
b�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
lB
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
pB
p�B
p�B
qB
p�B
qB
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
s�B
tB
t�B
t�B
t�B
t�B
uB
vB
vB
vB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
v�B
w�B
w�B
xB
w�B
w�B
w�B
w�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<0�m<9%~<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.26(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709250040452017092500404520170925004045202211182131482022111821314820221118213148201804031937152018040319371520180403193715  JA  ARFMdecpA19c                                                                20170911003507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170910153540  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170910153541  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170910153542  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170910153542  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170910153542  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170910153542  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170910153542  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170910153543  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170910153543                      G�O�G�O�G�O�                JA  ARUP                                                                        20170910155634                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170910153126  CV  JULD            G�O�G�O�F�)                JM  ARCAJMQC2.0                                                                 20170924154045  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170924154045  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103715  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171527                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123148  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                