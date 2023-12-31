CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-23T00:42:41Z creation;2020-01-23T00:42:47Z conversion to V3.1;2023-06-29T05:50:11Z update;     
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
_FillValue                 �  ]   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_ADJUSTED_QC         
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
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݨ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20200123004241  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_205                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @��8ĥ[�1   @��9��>�@7V�t��b��p:�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@ҏ\A	G�A)G�AIG�AiG�A���A���A���A���Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2Q�B:Q�BBQ�BJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B���B�(�B�(�B�(�B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{Cz�C�{C�{C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�{C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�{CF�{CH�{CJ�{CL�{CN�{CP�CR�{CT�{CV�{CX�{CZ�{C\�{C^�{C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�=qC�=qC�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�Du%Du�Dv%Dv�Dw%Dw�Dx%Dx�Dy%Dy�Dz%Dz�D{%D{�D|%D|�D}%D}�D~%D~�D%D�D��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�U�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�O\D���D�ҏD��D�U�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D�D�ҏD��D�R�DÒ�D�ҏD��D�R�DĒ�D�ҏD��D�R�DŒ�D�ҏD��D�R�Dƒ�D�ҏD��D�R�Dǒ�D�ҏD��D�R�DȒ�D�ҏD��D�R�Dɒ�D�ҏD��D�R�Dʒ�D�ҏD��D�R�D˒�D�ҏD��D�R�D̒�D�ҏD��D�R�D͒�D�ҏD��D�R�DΒ�D�ҏD��D�R�Dϒ�D�ҏD��D�R�DВ�D�ҏD��D�R�Dђ�D�ҏD��D�R�DҒ�D�ҏD��D�R�DӒ�D�ҏD��D�R�DԒ�D�ҏD��D�R�DՒ�D�ҏD��D�R�D֒�D�ҏD��D�R�Dג�D�ҏD��D�R�Dؒ�D�ҏD��D�R�Dْ�D�ҏD��D�R�Dڒ�D�ҏD��D�R�Dے�D�ҏD��D�R�Dܒ�D�ҏD��D�R�Dݒ�D�ҏD��D�R�Dޒ�D�ҏD��D�R�Dߒ�D�ҏD��D�R�D���D�ҏD��D�R�DᒏD�ҏD��D�R�D⒏D�ҏD��D�R�D㒏D�ҏD��D�R�D䒏D�ҏD��D�R�D咏D�ҏD��D�R�D撏D�ҏD��D�R�D璏D�ҏD��D�R�D蒏D�ҏD��D�R�D钏D�ҏD��D�R�D꒏D�ҏD��D�R�D뒏D�ҏD��D�R�D쒏D�ҏD��D�R�D풏D�ҏD��D�R�DD�ҏD��D�R�DD�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�U�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1'A�+A���A��`A��TA��HA��;A��;A��HA��HA��HA��HA��TA��HA��HA��HA��TA��TA��HA��HA��TA��HA��;A��;A��;A��;A��HA��HA��TA��TA��TA��TA��TA��`A��`A��mA��mA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��#A���A�p�A��;A��HA�=qA���A���A��;A�E�A��A�/A���A�dZA�dZA��A�jA�9XA�33A��;A���A��HA�$�A��9A���A�9XA���A��A���A�ƨA��yA�C�A��A��hA���A�1A���A��9A���A�r�A���A�9XA�"�A��uA���A��#A��TA��A�E�A�I�A�C�A�v�A�l�A�O�A���A��A�33A�$�A���A��7A�"�A��A�|�A�A{ƨAz�uAx��Aw&�As��Arn�Ar�Aq�PApz�An-AljAiƨAe�mAcXAa�
A^ĜA]��A\�DA[O�AX�HAV��AUXAT�ARbAO��AM��AL�uAK`BAJ{AG��AD��ACƨACl�AB�\AAC�A@jA@1A?�A>{A<=qA;&�A:I�A8�/A8{A7l�A6z�A4JA1+A0  A/�;A/�#A/;dA-�A,�+A,1A*9XA(JA%|�A#��A#&�A"��A!hsA ��A��AVAjA�Al�A�RA�A�hAA-A�;Ax�AjA��Ax�A�AI�A�AM�A��A��A%AĜAz�A�RAbA�A��AI�A�Ax�AoA��A��A
�9A
r�A	��A�9AXAbNA��AA��A�A r�@��@�p�@��@�z�@��m@�K�@��@�\)@��@�1'@�5?@���@��;@��@홚@��/@��@��@�M�@�I�@��m@�\)@���@�^5@�hs@���@�(�@�dZ@�S�@ޗ�@݁@� �@ە�@ڰ!@�X@�Ĝ@��m@���@Ӿw@��@�$�@щ7@��@ЋD@�1'@���@�S�@Ͳ-@�9X@�K�@��y@�=q@��#@ɡ�@�x�@��@�t�@Ɨ�@��T@��/@��m@�|�@��@���@�V@��@��@��y@���@�`B@��@�Ĝ@�r�@�I�@��@�|�@�"�@�33@�33@�b@���@�|�@�7L@�Z@�bN@���@��@��!@�~�@�5?@���@��`@��@�1'@��
@�Z@�Ĝ@�S�@���@�C�@��@���@�33@�r�@��P@���@�@�`B@���@�n�@���@�V@�O�@���@�b@��@��@��@��9@�  @�S�@�;d@�33@�G�@��@�$�@���@���@��
@��/@��!@���@�v�@�E�@�ȴ@���@�C�@�7L@�  @��@�;d@�"�@�C�@�"�@�{@��^@�V@��`@���@���@�/@��/@��j@�&�@��;@�x�@�-@��@�K�@�l�@�t�@�S�@�33@�@��y@��R@�n�@�n�@�x�@��@�&�@��/@�1'@��P@�ȴ@�M�@��@��#@���@��-@�?}@�ȴ@�M�@�M�@�V@�ff@�v�@���@�@�~�@�^5@��@��+@�5?@��#@�x�@���@�A�@��y@��\@�n�@�`B@���@��#@�=q@���@�X@���@�J@��@�7L@�Ĝ@��w@�K�@��y@�J@�E�@���@�-@���@���@�@���@�1'@�(�@�1@���@�\)@�
=@��y@��y@��@��H@���@��H@��R@�M�@���@��^@�?}@���@���@���@��@���@�|�@�33@��@��@��R@�V@�$�@�J@��T@���@��@�`B@�G�@�7L@�&�@���@��j@��u@�j@�A�@�1'@�(�@�1@��m@��w@��@���@�l�@�C�@��@�ȴ@��!@��\@�^5@�=q@�$�@��@���@�`B@��@�%@���@�z�@�(�@�1@�@\)@;d@
=@~�y@~��@~{@}�-@}�@|��@|�D@|1@{�F@{C�@{o@z~�@y��@y��@y�7@yG�@y�@x�`@x�9@xr�@wl�@w�@vv�@v$�@u�@u�@u`B@u/@t�@tj@t9X@s�F@so@r��@r�\@r^5@r-@rJ@q�@q�7@q&�@pĜ@pr�@pQ�@pb@o��@o;d@n�@n@m@mp�@m�@mV@l�/@l(�@k�
@k��@k33@j�!@jJ@i��@i&�@h��@hbN@g�@g�w@gl�@g;d@f�@f��@f�+@fV@e�T@e�h@e?}@d�@d��@d(�@c�m@c�
@c��@ct�@c"�@b=q@a��@ahs@a7L@`Ĝ@`��@`1'@_|�@_;d@^��@^v�@^$�@]�T@]�@]V@\�j@\��@\Z@[�
@[��@[�@[C�@["�@[@Z��@Z��@Zn�@Z=q@Y�^@YX@X��@X�@W�;@W|�@W;d@Vȴ@Vff@Vff@VE�@V{@U�T@U��@Up�@U?}@U�@T��@T��@T�@Tj@Sƨ@St�@S@R��@R~�@R=q@Q�#@Q��@Qhs@Q%@PĜ@P�u@PA�@Pb@O�w@O+@O
=@O
=@O
=@O
=@N�@NV@M�@M��@M?}@L�/@LI�@K��@K�
@K��@K"�@J~�@Jn�@Jn�@Jn�@J-@I��@IX@H��@H�@HQ�@HA�@H �@H  @G�@G��@G�w@G\)@F�y@F�@F�@F�R@Fv�@F5?@F@E�h@E`B@E?}@E/@EV@D��@D9X@C�m@C��@CC�@C"�@C"�@B�!@BM�@BM�@A�#@A��@A��@A�7@A7L@@��@@Ĝ@@��@@�u@@Q�@?�@?�w@?�P@?\)@>�y@>��@>ff@=�@=��@=?}@<��@<Z@<�@;�
@;�@;C�@;o@:�H@:n�@:M�@:-@9�@9��@9x�@9X@9G�@9&�@9%@8Ĝ@8�@8bN@8b@7�w@7\)@6�y@6�R@6�R@6ff@5�T@5�h@5?}@5/@5/@4��@4�/@4�j@4��@4�D@4j@4(�@3�m@3��@3dZ@3C�@2�H@2�!@2n�@2=q@1�@1��@1x�@1%@0�`@0��@0�u@0�@0A�@0 �@0  @/�@/�;@/��@/+@.�y@.�+@.V@.{@-@-��@-�@-p�@-p�@-p�@-`B@-`B@-�@,�@,�D@,Z@,I�@,1@+�m@+��@+t�@+33@+o@+@*��@*��@*�\@*^5@*-@)��@)�7@)hs@)&�@)�@(��@(��@(Q�@(A�@'�@'�@'|�@'�@&�R@&��@&ff@&E�@&$�@%�-@%�-@%��@%�@$�/@$Z@$�@#�
@#�@#C�@#@"��@"��@"n�@"M�@"-@!��@!�7@!x�@!7L@!%@ �`@ ��@ �9@ �u@ �@ bN@  �@�@�;@��@�P@|�@K�@��@��@ff@E�@�T@�h@O�@��@�@��@��@�@ƨ@t�@t�@C�@�@��@~�@n�@=q@�@J@�#@��@&�@��@�@1'@  @�@\)@+@�@�R@�+@5?@{@�T@�-@��@p�@?}@�@�@�@Z@(�@1@�m@�
@��@��@t�@dZ@S�@o@�@��@�!@�\@-@��@��@�7@X@G�@7L@�@��@�9@�u@r�@1'@�;@��@l�@+@�@��@�+@E�@5?@$�@{@@��@�@O�@V@��@�D@9X@1@�m@�
@��@�@�@�@t�@t�@dZ@C�@o@@
�@
�H@
��@
~�@
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1'A�+A���A��`A��TA��HA��;A��;A��HA��HA��HA��HA��TA��HA��HA��HA��TA��TA��HA��HA��TA��HA��;A��;A��;A��;A��HA��HA��TA��TA��TA��TA��TA��`A��`A��mA��mA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��#A���A�p�A��;A��HA�=qA���A���A��;A�E�A��A�/A���A�dZA�dZA��A�jA�9XA�33A��;A���A��HA�$�A��9A���A�9XA���A��A���A�ƨA��yA�C�A��A��hA���A�1A���A��9A���A�r�A���A�9XA�"�A��uA���A��#A��TA��A�E�A�I�A�C�A�v�A�l�A�O�A���A��A�33A�$�A���A��7A�"�A��A�|�A�A{ƨAz�uAx��Aw&�As��Arn�Ar�Aq�PApz�An-AljAiƨAe�mAcXAa�
A^ĜA]��A\�DA[O�AX�HAV��AUXAT�ARbAO��AM��AL�uAK`BAJ{AG��AD��ACƨACl�AB�\AAC�A@jA@1A?�A>{A<=qA;&�A:I�A8�/A8{A7l�A6z�A4JA1+A0  A/�;A/�#A/;dA-�A,�+A,1A*9XA(JA%|�A#��A#&�A"��A!hsA ��A��AVAjA�Al�A�RA�A�hAA-A�;Ax�AjA��Ax�A�AI�A�AM�A��A��A%AĜAz�A�RAbA�A��AI�A�Ax�AoA��A��A
�9A
r�A	��A�9AXAbNA��AA��A�A r�@��@�p�@��@�z�@��m@�K�@��@�\)@��@�1'@�5?@���@��;@��@홚@��/@��@��@�M�@�I�@��m@�\)@���@�^5@�hs@���@�(�@�dZ@�S�@ޗ�@݁@� �@ە�@ڰ!@�X@�Ĝ@��m@���@Ӿw@��@�$�@щ7@��@ЋD@�1'@���@�S�@Ͳ-@�9X@�K�@��y@�=q@��#@ɡ�@�x�@��@�t�@Ɨ�@��T@��/@��m@�|�@��@���@�V@��@��@��y@���@�`B@��@�Ĝ@�r�@�I�@��@�|�@�"�@�33@�33@�b@���@�|�@�7L@�Z@�bN@���@��@��!@�~�@�5?@���@��`@��@�1'@��
@�Z@�Ĝ@�S�@���@�C�@��@���@�33@�r�@��P@���@�@�`B@���@�n�@���@�V@�O�@���@�b@��@��@��@��9@�  @�S�@�;d@�33@�G�@��@�$�@���@���@��
@��/@��!@���@�v�@�E�@�ȴ@���@�C�@�7L@�  @��@�;d@�"�@�C�@�"�@�{@��^@�V@��`@���@���@�/@��/@��j@�&�@��;@�x�@�-@��@�K�@�l�@�t�@�S�@�33@�@��y@��R@�n�@�n�@�x�@��@�&�@��/@�1'@��P@�ȴ@�M�@��@��#@���@��-@�?}@�ȴ@�M�@�M�@�V@�ff@�v�@���@�@�~�@�^5@��@��+@�5?@��#@�x�@���@�A�@��y@��\@�n�@�`B@���@��#@�=q@���@�X@���@�J@��@�7L@�Ĝ@��w@�K�@��y@�J@�E�@���@�-@���@���@�@���@�1'@�(�@�1@���@�\)@�
=@��y@��y@��@��H@���@��H@��R@�M�@���@��^@�?}@���@���@���@��@���@�|�@�33@��@��@��R@�V@�$�@�J@��T@���@��@�`B@�G�@�7L@�&�@���@��j@��u@�j@�A�@�1'@�(�@�1@��m@��w@��@���@�l�@�C�@��@�ȴ@��!@��\@�^5@�=q@�$�@��@���@�`B@��@�%@���@�z�@�(�@�1@�@\)@;d@
=@~�y@~��@~{@}�-@}�@|��@|�D@|1@{�F@{C�@{o@z~�@y��@y��@y�7@yG�@y�@x�`@x�9@xr�@wl�@w�@vv�@v$�@u�@u�@u`B@u/@t�@tj@t9X@s�F@so@r��@r�\@r^5@r-@rJ@q�@q�7@q&�@pĜ@pr�@pQ�@pb@o��@o;d@n�@n@m@mp�@m�@mV@l�/@l(�@k�
@k��@k33@j�!@jJ@i��@i&�@h��@hbN@g�@g�w@gl�@g;d@f�@f��@f�+@fV@e�T@e�h@e?}@d�@d��@d(�@c�m@c�
@c��@ct�@c"�@b=q@a��@ahs@a7L@`Ĝ@`��@`1'@_|�@_;d@^��@^v�@^$�@]�T@]�@]V@\�j@\��@\Z@[�
@[��@[�@[C�@["�@[@Z��@Z��@Zn�@Z=q@Y�^@YX@X��@X�@W�;@W|�@W;d@Vȴ@Vff@Vff@VE�@V{@U�T@U��@Up�@U?}@U�@T��@T��@T�@Tj@Sƨ@St�@S@R��@R~�@R=q@Q�#@Q��@Qhs@Q%@PĜ@P�u@PA�@Pb@O�w@O+@O
=@O
=@O
=@O
=@N�@NV@M�@M��@M?}@L�/@LI�@K��@K�
@K��@K"�@J~�@Jn�@Jn�@Jn�@J-@I��@IX@H��@H�@HQ�@HA�@H �@H  @G�@G��@G�w@G\)@F�y@F�@F�@F�R@Fv�@F5?@F@E�h@E`B@E?}@E/@EV@D��@D9X@C�m@C��@CC�@C"�@C"�@B�!@BM�@BM�@A�#@A��@A��@A�7@A7L@@��@@Ĝ@@��@@�u@@Q�@?�@?�w@?�P@?\)@>�y@>��@>ff@=�@=��@=?}@<��@<Z@<�@;�
@;�@;C�@;o@:�H@:n�@:M�@:-@9�@9��@9x�@9X@9G�@9&�@9%@8Ĝ@8�@8bN@8b@7�w@7\)@6�y@6�R@6�R@6ff@5�T@5�h@5?}@5/@5/@4��@4�/@4�j@4��@4�D@4j@4(�@3�m@3��@3dZ@3C�@2�H@2�!@2n�@2=q@1�@1��@1x�@1%@0�`@0��@0�u@0�@0A�@0 �@0  @/�@/�;@/��@/+@.�y@.�+@.V@.{@-@-��@-�@-p�@-p�@-p�@-`B@-`B@-�@,�@,�D@,Z@,I�@,1@+�m@+��@+t�@+33@+o@+@*��@*��@*�\@*^5@*-@)��@)�7@)hs@)&�@)�@(��@(��@(Q�@(A�@'�@'�@'|�@'�@&�R@&��@&ff@&E�@&$�@%�-@%�-@%��@%�@$�/@$Z@$�@#�
@#�@#C�@#@"��@"��@"n�@"M�@"-@!��@!�7@!x�@!7L@!%@ �`@ ��@ �9@ �u@ �@ bN@  �@�@�;@��@�P@|�@K�@��@��@ff@E�@�T@�h@O�@��@�@��@��@�@ƨ@t�@t�@C�@�@��@~�@n�@=q@�@J@�#@��@&�@��@�@1'@  @�@\)@+@�@�R@�+@5?@{@�T@�-@��@p�@?}@�@�@�@Z@(�@1@�m@�
@��@��@t�@dZ@S�@o@�@��@�!@�\@-@��@��@�7@X@G�@7L@�@��@�9@�u@r�@1'@�;@��@l�@+@�@��@�+@E�@5?@$�@{@@��@�@O�@V@��@�D@9X@1@�m@�
@��@�@�@�@t�@t�@dZ@C�@o@@
�@
�H@
��@
~�@
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B#�B/B��B��B��B�PB��B�RB�)B  B+BbB�B�B8RBF�BA�B;dB9XB5?B0!B49B-B$�B!�B �B%�B&�B$�B�B �BPBB��B��B�B�TB�;B�BB�;B��BŢB�^B�B��B�JB�1B�Bq�BZB>wB%�B
��B
��B
ƨB
��B
�bB
�1B
s�B
XB
Q�B
<jB
0!B
�B
VB
  B	��B	�NB	��B	��B	ŢB	�wB	�B	��B	�\B	s�B	^5B	R�B	B�B	:^B	49B	.B	 �B	�B	JB	B��B�B�TB�/B�B��BǮB�^B�3B�'B�B��B��B��B��B��B��B�uB�bB�JB�=B�+B�B{�Bx�Bt�Br�Bq�Bq�Bm�BjBhsBhsBhsB`BBbNBaHBn�Bs�Br�Br�Bs�Bw�Bv�Bv�Bu�Bu�Bu�Bu�Bt�Bs�Bs�Br�Bp�Br�Bt�Bu�Bs�Bp�Br�Bt�Bu�Bu�Bs�Bt�Bs�Bq�Bq�Bo�Bs�Bt�Bu�Bu�Bw�Bw�Bu�Br�Bl�BgmBffBe`B^5B]/B[#BW
BR�BP�BXB[#B_;B`BB_;BQ�BR�BP�BJ�BG�BD�BH�BJ�BM�BK�BG�BE�B@�B?}B@�B@�BA�BB�B@�B?}BA�BH�BN�BN�BP�BQ�BS�BS�BR�BR�BVBW
BXBXBXBZB_;BdZBhsBhsBhsBffBhsBo�Bw�B{�B{�B}�B� B�B�7B�PB�\B�\B�bB�hB�oB�oB�hB�hB�oB�hB�hB�hB�hB�hB�bB�hB��B��B��B��B��B�B��B��B��B��B�B�'B�-B�'B�!B�!B�9B�?B�?B�?B�^B��BƨB��B��B�
B�)B�)B�
B��B�B�B�`B�mB�B�B�B��B	  B	%B��B��B��B��B��B	B	B	B	oB	oB	\B	bB	�B	"�B	�B	{B	�B	�B	�B	#�B	5?B	2-B	+B	-B	2-B	6FB	:^B	=qB	?}B	A�B	B�B	C�B	C�B	E�B	G�B	K�B	K�B	O�B	S�B	dZB	u�B	{�B	~�B	�B	�+B	�7B	�7B	�=B	�JB	�\B	�\B	�\B	�\B	�bB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�3B	�3B	�3B	�?B	�-B	�B	�B	�'B	�'B	�'B	�dB	��B	��B	�}B	ƨB	ƨB	ŢB	ĜB	ÖB	��B	�wB	�wB	�qB	��B	�}B	B	��B	ɺB	ȴB	ĜB	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
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
JB
JB
PB
VB
VB
VB
\B
\B
bB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
-B
-B
-B
.B
.B
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
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
49B
5?B
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
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
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
>wB
=qB
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
@�B
@�B
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
C�B
C�B
C�B
C�B
C�B
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
F�B
F�B
F�B
F�B
G�B
G�B
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
J�B
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
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
ZB
ZB
ZB
ZB
[#B
[#B
[#B
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
jB
jB
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
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�BqBaBWBqBqBqBqBqBqBqBqBqBqBqBqBqBqBqBqBqBqBqBqBqBqBqBqBqBqB�BqBqBqBqBqBqBqBqBqBqBqBxBxBxBxBxBxB�B�B�B�B#�B'RB9XB�B� B�kB�B��B��BޞB;B�B�B�B�B:�BIBC�B>BB;�B6�B2B6�B.}B&LB"�B"4B'B(�B&�B!bB"�B�BB��B�VB�|B�B�B�NB��B�B�B�PB��B��B�<B�	B�MBs�B\)BAoB)�B
�]B
�{B
�lB
�"B
��B
�)B
v�B
[	B
U2B
>�B
33B
_B
bB
'B	��B	�B	�}B	�~B	�EB	�B	��B	�>B	�[B	vzB	`\B	U�B	C�B	;�B	5�B	0�B	#B	KB	"B	�B�JB��B��B޸B��BЗB�rB�B��B�-B�OB��B��B�B� B��B��B��B��B�PB�DB��B��B~�By�Bt�Br�Br�BsMBn�Bk�Bj�BkBk6Ba�Bc Bb4Bo�Bt�Bs�BshBtTBxBw�Bw�Bv`Bv`Bv`BvzBuBtTBt�BsMBp�BsMBu�BwBt�BqABr�BuZBvBv`ButBuZBtTBrGBr-BpBt9Bu%BvFBv�Bx�BxBv�BtBnBh�BgmBgRB_�B^�B\]BXBS�BQBXEB[WB_�Ba-B`�BR�BS�BQ�BKxBH1BEBI�BK�BOvBL�BH�BF�B@�B?�B@�B@�BA�BB�B@�B?�BA�BIBO\BOvBQ4BRoBT�BTFBS�BT,BW
BWYBX�BXEBXEBZ7B_pBdtBh�BiDBi*Bf�Bh�Bo�Bw�B{�B{�B~BB��B��B��B��B��B��B�bB�hB��B��B��B��B��B�hB�hB�hB�hB�NB�bB��B�gB�9B�+B��B�pB��B�B�LB��B�B�B�[B�B�AB�UB�oB�9B�?B�%B��B�DB��B��B�@B��BּBܬB�~B׍B�FB�mB�B��B�B��B��B�vB�B	 �B	EB	 B�B�B�2B�B	 �B	�B	GB	B	B	(B	vB	�B	$ZB	�B	aB	SB	EB	�B	#B	5�B	3B	+�B	-B	2-B	6B	:*B	=qB	?�B	A�B	B�B	CaB	CGB	ESB	GzB	K�B	K^B	N�B	R:B	c B	t�B	{0B	~wB	��B	��B	�B	�B	�#B	�B	�BB	�BB	�BB	��B	��B	��B	��B	��B	��B	��B	��B	�sB	�YB	�eB	��B	�&B	��B	��B	�KB	�kB	�dB	�hB	�tB	��B	�B	��B	��B	��B	�3B	�MB	�3B	�B	��B	��B	�B	�B	�vB	�B	�UB	��B	��B	�iB	��B	�tB	��B	ŢB	ĶB	��B	��B	��B	��B	�"B	�iB	��B	��B	��B	�#B	�7B	ĶB	�aB	ĜB	ňB	ƨB	ǔB	ȀB	�fB	�lB	ʌB	˒B	͟B	��B	��B	��B	�B	��B	��B	ҽB	ҽB	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�-B	�B	� B	� B	� B	�@B	�FB	�`B	�2B	�2B	�8B	�RB	�>B	�DB	�_B	�B	�kB	�qB	�]B	�wB	�}B	�B	�oB	�B	�vB	�|B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
B
�B
B
�B
B
�B
1B
	B
	B

=B
B
B
B
0B
B
0B
B
"B
<B
VB
BB
BB
HB
4B
NB
hB
:B
:B
:B
:B
:B
@B
@B
[B
@B
FB
FB
FB
{B
MB
MB
SB
SB
sB
�B
sB
�B
yB
�B
_B
�B
B
kB
eB
B
�B
kB
�B
�B
qB
qB
�B
�B
xB
xB
~B
~B
~B
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
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
,�B
-�B
-�B
-�B
-�B
-�B
/ B
.�B
.�B
.�B
.�B
/ B
/ B
/B
/�B
/�B
/�B
0�B
0�B
0�B
1B
0�B
1B
1'B
1�B
1�B
1�B
2�B
2�B
4B
49B
5B
6B
6B
6B
6B
6+B
7B
72B
7B
7B
7B
88B
8B
8B
88B
9	B
9>B
9$B
:*B
:*B
;0B
;0B
;0B
;0B
;0B
<6B
;JB
<6B
<PB
<6B
<6B
<PB
=<B
=VB
=qB
=qB
>BB
=<B
>BB
>BB
>BB
>BB
?HB
?HB
?HB
?HB
?HB
@OB
@OB
@OB
@OB
@OB
@iB
AUB
AUB
AUB
AoB
BuB
BuB
BuB
BuB
BuB
BuB
CaB
CaB
CGB
CGB
C{B
DgB
D�B
DgB
DgB
D�B
DgB
D�B
EmB
EmB
E�B
E�B
FtB
FtB
FtB
FtB
GzB
GzB
GzB
GzB
GzB
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
I�B
J�B
J�B
J�B
J�B
J�B
JrB
J�B
KxB
J�B
KxB
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
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
Y�B
Y�B
Y�B
ZB
Z�B
[	B
Z�B
[�B
[�B
\B
\)B
\�B
]B
\�B
\�B
^B
^B
^B
^B
^B
^B
^B
_B
_!B
_!B
`B
`'B
`B
`'B
`'B
`B
aB
aB
aB
aB
bB
b4B
bB
bB
bB
b4B
c:B
c:B
c B
c B
c B
c B
d&B
d&B
dB
d&B
d&B
d&B
dB
e,B
eFB
e,B
eFB
e,B
e,B
eFB
f2B
f2B
f2B
fLB
g8B
gB
g8B
g8B
gRB
g8B
gRB
g8B
hXB
h>B
h>B
iDB
i_B
iDB
iDB
i_B
j0B
j0B
j0B
jKB
jKB
jKB
jKB
kQB
kQB
kQB
kkB
lWB
lWB
l=B
lWB
l=B
l=B
l=B
l=B
l=B
l=B
lWB
m]B
mCB
m]B
m]B
m]B
m]B
ncB
nI1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'& <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.58(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001280035432020012800354320200128003543202306231720192023062317201920230623172019202001290022052020012900220520200129002205  JA  ARFMdecpA19c                                                                20200123094213  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200123004241  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200123004244  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200123004244  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200123004245  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200123004245  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200123004246  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200123004246  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200123004246  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200123004247                      G�O�G�O�G�O�                JA  ARUP                                                                        20200123005715                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20200123153635  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20200127153543  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200127153543  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200128152205  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082019  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                