CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-02-24T12:40:19Z creation;2019-02-24T12:40:22Z conversion to V3.1;2019-12-23T06:06:28Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190224124019  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0675_127                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ة���- 1   @ة�fff�@8�;dZ�c4 ѷY1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D���D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�3D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�<�Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ Dۼ�D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@ҏ\A	G�A)G�AIG�AiG�A���A���A���A���Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2Q�B:Q�BBQ�BJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�{C.�{C0�{C2�{C4�{C6�{C8�{C:�C<�{C>�{C@�{CB�{CD�{CF�{CH�{CJ�{CL�{CN�{CP�{CR�{CT�{CVz�CX�{CZ�{C\�{C^�{C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�W
C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�Du%Du�Dv%Dv�Dw%Dw�Dx%Dx�Dy%Dy�Dz%Dz�D{%D{�D|%D|�D}%D}�D~%D~�D%D�D��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�O\D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D�D�ҏD��D�R�DÒ�D�ҏD��D�R�DĒ�D�ҏD��D�R�DŒ�D�ҏD��D�R�Dƒ�D�ҏD��D�R�Dǒ�D�ҏD�\D�R�DȒ�D�ҏD��D�R�Dɒ�D�ҏD��D�R�Dʒ�D�ҏD��D�R�D˒�D�ҏD��D�R�D̒�D�ҏD��D�R�D͒�D�ҏD��D�R�DΒ�D�ҏD��D�R�Dϒ�D�ҏD��D�R�DВ�D�ҏD��D�R�Dђ�D�ҏD��D�R�DҒ�D�ҏD��D�R�DӒ�D�ҏD��D�R�DԒ�D�ҏD��D�R�DՒ�D�ҏD��D�R�D֒�D�ҏD��D�R�Dג�D�ҏD��D�R�Dؒ�D�ҏD��D�O\Dْ�D�ҏD��D�R�Dڒ�D�ҏD��D�R�Dے�D��\D��D�R�Dܒ�D�ҏD��D�R�Dݒ�D�ҏD��D�R�Dޒ�D�ҏD��D�R�Dߒ�D�ҏD��D�R�D���D�ҏD��D�R�DᒏD�ҏD��D�R�D⒏D�ҏD��D�R�D㒏D�ҏD��D�R�D䒏D�ҏD��D�R�D咏D�ҏD��D�R�D撏D�ҏD��D�R�D璏D�ҏD��D�R�D蒏D�ҏD��D�R�D钏D�ҏD��D�R�D꒏D�ҏD��D�R�D뒏D�ҏD��D�R�D쒏D�ҏD��D�R�D풏D�ҏD��D�R�DD�ҏD��D�R�DD�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�U�D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ĜA���A��+A�v�A�n�A�bNA�S�A�7LA�%A��A���A��wA���A��A�x�A�v�A�x�A�x�A�x�A�x�A�x�A�p�A�hsA�l�A�XA�G�A�C�A�A�A�?}A�?}A�?}A�=qA�;dA�/A� �A�bA���A��A��A��A��A��A��A��A��A��
A��9A�v�A�Q�A�9XA�A��A��uA�ffA�E�A��A���A��-A���A��A��!A��RA��wA���A��wA�/A��!A��hA��A��A�I�A��A�ffA��A���A�33A�bA��jA�XA�I�A�O�A���A�A���A�?}A�O�A��;A��DA���A��+A�G�A�S�A�hsA��A��FA��`A�r�A��!A��#A�VA��TA�hsA��^A�bNA�l�A�I�A��A�n�A�n�A�ȴA�l�A���A�+A�ƨA�A��#A��A�ȴA�`BA�bNA�x�A�VA�=qA}�FA}`BA}33Ay�^At�As�Ao�FAk/Ag��AdjA`9XA]��A\�A\~�A[t�AYoAT��AS�7AQ�-AMS�AKAI33AH�+AG��AF�AE;dAC�ABbAA|�A@��A>~�A=`BA=VA<5?A;G�A;"�A:bA8��A8M�A85?A85?A7O�A6�A5;dA4�\A3hsA2��A1�7A/S�A-��A-VA,bA*�DA)��A'�A&�A$��A$ffA"��A!��A �RAx�A�A��A�A�A"�A�!A-A+A��A�!A1AC�AZA�^A-AVA1'A;dA�-A�HA9XA��AK�A
�`A
$�A	K�A	
=A�+A�A
=A��A�AS�A�/A�^A
=AM�Ap�A�A �A   @�33@�n�@���@�G�@�&�@�j@��\@��@���@��@�
=@��@�Ĝ@�P@�Ĝ@�9X@���@�C�@�M�@���@�9X@�n�@�7@���@�5?@㝲@�"�@�n�@�hs@�I�@߮@�@�^5@�%@�r�@۝�@�
=@٩�@׍P@���@�|�@ҟ�@мj@�+@��@��`@˾w@�J@Ȭ@�K�@�@�Z@���@��@���@��@�?}@��/@���@� �@���@��`@�1'@�C�@�`B@���@���@�1'@��w@�\)@�n�@���@�hs@�/@�Ĝ@�I�@���@���@��@�p�@�p�@�Ĝ@�|�@���@��+@�@���@�&�@��j@�A�@��
@���@�C�@��R@��@���@�%@�9X@���@�l�@�\)@�+@���@��!@�v�@�-@�X@���@��j@��@���@��D@�j@�Q�@�1'@��@�\)@�+@���@��y@���@���@��T@�O�@�/@���@�Ĝ@���@�I�@�1'@��;@��F@���@�;d@�
=@���@��H@���@�ff@�V@�E�@��#@��7@�`B@���@�z�@�r�@�j@�9X@���@��P@�
=@�~�@���@��h@�`B@�/@���@�Z@��@���@�ȴ@��!@���@�v�@�=q@��@��@��h@��@���@��@��/@��9@� �@��;@��@��P@�t�@�dZ@�K�@���@��!@�v�@�n�@��@��@��#@���@��7@�`B@�O�@�V@�%@���@���@���@�V@��@���@���@���@��@��@�  @�  @��;@���@��@�;d@��@���@���@��R@��\@���@���@��@�/@�1'@�ƨ@�K�@�+@��@�
=@��@��R@��\@�~�@��+@�^5@�E�@�@��T@��T@���@��#@���@�/@���@��j@�bN@� �@�  @��m@��w@�\)@�ȴ@��\@�v�@�^5@�=q@��T@���@��h@�x�@�`B@�X@�O�@���@���@���@�b@��
@���@��w@���@�|�@�l�@�C�@��@�@��H@���@�^5@�@�x�@�7L@�/@�/@��@���@��j@�z�@�9X@�@�@�@~��@~V@~@}�@}@}��@}�h@}�h@}�@}/@|�/@|Z@{��@{ƨ@{�F@{��@z��@zM�@y�7@y�@x��@x��@x�9@x�@x1'@w�;@w;d@vȴ@v�+@u@u/@t�j@tz�@s�F@s33@r�H@rJ@qG�@p�`@p��@p�9@p��@pQ�@o+@n�@n$�@m@m�@mO�@l�@l�D@k�
@kt�@j��@jn�@i��@iX@h�`@h��@hbN@h  @g�w@g;d@f�R@f�+@fff@fV@f@ep�@e`B@eV@d�/@d��@d��@d��@dz�@d(�@cƨ@cƨ@c�F@ct�@cS�@c@b��@b��@bn�@b�@ax�@`��@`�@`  @_��@_K�@_
=@^��@^��@^�@^ȴ@^��@^ff@]@]�-@]�@]/@\�/@\z�@\(�@\1@[ƨ@[�@["�@Z��@Z��@Z^5@Z=q@Y��@Y��@Y7L@X�9@XbN@X �@Wl�@W
=@V�@V�+@Vv�@VV@V{@U��@U��@T��@T�@T�D@T(�@S�m@S�@S@R��@Rn�@Q��@Q��@Qx�@Q7L@P�9@P�@PA�@O�@O��@Ol�@OK�@N��@N��@N$�@M�@M�-@MV@L��@L�j@L�@L��@L9X@K�F@K��@KdZ@KC�@K"�@J�@J�\@J^5@J�@I��@I��@I7L@I%@HĜ@H�u@H1'@G��@G�@Fȴ@F��@Fv�@F{@E�T@E��@E�h@EO�@E�@D�@D�/@D��@D�D@D9X@C�
@Cƨ@C�F@C�@Ct�@CC�@B��@A�#@A�^@A�^@A�7@AG�@@Ĝ@@bN@@Q�@@1'@@ �@@b@?��@?;d@?+@?
=@>�y@>�@>�R@>V@>{@>@=�T@=`B@<�@<�@<�D@<z�@<9X@;��@;�@;dZ@;S�@;"�@:��@:J@9�^@9hs@9&�@9%@8Ĝ@8Q�@7�;@7�w@7�P@7;d@6�y@6�+@6V@6{@5��@5�h@4�@4j@3�m@3�m@3�m@3��@3�
@3�F@3dZ@2�H@2^5@2-@1��@1��@1hs@1&�@0��@0�`@0Q�@/�w@/�P@/K�@/+@/�@.ȴ@.��@.v�@.$�@-��@-@-�@-`B@-`B@-?}@-V@,�@,�@,Z@,(�@+��@+�m@+�
@+��@+S�@+"�@*�H@*��@*��@*�\@*=q@*�@*J@)��@)��@)x�@)7L@(��@(�9@(Q�@(1'@( �@(  @'��@'\)@'�@&�y@&�+@&V@&{@%�T@%��@%p�@%p�@%`B@%`B@%/@$��@$�@$�D@$j@$I�@$9X@$9X@$9X@$(�@$�@#�F@#t�@#S�@#S�@#S�@#33@"�H@"~�@"-@!�@!��@!&�@!�@ �`@ ��@ bN@ A�@ 1'@��@�P@+@�y@V@{@�@��@?}@�@�/@��@�D@j@(�@�m@��@C�@33@@�H@�!@n�@^5@M�@�@J@��@��@�7@X@7L@�@�`@��@Ĝ@�9@�u@Q�@�@�w@�@|�@l�@\)@;d@;d@
=@��@�@��@�+@v�@V@5?@$�@�@�@�@V@�@�j@�@z�@9X@1@�
@S�@�H@��@n�@=q@��@��@�#@hs@�@Ĝ@�@A�@b@�;@|�@K�@�@�y@�@ȴ@��@v�@V@@��@�h@`B@?}@V@��@�/@�j@�D@I�@1@1@1@��@�
@�F@��@33@
�@
��@
��@
��@
n�@
J@	x�@	G�@	7L@	%@��@��@��@�9@�@b@�;@�P@l�@K�@K�@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ĜA���A��+A�v�A�n�A�bNA�S�A�7LA�%A��A���A��wA���A��A�x�A�v�A�x�A�x�A�x�A�x�A�x�A�p�A�hsA�l�A�XA�G�A�C�A�A�A�?}A�?}A�?}A�=qA�;dA�/A� �A�bA���A��A��A��A��A��A��A��A��A��
A��9A�v�A�Q�A�9XA�A��A��uA�ffA�E�A��A���A��-A���A��A��!A��RA��wA���A��wA�/A��!A��hA��A��A�I�A��A�ffA��A���A�33A�bA��jA�XA�I�A�O�A���A�A���A�?}A�O�A��;A��DA���A��+A�G�A�S�A�hsA��A��FA��`A�r�A��!A��#A�VA��TA�hsA��^A�bNA�l�A�I�A��A�n�A�n�A�ȴA�l�A���A�+A�ƨA�A��#A��A�ȴA�`BA�bNA�x�A�VA�=qA}�FA}`BA}33Ay�^At�As�Ao�FAk/Ag��AdjA`9XA]��A\�A\~�A[t�AYoAT��AS�7AQ�-AMS�AKAI33AH�+AG��AF�AE;dAC�ABbAA|�A@��A>~�A=`BA=VA<5?A;G�A;"�A:bA8��A8M�A85?A85?A7O�A6�A5;dA4�\A3hsA2��A1�7A/S�A-��A-VA,bA*�DA)��A'�A&�A$��A$ffA"��A!��A �RAx�A�A��A�A�A"�A�!A-A+A��A�!A1AC�AZA�^A-AVA1'A;dA�-A�HA9XA��AK�A
�`A
$�A	K�A	
=A�+A�A
=A��A�AS�A�/A�^A
=AM�Ap�A�A �A   @�33@�n�@���@�G�@�&�@�j@��\@��@���@��@�
=@��@�Ĝ@�P@�Ĝ@�9X@���@�C�@�M�@���@�9X@�n�@�7@���@�5?@㝲@�"�@�n�@�hs@�I�@߮@�@�^5@�%@�r�@۝�@�
=@٩�@׍P@���@�|�@ҟ�@мj@�+@��@��`@˾w@�J@Ȭ@�K�@�@�Z@���@��@���@��@�?}@��/@���@� �@���@��`@�1'@�C�@�`B@���@���@�1'@��w@�\)@�n�@���@�hs@�/@�Ĝ@�I�@���@���@��@�p�@�p�@�Ĝ@�|�@���@��+@�@���@�&�@��j@�A�@��
@���@�C�@��R@��@���@�%@�9X@���@�l�@�\)@�+@���@��!@�v�@�-@�X@���@��j@��@���@��D@�j@�Q�@�1'@��@�\)@�+@���@��y@���@���@��T@�O�@�/@���@�Ĝ@���@�I�@�1'@��;@��F@���@�;d@�
=@���@��H@���@�ff@�V@�E�@��#@��7@�`B@���@�z�@�r�@�j@�9X@���@��P@�
=@�~�@���@��h@�`B@�/@���@�Z@��@���@�ȴ@��!@���@�v�@�=q@��@��@��h@��@���@��@��/@��9@� �@��;@��@��P@�t�@�dZ@�K�@���@��!@�v�@�n�@��@��@��#@���@��7@�`B@�O�@�V@�%@���@���@���@�V@��@���@���@���@��@��@�  @�  @��;@���@��@�;d@��@���@���@��R@��\@���@���@��@�/@�1'@�ƨ@�K�@�+@��@�
=@��@��R@��\@�~�@��+@�^5@�E�@�@��T@��T@���@��#@���@�/@���@��j@�bN@� �@�  @��m@��w@�\)@�ȴ@��\@�v�@�^5@�=q@��T@���@��h@�x�@�`B@�X@�O�@���@���@���@�b@��
@���@��w@���@�|�@�l�@�C�@��@�@��H@���@�^5@�@�x�@�7L@�/@�/@��@���@��j@�z�@�9X@�@�@�@~��@~V@~@}�@}@}��@}�h@}�h@}�@}/@|�/@|Z@{��@{ƨ@{�F@{��@z��@zM�@y�7@y�@x��@x��@x�9@x�@x1'@w�;@w;d@vȴ@v�+@u@u/@t�j@tz�@s�F@s33@r�H@rJ@qG�@p�`@p��@p�9@p��@pQ�@o+@n�@n$�@m@m�@mO�@l�@l�D@k�
@kt�@j��@jn�@i��@iX@h�`@h��@hbN@h  @g�w@g;d@f�R@f�+@fff@fV@f@ep�@e`B@eV@d�/@d��@d��@d��@dz�@d(�@cƨ@cƨ@c�F@ct�@cS�@c@b��@b��@bn�@b�@ax�@`��@`�@`  @_��@_K�@_
=@^��@^��@^�@^ȴ@^��@^ff@]@]�-@]�@]/@\�/@\z�@\(�@\1@[ƨ@[�@["�@Z��@Z��@Z^5@Z=q@Y��@Y��@Y7L@X�9@XbN@X �@Wl�@W
=@V�@V�+@Vv�@VV@V{@U��@U��@T��@T�@T�D@T(�@S�m@S�@S@R��@Rn�@Q��@Q��@Qx�@Q7L@P�9@P�@PA�@O�@O��@Ol�@OK�@N��@N��@N$�@M�@M�-@MV@L��@L�j@L�@L��@L9X@K�F@K��@KdZ@KC�@K"�@J�@J�\@J^5@J�@I��@I��@I7L@I%@HĜ@H�u@H1'@G��@G�@Fȴ@F��@Fv�@F{@E�T@E��@E�h@EO�@E�@D�@D�/@D��@D�D@D9X@C�
@Cƨ@C�F@C�@Ct�@CC�@B��@A�#@A�^@A�^@A�7@AG�@@Ĝ@@bN@@Q�@@1'@@ �@@b@?��@?;d@?+@?
=@>�y@>�@>�R@>V@>{@>@=�T@=`B@<�@<�@<�D@<z�@<9X@;��@;�@;dZ@;S�@;"�@:��@:J@9�^@9hs@9&�@9%@8Ĝ@8Q�@7�;@7�w@7�P@7;d@6�y@6�+@6V@6{@5��@5�h@4�@4j@3�m@3�m@3�m@3��@3�
@3�F@3dZ@2�H@2^5@2-@1��@1��@1hs@1&�@0��@0�`@0Q�@/�w@/�P@/K�@/+@/�@.ȴ@.��@.v�@.$�@-��@-@-�@-`B@-`B@-?}@-V@,�@,�@,Z@,(�@+��@+�m@+�
@+��@+S�@+"�@*�H@*��@*��@*�\@*=q@*�@*J@)��@)��@)x�@)7L@(��@(�9@(Q�@(1'@( �@(  @'��@'\)@'�@&�y@&�+@&V@&{@%�T@%��@%p�@%p�@%`B@%`B@%/@$��@$�@$�D@$j@$I�@$9X@$9X@$9X@$(�@$�@#�F@#t�@#S�@#S�@#S�@#33@"�H@"~�@"-@!�@!��@!&�@!�@ �`@ ��@ bN@ A�@ 1'@��@�P@+@�y@V@{@�@��@?}@�@�/@��@�D@j@(�@�m@��@C�@33@@�H@�!@n�@^5@M�@�@J@��@��@�7@X@7L@�@�`@��@Ĝ@�9@�u@Q�@�@�w@�@|�@l�@\)@;d@;d@
=@��@�@��@�+@v�@V@5?@$�@�@�@�@V@�@�j@�@z�@9X@1@�
@S�@�H@��@n�@=q@��@��@�#@hs@�@Ĝ@�@A�@b@�;@|�@K�@�@�y@�@ȴ@��@v�@V@@��@�h@`B@?}@V@��@�/@�j@�D@I�@1@1@1@��@�
@�F@��@33@
�@
��@
��@
��@
n�@
J@	x�@	G�@	7L@	%@��@��@��@�9@�@b@�;@�P@l�@K�@K�@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��BhB-BA�BM�BbNBy�B��B��B��B��B�B�3B�?B�LB�XB�wB��BĜBƨBȴB��B�BB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��BB��BB%BVBoB�B�B"�B(�B/B%�B&�B(�B+B1'B7LB<jBO�BZB_;B^5BcTBgmBW
BE�B8RB2-B/B1'B8RB;dB=qB=qBA�BA�BE�BE�BD�BG�B?}B/B&�B!�B �B�BDB��B�B�HB�
B��B��B�JBv�Bl�B\)BR�BD�B5?B/B-B�BoB
=B  B
�fB
�)B
��B
�dB
��B
�B
jB
I�B
7LB
5?B
(�B
{B
uB
bB
  B	�B	��B	�LB	�uB	x�B	bNB	A�B	.B	%�B	#�B	�B	oB��B�B�fB��BȴB�wB�}B��B��B�wB�RB�'B�B�B��B��B��B��B��B��B��B�bB�JB�VB�oB�VB�=B�7B�+B�B�%B� Bp�BgmBcTBbNB^5B_;BZBVBR�BP�BQ�BN�BN�BK�BJ�BI�BG�BF�BD�BD�BB�BA�BC�B?}B>wB;dB9XB8RB8RB6FB5?B5?B33B33B1'B1'B1'B0!B0!B0!B/B/B/B-B-B,B,B+B,B+B+B+B)�B+B,B,B,B,B,B+B,B-B,B+B)�B)�B)�B)�B+B-B,B+B+B+B/B/B0!B0!B6FB:^B=qB<jB=qB=qB<jB;dB<jB=qB?}B?}BE�BC�BE�BC�BF�BJ�BL�BH�BI�BJ�BK�BN�BK�BI�BI�BK�BM�BQ�BS�BS�BS�BS�BS�BS�BW
B\)B^5B_;BdZBiyBjBl�Bo�Bo�Bq�Bt�Bw�By�By�By�B{�B|�B}�B�B�B�%B�1B�JB�JB�VB�\B�bB�oB�uB��B��B��B��B��B��B��B��B�B�B�B�B�'B�3B�9B�?B�LB�qB��BBBBÖBŢBƨBǮB��B��B��B��B��B�B�
B�NB�B�B�B�B�B��B��B��B��B��B	B	B	B	%B	
=B	JB	JB	JB	bB	uB	{B	�B	�B	�B	�B	�B	 �B	"�B	'�B	,B	0!B	2-B	49B	5?B	7LB	;dB	?}B	F�B	G�B	I�B	I�B	L�B	O�B	Q�B	S�B	VB	ZB	\)B	]/B	]/B	]/B	bNB	dZB	ffB	gmB	hsB	hsB	iyB	m�B	p�B	t�B	v�B	z�B	}�B	~�B	�B	�=B	�VB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�-B	�9B	�LB	�XB	�^B	�XB	�dB	�qB	�jB	�dB	�dB	�jB	�}B	��B	��B	ÖB	ÖB	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�;B	�BB	�HB	�HB	�HB	�HB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
1B

=B

=B
DB
DB
JB
JB
JB
PB
VB
VB
\B
bB
bB
hB
hB
hB
hB
oB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
.B
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
33B
33B
49B
49B
5?B
5?B
5?B
5?B
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
7LB
8RB
7LB
7LB
8RB
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
;dB
;dB
;dB
;dB
<jB
<jB
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
@�B
@�B
@�B
@�B
@�B
A�B
A�B
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
E�B
E�B
F�B
F�B
F�B
F�B
F�B
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
I�B
J�B
J�B
J�B
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
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
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
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B4B,�BAUBM�BbBy�B�kB��B��B��B��B��B�B�B�$B�BB�OB�gB�tBȀBΥB�B�DB�QB�]B�]B�cB�iB�oB�vB�vB�|B�B�B�|B�B�B��B��B��B��B��B��B��B��B �B��B�B�B"B:BMBkB"�B(�B.�B%�B&�B(�B*�B0�B7B<6BO�BY�B_B^Bc Bg8BV�BEmB8B1�B.�B0�B8B;0B=<B=<BAUBAUBEmBEmBDgBGzB?HB.�B&�B!�B �BYBB��B�]B�BּB�UB��B��Bv�Bl=B[�BR�BDgB5B.�B,�BpB:B
	B
��B
�2B
��B
ϫB
�0B
��B
��B
jKB
I�B
7B
4�B
(�B
FB
&B
.B	��B	��B	ʌB	��B	�@B	x�B	bB	A;B	-�B	%�B	#�B	~B	:B��B�iB�BϫBȀB�BB�HB�UB�UB�BB�B��B��B��B��B�dB�~B�WB�kB�EB�YB�.B��B�B�:B�"B��B��B��B��B��B�BpoBgBcBbB^B^�BY�BU�BR�BP�BQ�BN�BN�BKxBJrBIlBG_BFYBDMBDgBBABAUBCaB?HB>BB;B9	B8B8B6B5B5B2�B2�B0�B0�B0�B/�B/�B/�B.�B.�B.�B,�B,�B+�B+�B*�B+�B*�B*�B*�B)�B*�B+�B+�B+�B+�B+�B*�B+�B,�B+�B*�B)�B)�B)�B)�B*�B,�B+�B*�B*�B*�B.�B.�B/�B/�B6B:*B=<B<B="B=<B<B;B<B="B?.B?HBESBCaBESBCGBFYBJrBL~BHfBIlBJrBKxBN�BKxBIlBIlBKxBM�BQ�BS�BS�BS�BS�BS�BS�BV�B[�B]�B^�BdBi*Bj0BlWBoiBoOBq[BtnBw�By�By�By�B{�B|�B}�B��B��B��B��B��B��B�B�B�.B�:B�&B�9B�EB�EB�kB�dB��B��B��B��B��B��B��B��B��B��B�B��B�"B�;B�AB�AB�AB�aB�SB�YB�_B�~BϑBѝBөBөBյBּB�B�6B�CB�OB�[B�hB��B��B��B��B��B	�B	�B	�B	�B		�B	�B	�B	�B	B	&B	FB	KB	WB	WB	WB	dB	 �B	"�B	'�B	+�B	/�B	1�B	4B	5B	6�B	;B	?.B	FYB	G_B	IlB	IlB	L~B	O�B	Q�B	S�B	U�B	Y�B	[�B	\�B	\�B	\�B	a�B	dB	fB	g8B	h$B	h$B	i*B	mCB	pUB	tnB	vzB	z�B	}�B	~�B	��B	��B	�B	�B	�&B	�9B	�EB	�KB	�WB	�]B	��B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�$B	�B	�"B	�B	�0B	�B	�B	�.B	�4B	�;B	�GB	�GB	�GB	�YB	�fB	�rB	�~B	ΊB	ϑB	ңB	��B	յB	ּB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�$B	�*B	�*B	�0B	�6B	�=B	�=B	�=B	�CB	�IB	�cB	�OB	�UB	�[B	�[B	�[B	�[B	�aB	�aB	�hB	�nB	�tB	�tB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

	B

�B

�B
�B
�B
�B
B
B
B
(B
B
B
4B
B
B
4B
 B
&B
&B
&B
&B
FB
,B
,B
2B
2B
2B
2B
2B
2B
9B
9B
9B
SB
?B
?B
EB
EB
EB
EB
EB
KB
QB
QB
QB
WB
WB
qB
]B
]B
]B
xB
]B
xB
dB
dB
dB
dB
jB
jB
�B
pB
�B
pB
pB
 vB
 vB
 vB
 vB
!|B
!|B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
-�B
.�B
.�B
.�B
.�B
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
1�B
1�B
1�B
2�B
2�B
2�B
2�B
3�B
3�B
4�B
5B
5B
4�B
4�B
4�B
4�B
6B
6B
6B
5�B
5�B
5�B
6�B
6�B
6�B
7B
8B
6�B
6�B
8B
9	B
9	B
9	B
9	B
9	B
:B
:B
:B
:B
:B
:B
;0B
;B
;B
;0B
;B
;B
;0B
<B
<B
<B
<B
=<B
=<B
="B
=<B
="B
>BB
>(B
>(B
?.B
?.B
?.B
?.B
@4B
@4B
@4B
@4B
@4B
A;B
A;B
BAB
BAB
BAB
BAB
B[B
CaB
CaB
CGB
CGB
CGB
DMB
DgB
DMB
ESB
EmB
FYB
FYB
FYB
FYB
FYB
FYB
FYB
FYB
FYB
G_B
GzB
GzB
G_B
HfB
HfB
HfB
H�B
I�B
JrB
JrB
J�B
KxB
KxB
L~B
L�B
L~B
L�B
L~B
L~B
L�B
L~B
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
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
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
\�B
\�B
\�B
\�B
^B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
_B
_B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
`B
_�B
_�B
`�B
`�B
`�B
`�B
aB
a�B
a�B
bB
a�B
a�B
a�B
bB
a�B
cB
cB
c B
cB
cB
cB
cB
dB
dB
d&B
dB
dB
d&B
d&B
eB
eB
eB
fB
fB
fB
gB
gB
gB
gB
gB
h$B
h>B
h$B
h$B
iDB
iDB
iDB
jKB
j0B
j0B
j0B
jKB
j0B
k6B
k6B
k6B
k6B
k6B
k6B
l=B
l=B
l=B
l=B
l=B
l=B
l=B
mCB
mCB
mCB
mCB
mCB
mCB
mCB
mCB
nIB
ncB
nIB
ncB
nIB
nIB
oOB
oOB
oiB
oOB
oOB
pUB
pUB
oOB
poB
poB
poB
q[B
q[B
q[B
qvB
q[B
q[11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.58(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201903010034102019030100341020190301003410201903020022092019030200220920190302002209JA  ARFMdecpA19c                                                                20190224213628  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190224124019  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190224124021  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190224124021  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190224124022  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190224124022  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190224124022  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190224124022  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190224124022  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190224124022                      G�O�G�O�G�O�                JA  ARUP                                                                        20190224125637                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190224154555  CV  JULD            G�O�G�O�F�O�                JM  ARCAJMQC2.0                                                                 20190228153410  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190228153410  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190301152209  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                