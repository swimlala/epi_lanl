CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-12-30T15:36:50Z creation;2018-12-30T15:36:54Z conversion to V3.1;2019-12-23T06:09:27Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181230153650  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               rA   JA  I2_0675_114                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؜�� 1   @؜�d� @7�*�0��c7��҉1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D���D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@ҏ\A	G�A'�AIG�AiG�A���A���A���A���Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2Q�B:Q�BBQ�BJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�{C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�{CF�{CH�{CJ�{CL�{CN�{CP�{CR�{CT�{CV�{CX�{CZ�{C\�{C^�{C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�=qC�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�Du%Du�Dv%Dv�Dw%Dw�Dx%Dx�Dy%Dy�Dz%Dz�D{%D{�D|%D|�D}%D}�D~%D~�D%D�D��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D�D�ҏD��D�R�DÒ�D�ҏD�\D�R�DĒ�D�ҏD��D�R�DŒ�D�ҏD��D�R�Dƒ�D�ҏD��D�R�Dǒ�D�ҏD��D�R�DȒ�D�ҏD��D�R�Dɒ�D�ҏD��D�R�Dʒ�D�ҏD��D�R�D˒�D�ҏD��D�R�D̒�D�ҏD��D�R�D͒�D�ҏD��D�R�DΒ�D�ҏD��D�R�Dϒ�D�ҏD��D�R�DВ�D�ҏD��D�R�Dђ�D�ҏD��D�R�DҒ�D�ҏD��D�R�DӒ�D�ҏD��D�R�DԒ�D�ҏD��D�R�DՒ�D�ҏD��D�R�D֒�D�ҏD��D�R�Dג�D�ҏD��D�R�Dؒ�D�ҏD��D�R�Dْ�D�ҏD��D�R�Dڒ�D�ҏD��D�R�Dے�D�ҏD��D�R�Dܒ�D�ҏD��D�R�Dݒ�D�ҏD��D�R�Dޒ�D�ҏD��D�R�Dߒ�D�ҏD��D�R�D���D�ҏD��D�R�DᒏD�ҏD��D�R�D⒏D�ҏD��D�R�D㒏D�ҏD��D�R�D䒏D�ҏD��D�R�D咏D�ҏD��D�R�D撏D�ҏD��D�U�D璏D�ҏD��D�R�D蒏D�ҏD��D�R�D钏D�ҏD��D�R�D꒏D�ҏD��D�R�D뒏D�ҏD��D�R�D쒏D�ҏD��D�R�D풏D�ҏD��D�U�DD�ҏD��D�R�DD�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�_\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aß�Aç�Aå�Aå�Aç�Aé�AîAîAé�A�t�A�`BA�r�A���A��A��A��A��AþwA�t�A�A�A�;dA���A�A���A�{A�$�A�A�A�;dAPA���A¡�A�$�A��9A���A��7A�VA��A�l�A��uA��hA��A��A�ĜA��TA��A�7LA��9A��-A�1'A��hA���A�G�A�bNA��FA�K�A�I�A�r�A�?}A�VA���A�t�A�9XA��A�33A��`A���A��A�l�A��A�G�A��yA���A��^A�Q�A�XA�n�A�"�A���A�~�A�A�A���A�p�A��
A�Q�A�33A��A��A��A�"�A���A���A��A�O�A�A�%A�v�A��mA��hA�7LA�A�"�A���A��yA��HA�5?A��\A���A���A�K�A��wA�ZA��A~ZAz�Ax1'Au��At �Ar��Ar��Apn�Am�Al=qAh�`AgG�Af�+Ae�AbȴA`��A`9XA`-A_�-A\�jAZ~�AX  AT�AQ�FAP1'ANbNAMC�AK��AKS�AK+AJ�jAI�AHA�AGAG�hAF��AE"�AD�AB�`AB �AA��AA��AAx�A@�\A=�7A;�TA:(�A9p�A9%A8�A7K�A6z�A4�A3�A3C�A2^5A17LA0�uA/�
A.z�A-VA+��A*{A(��A'x�A&M�A%�
A%33A#�
A"jA!oA�hAA  A��A�A�A�AffA��A��A-A�^A��A �A�RA\)AA�A�^A7LA1'A��A
��A	��A��A9XA�A��AdZAA��AJA�#A�FA�A �A�A`BAr�A��A �RA  �@���@��@��H@��@�I�@���@��y@�E�@��@�Z@�\@�7L@��@�+@�$�@�9@�1@�dZ@���@�v�@�-@�?}@��/@�u@�Q�@�t�@�^5@�`B@��@�G�@��@��@��H@�E�@��#@�%@׮@���@���@���@���@�
=@�7L@�Z@��;@��y@Ͳ-@��/@�I�@��@Ɂ@ȓu@��@�v�@ģ�@\@��@�%@�\)@���@�9X@��;@��@���@�M�@���@��`@��
@���@�33@�^5@�O�@��9@��u@�1'@��@���@�"�@���@���@�hs@�hs@�/@�j@��;@�
=@�$�@��-@��@��u@�A�@���@�t�@�C�@��y@��\@�V@��T@��-@��7@�/@���@�9X@��;@�dZ@���@���@��\@���@�hs@�G�@���@�j@�Q�@�1'@�b@���@��@�dZ@�33@�
=@�ȴ@��+@�$�@��@���@�&�@���@��@��u@�z�@��@�@��R@��\@���@��@�X@�V@���@���@�9X@��;@�|�@�+@�o@���@�V@�-@��@�@��7@�O�@�&�@��`@���@�Z@�Q�@�(�@��;@��@�|�@�K�@��@�@��H@���@�n�@�V@�J@�X@�7L@��@�%@��/@���@��u@��u@��;@�;d@��@�o@�ȴ@���@�hs@�7L@�Ĝ@���@���@��D@�r�@�Z@�9X@�I�@�j@�(�@��
@��@�dZ@��y@�S�@�C�@���@���@�ff@�E�@���@�-@�@��#@���@��T@��#@��T@��-@���@�hs@�x�@��@���@��@��u@�A�@���@��m@���@�t�@�S�@�"�@��@�M�@��@��-@��@��@���@���@��@���@��u@�r�@�A�@��@��m@��
@���@�C�@�o@���@���@�n�@�5?@���@��^@���@��@�p�@�hs@�/@���@��D@�bN@�9X@� �@��;@�ƨ@��
@�ƨ@��w@��
@���@��@��@���@�dZ@�l�@�dZ@�S�@�+@�
=@���@���@�n�@�=q@�{@��T@��#@���@��-@�p�@��@��`@�Ĝ@���@�j@� �@��@~�y@~��@~��@~v�@}�T@|�D@{ƨ@{"�@z�@z��@z-@y��@yX@x��@x�9@x�u@xQ�@w��@wl�@wK�@w
=@v��@v�+@vff@vff@vff@v5?@v@u�-@t��@t�@s�F@s��@sS�@sC�@r�H@r~�@r�@q��@q��@q�7@q7L@p��@p�@p �@o�@o|�@o;d@o
=@n��@n5?@m��@m?}@lz�@l�@k��@kƨ@k33@j��@j��@jJ@ix�@i7L@h��@hĜ@h��@h�u@h�@hr�@hbN@hb@g�@g|�@gK�@f�+@e�@e�@d�@d�@dj@dI�@d9X@d�@cƨ@ct�@c33@b��@b��@bM�@b�@a��@a��@`��@`�9@`��@`�9@`��@`��@`�u@`r�@_�;@_
=@^��@^v�@^V@^V@^{@]�T@]�-@]p�@]`B@]O�@]/@\��@\�/@\�@\�D@\z�@\z�@\j@\I�@[�m@[S�@Z�H@Z�H@Z��@Z��@Z^5@Y�^@YG�@XĜ@XQ�@W�@W�;@W|�@V��@VE�@V@U@U�@U?}@T�@T��@T(�@S�
@S�F@S"�@R�\@Rn�@R=q@RJ@Q�@Q��@Qx�@QG�@Q%@PĜ@O�@O|�@O\)@O;d@O�@O
=@N�+@N$�@M@M�@L�@LI�@K�F@K33@K33@K"�@K"�@K"�@Ko@K@J�@J��@J~�@JM�@J�@I�#@I&�@I%@H��@H�`@H�u@HA�@G�@G|�@G+@F�R@F5?@E�T@E��@E/@D�j@D�@Dz�@D�@Cƨ@C33@B�!@B~�@B^5@B-@A��@A�7@AG�@@��@@Ĝ@@bN@@b@?�w@?��@?l�@?�@>�y@>�+@>V@>{@=��@=�@=?}@<�@<�j@<��@<z�@<Z@<(�@;ƨ@;��@;dZ@:��@:n�@9��@9��@9��@9�7@9G�@8��@8�9@8�9@8�u@8�@8�@8A�@7�@6�@6ȴ@6��@6E�@5�@5�@5/@5V@4��@4I�@3��@3�@3C�@2�H@2��@2��@2~�@2n�@2M�@2=q@2-@2-@2�@1�#@1�7@1x�@1hs@17L@0��@0bN@0  @/��@/;d@/�@/�@.��@.�y@.�@.ȴ@.��@.�+@.ff@.E�@.5?@.{@-�@-�@,�@,��@,z�@,I�@,9X@,1@+�
@+ƨ@+�@+33@*��@*�\@*^5@*=q@)�@)�7@)�7@)X@)7L@)%@(Ĝ@(�u@(bN@(A�@( �@(b@'�w@'�P@'�P@'K�@&�y@&v�@&$�@%?}@$�j@$�@$j@$�@#�
@#�F@#��@#t�@#"�@"�H@"��@"�!@"�\@"�@"J@!�@!�#@!�^@!x�@!%@ �u@ r�@ Q�@ b@��@|�@\)@;d@
=@�@ȴ@�+@ff@V@E�@E�@5?@?}@�j@j@(�@��@�m@�
@�F@�@C�@�@�\@�@��@�7@x�@hs@7L@7L@%@��@bN@ �@�@�@|�@�@
=@��@�y@�y@ȴ@�R@��@�+@�+@V@5?@@�h@/@�/@��@��@z�@z�@Z@9X@(�@(�@(�@��@��@��@33@"�@�@�!@~�@-@�@�7@hs@X@G�@&�@��@��@�u@�u@bN@A�@�;@|�@K�@�@��@�y@�@ȴ@��@�+@V@5?@�T@@p�@?}@��@�j@��@z�@Z@I�@I�@(�@�@�m@�@33@"�@o@o@o@@
�@
�H@
��@
��@
n�@
^5@
M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aß�Aç�Aå�Aå�Aç�Aé�AîAîAé�A�t�A�`BA�r�A���A��A��A��A��AþwA�t�A�A�A�;dA���A�A���A�{A�$�A�A�A�;dAPA���A¡�A�$�A��9A���A��7A�VA��A�l�A��uA��hA��A��A�ĜA��TA��A�7LA��9A��-A�1'A��hA���A�G�A�bNA��FA�K�A�I�A�r�A�?}A�VA���A�t�A�9XA��A�33A��`A���A��A�l�A��A�G�A��yA���A��^A�Q�A�XA�n�A�"�A���A�~�A�A�A���A�p�A��
A�Q�A�33A��A��A��A�"�A���A���A��A�O�A�A�%A�v�A��mA��hA�7LA�A�"�A���A��yA��HA�5?A��\A���A���A�K�A��wA�ZA��A~ZAz�Ax1'Au��At �Ar��Ar��Apn�Am�Al=qAh�`AgG�Af�+Ae�AbȴA`��A`9XA`-A_�-A\�jAZ~�AX  AT�AQ�FAP1'ANbNAMC�AK��AKS�AK+AJ�jAI�AHA�AGAG�hAF��AE"�AD�AB�`AB �AA��AA��AAx�A@�\A=�7A;�TA:(�A9p�A9%A8�A7K�A6z�A4�A3�A3C�A2^5A17LA0�uA/�
A.z�A-VA+��A*{A(��A'x�A&M�A%�
A%33A#�
A"jA!oA�hAA  A��A�A�A�AffA��A��A-A�^A��A �A�RA\)AA�A�^A7LA1'A��A
��A	��A��A9XA�A��AdZAA��AJA�#A�FA�A �A�A`BAr�A��A �RA  �@���@��@��H@��@�I�@���@��y@�E�@��@�Z@�\@�7L@��@�+@�$�@�9@�1@�dZ@���@�v�@�-@�?}@��/@�u@�Q�@�t�@�^5@�`B@��@�G�@��@��@��H@�E�@��#@�%@׮@���@���@���@���@�
=@�7L@�Z@��;@��y@Ͳ-@��/@�I�@��@Ɂ@ȓu@��@�v�@ģ�@\@��@�%@�\)@���@�9X@��;@��@���@�M�@���@��`@��
@���@�33@�^5@�O�@��9@��u@�1'@��@���@�"�@���@���@�hs@�hs@�/@�j@��;@�
=@�$�@��-@��@��u@�A�@���@�t�@�C�@��y@��\@�V@��T@��-@��7@�/@���@�9X@��;@�dZ@���@���@��\@���@�hs@�G�@���@�j@�Q�@�1'@�b@���@��@�dZ@�33@�
=@�ȴ@��+@�$�@��@���@�&�@���@��@��u@�z�@��@�@��R@��\@���@��@�X@�V@���@���@�9X@��;@�|�@�+@�o@���@�V@�-@��@�@��7@�O�@�&�@��`@���@�Z@�Q�@�(�@��;@��@�|�@�K�@��@�@��H@���@�n�@�V@�J@�X@�7L@��@�%@��/@���@��u@��u@��;@�;d@��@�o@�ȴ@���@�hs@�7L@�Ĝ@���@���@��D@�r�@�Z@�9X@�I�@�j@�(�@��
@��@�dZ@��y@�S�@�C�@���@���@�ff@�E�@���@�-@�@��#@���@��T@��#@��T@��-@���@�hs@�x�@��@���@��@��u@�A�@���@��m@���@�t�@�S�@�"�@��@�M�@��@��-@��@��@���@���@��@���@��u@�r�@�A�@��@��m@��
@���@�C�@�o@���@���@�n�@�5?@���@��^@���@��@�p�@�hs@�/@���@��D@�bN@�9X@� �@��;@�ƨ@��
@�ƨ@��w@��
@���@��@��@���@�dZ@�l�@�dZ@�S�@�+@�
=@���@���@�n�@�=q@�{@��T@��#@���@��-@�p�@��@��`@�Ĝ@���@�j@� �@��@~�y@~��@~��@~v�@}�T@|�D@{ƨ@{"�@z�@z��@z-@y��@yX@x��@x�9@x�u@xQ�@w��@wl�@wK�@w
=@v��@v�+@vff@vff@vff@v5?@v@u�-@t��@t�@s�F@s��@sS�@sC�@r�H@r~�@r�@q��@q��@q�7@q7L@p��@p�@p �@o�@o|�@o;d@o
=@n��@n5?@m��@m?}@lz�@l�@k��@kƨ@k33@j��@j��@jJ@ix�@i7L@h��@hĜ@h��@h�u@h�@hr�@hbN@hb@g�@g|�@gK�@f�+@e�@e�@d�@d�@dj@dI�@d9X@d�@cƨ@ct�@c33@b��@b��@bM�@b�@a��@a��@`��@`�9@`��@`�9@`��@`��@`�u@`r�@_�;@_
=@^��@^v�@^V@^V@^{@]�T@]�-@]p�@]`B@]O�@]/@\��@\�/@\�@\�D@\z�@\z�@\j@\I�@[�m@[S�@Z�H@Z�H@Z��@Z��@Z^5@Y�^@YG�@XĜ@XQ�@W�@W�;@W|�@V��@VE�@V@U@U�@U?}@T�@T��@T(�@S�
@S�F@S"�@R�\@Rn�@R=q@RJ@Q�@Q��@Qx�@QG�@Q%@PĜ@O�@O|�@O\)@O;d@O�@O
=@N�+@N$�@M@M�@L�@LI�@K�F@K33@K33@K"�@K"�@K"�@Ko@K@J�@J��@J~�@JM�@J�@I�#@I&�@I%@H��@H�`@H�u@HA�@G�@G|�@G+@F�R@F5?@E�T@E��@E/@D�j@D�@Dz�@D�@Cƨ@C33@B�!@B~�@B^5@B-@A��@A�7@AG�@@��@@Ĝ@@bN@@b@?�w@?��@?l�@?�@>�y@>�+@>V@>{@=��@=�@=?}@<�@<�j@<��@<z�@<Z@<(�@;ƨ@;��@;dZ@:��@:n�@9��@9��@9��@9�7@9G�@8��@8�9@8�9@8�u@8�@8�@8A�@7�@6�@6ȴ@6��@6E�@5�@5�@5/@5V@4��@4I�@3��@3�@3C�@2�H@2��@2��@2~�@2n�@2M�@2=q@2-@2-@2�@1�#@1�7@1x�@1hs@17L@0��@0bN@0  @/��@/;d@/�@/�@.��@.�y@.�@.ȴ@.��@.�+@.ff@.E�@.5?@.{@-�@-�@,�@,��@,z�@,I�@,9X@,1@+�
@+ƨ@+�@+33@*��@*�\@*^5@*=q@)�@)�7@)�7@)X@)7L@)%@(Ĝ@(�u@(bN@(A�@( �@(b@'�w@'�P@'�P@'K�@&�y@&v�@&$�@%?}@$�j@$�@$j@$�@#�
@#�F@#��@#t�@#"�@"�H@"��@"�!@"�\@"�@"J@!�@!�#@!�^@!x�@!%@ �u@ r�@ Q�@ b@��@|�@\)@;d@
=@�@ȴ@�+@ff@V@E�@E�@5?@?}@�j@j@(�@��@�m@�
@�F@�@C�@�@�\@�@��@�7@x�@hs@7L@7L@%@��@bN@ �@�@�@|�@�@
=@��@�y@�y@ȴ@�R@��@�+@�+@V@5?@@�h@/@�/@��@��@z�@z�@Z@9X@(�@(�@(�@��@��@��@33@"�@�@�!@~�@-@�@�7@hs@X@G�@&�@��@��@�u@�u@bN@A�@�;@|�@K�@�@��@�y@�@ȴ@��@�+@V@5?@�T@@p�@?}@��@�j@��@z�@Z@I�@I�@(�@�@�m@�@33@"�@o@o@o@@
�@
�H@
��@
��@
n�@
^5@
M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�fB
��B
=B�Bk�B�%B�PB�oB��B��B��B�B�FB��BȴB�)B��B{B�B?}B.B?}BF�B6FB$�B$�B$�B!�B�B{B�B6FBK�BI�BH�B_;BdZBt�B�B�=B�7B�DB�PB�uB�uB��B��B��B�\B�1B�Bz�BjBS�BH�BA�B?}BC�BH�BT�BK�BL�BYBaHBQ�BM�BD�B49B0!B(�B%�B"�B�B�B\B1B%BBB��B��B�yBƨB�B��B�bB�+B�B~�Bw�Br�BhsB_;BN�B;dB�B
�NB
��B
�!B
��B
|�B
p�B
jB
E�B
/B	��B	�B	�)B	��B	��B	�5B	��B	�B	��B	�1B	{�B	u�B	n�B	dZB	\)B	YB	XB	YB	C�B	0!B	)�B	hB�B�B�5B�;B�)B�/B�/B�)B�B��B��B��B�B�B�B�B��B��B��B��B��BȴBB�jB�RB�?B�-B�B�B��B��B��B��B��B�uB�oB�\B�7B�B}�Bz�Bv�Br�Bq�Bo�Bm�BgmBffBaHB^5B[#BS�BK�BF�BL�BH�BA�B>wBA�BC�BA�B:^B6FB49B2-B0!B1'B0!B.B,B,B+B)�B)�B(�B(�B'�B'�B#�B'�B'�B&�B&�B%�B%�B%�B$�B#�B"�B"�B �B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B"�B"�B#�B#�B#�B#�B#�B#�B$�B#�B$�B&�B'�B&�B'�B(�B)�B)�B)�B,B/B0!B0!B1'B33B6FB<jB<jB=qB@�BB�B@�B?}BA�BC�BG�BI�BL�BP�BR�BXBXB[#B^5B_;B`BBcTBdZBdZBe`BgmBjBl�Bp�Bt�Bt�Bu�Bu�Bu�Bw�Bx�Bw�Bx�Bz�B|�B�B�%B�7B�JB�hB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�3B�?B�XB�}B��B��BÖBɺB��B��B��B��B��B�B�B�)B�BB�ZB�B�B�B�B�B��B��B	  B	
=B	oB	�B	�B	�B	!�B	#�B	%�B	'�B	+B	.B	0!B	1'B	2-B	33B	7LB	9XB	;dB	=qB	?}B	A�B	C�B	D�B	F�B	H�B	L�B	L�B	N�B	P�B	Q�B	S�B	VB	W
B	XB	YB	[#B	]/B	]/B	`BB	gmB	jB	l�B	m�B	o�B	q�B	r�B	s�B	u�B	u�B	v�B	y�B	y�B	y�B	x�B	x�B	y�B	{�B	|�B	|�B	}�B	}�B	~�B	�B	�%B	�DB	�VB	�bB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�'B	�-B	�3B	�?B	�RB	�XB	�^B	�dB	�qB	�}B	��B	��B	��B	B	ÖB	ŢB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�)B	�/B	�/B	�;B	�HB	�NB	�TB	�ZB	�fB	�mB	�sB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
JB
JB
JB
PB
PB
VB
\B
\B
\B
\B
bB
bB
bB
hB
oB
oB
oB
oB
oB
oB
uB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
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
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
,B
,B
-B
-B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
33B
33B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
7LB
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
<jB
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
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
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
C�B
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
G�B
G�B
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
H�B
H�B
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
J�B
J�B
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
P�B
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
R�B
R�B
T�B
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
YB
YB
ZB
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
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
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
ffB
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
iyB
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
k�B
k�B
k�B
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
n�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�&B
�&B
�&B
�&B
�&B
�&B
�&B
�&B
�2B
��B
	BxBkQB��B�B�:B�_B�~B��B��B�B�OBȀB��B��BFB~B?HB-�B?HBFtB6B$�B$�B$�B!�BMBFB~B6BK�BI�BH�B_Bd&Bt�B��B�	B�B�B�B�@B�@B�MB�kB�SB�(B��B��Bz�BjKBS�BH�BAUB?HBCaBH�BT�BK�BL�BX�BaBQ�BM�BDgB4B/�B(�B%�B"�BqBSB(B�B�B�B�B��B��B�DB�tB��B��B�.B��B��B~�Bw�Br|Bh>B_BN�B;0B_B
�B
�OB
��B
�MB
|�B
pUB
jKB
EmB
.�B	��B	�aB	��B	ѷB	��B	�B	ʌB	��B	��B	��B	{�B	u�B	nIB	d&B	[�B	X�B	W�B	X�B	CGB	/�B	)�B	4B�iB�0B��B��B��B��B��B��B��BөBѷBҽBյB��B��B��BөBҽBѷBϑB͟BȀB�[B�B�B�B��B��B��B��B�|B�~B�QB�SB�@B�:B�(B�B��B}�Bz�Bv�Br|Bq[BoOBm]BgBfB`�B^BZ�BS�BK�BFtBL~BHfBA;B>BBA;BCaBA;B:*B5�B4B1�B/�B0�B/�B-�B+�B+�B*�B)�B)�B(�B(�B'�B'�B#�B'�B'�B&�B&�B%�B%�B%�B$�B#�B"�B"�B vB!|B vBpBjB�BjBjBjBjBpBjBjBjB�B�BpB �B�B"�B"�B#�B#�B#�B#�B#�B#�B$�B#�B$�B&�B'�B&�B'�B(�B)�B)�B)�B+�B.�B/�B/�B0�B2�B5�B<B<B=<B@4BBAB@4B?.BA;BCGBG_BIlBL~BP�BR�BW�BW�BZ�B]�B_B_�BcBd&BdBe,BgBjKBl=BpUBtnBtnButBu�ButBw�Bx�Bw�Bx�Bz�B|�B��B��B��B��B�4B�,B�SB�EB�KB�qB�dB�jB�vB�|B��B��B��B��B��B��B��B��B�	B�HB�OB�;B�aB�lBʌB�xB�~BЗB��BյB��B��B��B�B�0B�CB�UB�aB�hB��B��B��B		�B	:B	9B	?B	jB	!|B	#�B	%�B	'�B	*�B	-�B	/�B	0�B	1�B	2�B	7B	9$B	;B	="B	?.B	AUB	CGB	DMB	FYB	HfB	L�B	L~B	N�B	P�B	Q�B	S�B	U�B	V�B	W�B	X�B	Z�B	\�B	\�B	_�B	gB	j0B	l=B	mCB	oOB	q[B	r|B	shB	utB	u�B	vzB	y�B	y�B	y�B	x�B	x�B	y�B	{�B	|�B	|�B	}�B	}�B	~�B	��B	��B	��B	�B	�B	�B	�B	�?B	�EB	�EB	�KB	�xB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�"B	�.B	�;B	�4B	�OB	�AB	�aB	�SB	�_B	�fB	ȀB	�fB	�rB	�xB	͟B	̈́B	ΊB	̈́B	ЗB	ңB	ңB	ңB	ԯB	ּB	ּB	ּB	ּB	ּB	ּB	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�$B	�*B	�$B	�$B	�*B	�0B	�QB	�=B	�=B	�]B	�CB	�IB	�OB	�OB	�UB	�UB	�UB	�oB	�[B	�[B	�aB	�aB	�hB	�nB	�nB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
	�B
	�B

�B

�B
B
B
�B
B
B
B
B
B
B
B
B
.B
.B
B
B
 B
 B
 B
 B
 B
&B
:B
&B
&B
@B
@B
,B
2B
2B
9B
9B
?B
?B
?B
YB
?B
?B
?B
EB
EB
EB
KB
KB
KB
eB
QB
QB
WB
WB
QB
QB
WB
WB
WB
xB
]B
~B
dB
dB
dB
dB
dB
�B
jB
jB
�B
�B
jB
jB
jB
jB
jB
jB
�B
�B
 vB
 vB
 vB
 vB
 vB
 vB
!|B
!�B
"�B
"�B
#�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
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
)�B
)�B
)�B
)�B
)�B
*�B
+�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
.�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
2�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
4B
4�B
4�B
4�B
5�B
5�B
6�B
7B
7B
6�B
8B
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
;B
;B
;B
;B
<B
<B
<B
="B
="B
="B
="B
>(B
>(B
>BB
>BB
>(B
>(B
?.B
?.B
?.B
?HB
?.B
@4B
@4B
A;B
A;B
A;B
A;B
A;B
BAB
BAB
BAB
BAB
BAB
BAB
BAB
CGB
DgB
DMB
DMB
DMB
DMB
ESB
EmB
ESB
EmB
FYB
G_B
G_B
GzB
G_B
GzB
G_B
G_B
HfB
H�B
HfB
H�B
HfB
HfB
HfB
HfB
HfB
HfB
HfB
H�B
IlB
I�B
J�B
JrB
JrB
JrB
JrB
J�B
JrB
KxB
KxB
KxB
KxB
K�B
KxB
KxB
L�B
L~B
L~B
L~B
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
P�B
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
R�B
R�B
T�B
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
X�B
X�B
Y�B
Y�B
Y�B
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
[�B
[�B
[�B
[�B
\�B
^B
]�B
]�B
]�B
^B
]�B
]�B
^B
^�B
_B
^�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
aB
aB
a�B
a�B
a�B
a�B
bB
a�B
cB
cB
cB
cB
cB
cB
cB
cB
cB
dB
dB
eB
eB
eB
eB
eB
e,B
eB
e,B
e,B
eB
e,B
eB
e,B
fB
fB
fB
fB
fB
g8B
gB
g8B
h$B
h$B
h$B
h$B
h$B
h$B
i*B
i*B
i*B
i*B
i*B
j0B
j0B
j0B
j0B
j0B
j0B
j0B
j0B
k6B
k6B
k6B
k6B
k6B
lWB
l=B
lWB
l=B
lWB
l=B
mCB
mCB
mCB
mCB
mCB
mCB
mCB
ncB
nIB
nIB
nIB
nIB
nIB
nIB
nIB
ncB
ncB
ncB
oOB
oO1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.58(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901050044082019010500440820190105004408201901060032402019010600324020190106003240JA  ARFMdecpA19c                                                                20181231003626  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181230153650  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181230153652  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181230153652  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181230153653  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181230153653  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181230153653  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181230153653  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181230153653  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181230153654                      G�O�G�O�G�O�                JA  ARUP                                                                        20181230155455                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181230153553  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20190104154408  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190104154408  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190105153240  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                