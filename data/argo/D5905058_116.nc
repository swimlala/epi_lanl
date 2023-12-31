CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-08T03:36:30Z creation;2019-01-08T03:36:33Z conversion to V3.1;2019-12-23T06:09:00Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190108033630  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               tA   JA  I2_0675_116                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؞=�tn�1   @؞>�m� @8*�0��c4�x���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:�fD;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��\@ҏ\A	G�A)G�AIG�AiG�A���A���A���A���Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2Q�B:Q�BBQ�BJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B�(�B�(�B�(�B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�{C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�{CF�{CH�{CJ�{CL�{CN�{CP�{CR�{CT�{CV�{CX�{CZ�{C\�{C^�{C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D��D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:��D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�Du%Du�Dv%Dv�Dw%Dw�Dx%Dx�Dy%Dy�Dz%Dz�D{%D{�D|%D|�D}%D}�D~%D~�D%D�D��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�U�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D�D�ҏD��D�R�DÒ�D�ҏD��D�R�DĒ�D�ҏD��D�R�DŒ�D�ҏD��D�R�Dƒ�D�ҏD��D�R�Dǒ�D�ҏD��D�R�DȒ�D�ҏD��D�R�Dɒ�D�ҏD��D�R�Dʒ�D�ҏD��D�R�D˒�D�ҏD��D�R�D̒�D�ҏD��D�R�D͒�D�ҏD��D�R�DΒ�D�ҏD��D�R�Dϒ�D�ҏD��D�R�DВ�D�ҏD��D�R�Dђ�D�ҏD��D�R�DҒ�D�ҏD��D�R�DӒ�D�ҏD��D�R�DԒ�D�ҏD��D�R�DՒ�D�ҏD��D�R�D֒�D�ҏD��D�R�Dג�D�ҏD��D�R�Dؒ�D�ҏD��D�R�Dْ�D�ҏD��D�R�Dڒ�D�ҏD��D�R�Dے�D�ҏD��D�R�Dܒ�D�ҏD��D�R�Dݒ�D�ҏD��D�R�Dޒ�D�ҏD��D�R�Dߒ�D�ҏD��D�R�D���D�ҏD��D�R�DᒏD�ҏD��D�R�D⒏D�ҏD��D�R�D㒏D�ҏD��D�R�D䒏D�ҏD��D�R�D咏D�ҏD��D�R�D撏D�ҏD��D�R�D璏D�ҏD��D�R�D蒏D�ҏD��D�R�D钏D�ҏD��D�R�D꒏D�ҏD��D�R�D뒏D�ҏD��D�R�D�\D�ҏD��D�R�D풏D�ҏD��D�R�DD�ҏD��D�R�DD�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�\)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��/A��#A��#A��#A��#A���A���A���A���A���A���A��wA��^A��FA��9A��9A��!A���A���A���A��uA��PA��7A��A��A�|�A�x�A�z�A�z�A�|�A�~�A�Q�A���A�;dA�oA�A���A���A���A��!A�Q�A���A���A��/A�%A���A�dZA�9XA��!A��A��A��;A���A�VA��A�ȴA���A�ƨA���A�+A��PA�t�A�l�A���A�=qA�A���A��A�I�A�;dA��!A��+A���A���A��TA�ZA��#A�M�A�\)A���A�{A�XA�oA�dZA��wA�-A���A��DA�"�A��PA�bA���A��uA�"�A�VA�bNA�x�A���A���A���A�E�A�x�A��+A�"�A���A���A�33A�-A�I�A�JA��\A��A~ffA{VAy%Ax5?AwƨAvJAt  Ar�An��Al��Aj1AgAb�yA^��A]S�A[33AZZAYAXI�AW�-AV��AT��AS`BAQ�#APJAN��AM�^AMAL  AJ  AHI�AF��AE�AD�!ACt�AB5?A@�!A@1A?ƨA?G�A>�\A=��A<�A;%A9��A9l�A8��A7\)A6��A65?A5O�A49XA3��A2�A1�FA0��A/�A.�A-dZA,ZA,bA+�#A+7LA*��A)�A)
=A'�;A&9XA%G�A$5?A"�A!VA bNA�/A=qA+AG�A�FA��AoAVA��A\)Az�AbA�!A�AVA�RAA��A��A�jA�A��AdZA	ƨA�HA�;A��Ap�A��AAC�A��A~�AA�A1A"�A�9A�DA=qA�Ax�Ap�A`BA ��A J@��@���@��
@��`@�b@��@��@�`B@�ƨ@�C�@��@�\)@ꗍ@�x�@�ƨ@�$�@�7@�j@㕁@��@���@�@��@�33@��@��@ܣ�@�ƨ@���@ٲ-@�G�@�/@�Q�@�\)@��y@և+@ՙ�@���@��@ёh@мj@��@�
=@�ff@�%@��@ʰ!@ț�@��@Ɨ�@�=q@��`@��
@¸R@�p�@�A�@�l�@�5?@��@�@���@��@�b@�dZ@��y@�V@��@��D@�ƨ@�dZ@�C�@��H@���@�V@�ƨ@�
=@��!@�=q@���@�%@�b@��
@��@��@�v�@�@�p�@�&�@��9@�b@���@�C�@���@��\@��@�hs@�/@���@�Z@�ƨ@�"�@��!@�E�@���@�&�@��9@�I�@�1@��m@��F@���@�S�@�@���@�ȴ@�n�@�=q@��@�@�@�hs@�/@�%@��j@�r�@� �@���@��@�K�@�33@���@���@���@���@�ff@��@��@��-@�hs@��@���@���@�A�@��@��m@��@�K�@��y@��+@�5?@���@��#@���@�p�@�/@��@���@��j@��@���@�r�@�A�@�1'@�b@��@�33@���@��@�@���@�hs@�/@�V@��@�O�@��@��`@���@��@�bN@� �@�dZ@��@��\@�~�@�ff@��H@�M�@�x�@�hs@��@�j@�Z@�Q�@�r�@�9X@���@��@��@��@�A�@�1'@�  @��;@��F@���@�t�@�o@��+@���@��h@�X@�7L@�/@�7L@�G�@�V@�z�@�9X@��@�9X@���@��F@�l�@�S�@���@���@���@��+@�^5@��T@��-@���@�x�@�p�@�p�@�G�@��`@���@�r�@�Q�@�b@��m@���@���@�|�@��@�ȴ@��R@��!@�M�@�$�@���@�@���@��@��@��@���@���@�Ĝ@���@��/@��j@��9@��@���@��@�Z@�I�@��@�  @�ƨ@���@�l�@�;d@�+@�@��H@���@�V@�$�@���@�@��@�X@�?}@��@���@��9@�z�@�Z@�9X@�;@\)@~ȴ@~�+@~{@}@}p�@|�@|��@|I�@|(�@{��@{�F@{"�@z��@z�\@z~�@z^5@z=q@z�@y�@y��@y��@y�7@yx�@y&�@x��@x��@x �@w�@wK�@w�@v�@vȴ@v��@vE�@v@u@u�@t��@s�F@sC�@so@r��@r=q@r-@q��@qhs@q&�@p�`@p�9@p�u@p �@o��@o�@o�@o;d@nv�@n@m`B@m�@l�j@lZ@l(�@l1@k�m@kƨ@k��@kS�@j�@j�!@j�\@jn�@j�@i�^@ix�@ihs@i&�@h��@h��@h�@hbN@h1'@g�w@g\)@f�R@f�+@fE�@e��@e?}@d��@d�/@d��@d�@dI�@d1@cƨ@c�@cdZ@c@b^5@b=q@bJ@a��@a��@a��@aG�@`Ĝ@`bN@_�P@_K�@_�@^�R@^V@^$�@]�T@]`B@]?}@]V@\��@\�@[�F@[o@Z��@Zn�@ZJ@Y��@Y%@X�u@X�u@X�u@X�@Xr�@XA�@X1'@X �@X  @W�@W�w@W�P@V�@Vff@V$�@U�-@U`B@U�@T�D@T(�@S��@SS�@So@S"�@R�@R�H@R��@R�H@R��@R��@R-@R�@R�@Q�@QX@P��@P �@O�@Ol�@OK�@O;d@N��@N��@NE�@M�@M@M�@M`B@M?}@MV@L�j@L�@L��@LZ@L1@K��@KdZ@KS�@KC�@K@Jn�@J=q@J�@JJ@I�^@IG�@I�@I%@H��@H�@H1'@G�P@G\)@G+@G
=@F��@F��@F�@Fff@F5?@E��@E��@Ep�@E�@EV@DI�@C�m@C��@B��@B-@BJ@A�^@Ahs@A%@@�9@@�@@Q�@@ �@?�@?�w@?+@>�R@>�+@>ff@>V@>5?@=��@=?}@<��@<��@<��@<z�@<Z@<1@<1@;�
@;��@:�H@:=q@9�#@9��@9x�@9�@8��@8��@8�9@8�u@8r�@8Q�@8 �@7�;@7�@7|�@7l�@7K�@6��@6��@6�y@6�R@6v�@6E�@5��@5��@5�h@5�@5?}@4�@4z�@4Z@49X@3��@3�@3C�@3C�@3"�@2�!@2M�@1��@1�#@1��@17L@1�@0��@0��@0�9@0A�@0b@/�@/�@/l�@/;d@.��@.ȴ@.�R@.��@.�+@.V@.@-�T@-�-@-p�@-`B@-/@,�j@,j@,�@+�
@+��@+t�@+@*��@*��@*�\@*M�@*J@*J@)��@)��@)x�@)G�@)&�@(�`@(��@(A�@(b@'�@'�@'|�@'\)@'+@'
=@&�y@&�@&��@&ff@&E�@&$�@%�T@%�h@%`B@%O�@%/@%V@$�/@$�@$��@$j@$(�@#�
@#�@#dZ@#C�@#"�@#@"��@"�!@"�\@"M�@!��@!��@!�^@!��@!X@!&�@!%@ ��@ 1'@ b@�@�@�@�@l�@K�@
=@��@�y@ȴ@��@E�@@�@�T@��@O�@�@��@�D@9X@�@ƨ@��@�@dZ@C�@��@~�@M�@-@�@��@�^@��@��@��@��@hs@�`@�@Q�@b@��@�w@��@�P@+@��@ȴ@�+@V@5?@{@�T@��@�-@�@O�@?}@/@�@��@��@z�@Z@I�@(�@��@�F@��@S�@"�@@�@��@��@n�@M�@=q@��@��@�7@x�@X@&�@�@�@��@Ĝ@�@r�@bN@Q�@1'@ �@b@b@  @�@�;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��/A��#A��#A��#A��#A���A���A���A���A���A���A��wA��^A��FA��9A��9A��!A���A���A���A��uA��PA��7A��A��A�|�A�x�A�z�A�z�A�|�A�~�A�Q�A���A�;dA�oA�A���A���A���A��!A�Q�A���A���A��/A�%A���A�dZA�9XA��!A��A��A��;A���A�VA��A�ȴA���A�ƨA���A�+A��PA�t�A�l�A���A�=qA�A���A��A�I�A�;dA��!A��+A���A���A��TA�ZA��#A�M�A�\)A���A�{A�XA�oA�dZA��wA�-A���A��DA�"�A��PA�bA���A��uA�"�A�VA�bNA�x�A���A���A���A�E�A�x�A��+A�"�A���A���A�33A�-A�I�A�JA��\A��A~ffA{VAy%Ax5?AwƨAvJAt  Ar�An��Al��Aj1AgAb�yA^��A]S�A[33AZZAYAXI�AW�-AV��AT��AS`BAQ�#APJAN��AM�^AMAL  AJ  AHI�AF��AE�AD�!ACt�AB5?A@�!A@1A?ƨA?G�A>�\A=��A<�A;%A9��A9l�A8��A7\)A6��A65?A5O�A49XA3��A2�A1�FA0��A/�A.�A-dZA,ZA,bA+�#A+7LA*��A)�A)
=A'�;A&9XA%G�A$5?A"�A!VA bNA�/A=qA+AG�A�FA��AoAVA��A\)Az�AbA�!A�AVA�RAA��A��A�jA�A��AdZA	ƨA�HA�;A��Ap�A��AAC�A��A~�AA�A1A"�A�9A�DA=qA�Ax�Ap�A`BA ��A J@��@���@��
@��`@�b@��@��@�`B@�ƨ@�C�@��@�\)@ꗍ@�x�@�ƨ@�$�@�7@�j@㕁@��@���@�@��@�33@��@��@ܣ�@�ƨ@���@ٲ-@�G�@�/@�Q�@�\)@��y@և+@ՙ�@���@��@ёh@мj@��@�
=@�ff@�%@��@ʰ!@ț�@��@Ɨ�@�=q@��`@��
@¸R@�p�@�A�@�l�@�5?@��@�@���@��@�b@�dZ@��y@�V@��@��D@�ƨ@�dZ@�C�@��H@���@�V@�ƨ@�
=@��!@�=q@���@�%@�b@��
@��@��@�v�@�@�p�@�&�@��9@�b@���@�C�@���@��\@��@�hs@�/@���@�Z@�ƨ@�"�@��!@�E�@���@�&�@��9@�I�@�1@��m@��F@���@�S�@�@���@�ȴ@�n�@�=q@��@�@�@�hs@�/@�%@��j@�r�@� �@���@��@�K�@�33@���@���@���@���@�ff@��@��@��-@�hs@��@���@���@�A�@��@��m@��@�K�@��y@��+@�5?@���@��#@���@�p�@�/@��@���@��j@��@���@�r�@�A�@�1'@�b@��@�33@���@��@�@���@�hs@�/@�V@��@�O�@��@��`@���@��@�bN@� �@�dZ@��@��\@�~�@�ff@��H@�M�@�x�@�hs@��@�j@�Z@�Q�@�r�@�9X@���@��@��@��@�A�@�1'@�  @��;@��F@���@�t�@�o@��+@���@��h@�X@�7L@�/@�7L@�G�@�V@�z�@�9X@��@�9X@���@��F@�l�@�S�@���@���@���@��+@�^5@��T@��-@���@�x�@�p�@�p�@�G�@��`@���@�r�@�Q�@�b@��m@���@���@�|�@��@�ȴ@��R@��!@�M�@�$�@���@�@���@��@��@��@���@���@�Ĝ@���@��/@��j@��9@��@���@��@�Z@�I�@��@�  @�ƨ@���@�l�@�;d@�+@�@��H@���@�V@�$�@���@�@��@�X@�?}@��@���@��9@�z�@�Z@�9X@�;@\)@~ȴ@~�+@~{@}@}p�@|�@|��@|I�@|(�@{��@{�F@{"�@z��@z�\@z~�@z^5@z=q@z�@y�@y��@y��@y�7@yx�@y&�@x��@x��@x �@w�@wK�@w�@v�@vȴ@v��@vE�@v@u@u�@t��@s�F@sC�@so@r��@r=q@r-@q��@qhs@q&�@p�`@p�9@p�u@p �@o��@o�@o�@o;d@nv�@n@m`B@m�@l�j@lZ@l(�@l1@k�m@kƨ@k��@kS�@j�@j�!@j�\@jn�@j�@i�^@ix�@ihs@i&�@h��@h��@h�@hbN@h1'@g�w@g\)@f�R@f�+@fE�@e��@e?}@d��@d�/@d��@d�@dI�@d1@cƨ@c�@cdZ@c@b^5@b=q@bJ@a��@a��@a��@aG�@`Ĝ@`bN@_�P@_K�@_�@^�R@^V@^$�@]�T@]`B@]?}@]V@\��@\�@[�F@[o@Z��@Zn�@ZJ@Y��@Y%@X�u@X�u@X�u@X�@Xr�@XA�@X1'@X �@X  @W�@W�w@W�P@V�@Vff@V$�@U�-@U`B@U�@T�D@T(�@S��@SS�@So@S"�@R�@R�H@R��@R�H@R��@R��@R-@R�@R�@Q�@QX@P��@P �@O�@Ol�@OK�@O;d@N��@N��@NE�@M�@M@M�@M`B@M?}@MV@L�j@L�@L��@LZ@L1@K��@KdZ@KS�@KC�@K@Jn�@J=q@J�@JJ@I�^@IG�@I�@I%@H��@H�@H1'@G�P@G\)@G+@G
=@F��@F��@F�@Fff@F5?@E��@E��@Ep�@E�@EV@DI�@C�m@C��@B��@B-@BJ@A�^@Ahs@A%@@�9@@�@@Q�@@ �@?�@?�w@?+@>�R@>�+@>ff@>V@>5?@=��@=?}@<��@<��@<��@<z�@<Z@<1@<1@;�
@;��@:�H@:=q@9�#@9��@9x�@9�@8��@8��@8�9@8�u@8r�@8Q�@8 �@7�;@7�@7|�@7l�@7K�@6��@6��@6�y@6�R@6v�@6E�@5��@5��@5�h@5�@5?}@4�@4z�@4Z@49X@3��@3�@3C�@3C�@3"�@2�!@2M�@1��@1�#@1��@17L@1�@0��@0��@0�9@0A�@0b@/�@/�@/l�@/;d@.��@.ȴ@.�R@.��@.�+@.V@.@-�T@-�-@-p�@-`B@-/@,�j@,j@,�@+�
@+��@+t�@+@*��@*��@*�\@*M�@*J@*J@)��@)��@)x�@)G�@)&�@(�`@(��@(A�@(b@'�@'�@'|�@'\)@'+@'
=@&�y@&�@&��@&ff@&E�@&$�@%�T@%�h@%`B@%O�@%/@%V@$�/@$�@$��@$j@$(�@#�
@#�@#dZ@#C�@#"�@#@"��@"�!@"�\@"M�@!��@!��@!�^@!��@!X@!&�@!%@ ��@ 1'@ b@�@�@�@�@l�@K�@
=@��@�y@ȴ@��@E�@@�@�T@��@O�@�@��@�D@9X@�@ƨ@��@�@dZ@C�@��@~�@M�@-@�@��@�^@��@��@��@��@hs@�`@�@Q�@b@��@�w@��@�P@+@��@ȴ@�+@V@5?@{@�T@��@�-@�@O�@?}@/@�@��@��@z�@Z@I�@(�@��@�F@��@S�@"�@@�@��@��@n�@M�@=q@��@��@�7@x�@X@&�@�@�@��@Ĝ@�@r�@bN@Q�@1'@ �@b@b@  @�@�;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bn�Bn�Bn�Bo�Bn�Bo�Bo�Bp�Bo�Bp�Bq�Bq�Br�Br�Bs�Bt�Bu�Bw�Bz�B}�B� B�B�B�B�B�B�B�B�B�B�B�jB��BoBhBbBbBbBoB�B"�B%�B.B8RBF�BgmBk�Bw�Bs�Bt�Bv�Bw�Bv�Bm�Be`BdZBaHB`BB_;B\)BZBVBP�BL�BK�BJ�BK�BJ�BK�BL�BJ�BI�BG�B?}B6FB�BbBB��B��B��BuB�B\B%B  B��B��B�B�5BĜB�B��B��B�oBw�BYB:^B.B �B
=B
�B
�HB
�)B
��B
ȴB
�^B
��B
��B
r�B
W
B
I�B
=qB
#�B
VB
B	��B	�B	�)B	��B	�'B	��B	�hB	w�B	ffB	K�B	A�B	6FB	2-B	%�B	�B	�B	�B	�B	�B	hB	+B	B��B��B�B�ZB�#B��B��BȴB��BŢB�jB�RB�LB�LB�LB��B�wB�qB�LB�?B�B��B��B��B��B��B��B��B��B��B�oB�VB�B{�B�B�B|�Bz�B{�By�Bw�Br�Bo�Bm�BgmB[#BVBR�BQ�BR�BN�BH�BI�BG�BG�BG�BF�BE�BC�BA�B>wB;dB9XB7LB49B33B1'B0!B/B/B.B,B,B+B+B)�B(�B'�B&�B&�B%�B%�B&�B%�B%�B%�B%�B$�B$�B#�B#�B#�B#�B"�B"�B"�B �B�B�B�B�B�B �B �B!�B!�B!�B"�B"�B#�B$�B$�B#�B#�B%�B%�B&�B&�B%�B'�B+B,B-B-B/B/B0!B/B0!B1'B2-B49B5?B7LB9XB9XB<jB=qB?}B>wB@�B?}B?}BB�BB�BE�BI�BL�BM�BR�BR�BR�BVBW
BXB[#B\)B^5BaHBdZBe`Be`Be`BhsBk�Bp�Bt�Bv�Bw�Bx�B{�B}�B�B�B�B�%B�1B�JB�PB�VB�hB��B��B��B��B��B��B��B��B��B�B�B�9B�RB�dB�}B��BĜBȴB��B��B��B��B��B�
B�B�)B�BB�NB�TB�TB�mB�B�B�B��B��B	B	B	1B	
=B	VB	\B	oB	{B	{B	�B	�B	�B	�B	 �B	"�B	$�B	(�B	,B	-B	/B	1'B	33B	5?B	7LB	:^B	<jB	=qB	>wB	@�B	C�B	E�B	F�B	G�B	H�B	H�B	J�B	K�B	L�B	M�B	O�B	R�B	S�B	VB	YB	[#B	\)B	^5B	_;B	bNB	iyB	jB	l�B	n�B	n�B	n�B	o�B	q�B	q�B	q�B	t�B	w�B	}�B	~�B	}�B	}�B	~�B	~�B	� B	�B	�B	�%B	�+B	�=B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�FB	�XB	�^B	�^B	�dB	�jB	�qB	�wB	�}B	�}B	��B	B	B	ĜB	ŢB	ŢB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�/B	�5B	�5B	�;B	�NB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
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
hB
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
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
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
�B
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
"�B
"�B
"�B
#�B
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
&�B
'�B
'�B
(�B
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
.B
.B
.B
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
2-B
2-B
2-B
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
7LB
7LB
8RB
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
<jB
<jB
<jB
<jB
<jB
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
C�B
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
E�B
E�B
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
ZB
ZB
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
\)B
\)B
]/B
\)B
\)B
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
`BB
`BB
`BB
`BB
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
hsB
hsB
hsB
hsB
hsB
iyB
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
iyB
iyB
iyB
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BncBncBncBoiBncBoiBoiBpoBoiBpoBqvBqvBr|Br|Bs�Bt�Bu�Bw�Bz�B}�B�B��B��B��B��B��B��B��B��B��B��B�6B��B:B4B.B.B.B:BYB"�B%�B-�B8BFtBg8BkQBw�Bs�Bt�Bv�Bw�Bv�Bm]Be,Bd&BaB`B_B[�BY�BU�BP�BL�BK�BJ�BK�BJ�BK�BL�BJ�BI�BGzB?HB6BxB.B�B��B��B��B@BYB(B�B��B��B��B�oB�B�gB��B��B�~B�:Bw�BX�B:*B-�B �B
	B
�oB
�B
��B
ѷB
ȀB
�*B
��B
�_B
raB
V�B
I�B
=<B
#�B
"B
 �B	��B	�OB	��B	̘B	��B	��B	�4B	w�B	f2B	KxB	AUB	6B	1�B	%�B	~B	]B	dB	dB	SB	4B	�B	�B��B��B�iB�B��B��BϫBȀBʌB�mB�6B�B�B�B�B�UB�BB�<B�B��B��B��B��B�|B�jB�kB�pB�]B�KB�2B�:B�"B��B{�B��B��B|�Bz�B{�By�Bw�Br|BoOBm]BgBZ�BU�BR�BQ�BR�BN�BH�BIlBG_BGzBG_BFYBESBCaBA;B>(B;B9	B6�B4B2�B0�B/�B.�B.�B-�B+�B+�B*�B*�B)�B(�B'�B&�B&�B%�B%�B&�B%�B%�B%�B%�B$�B$�B#�B#�B#�B#�B"�B"�B"�B vBpBpBpB�BjB vB vB!|B!|B!|B"�B"�B#�B$�B$�B#�B#�B%�B%�B&�B&�B%�B'�B*�B+�B,�B,�B.�B.�B/�B.�B/�B0�B1�B3�B5B6�B9	B9	B<B="B?HB>BB@4B?.B?.BBABBABEmBIlBL~BM�BR�BR�BR�BU�BV�BW�BZ�B[�B]�B`�BdBeBeBeBh>Bk6BpUBtnBvzBw�Bx�B{�B}�B��B��B��B��B��B��B�B�B�4B�MB�?B�kB�xB�]B�vB��B��B��B��B��B��B�B�B�.B�;B�MBȀBʌB˒B̈́BϑBөB��B��B��B��B��B�B�B�B�6B�OB�B��B��B	 �B	�B	�B		�B	B	B	 B	FB	,B	2B	EB	WB	dB	 vB	"�B	$�B	(�B	+�B	,�B	.�B	0�B	2�B	4�B	6�B	:B	<B	="B	>(B	@OB	CGB	ESB	FYB	G_B	HfB	HfB	JrB	KxB	L~B	M�B	O�B	R�B	S�B	U�B	X�B	Z�B	[�B	]�B	^�B	a�B	iDB	jKB	l=B	ncB	nIB	nIB	oOB	q[B	q[B	q[B	tnB	w�B	}�B	~�B	}�B	}�B	~�B	~�B	�B	��B	��B	��B	��B	��B	�B	�FB	�?B	�EB	�EB	�KB	�QB	�QB	�qB	�WB	�QB	�QB	�dB	��B	�pB	�pB	�|B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�"B	�(B	�.B	�HB	�;B	�AB	�AB	�MB	�SB	�SB	�_B	�lB	�lB	�rB	�rB	�xB	�~B	̈́B	ϑB	ѝB	өB	ңB	ңB	өB	ԯB	ԯB	յB	յB	��B	ּB	��B	��B	��B	��B	�B	��B	�B	�,B	�B	�2B	�B	�B	�B	�B	�B	�B	�>B	�*B	�6B	�=B	�WB	�=B	�CB	�IB	�OB	�OB	�OB	�OB	�oB	�[B	�[B	�aB	�aB	�aB	�hB	�hB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	�B
	�B
	�B

�B

�B

�B

�B
�B
B
B
B
B
B
B
B
(B
B
.B
B
B
B
B
4B
B
 B
 B
 B
:B
 B
&B
&B
&B
&B
&B
,B
,B
2B
2B
MB
9B
9B
9B
9B
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
KB
QB
QB
QB
QB
WB
]B
xB
]B
]B
dB
dB
dB
�B
jB
jB
jB
pB
pB
 vB
 vB
 vB
!|B
!|B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
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
&�B
'�B
'�B
(�B
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
-�B
-�B
-�B
.�B
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
4B
4�B
4�B
5B
4�B
5B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
8B
9$B
9$B
9	B
:B
:B
:B
:*B
;B
;B
;B
;B
;B
<B
<B
<B
<B
<6B
=<B
="B
>(B
>(B
>(B
>(B
>(B
>BB
?.B
?HB
?HB
?.B
@4B
@OB
AUB
A;B
A;B
BAB
BAB
BAB
B[B
BAB
BAB
BAB
BAB
CGB
CGB
CGB
CGB
CaB
CGB
DgB
DMB
DMB
DMB
DMB
ESB
ESB
ESB
ESB
EmB
ESB
FYB
FYB
FtB
FYB
G_B
G_B
G_B
G_B
G_B
HfB
H�B
HfB
HfB
IlB
IlB
IlB
I�B
IlB
J�B
J�B
JrB
JrB
J�B
J�B
KxB
KxB
K�B
KxB
KxB
K�B
KxB
KxB
L~B
L~B
L~B
L�B
M�B
M�B
M�B
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
T�B
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
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
[�B
[�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
^B
]�B
]�B
^�B
^�B
^�B
_�B
_�B
`B
_�B
`B
_�B
_�B
_�B
_�B
`B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
bB
a�B
cB
cB
cB
dB
dB
dB
dB
dB
dB
dB
d&B
dB
e,B
e,B
eB
eB
eB
eB
e,B
fB
f2B
fB
fB
g8B
gB
g8B
gB
gB
gB
gB
h$B
h$B
h>B
h$B
h>B
i*B
h$B
h$B
iDB
iDB
i*B
i*B
i*B
iDB
i*B
iDB
i*B
i*B
i*B
iDB
j01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.58(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901130043352019011300433520190113004335201901140027312019011400273120190114002731JA  ARFMdecpA19c                                                                20190108123628  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190108033630  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190108033631  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190108033632  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190108033632  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190108033632  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190108033632  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190108033632  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190108033633  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190108033633                      G�O�G�O�G�O�                JA  ARUP                                                                        20190108035501                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190108153858  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20190112154335  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190112154335  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190113152731  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                