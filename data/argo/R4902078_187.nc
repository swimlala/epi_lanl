CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-09T10:00:29Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200609100029  20200609100029  4902078 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5439                            2B  A   NAVIS_A                         0460                            011514                          863 @��\��1   @��M���@+������d���`A�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B���B���B�  B�33B�33B�  B���B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@ҏ\A	G�A)G�AIG�AiG�A���A���A���A���Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2Q�B:Q�BBQ�BJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�\)B�\)B���B���B�(�B�\)B�\)B�(�B���B�(�B�(�B�(�B���B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�{C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�{CF�{CH�{CJ�{CL�{CN�{CP�{CR�{CT�{CV�{CX�{CZ�{C\�{C^�{C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�W
C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�Du%Du�Dv%Dv�Dw%Dw�Dx%Dx�Dy%Dy�Dz%Dz�D{%D{�D|%D|�D}%D}�D~%D~�D%D�D��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D��\D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�O\D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D���D��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D�D�ҏD��D�R�DÒ�D�ҏD��D�R�DĒ�D�ҏD��D�R�DŒ�D�ҏD��D�R�Dƒ�D�ҏD��D�R�Dǒ�D�ҏD��D�R�DȒ�D�ҏD��D�R�Dɒ�D�ҏD��D�R�Dʒ�D�ҏD��D�R�D˒�D�ҏD��D�R�D̒�D�ҏD��D�R�D͒�D�ҏD��D�R�DΒ�D�ҏD��D�R�Dϒ�D�ҏD��D�R�DВ�D�ҏD��D�R�Dђ�D�ҏD��D�R�DҒ�D�ҏD��D�R�DӒ�D�ҏD��D�R�DԒ�D�ҏD��D�R�DՒ�D�ҏD��D�R�D֒�D�ҏD��D�R�Dג�D�ҏD��D�R�Dؒ�D�ҏD��D�R�Dْ�D�ҏD��D�R�Dڒ�D�ҏD��D�R�Dے�D�ҏD��D�R�Dܒ�D�ҏD��D�R�Dݒ�D�ҏD��D�R�Dޒ�D�ҏD��D�R�Dߒ�D�ҏD��D�R�D���D�ҏD��D�R�DᒏD�ҏD��D�R�D⒏D�ҏD��D�R�D㒏D�ҏD��D�R�D䒏D�ҏD��D�R�D咏D�ҏD��D�R�D撏D�ҏD��D�R�D璏D�ҏD��D�R�D蒏D�ҏD��D�R�D钏D�ҏD��D�R�D꒏D�ҏD��D�R�D뒏D�ҏD��D�R�D쒏D�ҏD��D�R�D풏D�ҏD��D�R�DD�ҏD��D�R�DD�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�(�A�5?A�33A�5?A�33A�33A�5?A�5?A�=qA�=qA�?}A�A�A�A�A�A�A�C�A�A�A�A�A�A�A�?}A�;dA�=qA�A�A�C�A�E�A�G�A�G�A�I�A�I�A�A�A�=qA�+A�$�A���A��A��TA��HA�ĜA؏\A���A���A���A��A���A�{A�+A��A�A�AՏ\A�ZA̲-Aȉ7A�"�A�t�A��uA�VA��A�ĜA���A�ƨA��A��-A���A�33A�&�A���A��TA�;dA��PA�`BA�{A���A�9XA�ƨA�(�A��RA���A��TA��9A�C�A�-A�O�Ap�A}`BA{G�Ay�-Ar��Ao��An�jAm�Aln�Aip�Adv�AbffAa;dA`z�A_oA^JA\=qAV�AQ�#AO�AI�;AFȴAGS�AG��AGAGt�AF��AE�PAA`BA@�A?;dA=�FA<bNA<1A:��A8��A8(�A6��A4-A2�yA2=qA1��A1+A0JA. �A-�A-/A,�/A,n�A+|�A*v�A)�7A)l�A(�A'�A&M�A$��A#t�A"�A!A A�
A|�An�AdZA��A9XA �A9XA9XAJA��A�9A�!A �AhsA{A33A9XA�-At�AhsAhsA;dA��Av�A��A�PA�A�A�A�A\)A33A^5A�^AO�A�A�A�`A~�A(�A�#A��AK�A%A�uAjAM�A-AA��A�PA�AhsA;dA��A�!An�AZA$�A��A��A�jA�A-A��A�7A�A
�\A	��A�uA^5A-A�
A�-A��A�hAO�A+A�Ar�AM�A  A/A��AA�A��A33AVA�A�jA^5A9XA$�AJA�mA��A|�AdZAK�A+A%A �RA �A r�A  �A   @��;@�dZ@��R@��\@�V@�5?@�@���@�x�@�?}@���@��@�j@�Z@�I�@�ƨ@�J@���@��h@�7L@�Q�@��F@�n�@�@��T@��^@��-@��h@���@�Q�@�w@���@�@�=q@�@�7@�&�@��@��@��
@�C�@��y@�$�@�p�@���@�Z@�1@�l�@�ȴ@�^5@�5?@�J@��#@�-@�@�&�@�@�z�@���@畁@�;d@�+@��#@�&�@���@���@� �@�w@�33@�$�@�`B@���@�z�@�I�@�b@��
@�t�@�K�@�~�@�M�@݁@��@��@��@��@���@ڇ+@�@���@�p�@�Q�@׾w@֏\@�^5@�$�@�J@��@���@ա�@Ձ@Լj@�ƨ@�|�@ҏ\@��@ѡ�@��@У�@�I�@��m@϶F@�
=@ͺ^@�hs@��@��@̃@���@�o@ʏ\@�V@���@�/@���@ȃ@�Z@�A�@� �@ǍP@Ɨ�@�n�@���@ř�@ļj@å�@ÍP@�t�@�"�@��@+@���@�&�@��@��P@�K�@��@�V@���@�`B@�?}@��/@�b@�ƨ@��@�+@��@��+@��@�%@���@��D@���@�l�@��R@�$�@��T@���@��7@�7L@��9@��D@�z�@�Q�@��@�\)@���@�$�@��T@��-@��@�`B@�/@��`@��D@�r�@�r�@�bN@�Q�@�Q�@��@�+@�@���@�M�@�$�@�@��@���@���@�hs@�?}@�7L@���@�Ĝ@�z�@��@�ƨ@���@�dZ@�;d@�o@��!@�M�@�@���@�@�X@�%@��@��@�@���@�~�@�^5@�-@�p�@��9@�9X@�1@��@��;@���@��y@��R@��R@�n�@���@�bN@��;@���@�|�@�C�@�o@��H@�~�@�J@��#@���@�@��-@���@�hs@��`@���@�(�@��@��@�|�@��@��H@��!@�v�@��@���@���@�?}@���@��9@�bN@��
@���@�o@�ȴ@��R@��!@���@���@��\@�V@��#@�7L@�j@���@�ƨ@��w@��w@��F@�C�@��H@���@�M�@�J@���@�7L@��@���@��`@��9@���@���@�j@�A�@��@��;@��P@�o@���@�V@�$�@���@��T@��^@�G�@�Ĝ@���@�Q�@���@���@�C�@��@���@�J@���@�hs@��@��`@��@�b@��;@���@�S�@��@��@���@�-@��T@���@��7@�G�@�Ĝ@��D@� �@���@���@�K�@��y@���@�M�@���@�x�@��@�A�@��@l�@~E�@}�T@}@}p�@|�@|�D@{�
@{S�@{"�@{@z�!@zJ@y�7@y7L@x��@x�u@w�@w��@w�P@w|�@w|�@w|�@w\)@w;d@u�T@t�j@tI�@s�m@sƨ@s��@s��@s�@s�@sdZ@sS�@sC�@sC�@sC�@s"�@r�@r�!@q�@q��@q�@p�u@pbN@pb@o��@o�P@n��@nff@nV@m�@l��@l9X@k��@k�
@k�F@kC�@j�\@i�#@i�^@i�7@ihs@h��@h�9@h�@h�@h�@hbN@h  @gl�@g�@f�R@e@e?}@d�@c�
@cƨ@c��@cdZ@b�H@b^5@a��@aX@a%@`��@`�9@`bN@_�w@_\)@^�+@^@]��@]�@]p�@]p�@]/@\�D@\(�@[��@[�
@[��@[t�@[33@Z�H@Z��@Z~�@Z�@Y�@Yx�@Y7L@X��@Xr�@W��@WK�@V�R@Vff@U�-@T��@T��@T��@TI�@T1@S��@St�@SC�@R��@R=q@Qx�@PbN@O�P@O�@N�+@N5?@N$�@M@L��@Lz�@LZ@L1@KC�@J��@J-@I��@I��@IX@H��@HQ�@GK�@F�@Fff@F5?@E�-@EO�@E�@EV@D��@D�@CC�@C@B��@B��@B~�@B-@Ax�@A�@@�`@@�9@@A�@?�;@?\)@>�@>��@>��@>�+@>$�@=�T@=��@=�@<�@<�j@<j@<(�@;�F@;"�@:�H@:�H@:�!@:^5@:J@9�^@9�@8�`@8�`@8�9@8�@8r�@8bN@8Q�@8 �@7|�@6�R@6��@6��@6��@6�+@6V@6{@5��@5�@5?}@5V@4��@49X@4�@3�
@3��@3��@3�@333@2�H@2n�@2=q@2-@1�@1��@1x�@1x�@17L@0��@0bN@0bN@0Q�@01'@0 �@/\)@/K�@/;d@/+@/
=@.��@.ff@.{@-��@-?}@,��@,1@+�@+C�@+"�@*��@*n�@*-@)�^@)G�@)�@(��@(�@(bN@(1'@'�w@'|�@'
=@&�+@&E�@&$�@%�@%@%��@%�@%`B@%O�@%O�@%�@$��@$�/@$��@$�@$9X@#��@#t�@#@"��@"�!@"�!@"�\@"n�@"�@!�#@!��@!�7@!hs@!&�@!�@!%@!%@!%@ ��@ �`@ Ĝ@ ��@ �u@ bN@ 1'@ b@�@�w@��@��@\)@�y@�@ȴ@��@�+@v�@ff@V@V@V@E�@E�@E�@5?@5?@5?@$�@{@@�-@p�@?}@?}@/@/@/@�@�/@�D@Z@��@�m@�
@ƨ@��@��@S�@��@^5@=q@�@�@J@��@�@�@�#@�@�@�@�#@�^@�7@hs@�@�`@Ĝ@Ĝ@�9@�u@r�@A�@A�@1'@1'@1'@b@�w@|�@+@�@�@��@��@5?@�@@�-@�h@p�@`B@O�@O�@/@z�@(�@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�(�A�5?A�33A�5?A�33A�33A�5?A�5?A�=qA�=qA�?}A�A�A�A�A�A�A�C�A�A�A�A�A�A�A�?}A�;dA�=qA�A�A�C�A�E�A�G�A�G�A�I�A�I�A�A�A�=qA�+A�$�A���A��A��TA��HA�ĜA؏\A���A���A���A��A���A�{A�+A��A�A�AՏ\A�ZA̲-Aȉ7A�"�A�t�A��uA�VA��A�ĜA���A�ƨA��A��-A���A�33A�&�A���A��TA�;dA��PA�`BA�{A���A�9XA�ƨA�(�A��RA���A��TA��9A�C�A�-A�O�Ap�A}`BA{G�Ay�-Ar��Ao��An�jAm�Aln�Aip�Adv�AbffAa;dA`z�A_oA^JA\=qAV�AQ�#AO�AI�;AFȴAGS�AG��AGAGt�AF��AE�PAA`BA@�A?;dA=�FA<bNA<1A:��A8��A8(�A6��A4-A2�yA2=qA1��A1+A0JA. �A-�A-/A,�/A,n�A+|�A*v�A)�7A)l�A(�A'�A&M�A$��A#t�A"�A!A A�
A|�An�AdZA��A9XA �A9XA9XAJA��A�9A�!A �AhsA{A33A9XA�-At�AhsAhsA;dA��Av�A��A�PA�A�A�A�A\)A33A^5A�^AO�A�A�A�`A~�A(�A�#A��AK�A%A�uAjAM�A-AA��A�PA�AhsA;dA��A�!An�AZA$�A��A��A�jA�A-A��A�7A�A
�\A	��A�uA^5A-A�
A�-A��A�hAO�A+A�Ar�AM�A  A/A��AA�A��A33AVA�A�jA^5A9XA$�AJA�mA��A|�AdZAK�A+A%A �RA �A r�A  �A   @��;@�dZ@��R@��\@�V@�5?@�@���@�x�@�?}@���@��@�j@�Z@�I�@�ƨ@�J@���@��h@�7L@�Q�@��F@�n�@�@��T@��^@��-@��h@���@�Q�@�w@���@�@�=q@�@�7@�&�@��@��@��
@�C�@��y@�$�@�p�@���@�Z@�1@�l�@�ȴ@�^5@�5?@�J@��#@�-@�@�&�@�@�z�@���@畁@�;d@�+@��#@�&�@���@���@� �@�w@�33@�$�@�`B@���@�z�@�I�@�b@��
@�t�@�K�@�~�@�M�@݁@��@��@��@��@���@ڇ+@�@���@�p�@�Q�@׾w@֏\@�^5@�$�@�J@��@���@ա�@Ձ@Լj@�ƨ@�|�@ҏ\@��@ѡ�@��@У�@�I�@��m@϶F@�
=@ͺ^@�hs@��@��@̃@���@�o@ʏ\@�V@���@�/@���@ȃ@�Z@�A�@� �@ǍP@Ɨ�@�n�@���@ř�@ļj@å�@ÍP@�t�@�"�@��@+@���@�&�@��@��P@�K�@��@�V@���@�`B@�?}@��/@�b@�ƨ@��@�+@��@��+@��@�%@���@��D@���@�l�@��R@�$�@��T@���@��7@�7L@��9@��D@�z�@�Q�@��@�\)@���@�$�@��T@��-@��@�`B@�/@��`@��D@�r�@�r�@�bN@�Q�@�Q�@��@�+@�@���@�M�@�$�@�@��@���@���@�hs@�?}@�7L@���@�Ĝ@�z�@��@�ƨ@���@�dZ@�;d@�o@��!@�M�@�@���@�@�X@�%@��@��@�@���@�~�@�^5@�-@�p�@��9@�9X@�1@��@��;@���@��y@��R@��R@�n�@���@�bN@��;@���@�|�@�C�@�o@��H@�~�@�J@��#@���@�@��-@���@�hs@��`@���@�(�@��@��@�|�@��@��H@��!@�v�@��@���@���@�?}@���@��9@�bN@��
@���@�o@�ȴ@��R@��!@���@���@��\@�V@��#@�7L@�j@���@�ƨ@��w@��w@��F@�C�@��H@���@�M�@�J@���@�7L@��@���@��`@��9@���@���@�j@�A�@��@��;@��P@�o@���@�V@�$�@���@��T@��^@�G�@�Ĝ@���@�Q�@���@���@�C�@��@���@�J@���@�hs@��@��`@��@�b@��;@���@�S�@��@��@���@�-@��T@���@��7@�G�@�Ĝ@��D@� �@���@���@�K�@��y@���@�M�@���@�x�@��@�A�@��@l�@~E�@}�T@}@}p�@|�@|�D@{�
@{S�@{"�@{@z�!@zJ@y�7@y7L@x��@x�u@w�@w��@w�P@w|�@w|�@w|�@w\)@w;d@u�T@t�j@tI�@s�m@sƨ@s��@s��@s�@s�@sdZ@sS�@sC�@sC�@sC�@s"�@r�@r�!@q�@q��@q�@p�u@pbN@pb@o��@o�P@n��@nff@nV@m�@l��@l9X@k��@k�
@k�F@kC�@j�\@i�#@i�^@i�7@ihs@h��@h�9@h�@h�@h�@hbN@h  @gl�@g�@f�R@e@e?}@d�@c�
@cƨ@c��@cdZ@b�H@b^5@a��@aX@a%@`��@`�9@`bN@_�w@_\)@^�+@^@]��@]�@]p�@]p�@]/@\�D@\(�@[��@[�
@[��@[t�@[33@Z�H@Z��@Z~�@Z�@Y�@Yx�@Y7L@X��@Xr�@W��@WK�@V�R@Vff@U�-@T��@T��@T��@TI�@T1@S��@St�@SC�@R��@R=q@Qx�@PbN@O�P@O�@N�+@N5?@N$�@M@L��@Lz�@LZ@L1@KC�@J��@J-@I��@I��@IX@H��@HQ�@GK�@F�@Fff@F5?@E�-@EO�@E�@EV@D��@D�@CC�@C@B��@B��@B~�@B-@Ax�@A�@@�`@@�9@@A�@?�;@?\)@>�@>��@>��@>�+@>$�@=�T@=��@=�@<�@<�j@<j@<(�@;�F@;"�@:�H@:�H@:�!@:^5@:J@9�^@9�@8�`@8�`@8�9@8�@8r�@8bN@8Q�@8 �@7|�@6�R@6��@6��@6��@6�+@6V@6{@5��@5�@5?}@5V@4��@49X@4�@3�
@3��@3��@3�@333@2�H@2n�@2=q@2-@1�@1��@1x�@1x�@17L@0��@0bN@0bN@0Q�@01'@0 �@/\)@/K�@/;d@/+@/
=@.��@.ff@.{@-��@-?}@,��@,1@+�@+C�@+"�@*��@*n�@*-@)�^@)G�@)�@(��@(�@(bN@(1'@'�w@'|�@'
=@&�+@&E�@&$�@%�@%@%��@%�@%`B@%O�@%O�@%�@$��@$�/@$��@$�@$9X@#��@#t�@#@"��@"�!@"�!@"�\@"n�@"�@!�#@!��@!�7@!hs@!&�@!�@!%@!%@!%@ ��@ �`@ Ĝ@ ��@ �u@ bN@ 1'@ b@�@�w@��@��@\)@�y@�@ȴ@��@�+@v�@ff@V@V@V@E�@E�@E�@5?@5?@5?@$�@{@@�-@p�@?}@?}@/@/@/@�@�/@�D@Z@��@�m@�
@ƨ@��@��@S�@��@^5@=q@�@�@J@��@�@�@�#@�@�@�@�#@�^@�7@hs@�@�`@Ĝ@Ĝ@�9@�u@r�@A�@A�@1'@1'@1'@b@�w@|�@+@�@�@��@��@5?@�@@�-@�h@p�@`B@O�@O�@/@z�@(�@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	W
B	VB	VB	VB	VB	VB	VB	VB	W
B	W
B	W
B	W
B	W
B	W
B	W
B	W
B	W
B	W
B	VB	VB	VB	W
B	VB	W
B	W
B	W
B	W
B	W
B	VB	VB	VB	T�B	R�B	Q�B	P�B	P�B	O�B	S�B	n�B	�B	�DB	�PB	�bB	�!B	ÖB	ĜB	��B	�7B	�DB	ÖB
�B
�B
�TB�B(�BG�B�B��B��B��B�DB�%B|�BgmBZBM�BO�BN�BL�BM�BL�BG�B9XBVB
�B
�^B
}�B
cTB
>wB
#�B
�B
\B
%B	��B	�B	�5B	��B	��B	ɺB	B	�qB	�FB	�B	��B	��B	��B	��B	��B	�oB	� B	k�B	E�B	K�B	bNB	k�B	r�B	v�B	u�B	s�B	y�B	�B	�B	�B	�B	�B	�%B	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	�B	�FB	�dB	�jB	�wB	��B	ŢB	�B	�NB	�fB	�B	�B	��B
B
B
B	��B	��B	��B	��B	��B
  B
oB
�B
�B
�B
!�B
%�B
 �B
'�B
$�B
 �B
�B
�B
(�B
/B
7LB
8RB
9XB
<jB
?}B
>wB
G�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
L�B
N�B
O�B
O�B
P�B
O�B
P�B
R�B
T�B
T�B
VB
VB
W
B
VB
VB
VB
VB
VB
VB
VB
VB
VB
T�B
T�B
T�B
T�B
T�B
T�B
S�B
R�B
Q�B
P�B
N�B
L�B
I�B
J�B
L�B
K�B
J�B
I�B
H�B
H�B
H�B
G�B
G�B
H�B
H�B
I�B
I�B
H�B
G�B
F�B
E�B
C�B
C�B
C�B
C�B
C�B
B�B
C�B
B�B
B�B
A�B
@�B
?}B
?}B
?}B
>wB
>wB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
>wB
?}B
>wB
>wB
=qB
=qB
=qB
=qB
<jB
<jB
:^B
<jB
<jB
;dB
:^B
9XB
8RB
8RB
8RB
8RB
7LB
7LB
6FB
5?B
5?B
49B
33B
33B
2-B
2-B
33B
2-B
1'B
1'B
1'B
1'B
0!B
/B
/B
/B
.B
.B
.B
.B
.B
.B
.B
.B
-B
-B
-B
,B
,B
,B
+B
+B
)�B
(�B
(�B
(�B
'�B
'�B
'�B
&�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
$�B
"�B
"�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
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
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
(�B
)�B
)�B
)�B
(�B
)�B
)�B
+B
,B
,B
-B
-B
-B
-B
-B
-B
-B
.B
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
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
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
:^B
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
A�B
A�B
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
E�B
F�B
F�B
F�B
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
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
K�B
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
O�B
O�B
O�B
O�B
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
Q�B
R�B
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
T�B
VB
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
W
B
XB
XB
YB
YB
YB
XB
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
`BB
aHB
aHB
aHB
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
e`B
e`B
e`B
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
jB
k�B
k�B
l�B
l�B
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
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
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
w�B
w�B
x�B
x�B
x�B
x�B
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
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	F�B	E�B	E�B	E�B	E�B	E�B	E�B	E�B	F�B	F�B	F�B	F�B	F�B	F�B	F�B	F�B	F�B	F�B	E�B	E�B	E�B	F�B	E�B	F�B	F�B	F�B	F�B	F�B	E�B	E�B	E�B	D�B	B�B	A�B	@�B	@�B	?�B	C�B	^MB	t�B	z�B	}B	�B	��B	�KB	�QB	��B	x�B	z�B	�KB
aB
q�B
�	BaB�B7cBs�B�aB�HB�BBz�Bu�Bl�BW"BI�B=�B?�B>�B<�B=�B<�B7cB)B
�B
�_B
�B
m�B
S	B
.,B
�B
HB	�B	��B	�B	�eB	��B	íB	��B	�oB	�DB	�&B	��B	��B	��B	��B	�zB	��B	��B	�$B	o�B	[:B	5WB	;|B	RB	[:B	beB	f~B	exB	ckB	i�B	p�B	t�B	t�B	r�B	r�B	u�B	B	�B	�*B	�BB	�OB	�aB	�aB	�gB	�mB	��B	��B	�B	�B	�,B	�8B	�WB	ŹB	�B	�B	�4B	�:B	�B	��B	��B	�B	��B	�B	�B	�B	�xB	�B
$B
BB

UB
aB
�B
�B
zB
�B
�B
zB

UB
mB
�B
�B
'B
(B
)B
,B
/2B
.,B
7cB
9oB
9oB
9oB
9oB
9oB
:vB
:vB
<�B
>�B
?�B
?�B
@�B
?�B
@�B
B�B
D�B
D�B
E�B
E�B
F�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
B�B
A�B
@�B
>�B
<�B
9oB
:vB
<�B
;|B
:vB
9oB
8iB
8iB
8iB
7cB
7cB
8iB
8iB
9oB
9oB
8iB
7cB
6]B
5WB
3KB
3KB
3KB
3KB
3KB
2DB
3KB
2DB
2DB
1>B
08B
/2B
/2B
/2B
.,B
.,B
-&B
-&B
-&B
-&B
-&B
-&B
.,B
/2B
/2B
/2B
/2B
.,B
/2B
.,B
.,B
-&B
-&B
-&B
-&B
,B
,B
*B
,B
,B
+B
*B
)B
(B
(B
(B
(B
'B
'B
%�B
$�B
$�B
#�B
"�B
"�B
!�B
!�B
"�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
zB
mB
gB
gB
aB
aB
gB
gB
gB
gB
aB
aB
[B
[B
[B
[B
[B
[B

UB

UB
	OB
HB
HB
HB
HB
HB
HB
HB
HB
HB
	OB
	OB
HB
HB
HB
BB
BB
BB
HB
HB
HB
HB
HB
HB
HB
HB
HB
	OB
	OB
	OB
	OB
	OB

UB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
HB
	OB
	OB
	OB

UB
[B

UB

UB

UB

UB

UB

UB
[B
[B

UB
[B

UB

UB

UB

UB
[B
aB
aB
aB
aB
aB
gB
gB
gB
gB
gB
gB
aB
gB
gB
gB
gB
mB
mB
mB
sB
zB
zB
sB
zB
zB
sB
zB
zB
zB
zB
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
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
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
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
%�B
%�B
%�B
'B
'B
(B
)B
)B
)B
*B
*B
*B
*B
*B
,B
,B
,B
,B
,B
-&B
-&B
-&B
-&B
-&B
-&B
.,B
/2B
/2B
08B
08B
08B
08B
08B
08B
1>B
1>B
1>B
1>B
1>B
1>B
2DB
2DB
2DB
3KB
3KB
3KB
4QB
4QB
4QB
4QB
5WB
6]B
6]B
6]B
7cB
8iB
8iB
8iB
8iB
8iB
8iB
8iB
9oB
9oB
9oB
9oB
9oB
:vB
:vB
:vB
:vB
;|B
;|B
;|B
;|B
;|B
;|B
;|B
:vB
;|B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
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
D�B
E�B
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
F�B
F�B
G�B
G�B
H�B
H�B
H�B
G�B
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
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
RB
RB
RB
RB
S	B
S	B
S	B
S	B
TB
TB
TB
TB
UB
UB
UB
VB
W"B
W"B
W"B
W"B
W"B
X(B
X(B
X(B
X(B
X(B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Z4B
Z4B
Z4B
Z4B
Z4B
Z4B
[:B
[:B
\@B
\@B
[:B
\@B
\@B
\@B
\@B
\@B
]FB
]FB
]FB
]FB
]FB
^MB
^MB
^MB
^MB
^MB
^MB
_SB
_SB
_SB
_SB
_SB
_SB
_SB
_SB
_SB
`YB
`YB
`YB
`YB
`YB
`YB
`YB
a_B
a_B
a_B
a_B
a_B
beB
beB
beB
beB
beB
beB
beB
ckB
ckB
ckB
ckB
ckB
dqB
dqB
dqB
dqB
dqB
exB
exB
exB
exB
exB
exB
f~B
f~B
f~B
f~B
f~B
f~B
f~B
f~B
f~B
g�B
g�B
h�B
h�B
h�B
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
m�B
m�B
m�B
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
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
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
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
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
w�B
w�B
w�B
w�B
w�B
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
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
}B
}B
}B
}B
}B
}B
}B
~B
~B
~B
~B
~B
~B
~B
~B
~B
~B
B
B
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.58 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0159092                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 15 06 2019 151 -0.0159092 0.0003 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20200609100029              20200609100029  AO  ARCAADJP                                                                    20200609100029    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20200609100029    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200609100029  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200609100029  QCF$                G�O�G�O�G�O�0               