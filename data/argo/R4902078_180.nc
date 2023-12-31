CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-03-31T11:00:30Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  `   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ox   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ۈ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۸   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ޸   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20200331110030  20200331110030  4902078 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5439                            2B  A   NAVIS_A                         0460                            011514                          863 @�Y.�Y1   @�Z���n@+�Q���dr�t�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D���D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@^�R@��\@ҏ\A	G�A)G�AIG�AiG�A��
A���A���A���Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"�RB*Q�B2Q�B:Q�BBQ�BJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�{CF�{CH�{CJ�{CL�{CN�{CP�{CR�{CT�{CV�{CXz�CZ�{C\�{C^�{C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D+�D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�Du%Du�Dv%Dv�Dw%Dw�Dx%Dx�Dy%Dy�Dz%Dz�D{%D{�D|%D|�D}%D}�D~%D~�D%D�D��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D�D�ҏD��D�R�DÒ�D�ҏD��D�R�DĒ�D�ҏD��D�R�DŒ�D�ҏD��D�R�Dƒ�D�ҏD��D�R�Dǒ�D�ҏD��D�R�DȒ�D�ҏD��D�R�Dɒ�D�ҏD��D�R�Dʒ�D�ҏD��D�R�D˒�D�ҏD��D�R�D̒�D�ҏD��D�R�D͒�D�ҏD��D�R�DΒ�D�ҏD��D�R�Dϒ�D�ҏD��D�R�DВ�D�ҏD��D�R�Dђ�D�ҏD��D�R�DҒ�D�ҏD��D�R�DӒ�D�ҏD��D�R�DԒ�D�ҏD��D�R�DՒ�D�ҏD��D�R�D֒�D�ҏD��D�R�Dג�D�ҏD��D�R�Dؒ�D�ҏD��D�R�Dْ�D�ҏD��D�R�Dڒ�D�ҏD��D�R�Dے�D�ҏD��D�R�Dܒ�D�ҏD��D�R�Dݒ�D�ҏD��D�R�Dޒ�D�ҏD��D�R�Dߒ�D�ҏD�\D�R�D���D�ҏD��D�R�DᒏD�ҏD��D�R�D⒏D�ҏD��D�R�D㒏D�ҏD��D�R�D䒏D�ҏD��D�R�D咏D�ҏD��D�R�D撏D�ҏD��D�R�D璏D�ҏD��D�R�D蒏D�ҏD��D�R�D钏D�ҏD��D�R�D꒏D�ҏD��D�R�D뒏D�ҏD��D�R�D쒏D�ҏD��D�R�D풏D�ҏD��D�R�DD�ҏD��D�R�DD�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�O\D�D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D���D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�ffA�ffA�l�A�n�A�n�A�p�A�p�A�jA�VA�O�A�C�A�7LA�"�A�  A��A��/A���AҾwA҉7A�A�A�$�A���A��`A��HA��#A���A�bNA�v�A�\)ÁA���AˁA�;dA��A�&�A�"�A�{A�VA�t�A�r�A˓uA�ZA�5?A��AʾwAɟ�A�z�A���A�x�A��A��;A�VA��`A��RA�9XA��A���A�;dA��A��A�bA��
A���A���A�p�A��
A�5?A���A��PA��#A���A� �A��7A��RA�n�A��^A���A�33A��mA��AS�A{�FAz�Ax��Aq�AlbNAbE�A`{A\5?AR�AP~�AO/AN��AM��AHȴAF��AE�^ACdZAB-AA�FA@v�A?�-A>�!A=hsA9dZA7�TA4�A1�A0=qA0�A0bA/�#A.r�A+S�A*�HA*��A(�A(ZA'`BA&�`A'XA'p�A'hsA&��A%�A%t�A$��A$bNA$I�A$9XA$ �A#�#A"��A!ƨA!l�A �9A I�A�#AA�A�A�hA�AȴAv�A�TA7LAA�A5?A�TA��A�wA?}AjA9XAA��A�mAXA33A+A��A�DA�7AhsA?}A&�An�A^5AZA�mA�
A�FA|�AO�A�`A��AA�A�
A��A��A��A`BA7LA�A��A��Ap�A?}A33A
=A
�A
��A
��A
�uA
r�A	��A	ƨA	�wA	�A	�A�jA�+A9XA��A�A�yA�/A�A5?A�mAA��A�hAl�A33A�AbNA=qA{A��A�TA�-AO�A��A��A��A�A��AXA7LA ��A �9A M�@��;@�t�@���@�=q@�-@�$�@��@�$�@���@���@��`@�Q�@��@���@�@���@�hs@���@��;@�l�@��@��\@��@���@�`B@�hs@�?}@�z�@��@��m@�w@�@�\)@�;d@�\@�{@�@�&�@�j@��;@�P@�|�@�S�@�C�@�R@�7@�%@���@���@�r�@띲@�v�@�{@�`B@���@�@�ƨ@�S�@��@�R@�ff@�-@�-@�?}@�r�@�w@��@��T@��@�@�9@��@��D@�  @�|�@�
=@�V@ݩ�@�G�@ܬ@�Z@�1'@ۥ�@��@�M�@ٙ�@ف@��@�Z@׍P@�33@��y@�v�@�O�@�  @���@�M�@��@�z�@��;@�l�@���@�-@��@���@�@ʗ�@�E�@�@���@ə�@�G�@��`@ȣ�@���@�
=@Ƈ+@�E�@���@�/@ă@�1'@�|�@¸R@�=q@��T@��h@��@��9@�bN@�1@��P@�K�@��@���@�J@���@�`B@�G�@�%@�9X@�l�@�"�@���@�ȴ@�$�@���@��@��u@�j@�9X@���@�S�@�o@�o@��@�ff@��@��@�@�`B@�&�@���@�Z@��@��w@�t�@�C�@�;d@�33@�"�@��@���@�E�@�/@���@���@��@��`@���@��@��D@�(�@��P@�K�@���@�M�@�J@���@�G�@���@��`@�Ĝ@��@���@��D@�bN@�A�@��@��;@�t�@�
=@���@��\@��#@�p�@���@���@�I�@� �@��;@�33@��R@�^5@�@���@�x�@��`@�A�@��m@��@�;d@�@��@�ȴ@�~�@�J@�hs@�%@��/@��@��@�l�@�S�@�o@��H@�ȴ@��+@��@��^@�x�@�?}@�V@�Ĝ@�r�@�t�@��@���@�~�@�ff@�-@��@��7@��@��@��u@�j@�Z@�9X@��
@�o@��!@�n�@�^5@�V@�{@���@�7L@���@��D@�1@���@�
=@�ȴ@���@�v�@�$�@��-@�&�@��`@��j@��@�r�@�Q�@�1'@�b@���@���@�33@�o@��@��\@�M�@���@��^@�`B@��`@���@�1'@�|�@�K�@��@�@��y@��@��!@�^5@�$�@��T@��h@�?}@��@���@���@�bN@�  @��
@���@�K�@��@��!@�V@�$�@��#@�@��^@���@�`B@��@�z�@��@��w@��@���@�33@���@�@�
=@�
=@�
=@�
=@���@�n�@�E�@�@��T@�hs@�/@��@�V@��/@���@�bN@�(�@�  @|�@~�@~$�@}�@}V@|�D@{�@z�H@z-@y�7@y7L@x��@xQ�@w�P@w+@v��@vȴ@v�+@u@t��@s��@r�H@r�!@q�@q%@p �@o��@oK�@n��@m�h@l��@l�@lj@k�m@kƨ@k�F@k��@ko@i�@i�7@iX@iG�@i7L@i%@h��@h��@g�P@f�@f5?@e�h@e�@d��@d�@c��@b��@b^5@bJ@a��@ahs@`��@`�@`b@_��@_K�@_+@^�+@]��@]�h@]?}@\�j@\I�@\�@[��@Z�!@ZM�@Y��@Y�^@Y��@YX@XĜ@X�@XA�@W�w@W�@V��@V5?@U�-@Up�@UO�@UV@TI�@S33@R��@R�!@R~�@RM�@R�@Q�#@PĜ@PQ�@O�@Ol�@N�y@N�@N��@N�+@N5?@Mp�@L��@Lz�@LZ@K��@J��@J��@Jn�@J=q@J-@I��@Ix�@I&�@H��@H��@H�`@HĜ@H1'@G�@G�w@G;d@F�R@F��@F��@F��@Fff@F{@E��@E�@EO�@EV@D�/@Dz�@DI�@C��@CS�@Co@B�H@B^5@B�@A�#@A�^@A��@A�7@A&�@A%@@��@@Q�@@  @?�;@?�@?\)@>�R@>�+@>�+@>�+@>ff@>@=O�@<�j@;��@;��@;t�@;ƨ@;�
@;t�@;o@:��@:M�@:=q@:�@9��@8bN@8r�@8bN@7�w@7��@7l�@7|�@7��@7�P@6��@6v�@65?@5�T@5V@4Z@49X@4(�@3t�@2�\@2��@2�\@2M�@1�@1�^@1��@1��@1x�@1&�@0��@0�`@0��@0��@0Ĝ@0�@0 �@/�;@/�@/|�@/+@.��@.�@.�R@.V@-��@-/@,��@,I�@+�m@+��@+@*�@*��@*�\@*�\@*M�@*�@)�@)��@)x�@)X@)�@(�9@(�@(bN@(  @'��@'��@'|�@'+@&ȴ@&�+@&V@%�T@$�@$�@$j@$1@#��@#dZ@#dZ@#t�@#�@#dZ@#33@#@"�@"�!@"�\@"�\@"�\@"�\@"n�@"M�@"=q@!��@!�#@!�^@!hs@!7L@!&�@!%@ ��@ �9@ �u@ Q�@  �@   @�@�@�;@�w@�P@l�@;d@��@�@�R@�+@ff@V@{@��@�h@p�@O�@/@�@�@V@�/@��@Z@1@��@�F@�@C�@�@�!@��@�\@~�@M�@-@��@x�@hs@%@��@Ĝ@��@�u@r�@ �@�@�w@|�@K�@+@
=@��@V@5?@5?@$�@��@�h@`B@`B@`B@/@�/@�@j@9X@�@1@�m@�
@�
@�
@ƨ@��@S�@"�@�@��@n�@�@��@��@��@X@7L@&�@��@�u@Q�@ �@�;@�;@�P@l�@;d@+@�y@�-@/@�@�j@�j@�@�@�@�@��@z�@ƨ@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�`BA�ffA�ffA�l�A�n�A�n�A�p�A�p�A�jA�VA�O�A�C�A�7LA�"�A�  A��A��/A���AҾwA҉7A�A�A�$�A���A��`A��HA��#A���A�bNA�v�A�\)ÁA���AˁA�;dA��A�&�A�"�A�{A�VA�t�A�r�A˓uA�ZA�5?A��AʾwAɟ�A�z�A���A�x�A��A��;A�VA��`A��RA�9XA��A���A�;dA��A��A�bA��
A���A���A�p�A��
A�5?A���A��PA��#A���A� �A��7A��RA�n�A��^A���A�33A��mA��AS�A{�FAz�Ax��Aq�AlbNAbE�A`{A\5?AR�AP~�AO/AN��AM��AHȴAF��AE�^ACdZAB-AA�FA@v�A?�-A>�!A=hsA9dZA7�TA4�A1�A0=qA0�A0bA/�#A.r�A+S�A*�HA*��A(�A(ZA'`BA&�`A'XA'p�A'hsA&��A%�A%t�A$��A$bNA$I�A$9XA$ �A#�#A"��A!ƨA!l�A �9A I�A�#AA�A�A�hA�AȴAv�A�TA7LAA�A5?A�TA��A�wA?}AjA9XAA��A�mAXA33A+A��A�DA�7AhsA?}A&�An�A^5AZA�mA�
A�FA|�AO�A�`A��AA�A�
A��A��A��A`BA7LA�A��A��Ap�A?}A33A
=A
�A
��A
��A
�uA
r�A	��A	ƨA	�wA	�A	�A�jA�+A9XA��A�A�yA�/A�A5?A�mAA��A�hAl�A33A�AbNA=qA{A��A�TA�-AO�A��A��A��A�A��AXA7LA ��A �9A M�@��;@�t�@���@�=q@�-@�$�@��@�$�@���@���@��`@�Q�@��@���@�@���@�hs@���@��;@�l�@��@��\@��@���@�`B@�hs@�?}@�z�@��@��m@�w@�@�\)@�;d@�\@�{@�@�&�@�j@��;@�P@�|�@�S�@�C�@�R@�7@�%@���@���@�r�@띲@�v�@�{@�`B@���@�@�ƨ@�S�@��@�R@�ff@�-@�-@�?}@�r�@�w@��@��T@��@�@�9@��@��D@�  @�|�@�
=@�V@ݩ�@�G�@ܬ@�Z@�1'@ۥ�@��@�M�@ٙ�@ف@��@�Z@׍P@�33@��y@�v�@�O�@�  @���@�M�@��@�z�@��;@�l�@���@�-@��@���@�@ʗ�@�E�@�@���@ə�@�G�@��`@ȣ�@���@�
=@Ƈ+@�E�@���@�/@ă@�1'@�|�@¸R@�=q@��T@��h@��@��9@�bN@�1@��P@�K�@��@���@�J@���@�`B@�G�@�%@�9X@�l�@�"�@���@�ȴ@�$�@���@��@��u@�j@�9X@���@�S�@�o@�o@��@�ff@��@��@�@�`B@�&�@���@�Z@��@��w@�t�@�C�@�;d@�33@�"�@��@���@�E�@�/@���@���@��@��`@���@��@��D@�(�@��P@�K�@���@�M�@�J@���@�G�@���@��`@�Ĝ@��@���@��D@�bN@�A�@��@��;@�t�@�
=@���@��\@��#@�p�@���@���@�I�@� �@��;@�33@��R@�^5@�@���@�x�@��`@�A�@��m@��@�;d@�@��@�ȴ@�~�@�J@�hs@�%@��/@��@��@�l�@�S�@�o@��H@�ȴ@��+@��@��^@�x�@�?}@�V@�Ĝ@�r�@�t�@��@���@�~�@�ff@�-@��@��7@��@��@��u@�j@�Z@�9X@��
@�o@��!@�n�@�^5@�V@�{@���@�7L@���@��D@�1@���@�
=@�ȴ@���@�v�@�$�@��-@�&�@��`@��j@��@�r�@�Q�@�1'@�b@���@���@�33@�o@��@��\@�M�@���@��^@�`B@��`@���@�1'@�|�@�K�@��@�@��y@��@��!@�^5@�$�@��T@��h@�?}@��@���@���@�bN@�  @��
@���@�K�@��@��!@�V@�$�@��#@�@��^@���@�`B@��@�z�@��@��w@��@���@�33@���@�@�
=@�
=@�
=@�
=@���@�n�@�E�@�@��T@�hs@�/@��@�V@��/@���@�bN@�(�@�  @|�@~�@~$�@}�@}V@|�D@{�@z�H@z-@y�7@y7L@x��@xQ�@w�P@w+@v��@vȴ@v�+@u@t��@s��@r�H@r�!@q�@q%@p �@o��@oK�@n��@m�h@l��@l�@lj@k�m@kƨ@k�F@k��@ko@i�@i�7@iX@iG�@i7L@i%@h��@h��@g�P@f�@f5?@e�h@e�@d��@d�@c��@b��@b^5@bJ@a��@ahs@`��@`�@`b@_��@_K�@_+@^�+@]��@]�h@]?}@\�j@\I�@\�@[��@Z�!@ZM�@Y��@Y�^@Y��@YX@XĜ@X�@XA�@W�w@W�@V��@V5?@U�-@Up�@UO�@UV@TI�@S33@R��@R�!@R~�@RM�@R�@Q�#@PĜ@PQ�@O�@Ol�@N�y@N�@N��@N�+@N5?@Mp�@L��@Lz�@LZ@K��@J��@J��@Jn�@J=q@J-@I��@Ix�@I&�@H��@H��@H�`@HĜ@H1'@G�@G�w@G;d@F�R@F��@F��@F��@Fff@F{@E��@E�@EO�@EV@D�/@Dz�@DI�@C��@CS�@Co@B�H@B^5@B�@A�#@A�^@A��@A�7@A&�@A%@@��@@Q�@@  @?�;@?�@?\)@>�R@>�+@>�+@>�+@>ff@>@=O�@<�j@;��@;��@;t�@;ƨ@;�
@;t�@;o@:��@:M�@:=q@:�@9��@8bN@8r�@8bN@7�w@7��@7l�@7|�@7��@7�P@6��@6v�@65?@5�T@5V@4Z@49X@4(�@3t�@2�\@2��@2�\@2M�@1�@1�^@1��@1��@1x�@1&�@0��@0�`@0��@0��@0Ĝ@0�@0 �@/�;@/�@/|�@/+@.��@.�@.�R@.V@-��@-/@,��@,I�@+�m@+��@+@*�@*��@*�\@*�\@*M�@*�@)�@)��@)x�@)X@)�@(�9@(�@(bN@(  @'��@'��@'|�@'+@&ȴ@&�+@&V@%�T@$�@$�@$j@$1@#��@#dZ@#dZ@#t�@#�@#dZ@#33@#@"�@"�!@"�\@"�\@"�\@"�\@"n�@"M�@"=q@!��@!�#@!�^@!hs@!7L@!&�@!%@ ��@ �9@ �u@ Q�@  �@   @�@�@�;@�w@�P@l�@;d@��@�@�R@�+@ff@V@{@��@�h@p�@O�@/@�@�@V@�/@��@Z@1@��@�F@�@C�@�@�!@��@�\@~�@M�@-@��@x�@hs@%@��@Ĝ@��@�u@r�@ �@�@�w@|�@K�@+@
=@��@V@5?@5?@$�@��@�h@`B@`B@`B@/@�/@�@j@9X@�@1@�m@�
@�
@�
@ƨ@��@S�@"�@�@��@n�@�@��@��@��@X@7L@&�@��@�u@Q�@ �@�;@�;@�P@l�@;d@+@�y@�-@/@�@�j@�j@�@�@�@�@��@z�@ƨ@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B\)B[#B[#B[#B[#B[#B[#B[#B[#B[#B[#B[#B[#B\)B\)B]/B]/B]/B]/B^5B`BBaHBbNBcTBdZBdZBdZBv�B�B	E�B	hsB	u�B	~�B	~�B	� B	�JB	��B	��B	�FB	ƨB	��B	�NB	��B
B

=B
�B
9XB
gmB
p�B
�7B
��B
ɺB
�TB
��B!�B49B=qB8RB�BuB�B#�B&�B6FBR�BbNB^5BP�B=qBP�B>wB
��B
��B
��B
�+B
n�B
R�B
F�B
5?B
 �B
oB
1B	�B	�yB	�)B	�^B	��B	l�B	_;B	I�B	)�B	#�B	%�B	+B	)�B	9XB	<jB	D�B	M�B	J�B	H�B	L�B	O�B	Q�B	R�B	VB	W
B	^5B	jB	r�B	s�B	r�B	r�B	z�B	��B	��B	�FB	�wB	��B	�}B	ȴB	�5B	�yB	�B	��B	��B	��B
B
VB
\B
bB
hB
�B
%�B
-B
1'B
9XB
;dB
=qB
>wB
?}B
C�B
K�B
Q�B
R�B
R�B
S�B
Q�B
O�B
J�B
I�B
L�B
Q�B
T�B
R�B
Q�B
O�B
K�B
G�B
J�B
L�B
L�B
L�B
L�B
M�B
P�B
Q�B
P�B
O�B
Q�B
R�B
Q�B
P�B
O�B
O�B
O�B
O�B
N�B
L�B
L�B
N�B
Q�B
S�B
T�B
S�B
N�B
H�B
H�B
H�B
G�B
I�B
K�B
M�B
N�B
N�B
O�B
O�B
M�B
L�B
L�B
K�B
J�B
I�B
F�B
D�B
B�B
A�B
A�B
A�B
?}B
?}B
@�B
C�B
G�B
H�B
G�B
G�B
F�B
E�B
E�B
E�B
E�B
D�B
D�B
C�B
A�B
A�B
@�B
=qB
;dB
9XB
8RB
6FB
6FB
5?B
49B
49B
49B
49B
49B
49B
5?B
6FB
8RB
7LB
5?B
33B
2-B
1'B
1'B
1'B
1'B
2-B
1'B
0!B
/B
/B
0!B
/B
.B
/B
1'B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
/B
/B
/B
/B
.B
-B
-B
-B
,B
,B
+B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
(�B
(�B
(�B
'�B
&�B
'�B
%�B
%�B
%�B
$�B
$�B
%�B
%�B
$�B
#�B
#�B
"�B
"�B
!�B
!�B
!�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
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
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
,B
+B
+B
,B
,B
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
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
0!B
/B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
49B
5?B
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
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
7LB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
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
@�B
@�B
A�B
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
C�B
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
I�B
I�B
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
K�B
L�B
L�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
M�B
N�B
N�B
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
Q�B
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
XB
XB
XB
XB
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
\)B
]/B
]/B
]/B
]/B
]/B
]/B
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
aHB
aHB
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
e`B
e`B
e`B
e`B
e`B
e`B
ffB
gmB
gmB
gmB
hsB
hsB
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
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
n�B
n�B
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
o�B
o�B
q�B
q�B
q�B
r�B
q�B
s�B
s�B
r�B
r�B
q�B
q�B
q�B
p�B
q�B
r�B
s�B
t�B
t�B
u�B
u�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
u�B
u�B
t�B
t�B
u�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�1B
�7B
�1B
�7B
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
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�uB
�oB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BK�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BK�BK�BL�BL�BL�BL�BM�BO�BP�BRBS	BTBTBTBf~BŹB	5WB	X(B	exB	n�B	n�B	o�B	{�B	�OB	�mB	��B	�]B	�vB	�B	�~B	��B	��B
<B
)B
W"B
`YB
x�B
�sB
�oB
�	B
�B�B#�B-&B(BaB*BHB�B�B%�BB�BRBM�B@�B-&B@�B.,B
��B
§B
��B
v�B
^MB
B�B
6]B
$�B
zB
$B	��B	�kB	�.B	��B	�B	�aB	\@B	N�B	9oB	�B	�B	�B	�B	�B	)B	,B	4QB	=�B	:vB	8iB	<�B	?�B	A�B	B�B	E�B	F�B	M�B	Z4B	beB	ckB	beB	beB	j�B	��B	��B	��B	�,B	�>B	�2B	�iB	��B	�.B	�SB	�B	�B	�B	��B	�B	�B
 B
B
BB
�B
�B
 �B
)B
+B
-&B
.,B
/2B
3KB
;|B
A�B
B�B
B�B
C�B
A�B
?�B
:vB
9oB
<�B
A�B
D�B
B�B
A�B
?�B
;|B
7cB
:vB
<�B
<�B
<�B
<�B
=�B
@�B
A�B
@�B
?�B
A�B
B�B
A�B
@�B
?�B
?�B
?�B
?�B
>�B
<�B
<�B
>�B
A�B
C�B
D�B
C�B
>�B
8iB
8iB
8iB
7cB
9oB
;|B
=�B
>�B
>�B
?�B
?�B
=�B
<�B
<�B
;|B
:vB
9oB
6]B
4QB
2DB
1>B
1>B
1>B
/2B
/2B
08B
3KB
7cB
8iB
7cB
7cB
6]B
5WB
5WB
5WB
5WB
4QB
4QB
3KB
1>B
1>B
08B
-&B
+B
)B
(B
%�B
%�B
$�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
(B
'B
$�B
"�B
!�B
 �B
 �B
 �B
 �B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
zB
zB
zB
zB
zB
zB
sB
sB
mB
mB
gB
mB
mB
gB
gB
gB
aB
aB
aB
aB
[B
[B

UB
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
aB
aB
aB
aB
[B
[B
[B
[B
aB
[B
[B
aB
aB
aB
aB
aB
aB
aB
aB
aB
gB
gB
aB
[B
aB
gB
gB
mB
mB
mB
mB
mB
mB
sB
sB
zB
zB
zB
zB
zB
zB
zB
zB
zB
zB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
'B
'B
'B
'B
'B
'B
'B
'B
'B
(B
(B
(B
'B
(B
(B
(B
)B
*B
*B
*B
*B
*B
*B
*B
+B
+B
+B
,B
-&B
-&B
-&B
-&B
-&B
.,B
.,B
.,B
/2B
/2B
/2B
08B
08B
1>B
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
3KB
4QB
5WB
5WB
5WB
5WB
5WB
6]B
6]B
6]B
6]B
7cB
7cB
7cB
7cB
7cB
7cB
8iB
8iB
8iB
8iB
8iB
9oB
9oB
9oB
9oB
:vB
:vB
:vB
;|B
;|B
;|B
;|B
;|B
;|B
<�B
<�B
;|B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
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
B�B
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
E�B
E�B
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
L�B
L�B
L�B
L�B
L�B
L�B
M�B
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
RB
RB
RB
RB
RB
RB
RB
S	B
S	B
S	B
TB
TB
TB
TB
TB
TB
UB
UB
UB
UB
UB
UB
VB
W"B
W"B
W"B
X(B
X(B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Z4B
Z4B
[:B
[:B
[:B
[:B
[:B
[:B
\@B
]FB
]FB
]FB
]FB
]FB
]FB
^MB
_SB
_SB
^MB
^MB
_SB
_SB
`YB
`YB
`YB
`YB
`YB
`YB
`YB
`YB
`YB
_SB
_SB
a_B
a_B
a_B
beB
a_B
ckB
ckB
beB
beB
a_B
a_B
a_B
`YB
a_B
beB
ckB
dqB
dqB
exB
exB
dqB
dqB
dqB
dqB
dqB
dqB
dqB
ckB
exB
exB
dqB
dqB
exB
f~B
f~B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j�B
k�B
k�B
l�B
l�B
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
p�B
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
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
w�B
x�B
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
~B
~B
~B
~B
~B
~B
B
B
B
B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�$B
�$B
�$B
�$B
�$B
�$B
�$B
�*B
�*B
�*B
�*B
�0B
�0B
�0B
�0B
�0B
�*B
�$B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�$1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.58 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0159092                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 15 06 2019 151 -0.0159092 0.0003 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20200331110030              20200331110030  AO  ARCAADJP                                                                    20200331110030    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20200331110030    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200331110030  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200331110030  QCF$                G�O�G�O�G�O�0               