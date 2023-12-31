CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-12T09:36:38Z creation;2019-01-12T09:36:42Z conversion to V3.1;2019-12-23T06:08:46Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190112093638  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               uA   JA  I2_0675_117                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؟O%��1   @؟O�-� @8�A���c3XbM�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A!��AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @XQ�@��\@ҏ\A	G�A*�HAJ�HAiG�A���A���A���A���Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2Q�B:Q�BBQ�BJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�{C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�{CF�{CH�{CJ�{CL�{CN�{CP�{CR�{CT�{CV�{CX�{CZ�{C\�{C^�{C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�=qC�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D�D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#+�D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*+�D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3��D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>�D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�Du%Du�Dv%Dv�Dw%Dw�Dx%Dx�Dy%Dy�Dz%Dz�D{%D{�D|%D|�D}%D}�D~%D~�D%D�D��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD�\D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D�D�ҏD��D�R�DÒ�D�ҏD��D�R�DĒ�D�ҏD��D�R�DŒ�D�ҏD��D�R�Dƒ�D�ҏD��D�R�Dǒ�D�ҏD��D�R�DȒ�D�ҏD��D�R�Dɒ�D�ҏD��D�R�Dʒ�D�ҏD��D�R�D˒�D�ҏD��D�R�D̒�D�ҏD��D�R�D͒�D�ҏD��D�R�DΒ�D�ҏD��D�R�Dϒ�D�ҏD��D�R�DВ�D�ҏD��D�R�Dђ�D�ҏD��D�R�DҒ�D�ҏD��D�R�DӒ�D�ҏD��D�R�DԒ�D�ҏD��D�R�DՒ�D�ҏD��D�R�D֒�D�ҏD��D�R�Dג�D�ҏD��D�R�Dؒ�D�ҏD��D�R�Dْ�D�ҏD��D�R�Dڒ�D�ҏD��D�R�Dے�D�ҏD��D�R�Dܒ�D�ҏD��D�R�Dݒ�D�ҏD��D�R�Dޒ�D�ҏD��D�R�Dߒ�D�ҏD��D�R�D���D�ҏD��D�R�DᒏD�ҏD��D�R�D⒏D�ҏD��D�R�D㒏D�ҏD��D�R�D䒏D�ҏD�\D�R�D咏D�ҏD��D�R�D撏D�ҏD��D�R�D璏D��\D��D�R�D蒏D�ҏD��D�R�D钏D�ҏD��D�R�D꒏D�ҏD��D�R�D뒏D�ҏD��D�R�D쒏D�ҏD��D�R�D풏D�ҏD��D�R�DD�ҏD��D�R�DD�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�U�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��jA��jA���A���A�ĜA�ĜA�ƨA�ĜA�ƨA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A���A���A�A�A�A��RA��9A��wA��FA�ĜA���A�bNA�$�A�O�A�5?A�+A��A���A���A��
A���A�`BA�9XA�+A���A��hA�/A�1A��A���A��wA��9A��DA��hA�hsA�dZA�S�A��A�\)A���A�hsA�dZA��HA���A�dZA���A�t�A��!A��TA���A�9XA��yA�n�A� �A��/A�
=A���A�;dA���A��`A�bA���A���A�ȴA�&�A�v�A�+A���A���A��7A�dZA��A��
A�$�A�VA��mA��A�5?A�JA�7LA�v�A�bA�~�A���A�=qA���A�oA���A��A��^A��RA��A��A�ZA�7LA���A���A��!A�^5A���A�\)A��A�-A|��AyC�Au�;As�7Aq;dAo�wAm��Al-Ak�Ai/Ag&�Ae\)AdbNAaA_�A^��AY�;AV��AS�TARv�AQ?}APZAN=qAL�9AKC�AI|�AHI�AG�AF1ABI�A@A>z�A=�A<1A;XA:I�A8VA6�`A5�wA4��A3�7A2�uA2(�A1O�A.��A-dZA,��A+�TA+?}A)��A)
=A(-A'VA%ƨA$�A#�TA"�uA!��A ��A �+A �A�FAXA�\Ap�AffA��AC�AVA��A1'AjA�
A�A�
A"�AI�A��A�mA��A�!A
~�A	\)A��AbA��A=qA�;A`BA�/A�AK�A��A~�A�AbAJAJA1AƨA �RA �@�33@���@��@��@��D@���@���@���@��9@�l�@�7@�z�@�l�@���@�9X@�R@�hs@�Z@�\@�p�@��m@⟾@�O�@ߥ�@ާ�@�=q@�hs@�?}@��@�V@ܛ�@��@�{@ٲ-@�I�@���@��@�`B@��`@ҟ�@��#@�7L@�Ĝ@�|�@��H@ͺ^@���@��@���@�Ĝ@�|�@ƸR@ř�@��@�l�@\@��#@�G�@�Q�@���@��@�ff@�@��@�/@��@��P@��H@�@�p�@��@���@�9X@��@��@�v�@�^5@�-@�hs@�%@��9@��@� �@�t�@�@�E�@��-@�z�@��m@�ƨ@�;d@��@��+@�V@���@�&�@���@�Ĝ@��@�j@�A�@�9X@�b@�ƨ@�S�@�@��+@��T@�p�@���@��`@��9@�I�@��;@��@�;d@�ȴ@�ff@���@�%@��@� �@��;@��@�l�@�\)@��@��@�$�@���@�x�@�`B@�G�@��@��@��u@���@��D@� �@��m@���@��P@�C�@��@��R@��\@�$�@��^@�p�@�G�@�/@�V@��j@�bN@� �@��
@��P@�;d@�+@��y@��T@��@�O�@�z�@�j@��u@��@�j@�b@��@�ƨ@�l�@��@��!@��+@�@�O�@�&�@���@��/@���@��j@�z�@�1'@�1@�ƨ@�|�@�l�@�+@��\@�$�@��@�$�@�J@���@��7@�`B@�O�@�/@��@��/@���@���@��@�j@�(�@�1@�  @���@�t�@�;d@�33@�@��@���@�v�@�V@�M�@�5?@�{@��@��@��T@��h@�G�@��^@�J@�{@��@�$�@�$�@�-@�-@�V@�J@��#@�@�x�@�hs@�O�@�V@��j@�bN@� �@�b@���@��F@���@��@�t�@�K�@���@���@�=q@�J@��h@�/@��9@�r�@�A�@��@��@��@�l�@�33@�@���@���@���@���@���@��@���@�n�@�@��#@��@�J@�5?@�-@�{@�{@���@���@�p�@��@��@��@�bN@�A�@�(�@�1@���@��w@�|�@�K�@�o@���@�^5@�5?@�-@�@���@��-@��7@�X@�7L@��@��@��@��@�r�@�9X@� �@��@l�@~�y@~��@~E�@~{@}@}?}@|�@|Z@|1@{ƨ@{33@z��@z�\@zJ@y�#@yhs@x��@x��@x1'@w�w@w;d@vȴ@vv�@vE�@u�T@up�@u�@t�/@t�@t�D@t9X@s��@s33@r�H@r��@r�!@r~�@r-@q��@qX@q&�@q�@q%@p��@pr�@pA�@p  @o�@o�@o|�@o|�@o\)@n��@n@m@mO�@l�/@l�@lz�@l�@k�F@kdZ@kC�@ko@j�!@j=q@i��@iG�@i%@h�9@h1'@g��@f��@fv�@e�T@ep�@d��@d��@dZ@d(�@d1@cƨ@cS�@c33@b�!@b^5@b�@a��@a��@a7L@a�@`r�@`A�@` �@_�@_��@_\)@^ȴ@^v�@^@]�T@]�-@]�@]p�@]p�@]?}@\Z@\(�@\�@[ƨ@[dZ@[33@Z�H@Z��@ZM�@Y�@Y��@Y�^@Yx�@YG�@Y7L@Y&�@Y&�@X��@X��@XA�@Xb@W��@W�P@W\)@W;d@V��@V�y@Vȴ@V�+@Vv�@VV@V{@U�h@U`B@U/@T�@T�@S��@St�@SS�@S@R=q@Qx�@Q%@PĜ@PbN@P1'@O�@O�;@O��@O�@Nȴ@Nff@M��@M�@Mp�@MV@L��@L(�@L�@K�m@Kƨ@K��@K"�@J�@J�\@J^5@I��@I�7@IX@IX@I�@H�@H �@G�@G;d@F��@F��@Fȴ@F�R@F��@F��@Fv�@F@E@E?}@D�@D�D@D1@Ct�@C"�@B��@BM�@B�@A��@A��@A�7@Ahs@A7L@@��@@�9@@r�@@1'@?�@?�w@?�@?|�@?K�@?�@>��@>�@>��@>v�@>V@>5?@=�T@=?}@=V@<�/@<��@<Z@<I�@<(�@;��@;�F@;��@;�@;"�@:�H@:��@:��@:~�@9�@9�7@9&�@8��@8r�@8b@8  @7�;@7�@7�P@7K�@7�@6�y@6��@6v�@6v�@6ff@6V@5�h@5V@4��@4�D@49X@3��@3�F@3��@3dZ@3C�@2�@2^5@2-@2J@1�#@1�^@1�7@1hs@1�@0�`@0��@0Q�@01'@0 �@0  @/��@/��@/�P@/K�@.��@.ȴ@.��@.$�@.@-�T@-�-@-�@-/@,��@,��@,�@,z�@,z�@,j@+��@+�@+t�@+33@*�H@*n�@*M�@*M�@*M�@*M�@*M�@*=q@)�#@)��@)7L@)&�@)%@(��@(��@(�9@(Ĝ@(�9@(r�@(Q�@( �@'�w@';d@'
=@&�@&ff@&@%@%`B@%?}@%/@%�@$��@$9X@$1@#��@#ƨ@#��@#�@#dZ@#dZ@#S�@"�@"�!@"�\@"~�@"n�@"M�@"�@!�#@!�^@!X@!&�@!&�@ �`@ �@  �@�;@�P@l�@\)@;d@+@�@��@ff@@��@�-@�h@/@V@�j@z�@Z@I�@I�@(�@1@�@S�@"�@��@^5@-@�@J@�#@��@��@X@&�@��@�9@�@bN@b@  @�;@�@|�@K�@�y@ȴ@��@��@�+@$�@��@��@�@`B@`B@O�@V@�/@�j@��@�D@Z@�
@�@S�@"�@��@�!@~�@-@�#@��@�7@hs@X@&�@�@%@%@��@�`@�`@Ĝ@�9@r�@A�@b@b@�;@�w@�@�@�P1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��jA��jA���A���A�ĜA�ĜA�ƨA�ĜA�ƨA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A���A���A�A�A�A��RA��9A��wA��FA�ĜA���A�bNA�$�A�O�A�5?A�+A��A���A���A��
A���A�`BA�9XA�+A���A��hA�/A�1A��A���A��wA��9A��DA��hA�hsA�dZA�S�A��A�\)A���A�hsA�dZA��HA���A�dZA���A�t�A��!A��TA���A�9XA��yA�n�A� �A��/A�
=A���A�;dA���A��`A�bA���A���A�ȴA�&�A�v�A�+A���A���A��7A�dZA��A��
A�$�A�VA��mA��A�5?A�JA�7LA�v�A�bA�~�A���A�=qA���A�oA���A��A��^A��RA��A��A�ZA�7LA���A���A��!A�^5A���A�\)A��A�-A|��AyC�Au�;As�7Aq;dAo�wAm��Al-Ak�Ai/Ag&�Ae\)AdbNAaA_�A^��AY�;AV��AS�TARv�AQ?}APZAN=qAL�9AKC�AI|�AHI�AG�AF1ABI�A@A>z�A=�A<1A;XA:I�A8VA6�`A5�wA4��A3�7A2�uA2(�A1O�A.��A-dZA,��A+�TA+?}A)��A)
=A(-A'VA%ƨA$�A#�TA"�uA!��A ��A �+A �A�FAXA�\Ap�AffA��AC�AVA��A1'AjA�
A�A�
A"�AI�A��A�mA��A�!A
~�A	\)A��AbA��A=qA�;A`BA�/A�AK�A��A~�A�AbAJAJA1AƨA �RA �@�33@���@��@��@��D@���@���@���@��9@�l�@�7@�z�@�l�@���@�9X@�R@�hs@�Z@�\@�p�@��m@⟾@�O�@ߥ�@ާ�@�=q@�hs@�?}@��@�V@ܛ�@��@�{@ٲ-@�I�@���@��@�`B@��`@ҟ�@��#@�7L@�Ĝ@�|�@��H@ͺ^@���@��@���@�Ĝ@�|�@ƸR@ř�@��@�l�@\@��#@�G�@�Q�@���@��@�ff@�@��@�/@��@��P@��H@�@�p�@��@���@�9X@��@��@�v�@�^5@�-@�hs@�%@��9@��@� �@�t�@�@�E�@��-@�z�@��m@�ƨ@�;d@��@��+@�V@���@�&�@���@�Ĝ@��@�j@�A�@�9X@�b@�ƨ@�S�@�@��+@��T@�p�@���@��`@��9@�I�@��;@��@�;d@�ȴ@�ff@���@�%@��@� �@��;@��@�l�@�\)@��@��@�$�@���@�x�@�`B@�G�@��@��@��u@���@��D@� �@��m@���@��P@�C�@��@��R@��\@�$�@��^@�p�@�G�@�/@�V@��j@�bN@� �@��
@��P@�;d@�+@��y@��T@��@�O�@�z�@�j@��u@��@�j@�b@��@�ƨ@�l�@��@��!@��+@�@�O�@�&�@���@��/@���@��j@�z�@�1'@�1@�ƨ@�|�@�l�@�+@��\@�$�@��@�$�@�J@���@��7@�`B@�O�@�/@��@��/@���@���@��@�j@�(�@�1@�  @���@�t�@�;d@�33@�@��@���@�v�@�V@�M�@�5?@�{@��@��@��T@��h@�G�@��^@�J@�{@��@�$�@�$�@�-@�-@�V@�J@��#@�@�x�@�hs@�O�@�V@��j@�bN@� �@�b@���@��F@���@��@�t�@�K�@���@���@�=q@�J@��h@�/@��9@�r�@�A�@��@��@��@�l�@�33@�@���@���@���@���@���@��@���@�n�@�@��#@��@�J@�5?@�-@�{@�{@���@���@�p�@��@��@��@�bN@�A�@�(�@�1@���@��w@�|�@�K�@�o@���@�^5@�5?@�-@�@���@��-@��7@�X@�7L@��@��@��@��@�r�@�9X@� �@��@l�@~�y@~��@~E�@~{@}@}?}@|�@|Z@|1@{ƨ@{33@z��@z�\@zJ@y�#@yhs@x��@x��@x1'@w�w@w;d@vȴ@vv�@vE�@u�T@up�@u�@t�/@t�@t�D@t9X@s��@s33@r�H@r��@r�!@r~�@r-@q��@qX@q&�@q�@q%@p��@pr�@pA�@p  @o�@o�@o|�@o|�@o\)@n��@n@m@mO�@l�/@l�@lz�@l�@k�F@kdZ@kC�@ko@j�!@j=q@i��@iG�@i%@h�9@h1'@g��@f��@fv�@e�T@ep�@d��@d��@dZ@d(�@d1@cƨ@cS�@c33@b�!@b^5@b�@a��@a��@a7L@a�@`r�@`A�@` �@_�@_��@_\)@^ȴ@^v�@^@]�T@]�-@]�@]p�@]p�@]?}@\Z@\(�@\�@[ƨ@[dZ@[33@Z�H@Z��@ZM�@Y�@Y��@Y�^@Yx�@YG�@Y7L@Y&�@Y&�@X��@X��@XA�@Xb@W��@W�P@W\)@W;d@V��@V�y@Vȴ@V�+@Vv�@VV@V{@U�h@U`B@U/@T�@T�@S��@St�@SS�@S@R=q@Qx�@Q%@PĜ@PbN@P1'@O�@O�;@O��@O�@Nȴ@Nff@M��@M�@Mp�@MV@L��@L(�@L�@K�m@Kƨ@K��@K"�@J�@J�\@J^5@I��@I�7@IX@IX@I�@H�@H �@G�@G;d@F��@F��@Fȴ@F�R@F��@F��@Fv�@F@E@E?}@D�@D�D@D1@Ct�@C"�@B��@BM�@B�@A��@A��@A�7@Ahs@A7L@@��@@�9@@r�@@1'@?�@?�w@?�@?|�@?K�@?�@>��@>�@>��@>v�@>V@>5?@=�T@=?}@=V@<�/@<��@<Z@<I�@<(�@;��@;�F@;��@;�@;"�@:�H@:��@:��@:~�@9�@9�7@9&�@8��@8r�@8b@8  @7�;@7�@7�P@7K�@7�@6�y@6��@6v�@6v�@6ff@6V@5�h@5V@4��@4�D@49X@3��@3�F@3��@3dZ@3C�@2�@2^5@2-@2J@1�#@1�^@1�7@1hs@1�@0�`@0��@0Q�@01'@0 �@0  @/��@/��@/�P@/K�@.��@.ȴ@.��@.$�@.@-�T@-�-@-�@-/@,��@,��@,�@,z�@,z�@,j@+��@+�@+t�@+33@*�H@*n�@*M�@*M�@*M�@*M�@*M�@*=q@)�#@)��@)7L@)&�@)%@(��@(��@(�9@(Ĝ@(�9@(r�@(Q�@( �@'�w@';d@'
=@&�@&ff@&@%@%`B@%?}@%/@%�@$��@$9X@$1@#��@#ƨ@#��@#�@#dZ@#dZ@#S�@"�@"�!@"�\@"~�@"n�@"M�@"�@!�#@!�^@!X@!&�@!&�@ �`@ �@  �@�;@�P@l�@\)@;d@+@�@��@ff@@��@�-@�h@/@V@�j@z�@Z@I�@I�@(�@1@�@S�@"�@��@^5@-@�@J@�#@��@��@X@&�@��@�9@�@bN@b@  @�;@�@|�@K�@�y@ȴ@��@��@�+@$�@��@��@�@`B@`B@O�@V@�/@�j@��@�D@Z@�
@�@S�@"�@��@�!@~�@-@�#@��@�7@hs@X@&�@�@%@%@��@�`@�`@Ĝ@�9@r�@A�@b@b@�;@�w@�@�@�P1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bu�Bu�Bu�Bz�Bx�Bw�B|�B�1B��BŢBoB6FBD�BE�BF�BG�BF�BM�BS�BP�BN�BR�BQ�BM�BR�BaHBm�Bx�B��B�?BBǮBƨBƨBĜBB�?B��B�%Bw�By�Bw�Bw�Bt�Bx�Bt�Bn�BjBffBiyBl�Bl�Bo�Bq�Bo�Bm�Bm�Bn�BjBaHB^5BXBJ�B<jB+B$�B%�B%�B%�B�B\B�mB�`B�NB�#B�B��BȴB�B��B�oBy�Bt�BZBF�B=qB2-B)�B�B
��B
�B
�)B
��B
�BB
�)B
��B
B
�B
�%B
k�B
P�B
8RB
{B	�B	�/B	ɺB	�jB	�B	��B	�{B	�+B	x�B	jB	cTB	R�B	D�B	9XB	�B	B��B�B��B��B�B�B�fB�ZB�NB�B�B��BƨB��B�qB�XB�FB�FB�?B�!B�B��B��B��B��B��B�PB�1B�=B�B�B�B{�Bw�Bt�Bq�Bm�Bl�Be`B`BB]/B_;BaHBaHB_;B[#BZBVBT�BR�BO�BO�BM�BI�BG�BF�BC�B@�B>wB;dB7LB6FB1'B0!B-B,B-B,B+B)�B)�B'�B'�B%�B%�B%�B$�B$�B$�B#�B#�B#�B#�B"�B"�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B$�B#�B$�B%�B&�B'�B'�B&�B'�B(�B(�B'�B(�B+B)�B)�B-B.B1'B1'B1'B1'B2-B33B33B7LB7LB8RB8RB7LB9XB@�B?}BA�BD�BC�BD�BG�BI�BJ�BM�BM�BO�BQ�BT�BT�BVBZB\)B^5BaHBbNBcTBdZBe`BgmBiyBl�Bl�Bl�Bn�Bo�Bq�Bq�Br�Bu�Bv�Bx�Bz�B�B�B�B�B�B�B�%B�=B�bB�hB�oB�uB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�LB�dB��BŢBȴB��B��B��B�B�B�B�#B�ZB�B�B�B�B�B��B��B��B	B	+B	
=B	JB	PB	bB	oB	uB	uB	�B	�B	�B	�B	�B	 �B	#�B	&�B	(�B	,B	.B	1'B	2-B	33B	33B	33B	8RB	;dB	>wB	B�B	G�B	I�B	L�B	N�B	O�B	Q�B	T�B	T�B	VB	XB	\)B	\)B	]/B	]/B	]/B	]/B	_;B	`BB	`BB	bNB	bNB	bNB	cTB	gmB	l�B	n�B	o�B	q�B	s�B	v�B	w�B	y�B	{�B	~�B	�B	�%B	�+B	�1B	�7B	�=B	�JB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�FB	�LB	�LB	�RB	�XB	�jB	�jB	�}B	B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ȴB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�;B	�HB	�HB	�NB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
JB
JB
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
\B
bB
bB
bB
bB
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
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
&�B
'�B
'�B
'�B
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
,B
-B
-B
.B
.B
/B
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
1'B
1'B
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
6FB
5?B
5?B
6FB
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
@�B
@�B
@�B
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
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
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
I�B
J�B
J�B
J�B
J�B
J�B
J�B
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
N�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
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
XB
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
XB
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
jB
jB
jB
jB
jB
jB
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bu�Bu�Bu�Bz�Bx�Bw�B|�B��B�qB�mB:B6BDgBEmBFtBGzBFtBM�BS�BP�BN�BR�BQ�BM�BR�BaBm]Bx�B��B�B�[B�zB�tB�tB�gB�[B�B�xB��Bw�By�Bw�Bw�Bt�Bx�Bt�BncBjKBf2BiDBlWBlWBoiBqvBoiBm]Bm]BncBjKBaB^BW�BJ�B<6B*�B$�B%�B%�B%�B�B(B�8B�,B�B��B��BѷBȀB��B��B�:By�Bt�BY�BFtB=<B1�B)�BkB
��B
�KB
��B
��B
�B
��B
ϑB
�[B
��B
��B
kQB
P�B
8B
FB	�|B	��B	�lB	�6B	��B	�]B	�FB	��B	x�B	jKB	cB	R�B	DMB	9$B	WB	�B��B�B�zB�zB�[B�cB�B�&B�B��BյBҽB�tB�UB�"B�$B��B��B��B��B��B��B�|B�dB�QB�?B�B��B��B��B��B��B{�Bw�Bt�BqvBm]Bl=Be,B_�B\�B_B`�B`�B^�BZ�BY�BU�BT�BR�BO�BO�BM�BIlBGzBFtBCGB@4B>(B;B6�B5�B0�B/�B,�B+�B,�B+�B*�B)�B)�B'�B'�B%�B%�B%�B$�B$�B$�B#�B#�B#�B#�B"�B"�B �B vB �BpBpB�BjBjBjBjB�BdBpBjB vB!�B"�B$�B#�B$�B%�B&�B'�B'�B&�B'�B(�B(�B'�B(�B*�B)�B)�B,�B-�B0�B0�B0�B0�B1�B2�B2�B6�B6�B8B8B6�B9	B@4B?.BA;BDgBCGBDMBGzBI�BJ�BM�BM�BO�BQ�BT�BT�BU�BY�B[�B]�B`�BbBcBdBeBgBi*Bl=Bl=BlWBnIBoOBq[Bq[BraButBvzBx�Bz�B��B��B��B��B��B��B��B��B�B�B� B�&B�&B�,B�,B�2B�9B�KB�kB�dB�|B��B��B��B��B��B��B��B��B��B�B�4B�SB�fB�rBΊBѝBյB��B��B��B�B�6B�]B�OB�UB�|B��B��B��B	 �B	�B		�B	�B	B	.B	 B	&B	&B	?B	WB	dB	jB	pB	 vB	#�B	&�B	(�B	+�B	-�B	0�B	1�B	2�B	2�B	2�B	8B	;B	>(B	BAB	G_B	IlB	L~B	N�B	O�B	Q�B	T�B	T�B	U�B	W�B	[�B	[�B	\�B	\�B	\�B	\�B	_B	_�B	_�B	a�B	a�B	a�B	cB	gB	l=B	nIB	oOB	q[B	shB	vzB	w�B	y�B	{�B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�4B	�2B	�9B	�EB	�KB	�KB	�xB	�jB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�B	�B	�.B	�[B	�AB	�GB	�MB	�SB	�SB	�SB	�mB	�mB	�SB	�tB	�fB	�fB	�fB	�fB	�fB	ʌB	�xB	�~B	̈́B	͟B	�~B	�~B	�~B	ΊB	ϑB	ЗB	ѝB	өB	ңB	өB	յB	յB	��B	յB	��B	յB	ּB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�8B	�$B	�*B	�*B	�*B	�*B	�0B	�6B	�=B	�=B	�]B	�IB	�iB	�OB	�UB	�[B	�[B	�aB	�aB	�hB	�hB	�hB	�B	�tB	�tB	�tB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

	B

	B
B

�B
�B
�B
�B
�B
�B
�B
B
"B
B
B
B
B
B
B
B
B
B
B
B
4B
 B
:B
&B
@B
&B
,B
,B
2B
9B
SB
9B
?B
?B
EB
EB
EB
KB
KB
QB
QB
kB
WB
WB
]B
]B
]B
]B
]B
]B
dB
dB
jB
jB
pB
pB
pB
pB
�B
pB
 vB
 vB
 vB
 vB
!|B
!|B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
&�B
'�B
'�B
'�B
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
+�B
,�B
,�B
-�B
-�B
.�B
.�B
.�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
2�B
2�B
2�B
2�B
3�B
3�B
4�B
4�B
4�B
5B
5B
5�B
5B
4�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
8B
8B
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
<B
<B
<B
<B
<B
<B
="B
="B
="B
="B
="B
="B
=<B
>(B
>(B
>(B
>BB
>(B
>(B
?.B
?.B
?HB
?.B
?.B
@4B
@4B
@OB
@4B
@4B
A;B
AUB
A;B
BAB
B[B
CaB
CGB
CGB
CGB
CaB
CGB
DMB
DMB
DMB
DMB
DMB
DMB
DMB
ESB
FYB
FYB
FYB
FYB
FYB
G_B
GzB
GzB
G_B
G_B
HfB
HfB
HfB
HfB
HfB
HfB
HfB
IlB
IlB
IlB
IlB
I�B
IlB
JrB
JrB
JrB
JrB
J�B
JrB
KxB
KxB
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
M�B
M�B
N�B
N�B
N�B
N�B
O�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
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
W�B
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
W�B
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
Z�B
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
^B
^B
^B
^�B
^�B
^�B
_B
^�B
_�B
_�B
_�B
`B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
cB
cB
cB
cB
cB
c B
dB
dB
dB
dB
dB
dB
dB
eB
eB
eB
e,B
e,B
fB
f2B
f2B
g8B
gB
gB
g8B
gB
h$B
h>B
h$B
h$B
h$B
h$B
iDB
i*B
i*B
i*B
iDB
i*B
i*B
iDB
i*B
i*B
j0B
j0B
j0B
j0B
jKB
j0B
j01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.58(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901170033252019011700332520190117003325201901180021422019011800214220190118002142JA  ARFMdecpA19c                                                                20190112183635  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190112093638  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190112093640  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190112093640  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190112093641  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190112093641  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190112093641  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190112093641  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190112093642  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190112093642                      G�O�G�O�G�O�                JA  ARUP                                                                        20190112095612                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190112154234  CV  JULD            G�O�G�O�F��z                JM  ARCAJMQC2.0                                                                 20190116153325  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190116153325  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190117152142  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                