CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:11Z AOML 3.0 creation; 2016-08-07T21:17:33Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221311  20160807141733  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_030                   2C  D   APEX                            6487                            072314                          846 @�6����1   @�6�m�?�@,�fffff�d�\(��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B���B���B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	�fD
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dys3D�  D�` D�s3D�� D���D�FfD�� D��3D��D�P D�y�D��3D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@ҏ\A	G�A)G�AIG�AiG�A���A���A���A���Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"�RB*Q�B2Q�B:Q�BBQ�BJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B���B���B���B�(�B�(�B�(�B�\)B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�{C.�{C0�{C2�{C4�{C6�C8�C:�{C<�{C>�{C@z�CB�{CD�{CF�{CH�{CJ�{CL�{CN�{CP�{CR�{CT�{CV�{CX�{CZ�{C\�{C^�{C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�W
C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	��D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&��D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM��DN%DN�DO%DO�DP+�DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl��Dm+�Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�RDy�RD��D�r�D���D��D�\D�X�D���D���D�\D�b�D��)D���D�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AѲ-AѮAѲ-Aѥ�Aѡ�Aѡ�Aѡ�Aџ�Aѝ�Aљ�Aљ�Aљ�Aѝ�Aџ�Aѡ�Aѩ�A���AҶFA��;A��mA��A�AҰ!A҃A�G�AѾwA��A�M�A��/A��A�%A�dZȂhA�Q�A�r�AʃA���A˙�A˝�A�
=A�S�Aɉ7A�ffA�dZA�+A�JA��A�`BA���A��A�hsA���A���A�=qA�dZA�(�A�JA���A���A�1A�/A�(�A�n�A�A�A�/A�+A��9A�(�A��`A���A�+A�z�A��jA��!A�ƨA��mA���A�K�A���A�\)A�5?A�t�A�v�A���A�z�A���A�7LA��A��Az�HAs�AoAhJAc�PAadZA[;dAT�!AQO�AL5?AG�AF��AE�7ADffAD{AC�A@��A?�A>�9A<�RA;;dA933A7XA5��A3��A3�A4�A3
=A2��A2�RA3�A3��A3��A3�A3`BA3hsA3�-A3��A3�A0jA,ĜA,��A,��A,v�A+�A*��A*��A*�uA*{A)�-A)K�A(JA&r�A%��A%�wA%p�A$�`A$�uA#��A#�A"�yA"�jA"5?A ��A33A�A�-AS�A&�AoAA�DA33AA�`A��A(�A��AdZAoAĜA��A�DAv�AVAbA��A�AhsA%AĜA��A~�AZA-AbA�-AG�AoA�uA�FA�/A(�A
=AQ�A��A�A%A�`A1'AS�A%A
n�A	��A	�FA	�-A	��A	p�A	&�A�yAȴA��A-A�hAl�AO�A�A%A��AĜAĜA5?Ap�A�A�RA�Av�A�\A��AbNA��A��Al�A/A�A��AA�A�wA|�A ��A v�A   @��@��@�
=@�ff@���@��@�`B@��@��j@�j@��;@�\)@�=q@�@�`B@��D@��F@�;d@��y@�J@���@��@���@�9X@���@�33@��H@�ff@�@���@�9X@�+@�x�@�I�@�K�@�ȴ@�@��@�(�@��@��@䛦@��m@�K�@�
=@�n�@���@� �@�@�ff@�/@��m@�|�@�~�@؃@؋D@�A�@��@׮@���@Ցh@�Z@�b@�ƨ@ӕ�@�S�@ҏ\@��@���@�1'@ύP@Η�@�V@�5?@�$�@Ͳ-@̬@�A�@�(�@�t�@��y@ʇ+@�-@ɲ-@���@Ȭ@Ǯ@�"�@���@�{@�/@���@�z�@�I�@Õ�@�S�@���@�V@���@�%@��9@�1'@��w@�S�@��@�~�@�{@��7@��D@�9X@��
@���@�l�@��@��H@��!@��+@�$�@��@��@���@�j@���@���@�"�@��!@�-@��7@��@�9X@��;@���@��w@���@��@�C�@�o@��H@�n�@�E�@���@�hs@�O�@��@��@���@�l�@�C�@��H@�~�@�V@�@��7@�X@�X@�?}@��@��@�r�@���@��P@�\)@�+@�ȴ@���@�v�@�J@��#@���@���@�bN@��;@��w@��@���@���@�t�@�C�@�
=@��!@�n�@�5?@��#@�`B@��@���@��9@�Z@���@���@�\)@�ȴ@�^5@��@���@���@�z�@���@��
@��w@��@���@�t�@��y@��R@���@�5?@��@��-@��@�`B@�G�@�?}@���@��D@�I�@�l�@�K�@�o@�@��!@�5?@���@�`B@�O�@�7L@�&�@���@��/@��9@��u@�r�@�Q�@�(�@�  @��@���@��!@�~�@�ff@�5?@��#@��7@�X@�V@���@��u@��@���@��-@�r�@v5?@n@f$�@[t�@U�-@M?}@Gl�@>��@7�@0�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  AѲ-AѮAѲ-Aѥ�Aѡ�Aѡ�Aѡ�Aџ�Aѝ�Aљ�Aљ�Aљ�Aѝ�Aџ�Aѡ�Aѩ�A���AҶFA��;A��mA��A�AҰ!A҃A�G�AѾwA��A�M�A��/A��A�%A�dZȂhA�Q�A�r�AʃA���A˙�A˝�A�
=A�S�Aɉ7A�ffA�dZA�+A�JA��A�`BA���A��A�hsA���A���A�=qA�dZA�(�A�JA���A���A�1A�/A�(�A�n�A�A�A�/A�+A��9A�(�A��`A���A�+A�z�A��jA��!A�ƨA��mA���A�K�A���A�\)A�5?A�t�A�v�A���A�z�A���A�7LA��A��Az�HAs�AoAhJAc�PAadZA[;dAT�!AQO�AL5?AG�AF��AE�7ADffAD{AC�A@��A?�A>�9A<�RA;;dA933A7XA5��A3��A3�A4�A3
=A2��A2�RA3�A3��A3��A3�A3`BA3hsA3�-A3��A3�A0jA,ĜA,��A,��A,v�A+�A*��A*��A*�uA*{A)�-A)K�A(JA&r�A%��A%�wA%p�A$�`A$�uA#��A#�A"�yA"�jA"5?A ��A33A�A�-AS�A&�AoAA�DA33AA�`A��A(�A��AdZAoAĜA��A�DAv�AVAbA��A�AhsA%AĜA��A~�AZA-AbA�-AG�AoA�uA�FA�/A(�A
=AQ�A��A�A%A�`A1'AS�A%A
n�A	��A	�FA	�-A	��A	p�A	&�A�yAȴA��A-A�hAl�AO�A�A%A��AĜAĜA5?Ap�A�A�RA�Av�A�\A��AbNA��A��Al�A/A�A��AA�A�wA|�A ��A v�A   @��@��@�
=@�ff@���@��@�`B@��@��j@�j@��;@�\)@�=q@�@�`B@��D@��F@�;d@��y@�J@���@��@���@�9X@���@�33@��H@�ff@�@���@�9X@�+@�x�@�I�@�K�@�ȴ@�@��@�(�@��@��@䛦@��m@�K�@�
=@�n�@���@� �@�@�ff@�/@��m@�|�@�~�@؃@؋D@�A�@��@׮@���@Ցh@�Z@�b@�ƨ@ӕ�@�S�@ҏ\@��@���@�1'@ύP@Η�@�V@�5?@�$�@Ͳ-@̬@�A�@�(�@�t�@��y@ʇ+@�-@ɲ-@���@Ȭ@Ǯ@�"�@���@�{@�/@���@�z�@�I�@Õ�@�S�@���@�V@���@�%@��9@�1'@��w@�S�@��@�~�@�{@��7@��D@�9X@��
@���@�l�@��@��H@��!@��+@�$�@��@��@���@�j@���@���@�"�@��!@�-@��7@��@�9X@��;@���@��w@���@��@�C�@�o@��H@�n�@�E�@���@�hs@�O�@��@��@���@�l�@�C�@��H@�~�@�V@�@��7@�X@�X@�?}@��@��@�r�@���@��P@�\)@�+@�ȴ@���@�v�@�J@��#@���@���@�bN@��;@��w@��@���@���@�t�@�C�@�
=@��!@�n�@�5?@��#@�`B@��@���@��9@�Z@���@���@�\)@�ȴ@�^5@��@���@���@�z�@���@��
@��w@��@���@�t�@��y@��R@���@�5?@��@��-@��@�`B@�G�@�?}@���@��D@�I�@�l�@�K�@�o@�@��!@�5?@���@�`B@�O�@�7L@�&�@���@��/@��9@��u@�r�@�Q�@�(�@�  @��@���@��!@�~�@�ff@�5?@��#@��7@�X@�V@���G�O�@��@���@��-@�r�@v5?@n@f$�@[t�@U�-@M?}@Gl�@>��@7�@0�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	+B	+B	+B	+B	+B	+B	1B	1B		7B	
=B	JB	VB	{B	�B	�B	�B	VB	ǮB	�;B	�sB	�B	�B	��B
B	��B	��B
�B
:^B
e`B
��B
�'B
��B
�HB
��B
�)B#�BhsB�wB�B��B��BVB#�B  B�BcTB~�B��B��B��B��B��BŢB��B�mB�B�B�fB�B��B��B�3B��BYB+B�B�B8RB33B%�BJB�B�B�9B��B�JB�\B�+B|�Bo�BW
B49B
��B
ŢB
�bB
n�B
F�B
+B
VB	�`B	�!B	�VB	]/B	B�B	1'B	\B��B�B�B�B�mB�ZB�TB�HB�/B�B��B��BɺB��B��B�B�#B�sB	�B	9XB	E�B	S�B	q�B	�B	��B	�?B	�RB	�^B	ÖB	��B	��B	��B	ŢB	�qB	�NB	��B	��B	��B	��B	��B
+B
1B
+B
+B
JB
hB
�B
1'B
49B
49B
33B
2-B
33B
2-B
1'B
/B
,B
$�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
�B
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
"�B
"�B
"�B
!�B
 �B
 �B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
{B
uB
uB
uB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
oB
oB
oB
oB
hB
hB
hB
oB
hB
oB
hB
hB
bB
bB
\B
\B
\B
VB
PB
PB
VB
PB
PB
JB

=B
	7B
+B
+B
+B
%B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
	7B
1B
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
PB
PB
VB
VB
VB
VB
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
hB
oB
uB
uB
uB
{B
uB
uB
{B
{B
{B
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
{B
{B
{B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
)�B
1'B
:^B
?}B
E�B
J�B
M�B
Q�B
XB
]/B
bNB
ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B	B	B	B	B	B	B	B	B		B	
 B	/B	<B	`B	fB	lB	�B	U�B	ǍB	�B	�QB	�gB	�B	��B
�B	��B	��B
�B
:5B
e9B
��B
��B
ͥB
�B
̢B
��B#�BhFB�HB�TB��B��B%B#�B��ByBc#B~�B�bB��B��B��B��B�pB��B�?B�oB�mB�5B��B��B̚B��B�iBX�B*�B�B�B8B2�B%�BB�vB��B�B�LB�B�$B��B|�BofBV�B4B
��B
�jB
�-B
n`B
FsB
*�B
"B	�/B	��B	�&B	\�B	BaB	0�B	0B��B�jB�B�WB�>B�)B�(B�B�B��B��BΪBɌBʔB��B��B��B�DB	�B	9#B	EqB	S�B	qsB	��B	�oB	�B	�B	�&B	�\B	͙B	ѱB	ѳB	�hB	�7B	�B	��B	��B	��B	��B	��B
�B
�B
�B
�B
B
*B
yB
0�B
3�B
3�B
2�B
1�B
2�B
1�B
0�B
.�B
+�B
$�B
tB
iB
jB
bB
]B
TB
IB
<B
:B
;B
;B
`B
oB
nB
uB
xB
yB
uB
lB
mB
pB
mB
lB
rB
zB
�B
B
~B
 �B
!�B
"�B
"�B
"�B
!�B
 �B
 �B
"�B
 �B
sB
hB
`B
iB
pB
sB
aB
SB
NB
GB
MB
LB
MB
LB
MB
KB
NB
OB
OB
EB
FB
FB
EB
GB
FB
IB
MB
TB
AB
<B
6B
2B
5B
:B
GB
ZB
ZB
[B
[B
WB
SB
RB
KB
EB
FB
AB
:B
4B
-B
:B
5B
4B
4B
+B
.B
,B
5B
5B
5B
6B
6B
4B
6B
,B
.B
.B
.B
%B
&B
&B
+B
&B
,B
%B
%B
 B
B
B
B
B
B
B
B
B
B
B
B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B
 B
B
B
B
B
B
B
B
B
B
B
 B
B
B
 B
B
 B
%B
%B
*B
(B
'B
'B
)B
*B
*B
 B
)B
/B
/B
1B
4B
3B
1B
7B
5B
5B
0B
1B
0B
4B
BB
@B
HB
HB
AB
BB
5B
7B
4B
EB
HB
GB
@B
JB
HB
GB
GB
OB
NB
LB
OB
MB
PB
NB
MB
NB
WB
[B
YB
ZB
ZB
\B
[B
_B
`B
bG�O�B
kB
#�B
)�B
0�B
:B
?4B
EZB
JyB
M�B
Q�B
W�B
\�B
bB
f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.58 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417332016080714173320160807141733  AO  ARCAADJP                                                                    20150226221311    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221311  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221311  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141733  IP                  G�O�G�O�G�O�                