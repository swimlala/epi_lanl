CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-12T20:16:48Z AOML 3.0 creation; 2016-08-07T21:17:43Z UW 3.1 conversion     
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
_FillValue                 �  A|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ct   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]    TEMP_ADJUSTED            
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
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151212201648  20160807141743  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ZA   AO  5285_8895_090                   2C  D   APEX                            6487                            072314                          846 @ׅ�Q)c�1   @ׅ����>@0�$�/�d,1&�x�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ZA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�ffB�  B�  B�  B���B�  B�33B�33B�33B�  B�  B�33B�ffB���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�ffB���C�fC�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DLy�DL��DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DSy�DS��DTy�DT��DU� DV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D�� D�I�D�i�D��3D���D�@ D���D��3D�fD�I�D�� D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@ҏ\A	G�A)G�AIG�AiG�A���A���A���A���Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2Q�B:Q�BBQ�BJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B��\B�(�B�(�B�(�B�B�(�B�\)B�\)B�\)B�(�B�(�B�\)B��\B���B�(�B�(�B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�\)B��\C z�Cz�Cz�C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �C"�{C$�{C&�C(�{C*�{C,�{C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�{CF�{CH�{CJ�{CL�{CN�{CP�{CR�{CT�{CV�{CX�{CZ�{C\�{C^�{C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�W
C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D�D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-+�D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL��DM�DM��DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR��DS%DS��DT�DT��DU�DU�DV%DV�DW%DW��DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da��Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�Dt��Dy޹D���D�\)D�|)D���D�)D�R�D��\D���D�(�D�\)D���D���D�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A�  A�  A�A�A�%A�1A�1A�
=A�JA�JA�oA��A��A��A�+A�$�A�+A�=qA�?}A�33A� �A�{A�
=A�/A��;A�r�A�$�AžwAř�Aţ�A�O�A���A�+A��^A��-A�Q�A�n�A��A��FA���A�C�A�&�A��DA�A���A��HA�{A��9A��PA�p�A���A��^A��A�`BA��^A�9XA|�!Aw\)As/Ao��Am+Ak�Ag+Ad-Aa�PA`v�A_�hA^��A^I�A\n�A[�7A[�AZ5?AX�HAW�AUƨARbNAQK�APȴAO33AL��AK�-AI�mAG%AD�`AA|�A>r�A<�RA:A8-A6��A3��A2=qA133A1��A1��A1��A1�A1�hA1t�A1"�A0�DA/�A-�^A,�DA*�A(bA'33A%�-A#�A n�Az�A��A��A�A�A�A�-A�A�A�7A33A�A��A`BAQ�AI�Ar�A
�A
��A
ȴA
�yAXA
��A
��A
^5A
(�A
I�A
�jA�A�A�Ar�A(�A�-A|�AG�AG�A�A�AbAM�A��A��AdZAoA�HA��A=qAv�A��AK�AC�A\)AA�;AƨA��AVA�\A1Al�A
�A
VA
(�A
  A	��A	;dA	�A�A5?A`BA=qAhsA%A��A
=AĜAn�A$�A1A��AO�A��A�/AE�A�A?}A �A bN@��P@�~�@��-@��j@�ƨ@��;@���@�
=@�ȴ@�@���@�V@��@�x�@�%@�z�@��F@�dZ@���@���@�O�@�%@�Ĝ@� �@�@�R@���@�V@���@�@�Z@��
@��@�V@���@���@�1'@�"�@���@�%@�z�@��;@畁@��y@���@��@�@��;@��@�@��y@��@�%@��@߅@�C�@�;d@��@��@�ff@��T@�/@ܴ9@�1@�ƨ@�|�@�\)@��@�v�@���@�G�@؃@�b@�|�@֧�@�-@ա�@�hs@�?}@�z�@�t�@Ұ!@�J@ѩ�@�X@���@�(�@�b@���@��
@϶F@ϝ�@�t�@�;d@���@��@�@�X@���@̣�@�j@� �@��@ˮ@ʸR@�O�@�/@���@ȋD@�Q�@�1'@�  @�l�@�ff@���@���@�r�@ċD@�I�@��y@��h@�V@�/@�V@�r�@��@��H@�E�@���@��/@�r�@�Q�@�A�@��w@�@�ff@�-@��T@��h@�7L@���@�1'@��F@�\)@�"�@��R@�=q@�@�G�@��/@��u@�Q�@�9X@�r�@��;@��y@��H@��R@�$�@��^@���@�O�@��@�%@���@���@�I�@��@�C�@�
=@��@��!@���@�~�@�V@�@��-@��@�X@�&�@���@��`@��9@�r�@�Z@�I�@�1'@�t�@�;d@�"�@���@��H@�v�@�-@���@��j@��D@�z�@�j@�1'@��F@�|�@�C�@�@���@�n�@�5?@��#@��^@���@���@��7@�x�@�hs@�/@���@��/@���@��j@���@��@�I�@�1'@��@��@���@�
=@��!@�5?@��#@�@���@���@��@�G�@�/@��@�Ĝ@��9@��@��@���@�j@�ƨ@�l�@��@��R@�E�@��@���@�x�@�G�@��@���@���@���@��j@��m@��@�;d@�
=@���@��@���@�`B@�?}@�&�@�%@��`@��@�1@���@��P@�\)@��@��H@���@�ff@�{@�@��7@�%@��7@��@���@�9X@u�-@m��@b-@U�-@IG�@>�+@7�@1x�@+��@#dZ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 A���A���A���A���A���A���A���A�  A�  A�A�A�%A�1A�1A�
=A�JA�JA�oA��A��A��A�+A�$�A�+A�=qA�?}A�33A� �A�{A�
=A�/A��;A�r�A�$�AžwAř�Aţ�A�O�A���A�+A��^A��-A�Q�A�n�A��A��FA���A�C�A�&�A��DA�A���A��HA�{A��9A��PA�p�A���A��^A��A�`BA��^A�9XA|�!Aw\)As/Ao��Am+Ak�Ag+Ad-Aa�PA`v�A_�hA^��A^I�A\n�A[�7A[�AZ5?AX�HAW�AUƨARbNAQK�APȴAO33AL��AK�-AI�mAG%AD�`AA|�A>r�A<�RA:A8-A6��A3��A2=qA133A1��A1��A1��A1�A1�hA1t�A1"�A0�DA/�A-�^A,�DA*�A(bA'33A%�-A#�A n�Az�A��A��A�A�A�A�-A�A�A�7A33A�A��A`BAQ�AI�Ar�A
�A
��A
ȴA
�yAXA
��A
��A
^5A
(�A
I�A
�jA�A�A�Ar�A(�A�-A|�AG�AG�A�A�AbAM�A��A��AdZAoA�HA��A=qAv�A��AK�AC�A\)AA�;AƨA��AVA�\A1Al�A
�A
VA
(�A
  A	��A	;dA	�A�A5?A`BA=qAhsA%A��A
=AĜAn�A$�A1A��AO�A��A�/AE�A�A?}A �A bN@��P@�~�@��-@��j@�ƨ@��;@���@�
=@�ȴ@�@���@�V@��@�x�@�%@�z�@��F@�dZ@���@���@�O�@�%@�Ĝ@� �@�@�R@���@�V@���@�@�Z@��
@��@�V@���@���@�1'@�"�@���@�%@�z�@��;@畁@��y@���@��@�@��;@��@�@��y@��@�%@��@߅@�C�@�;d@��@��@�ff@��T@�/@ܴ9@�1@�ƨ@�|�@�\)@��@�v�@���@�G�@؃@�b@�|�@֧�@�-@ա�@�hs@�?}@�z�@�t�@Ұ!@�J@ѩ�@�X@���@�(�@�b@���@��
@϶F@ϝ�@�t�@�;d@���@��@�@�X@���@̣�@�j@� �@��@ˮ@ʸR@�O�@�/@���@ȋD@�Q�@�1'@�  @�l�@�ff@���@���@�r�@ċD@�I�@��y@��h@�V@�/@�V@�r�@��@��H@�E�@���@��/@�r�@�Q�@�A�@��w@�@�ff@�-@��T@��h@�7L@���@�1'@��F@�\)@�"�@��R@�=q@�@�G�@��/@��u@�Q�@�9X@�r�@��;@��y@��H@��R@�$�@��^@���@�O�@��@�%@���@���@�I�@��@�C�@�
=@��@��!@���@�~�@�V@�@��-@��@�X@�&�@���@��`@��9@�r�@�Z@�I�@�1'@�t�@�;d@�"�@���@��H@�v�@�-@���@��j@��D@�z�@�j@�1'@��F@�|�@�C�@�@���@�n�@�5?@��#@��^@���@���@��7@�x�@�hs@�/@���@��/@���@��j@���@��@�I�@�1'@��@��@���@�
=@��!@�5?@��#@�@���@���@��@�G�@�/@��@�Ĝ@��9@��@��@���@�j@�ƨ@�l�@��@��R@�E�@��@���@�x�@�G�@��@���@���@���@��j@��m@��@�;d@�
=@���@��@���@�`B@�?}@�&�@�%@��`@��@�1@���@��P@�\)@��@��H@���@�ff@�{@�@��7G�O�@��7@��@���@�9X@u�-@m��@b-@U�-@IG�@>�+@7�@1x�@+��@#dZ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	{B	uB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	)�B	9XB	G�B	bNB	{�B	�PB	�bB	��B	��B	��B	��B	��B	��B	�B	�'B	�RB	�^B
:^B
R�B
E�B
8RB�B"�B"�B?}B%�B�B)�B�B
�B
��B
�B
��B
��B
��B
�\B
�B
��B
��B
��B
��B
k�B
7LB	��B	��B	��B
�B	��B	�/B	ĜB	�9B	��B	�PB	w�B	iyB	bNB	]/B	XB	S�B	K�B	F�B	C�B	=qB	5?B	)�B	�B	hB		7B	B��B�sB�B�ZB�B��BɺB�}B�FB�B��B��B��B��B�B�9BƨB�/B�B�B��B	B	�B	!�B	'�B	(�B	�B	uB	�B	&�B	+B	'�B	#�B	!�B	{B		7B	VB	hB		7B	B	B	B	%B	�B	>wB	XB	D�B	6FB	'�B	�B	�B	 �B	'�B	B�B	C�B	E�B	H�B	J�B	O�B	YB	S�B	S�B	_;B	ɺB
\B
DB

=B

=B
JB
{B
!�B
$�B
,B
-B
-B
,B
+B
)�B
(�B
&�B
�B
�B
�B
�B
�B
&�B
'�B
(�B
&�B
$�B
#�B
"�B
 �B
�B
"�B
%�B
'�B
&�B
$�B
"�B
�B
�B
uB
JB
1B
+B
1B
uB
�B
{B
uB
oB
oB
oB
�B
�B
�B
�B
�B
{B
hB
\B
PB
JB

=B

=B
\B
hB
bB
uB
�B
�B
{B
�B
{B
uB
uB
oB
oB
hB
hB
hB
hB
hB
bB
bB
\B
\B
VB
VB
PB
VB
PB
JB

=B

=B
	7B
1B
+B
B
B
B
B
B
%B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B	��B
B
B
B
B
B
B
B
+B
%B
%B
+B
+B
+B
+B
+B
1B
	7B
	7B

=B
DB
JB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
\B
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
uB
uB
uB
uB
uB
uB
{B
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
!�B
#�B
!�B
&�B
(�B
1'B
:^B
<jB
;dB
D�B
K�B
R�B
YB
]/B
`BB
ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 B	dB	ZB	bB	bB	fB	fB	fB	lB	lB	rB	pB	yB	xB	xB	B	�B	�B	)�B	9=B	G�B	b1B	{�B	�0B	�CB	�gB	��B	��B	��B	��B	��B	��B	�B	�0B	�<B
:3B
R�B
EzB
8(ByB"�B"�B?IB%�BTB)�B�B
�jB
̙B
��B
��B
��B
��B
�)B
��B
�ZB
��B
��B
�hB
kQB
7B	��B	��B	ϪB
RB	��B	��B	�iB	�B	��B	�B	w�B	iIB	bB	\�B	W�B	S�B	K�B	FyB	CfB	=CB	5B	)�B	�B	:B		B	�B��B�DB�TB�-B��BϵBɍB�PB�B��B��B��B��B��B��B�B�wB��B�sB�B��B	�B	�B	!�B	'�B	(�B	iB	AB	XB	&�B	*�B	'�B	#�B	!�B	HB		B	 B	1B		B	�B	�B	�B	�B	TB	>>B	W�B	DdB	6B	'�B	zB	�B	 �B	'�B	BRB	C]B	EkB	HyB	J�B	O�B	X�B	S�B	S�B	_ B	�}B
B
B
	�B
	�B

B
:B
!�B
$�B
+�B
,�B
,�B
+�B
*�B
)�B
(�B
&�B
yB
_B
`B
lB
B
&�B
'�B
(�B
&�B
$�B
#�B
"�B
 �B
wB
"�B
%�B
'�B
&�B
$�B
"�B
�B
^B
4B
B
�B
�B
�B
6B
FB
:B
3B
-B
.B
-B
MB
^B
MB
?B
?B
8B
&B
B
B

B
	�B
	�B
B
'B
 B
6B
EB
?B
;B
@B
6B
4B
4B
,B
-B
%B
&B
&B
'B
#B
!B
"B
B
B
B
B
B
B
B
B
	�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
�B
 �B
�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�tB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�}B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
 B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
"B
$B
"B
"B
#B
'B
*B
)B
4B
1B
1B
2B
2B
2B
0B
0B
1B
1B
/B
2B
2B
7B
/B
1B
0B
5B
9B
>B
BB
DB
DB
DB
BB
AB
IB
IB
IB
IB
HB
IB
HB
HB
IB
OB
NB
RB
SB
ZB
ZB
`B
`B
aB
`B
`B
bB
bB
XB
cB
gB
nB
kB
mB
rB
sB
xB
zB
yB
{B
zB
xB
 B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
!�G�O�B
!�B
&�B
(�B
0�B
:B
<#B
;B
DSB
K~B
R�B
X�B
\�B
_�B
f!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.58 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417432016080714174320160807141743  AO  ARCAADJP                                                                    20151212201648    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151212201648  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151212201648  QCF$                G�O�G�O�G�O�4000            UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141743  IP                  G�O�G�O�G�O�                