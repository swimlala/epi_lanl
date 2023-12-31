CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-20T09:16:59Z AOML 3.0 creation; 2016-08-07T21:17:35Z UW 3.1 conversion     
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
_FillValue                 �  At   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cl   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
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
resolution        :�o     �  px   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  zD   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150320091659  20160807141735  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               'A   AO  5285_8895_039                   2C  D   APEX                            6487                            072314                          846 @�B�9u01   @�B���
@-���O�;�c���R1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    'A   B   B   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBo33Bw��B�  B�33B���B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dys3D�  D�C3D��3D��fD��D�FfD���D��3D��D�9�D�p DǦf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@ҏ\A�A)G�AIG�AiG�A���A���A���A���Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2Q�B:Q�BBQ�BJQ�BRQ�BZQ�BbQ�Bj�RBq�By�B�(�B�\)B�B�(�B�(�B�(�B�\)B�(�B�(�B���B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�{C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�{CF�{CH�{CJ�{CL�{CN�{CP�{CR�{CT�{CV�{CX�{CZ�{C\�{C^�C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG+�DG�DH%DH��DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�RDy�RD�2�D�U�D���D���D�\D�X�D��)D���D�,)D�L)D���DǸ�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AԁAԁAԃAԃAԅAԅAԇ+AԅAԅA�~�A�~�AԅA�~�A�|�AԁA�p�A�9XA�-A�(�A�&�A�$�A�&�A�(�A�&�A� �A��A��A�bA�ĜAї�A�?}A͸RAʗ�A�C�A�C�A�%A�/A���A��A��HA�/A�jA�$�A��hA�/A���A��A��A�;dA��#A�bA�1'A�=qA��A�%A�+A��hA��A���A��9A��A��DA�v�A�JA��!A�&�A��jA�I�A�VA}�7A{+Ayx�AuO�Ar1Ap�uAm��Aj��AhĜAg�
Af�!AdM�A\$�AUp�AR �AN��AL��AJ�RAI�FAI�AH�AG7LAD�ACdZABjA?VA;�FA:VA9��A8��A6=qA5dZA4�\A2��A1/A/�A,��A,bNA*v�A)\)A(�A'��A&5?A%�PA$�jA$v�A"�yA 5?A =qA E�A ZA bNA�TA�`A�AoA�
A��A�PAA��AI�A��A�A��A5?A�mAoAffAA��A7LA�`A�\A�#AoAE�A�PA"�An�A�
AK�A?}A�A��A��A&�A
�!A
n�A	�A�HA��A�A��Av�A$�AA;dA-A;dA/A��A��AdZAXAS�A?}A �HA �@�=q@��/@��7@�5?@��@���@�bN@�  @���@�M�@�v�@�v�@�M�@��#@�hs@�G�@��@�9X@��!@�`B@��@��@��+@��+@�$�@���@�O�@�I�@�t�@�o@��@��T@��`@��;@�5?@���@���@�t�@�33@��@�\@�n�@�5?@���@�^@�%@�+@�J@�@�z�@�t�@�v�@�X@�I�@�ƨ@�33@�"�@���@ޏ\@�&�@���@ۥ�@�;d@�^5@�x�@��/@�1'@�Z@�bN@�dZ@��@�@���@�G�@Ԭ@���@�ƨ@Ӯ@�\)@ҏ\@���@щ7@��@д9@�Q�@�1@Ϯ@�S�@��@θR@��@���@�@ͩ�@͉7@�G�@�?}@�?}@�?}@�&�@�Ĝ@�Q�@�  @˾w@˥�@�dZ@�@���@�M�@ɡ�@�G�@���@Ȭ@�9X@� �@�1@��;@�l�@���@Ɨ�@�^5@�@őh@�hs@�Ĝ@��@�ƨ@Ý�@�l�@�"�@�ff@�p�@�%@���@�1'@��
@�|�@���@�-@��@�7L@��`@�z�@�  @��
@�|�@��@�E�@��T@��7@��`@�z�@�(�@�dZ@���@�M�@���@�hs@���@��9@���@�z�@�j@�9X@��@��@�dZ@��H@�^5@�5?@�$�@��@��-@�G�@���@���@�j@�A�@�b@�ƨ@��@�\)@�
=@���@�~�@�=q@�J@��@��-@�O�@���@�Z@��@��F@�|�@�"�@���@���@�5?@��@���@��@�O�@���@��j@��D@�bN@�b@�t�@�@���@��R@��+@�M�@��@�7L@��@���@�bN@���@���@��@�33@��@��!@�n�@�$�@���@��@��h@�%@���@�1@���@��@��P@�dZ@��!@��@���@���@��h@�p�@�7L@���@�Ĝ@��@�Q�@��@���@�K�@�
=@���@��+@�^5@�$�@��h@�?}@��9@�Q�@�(�@��@��
@�\)@�@��@���@���@���@��#@�p�@��@���@��u@�r�@�I�@���@��m@��
@�ƨ@��P@���@�;d@��R@�ff@�5?@�@��#@��@��@�Q�@�9X@�  @��
@��@�dZ@�o@���@�n�@��#@�?}@��@���@��/@��u@��m@��P@�S�@�33@��@��T@�C�@}/@tI�@l�D@d(�@Yx�@QX@F�y@=��@5�@0��@,(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   AԁAԁAԃAԃAԅAԅAԇ+AԅAԅA�~�A�~�AԅA�~�A�|�AԁA�p�A�9XA�-A�(�A�&�A�$�A�&�A�(�A�&�A� �A��A��A�bA�ĜAї�A�?}A͸RAʗ�A�C�A�C�A�%A�/A���A��A��HA�/A�jA�$�A��hA�/A���A��A��A�;dA��#A�bA�1'A�=qA��A�%A�+A��hA��A���A��9A��A��DA�v�A�JA��!A�&�A��jA�I�A�VA}�7A{+Ayx�AuO�Ar1Ap�uAm��Aj��AhĜAg�
Af�!AdM�A\$�AUp�AR �AN��AL��AJ�RAI�FAI�AH�AG7LAD�ACdZABjA?VA;�FA:VA9��A8��A6=qA5dZA4�\A2��A1/A/�A,��A,bNA*v�A)\)A(�A'��A&5?A%�PA$�jA$v�A"�yA 5?A =qA E�A ZA bNA�TA�`A�AoA�
A��A�PAA��AI�A��A�A��A5?A�mAoAffAA��A7LA�`A�\A�#AoAE�A�PA"�An�A�
AK�A?}A�A��A��A&�A
�!A
n�A	�A�HA��A�A��Av�A$�AA;dA-A;dA/A��A��AdZAXAS�A?}A �HA �@�=q@��/@��7@�5?@��@���@�bN@�  @���@�M�@�v�@�v�@�M�@��#@�hs@�G�@��@�9X@��!@�`B@��@��@��+@��+@�$�@���@�O�@�I�@�t�@�o@��@��T@��`@��;@�5?@���@���@�t�@�33@��@�\@�n�@�5?@���@�^@�%@�+@�J@�@�z�@�t�@�v�@�X@�I�@�ƨ@�33@�"�@���@ޏ\@�&�@���@ۥ�@�;d@�^5@�x�@��/@�1'@�Z@�bN@�dZ@��@�@���@�G�@Ԭ@���@�ƨ@Ӯ@�\)@ҏ\@���@щ7@��@д9@�Q�@�1@Ϯ@�S�@��@θR@��@���@�@ͩ�@͉7@�G�@�?}@�?}@�?}@�&�@�Ĝ@�Q�@�  @˾w@˥�@�dZ@�@���@�M�@ɡ�@�G�@���@Ȭ@�9X@� �@�1@��;@�l�@���@Ɨ�@�^5@�@őh@�hs@�Ĝ@��@�ƨ@Ý�@�l�@�"�@�ff@�p�@�%@���@�1'@��
@�|�@���@�-@��@�7L@��`@�z�@�  @��
@�|�@��@�E�@��T@��7@��`@�z�@�(�@�dZ@���@�M�@���@�hs@���@��9@���@�z�@�j@�9X@��@��@�dZ@��H@�^5@�5?@�$�@��@��-@�G�@���@���@�j@�A�@�b@�ƨ@��@�\)@�
=@���@�~�@�=q@�J@��@��-@�O�@���@�Z@��@��F@�|�@�"�@���@���@�5?@��@���@��@�O�@���@��j@��D@�bN@�b@�t�@�@���@��R@��+@�M�@��@�7L@��@���@�bN@���@���@��@�33@��@��!@�n�@�$�@���@��@��h@�%@���@�1@���@��@��P@�dZ@��!@��@���@���@��h@�p�@�7L@���@�Ĝ@��@�Q�@��@���@�K�@�
=@���@��+@�^5@�$�@��h@�?}@��9@�Q�@�(�@��@��
@�\)@�@��@���@���@���@��#@�p�@��@���@��u@�r�@�I�@���@��m@��
@�ƨ@��P@���@�;d@��R@�ff@�5?@�@��#@��@��@�Q�@�9X@�  @��
@��@�dZ@�o@���@�n�@��#@�?}@��@���@��/@��u@��m@��P@�S�@�33G�O�@��T@�C�@}/@tI�@l�D@d(�@Yx�@QX@F�y@=��@5�@0��@,(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oBP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BO�BP�BQ�BQ�BQ�BR�BS�BT�BVBXBYB[#B`BBjB�oB	y�B	��B
�B
��B
ĜB
��B
�B)�BG�BYB[#BdZBffBl�BjBn�B_;BF�B%�BbB%BuB%�B"�B!�B0!B/B"�B�B
=B
�B
��B
�FB
��B
y�B
\)B
E�B
1'B
{B	��B	�B	�/B	��B	�^B	��B	��B	�PB	|�B	q�B	iyB	_;B	K�B	$�B	  B�B�fB�HB�)B�)B�)B�B�;B�HB�BB�/B�5B�HB�NB�NB�ZB��B��B��B��B	B	
=B	uB	�B	 �B	-B	0!B	.B	,B	(�B	&�B	%�B	�B	"�B	%�B	&�B	+B	.B	/B	,B	+B	/B	-B	,B	.B	5?B	33B	0!B	2-B	5?B	6FB	9XB	;dB	;dB	<jB	<jB	=qB	G�B	I�B	H�B	J�B	K�B	K�B	L�B	L�B	L�B	R�B	T�B	YB	YB	W
B	W
B	XB	YB	XB	XB	_;B	`BB	^5B	\)B	\)B	[#B	XB	VB	Q�B	O�B	N�B	M�B	K�B	VB	_;B	aHB	aHB	_;B	ZB	ZB	aHB	n�B	v�B	w�B	� B	�=B	�JB	�DB	�DB	�uB	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�LB	�wB	��B	��B	B	B	ĜB	ŢB	ŢB	ŢB	ĜB	ÖB	B	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�
B	�B	�#B	�#B	�#B	�B	�B	�5B	�NB	�HB	�HB	�TB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
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

=B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
JB
PB
PB
VB
VB
\B
VB
\B
\B
\B
bB
bB
bB
bB
bB
hB
hB
oB
oB
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
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
.B
6FB
=qB
E�B
H�B
N�B
R�B
W
B
[#B
aHB
gmB
j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BO�BP�BQ�BQ�BQ�BR�BS�BT�BU�BW�BX�BZ�B`BjSB�AB	y�B	��B
��B
��B
�rB
жB
�wB)�BGBX�BZ�Bd&Bf5BlZBjLBngB_BFtB%�B0B�BAB%�B"�B!�B/�B.�B"�B]B
B
�ZB
��B
�B
�gB
y�B
[�B
ErB
0�B
HB	��B	�SB	��B	жB	�,B	��B	��B	�B	|�B	q{B	iHB	_B	K�B	$�B��B�jB�6B�B��B��B��B��B�B�B�B�B�B�B� B�!B�+B�B�B�B��B	�B	
B	@B	MB	 �B	,�B	/�B	-�B	+�B	(�B	&�B	%�B	�B	"�B	%�B	&�B	*�B	-�B	.�B	+�B	*�B	.�B	,�B	+�B	-�B	5B	2�B	/�B	1�B	5	B	6B	9!B	;.B	;+B	<2B	<0B	=9B	GvB	I�B	H{B	J�B	K�B	K�B	L�B	L�B	L�B	R�B	T�B	X�B	X�B	V�B	V�B	W�B	X�B	W�B	W�B	^�B	`B	]�B	[�B	[�B	Z�B	W�B	U�B	Q�B	O�B	N�B	M�B	K�B	U�B	^�B	aB	aB	^�B	Y�B	Y�B	aB	nZB	v�B	w�B	�B	� B	�B	�B	�B	�7B	�gB	�uB	�nB	�tB	��B	��B	��B	��B	��B	��B	�B	�8B	�IB	�IB	�OB	�NB	�[B	�cB	�bB	�cB	�ZB	�VB	�RB	�tB	�yB	ˇB	͒B	ϝB	ѫB	ұB	ԿB	��B	��B	��B	��B	ҳB	ҰB	ϞB	ΗB	ϡB	ΛB	͓B	ϝB	ѫB	ѭB	ӶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�+B	�)B	�:B	�=B	�<B	�:B	�IB	�OB	�NB	�OB	�NB	�OB	�TB	�VB	�TB	�VB	�UB	�\B	�ZB	�\B	�[B	�\B	�_B	�cB	�\B	�WB	�UB	�TB	�ZB	�aB	�bB	�`B	�aB	�gB	�mB	�mB	�nB	�kB	�lB	�kB	�kB	�vB	�tB	�uB	�nB	�lB	�qB	�tB	�xB	��B	�xB	�B	��B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
	�B
	�B
	�B

�B
 B
B

�B
B
B
B
B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
$B
#B
*B
*B
)B
)B
(B
'B
(B
/B
1B
0B
1B
7B
5B
5B
6B
6B
5B
=B
HB
BB
FB
JB
IB
OB
OB
NB
LB
YB
dB
fB
aB
_B
YB
YB
YB
aB
`B
dB
qB
wB
rB
qB
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
#�B
#�B
#�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
$�B
$�B
%�B
%�B
%�B
%�G�O�B
&�B
-�B
6B
=*B
E]B
HmB
N�B
R�B
V�B
Z�B
a B
g%B
j8111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.58 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417352016080714173520160807141735  AO  ARCAADJP                                                                    20150320091659    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150320091659  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150320091659  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141735  IP                  G�O�G�O�G�O�                