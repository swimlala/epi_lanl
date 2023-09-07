CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-01T00:01:23Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20171001000123  20190405100808  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�*3>��1   @�*3βZB@-�O�;dZ�d�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$�C&  C'�fC)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN�CO�fCR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dyl�D� D�VfD��fD��3D�3D�I�D�� D�y�D�fD�FfD�ffD��fD�3D�@ Dڌ�D�fD��D�<�D�|�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@ҏ\A	G�A)G�AIG�AiG�A���A���A���A���Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2Q�B:Q�BB�RBJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B���B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �{C"�C$�C&�{C(z�C*z�C,�{C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�{CF�{CH�{CJ�{CL�CN�CPz�CR�{CT�{CV�{CX�{CZz�C\�{C^�{C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�W
C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�=pC�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�=pC�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�W
C�W
C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D+�D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�RDy��D�"�D�h�D���D���D��D�\)D���D��)D��D�X�D�x�D���D�%�D�R�Dڟ\D��D�\D�O\D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ZA�XA�S�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�Q�A�VA�Q�A�O�A�I�A�I�A�C�A�A�A�7LA�&�A�$�A� �A��A� �A��A�oA�oA�JA�%A��A��Aݟ�A��;A؃A�ƨA���A�VA�Aҟ�A�bA�dZA�$�AЗ�A��HA�?}A���A͑hA��HAʟ�A�ƨAȧ�A�1'A�?}A�\)A��A�A��
A×�AÏ\A�K�A��A�hsA��A�JA���A�?}A���A���A��TA�M�A��mA�A��hA�ZA���A��A�A���A�dZA�ZA��A��/A�jA�A���A��-A�v�A�-A���A�?}A��TA���A��FA�M�A�%A��`A�^5A�|�A�9XA��^A�\)A���A�jA�~�A�
=A�  A�M�A���A��TA�-A���A�&�A�?}A�A��A�r�A�A�A�v�A�FA|ffAv��As&�Ap��AoAmC�Ajr�Af��AcVAa
=A_��A]��AY�TAT�9ANA�AM\)AJ��AG�AE��ADM�AC�7AA�FA?��A<�DA;l�A:�A9��A9�-A8�`A7��A7`BA6��A6bA533A4JA1�#A1"�A/�A.�DA.�A.�RA/A/�hA.��A-t�A-O�A-��A,�uA+�mA+&�A*��A*n�A)��A)�
A)XA)VA(�HA(ȴA(�9A(�A(bA'�A'x�A'XA'�A&��A&  A%\)A$�A$�+A$Q�A${A#�#A#?}A"r�A!XA�;AG�AĜAjA�mA�A�jAA�AbA�A��AXA&�A�`Az�AA�AƨAt�A�A�DA��A��A\)A;dA��A  A�RA{A�hA|�A�uA�A�A7LA"�AĜAn�AhsA�HA��AA�A��At�A33A�/A��AVA-A��Al�AO�A/A�A
��A
��A
A	�PA	S�A	�A�A��A��Av�AQ�A5?AbA��A�
Ax�A?}A
=A�AȴA��A�DAbNA=qA�FA�/AbAXA��A ��@�+@��#@�O�@�%@��u@��
@���@�l�@��@�@��@�V@��-@�7L@���@�@�bN@�I�@� �@��;@��@���@�M�@�?}@�1'@�|�@�ȴ@�E�@�7L@� �@�1'@���@��@�w@�C�@���@�~�@�@�hs@�  @�dZ@��@�~�@�^@��@�Z@�S�@◍@�v�@�ff@�M�@�=q@�-@���@�h@�?}@��@�z�@߶F@���@�ȴ@�5?@�p�@܃@���@���@�t�@�-@պ^@ՙ�@�X@Դ9@ӕ�@�ff@�G�@�Z@ύP@��@͑h@� �@���@�J@ȣ�@Ɨ�@�@�`B@�/@�V@���@ļj@ģ�@ēu@ă@�z�@�Q�@��@Ý�@�+@�@��-@�O�@�V@��@�bN@��
@���@�$�@��@��-@��@��@��9@�r�@�1@��P@�@���@��@���@���@���@���@���@��@��/@��j@�z�@�bN@�Q�@�9X@�1@��;@��;@��@��@��@�p�@��@���@�1@���@���@�K�@�o@���@�ȴ@���@��+@�v�@�ff@�V@�J@��#@���@�@���@��7@��h@��7@�x�@�p�@�?}@��`@�r�@���@���@�J@��#@���@�?}@�Q�@�ƨ@�C�@��y@�n�@��@�?}@�%@���@��@�Q�@��m@��w@���@�@��R@��@�@�"�@��@���@��7@��@�1'@�S�@���@��H@�v�@�{@�@�`B@���@��@�9X@���@���@�{@���@�O�@���@�p�@�5?@�=q@���@�
=@|z�@r�!@d��@]�T@Vff@Kƨ@D�j@>�y@;�@4�@/\)@&5?@ȴ@��@I�@1'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ZA�XA�S�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�Q�A�VA�Q�A�O�A�I�A�I�A�C�A�A�A�7LA�&�A�$�A� �A��A� �A��A�oA�oA�JA�%A��A��Aݟ�A��;A؃A�ƨA���A�VA�Aҟ�A�bA�dZA�$�AЗ�A��HA�?}A���A͑hA��HAʟ�A�ƨAȧ�A�1'A�?}A�\)A��A�A��
A×�AÏ\A�K�A��A�hsA��A�JA���A�?}A���A���A��TA�M�A��mA�A��hA�ZA���A��A�A���A�dZA�ZA��A��/A�jA�A���A��-A�v�A�-A���A�?}A��TA���A��FA�M�A�%A��`A�^5A�|�A�9XA��^A�\)A���A�jA�~�A�
=A�  A�M�A���A��TA�-A���A�&�A�?}A�A��A�r�A�A�A�v�A�FA|ffAv��As&�Ap��AoAmC�Ajr�Af��AcVAa
=A_��A]��AY�TAT�9ANA�AM\)AJ��AG�AE��ADM�AC�7AA�FA?��A<�DA;l�A:�A9��A9�-A8�`A7��A7`BA6��A6bA533A4JA1�#A1"�A/�A.�DA.�A.�RA/A/�hA.��A-t�A-O�A-��A,�uA+�mA+&�A*��A*n�A)��A)�
A)XA)VA(�HA(ȴA(�9A(�A(bA'�A'x�A'XA'�A&��A&  A%\)A$�A$�+A$Q�A${A#�#A#?}A"r�A!XA�;AG�AĜAjA�mA�A�jAA�AbA�A��AXA&�A�`Az�AA�AƨAt�A�A�DA��A��A\)A;dA��A  A�RA{A�hA|�A�uA�A�A7LA"�AĜAn�AhsA�HA��AA�A��At�A33A�/A��AVA-A��Al�AO�A/A�A
��A
��A
A	�PA	S�A	�A�A��A��Av�AQ�A5?AbA��A�
Ax�A?}A
=A�AȴA��A�DAbNA=qA�FA�/AbAXA��A ��@�+@��#@�O�@�%@��u@��
@���@�l�@��@�@��@�V@��-@�7L@���@�@�bN@�I�@� �@��;@��@���@�M�@�?}@�1'@�|�@�ȴ@�E�@�7L@� �@�1'@���@��@�w@�C�@���@�~�@�@�hs@�  @�dZ@��@�~�@�^@��@�Z@�S�@◍@�v�@�ff@�M�@�=q@�-@���@�h@�?}@��@�z�@߶F@���@�ȴ@�5?@�p�@܃@���@���@�t�@�-@պ^@ՙ�@�X@Դ9@ӕ�@�ff@�G�@�Z@ύP@��@͑h@� �@���@�J@ȣ�@Ɨ�@�@�`B@�/@�V@���@ļj@ģ�@ēu@ă@�z�@�Q�@��@Ý�@�+@�@��-@�O�@�V@��@�bN@��
@���@�$�@��@��-@��@��@��9@�r�@�1@��P@�@���@��@���@���@���@���@���@��@��/@��j@�z�@�bN@�Q�@�9X@�1@��;@��;@��@��@��@�p�@��@���@�1@���@���@�K�@�o@���@�ȴ@���@��+@�v�@�ff@�V@�J@��#@���@�@���@��7@��h@��7@�x�@�p�@�?}@��`@�r�@���@���@�J@��#@���@�?}@�Q�@�ƨ@�C�@��y@�n�@��@�?}@�%@���@��@�Q�@��m@��w@���@�@��R@��@�@�"�@��@���@��7@��@�1'@�S�@���@��H@�v�@�{@�@�`B@���@��@�9X@���@���@�{@���@�O�@���@�p�@�5?@�=q@���@�
=@|z�@r�!@d��@]�T@Vff@Kƨ@D�j@>�y@;�@4�@/\)@&5?@ȴ@��@I�@1'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
9XB
:^B
:^B
9XB
:^B
:^B
:^B
:^B
:^B
:^B
9XB
9XB
9XB
9XB
9XB
8RB
8RB
8RB
7LB
6FB
6FB
6FB
6FB
6FB
5?B
5?B
5?B
49B
49B
2-B
 �B

=B	��B	�B	��B	�B	��B
  B
VB
�B
#�B
&�B
0!B
@�B
Q�B
iyB
�B
��B
ɺB
�NB
��B%�BG�BN�BO�BN�B}�B�=B�DB��B�3B�qB�B�B+BhB{B�B!�B'�B+B-B/B/B2-B5?B7LB8RB8RB:^B9XB9XB8RB2-B,B#�B�B�BhBVBDBB��B��B�B�mB�B��BÖB�-B�hBl�B!�B
�sB
�B
ŢB
�dB
�'B
��B
�PB
�B
x�B
]/B
H�B
5?B
�B	��B	�B	�5B	ÖB	��B	�PB	}�B	z�B	v�B	cTB	K�B	=qB	49B	-B	"�B	�B	
=B��B��B�B�B�B�B�B�B�B�B�B��B��B��B	  B	
=B	JB	\B	oB	�B	&�B	N�B	R�B	XB	]/B	jB	}�B	�DB	�LB	��B	�/B	��B
  B	��B
+B
1B
1B
\B
�B
 �B
!�B
"�B
!�B
 �B
 �B
 �B
 �B
!�B
#�B
#�B
$�B
&�B
.B
2-B
9XB
:^B
:^B
:^B
:^B
9XB
7LB
6FB
<jB
=qB
=qB
=qB
=qB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
;dB
;dB
<jB
<jB
<jB
;dB
;dB
;dB
<jB
=qB
=qB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
;dB
;dB
;dB
:^B
9XB
8RB
7LB
6FB
6FB
5?B
5?B
49B
49B
49B
33B
33B
33B
2-B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
0!B
/B
.B
-B
+B
(�B
$�B
�B
�B
uB
oB
oB
hB
bB
\B
\B
VB
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
PB
PB
PB
PB
PB
VB
VB
PB
JB
PB
bB
hB
oB
oB
oB
hB
oB
hB
hB
bB
VB
PB
JB
DB

=B
1B
+B
B
B
B
B
B
B
B
B
B
B
	7B

=B
JB
PB
PB
JB
DB
	7B
%B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�yB	�mB	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
%B
+B
1B
+B
+B
1B

=B
DB
JB
JB
JB
JB
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
JB
PB
VB
bB
hB
hB
hB
uB
{B
�B
'�B
-B
1'B
:^B
?}B
C�B
H�B
L�B
P�B
T�B
XB
ZB
_;B
cTB
jB
p�B
w�B
z�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
9,B
:4B
:4B
9,B
:4B
:1B
:4B
:1B
:1B
:1B
9,B
9*B
9*B
9,B
9(B
8$B
8(B
8%B
7!B
6B
6B
6B
6B
6B
5B
5B
5B
4B
4B
2 B
 �B

B	��B	�B	�B	�B	��B	��B
)B
dB
#�B
&�B
/�B
@VB
Q�B
iHB
��B
��B
ɉB
�"B
��B%�BG}BN�BO�BN�B}�B�B�B��B�B�>B��B�B�B5BIBdB!�B'�B*�B,�B.�B.�B1�B5B7B8B8!B:-B9&B9"B8B1�B+�B#�B�BlB2BBB�B��B��B�xB�:B��BϪB�aB��B�1BlTB!�B
�8B
��B
�kB
�,B
��B
��B
�B
��B
x�B
\�B
HzB
5B
�B	��B	�OB	��B	�\B	��B	�B	}�B	z�B	v�B	cB	K�B	=5B	3�B	,�B	"�B	DB		�B��B��B�qB�]B�jB�lB�cB�hB�^B�tB�uB��B��B��B��B		�B	B	B	/B	KB	&�B	N�B	R�B	W�B	\�B	j=B	}�B	�B	�
B	ϜB	��B	��B	��B	��B
�B
�B
�B
B
]B
 �B
!�B
"�B
!�B
 �B
 �B
 �B
 �B
!�B
#�B
#�B
$�B
&�B
-�B
1�B
9B
:B
:B
:B
:B
9B
7
B
6B
<&B
=-B
=-B
=+B
=,B
<'B
<'B
<'B
<#B
<&B
<(B
<$B
<$B
<%B
<#B
<$B
<%B
;!B
;B
;B
<&B
<%B
<&B
; B
; B
;B
<%B
=*B
=+B
<%B
=+B
=.B
=,B
=,B
=,B
=,B
<#B
=+B
=+B
=)B
=*B
=)B
=*B
<#B
<#B
<$B
<&B
<%B
<%B
;B
;B
;B
;B
:B
9B
8B
7B
5�B
5�B
4�B
4�B
3�B
3�B
3�B
2�B
2�B
2�B
1�B
1�B
1�B
1�B
1�B
0�B
0�B
0�B
/�B
.�B
-�B
,�B
*�B
(�B
$�B
dB
@B
-B
(B
(B
 B
B
B
B
B
B
B
	B
B
B
	B

B
B
B
B

B
B
B
B
B
B
B
B
B
B
B
B
(B
&B
'B
B
(B
B
 B
B
B
B
 B

�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
B
B
	B
 B

�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	�~B	�xB	��B	�xB	�nB	�YB	�AB	�/B	�%B	�B	�B	�B	�#B	�#B	�$B	�*B	�/B	�.B	�7B	�5B	�7B	�<B	�CB	�GB	�OB	�OB	�ZB	�cB	�`B	�`B	�dB	�gB	�lB	�gB	�hB	�fB	�fB	�iB	�eB	�fB	�gB	�gB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B
 B
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
 B
B
B
B
B
 B
B
)B
1B
fB
'�B
,�B
0�B
:B
?1B
CNB
HiB
L�B
P�B
T�B
W�B
Y�B
^�B
c	B
j4B
p\B
w�B
z�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.58 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008082019040510080820190405100808  AO  ARCAADJP                                                                    20171001000123    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171001000123  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171001000123  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100808  IP                  G�O�G�O�G�O�                