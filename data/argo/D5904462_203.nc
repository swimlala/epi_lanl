CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-07-19T00:00:47Z creation      
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170719000047  20190405100805  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��eC@1   @���57�@,�I�^5?�dm�E��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B���B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy�fD�fD�VfD��3D���D��D�FfD�|�D�y�D�fD�I�D�s3DǬ�D��D�@ DږfD���D�fD�@ D�c3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @e�@��\@ҏ\A	G�A)G�AIG�AiG�A���A���A���A�p�Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2�RB:�RBBQ�BJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B�(�B�(�B�(�B�(�B�(�B�B���B�(�B�(�B�\)B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�{C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�{CF�CH�{CJ�{CL�{CN�{CP�{CR�{CT�{CV�{CX�{CZ�{C\�{C^�{C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�W
C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-+�D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt��Dy˅D�(�D�h�D���D��\D�\D�X�D��\D��)D�(�D�\)D���Dǿ\D�,)D�R�Dڨ�D��\D��D�R�D�u�D�ҏ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�VA���A��A���Aغ^AؓuA؁A�x�A�t�A�n�A�jA�ffA�bNA�\)A�ZA�VA�S�A�S�A�Q�A�Q�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�Q�A�M�A�G�A�C�A�C�A�C�A�33A�"�Aץ�A�-A�S�A���A��A�&�A�
=A��A�K�A�{AȲ-A�1'Aǰ!A��mA�E�A�\)A�r�A�~�A�9XA��A\A�ĜA�O�A�5?A�n�A�1'A��RA��A�7LA��A�{A���A��A�bA�~�A�7LA��A�+A���A�
=A�bA��A��A���A�t�A�&�A��DA��A��A�ZA���A�n�A��A�VA�x�A�9XA�%A��yA�\)A�t�A�dZA�/A�"�A��7A�jA��A|$�Av��Aq
=Alv�Ai��AgG�Ae�^A`�DAZ5?AYAVM�AT5?ASƨASK�AR�9AQ�mAQ33APQ�AOS�AKl�AHbNAFbNAD�uADI�AC
=A??}A=O�A;S�A:v�A:VA9%A8JA5��A3�PA1�^A01A.�yA,r�A(�DA%�A"�\A"(�A r�A#l�A$v�A$�yA$A"�DA#A#�A"��A"�!A!��A!S�A VAt�A��A�RA��A�hA�+A�
A|�A��A9XA?}A�uA~�AZAjA�RA$�AO�A��A;dA
ZA^5A�A�A	�wA	�#AM�A�/A^5A��AȴA  A�A  AM�AjA��AoA33A��A�PAO�A ��@���@�Q�@���@���@�Z@�1'@�ff@���@�
=@�o@�+@�+@�7L@��@�ff@�t�@��@�M�@���@�7L@�@��#@�n�@�x�@�?}@���@�{@�@�7L@��`@�l�@�@�\@�{@���@�7@�hs@�G�@�&�@��@�9@�@�bN@�9X@���@�dZ@ꟾ@�\@�-@�b@�
=@���@�&�@��@��@��@�9@���@��/@��@�%@���@�%@��@��@�X@�X@�/@���@�9@�Z@���@��@�n�@�=q@�-@�-@�X@��@��@��@�(�@��y@���@���@�@ݲ-@݁@�%@�1'@��@�l�@��@ڗ�@��T@ؼj@�z�@� �@׶F@�o@���@և+@�@�Ĝ@�1@ӝ�@�C�@҇+@ѡ�@�Ĝ@Гu@��;@�C�@Χ�@Ͳ-@�Ĝ@�A�@�I�@˶F@�@���@�$�@�?}@�1@�S�@�
=@���@��@�ȴ@�V@ź^@�&�@���@��`@���@�Q�@î@�dZ@�K�@�"�@°!@�J@��7@��`@�j@��m@���@�"�@��!@�-@��^@��h@�Ĝ@��@�1'@��m@��@�C�@���@���@�M�@�@�hs@�%@��/@���@��@��m@�l�@�C�@���@�{@��h@�hs@�7L@���@��D@�  @�|�@��@�~�@�5?@��@���@���@���@���@��@�X@��@��`@�A�@��@�S�@��@���@�ȴ@�-@��-@��@�`B@�/@��j@�bN@�ƨ@�|�@�dZ@�+@���@��@��^@�p�@���@�z�@��@�;d@�\)@�dZ@�;d@�=q@�?}@��`@��@�(�@��w@�o@��!@��@���@�X@�Ĝ@�Z@���@�|�@�
=@�ff@���@�hs@���@�r�@�9X@�  @���@�|�@�C�@��@�@���@�n�@�{@��@�x�@�9X@�  @��@�t�@�K�@�+@�
=@���@�-@��#@���@�hs@�O�@�?}@�?}@�&�@��@���@��9@�9X@��@���@�|�@�\)@�K�@�33@�@���@�v�@�-@��T@��T@���@�p�@��@��D@�I�@�v�@�{@�Q�@+@v�+@n@cS�@\�/@U�-@N�+@Gl�@@1'@8�@0��@(�`@"�\@@�@��@�@�h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7LA�VA���A��A���Aغ^AؓuA؁A�x�A�t�A�n�A�jA�ffA�bNA�\)A�ZA�VA�S�A�S�A�Q�A�Q�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�Q�A�M�A�G�A�C�A�C�A�C�A�33A�"�Aץ�A�-A�S�A���A��A�&�A�
=A��A�K�A�{AȲ-A�1'Aǰ!A��mA�E�A�\)A�r�A�~�A�9XA��A\A�ĜA�O�A�5?A�n�A�1'A��RA��A�7LA��A�{A���A��A�bA�~�A�7LA��A�+A���A�
=A�bA��A��A���A�t�A�&�A��DA��A��A�ZA���A�n�A��A�VA�x�A�9XA�%A��yA�\)A�t�A�dZA�/A�"�A��7A�jA��A|$�Av��Aq
=Alv�Ai��AgG�Ae�^A`�DAZ5?AYAVM�AT5?ASƨASK�AR�9AQ�mAQ33APQ�AOS�AKl�AHbNAFbNAD�uADI�AC
=A??}A=O�A;S�A:v�A:VA9%A8JA5��A3�PA1�^A01A.�yA,r�A(�DA%�A"�\A"(�A r�A#l�A$v�A$�yA$A"�DA#A#�A"��A"�!A!��A!S�A VAt�A��A�RA��A�hA�+A�
A|�A��A9XA?}A�uA~�AZAjA�RA$�AO�A��A;dA
ZA^5A�A�A	�wA	�#AM�A�/A^5A��AȴA  A�A  AM�AjA��AoA33A��A�PAO�A ��@���@�Q�@���@���@�Z@�1'@�ff@���@�
=@�o@�+@�+@�7L@��@�ff@�t�@��@�M�@���@�7L@�@��#@�n�@�x�@�?}@���@�{@�@�7L@��`@�l�@�@�\@�{@���@�7@�hs@�G�@�&�@��@�9@�@�bN@�9X@���@�dZ@ꟾ@�\@�-@�b@�
=@���@�&�@��@��@��@�9@���@��/@��@�%@���@�%@��@��@�X@�X@�/@���@�9@�Z@���@��@�n�@�=q@�-@�-@�X@��@��@��@�(�@��y@���@���@�@ݲ-@݁@�%@�1'@��@�l�@��@ڗ�@��T@ؼj@�z�@� �@׶F@�o@���@և+@�@�Ĝ@�1@ӝ�@�C�@҇+@ѡ�@�Ĝ@Гu@��;@�C�@Χ�@Ͳ-@�Ĝ@�A�@�I�@˶F@�@���@�$�@�?}@�1@�S�@�
=@���@��@�ȴ@�V@ź^@�&�@���@��`@���@�Q�@î@�dZ@�K�@�"�@°!@�J@��7@��`@�j@��m@���@�"�@��!@�-@��^@��h@�Ĝ@��@�1'@��m@��@�C�@���@���@�M�@�@�hs@�%@��/@���@��@��m@�l�@�C�@���@�{@��h@�hs@�7L@���@��D@�  @�|�@��@�~�@�5?@��@���@���@���@���@��@�X@��@��`@�A�@��@�S�@��@���@�ȴ@�-@��-@��@�`B@�/@��j@�bN@�ƨ@�|�@�dZ@�+@���@��@��^@�p�@���@�z�@��@�;d@�\)@�dZ@�;d@�=q@�?}@��`@��@�(�@��w@�o@��!@��@���@�X@�Ĝ@�Z@���@�|�@�
=@�ff@���@�hs@���@�r�@�9X@�  @���@�|�@�C�@��@�@���@�n�@�{@��@�x�@�9X@�  @��@�t�@�K�@�+@�
=@���@�-@��#@���@�hs@�O�@�?}@�?}@�&�@��@���@��9@�9X@��@���@�|�@�\)@�K�@�33@�@���@�v�@�-@��T@��T@���@�p�@��@��D@�I�@�v�@�{@�Q�@+@v�+@n@cS�@\�/@U�-@N�+@Gl�@@1'@8�@0��@(�`@"�\@@�@��@�@�h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	hsB	iyB	hsB	hsB	hsB	gmB	gmB	gmB	ffB	gmB	gmB	gmB	gmB	ffB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	e`B	e`B	�PB	�dB	��B
&�B
YB
�hB
�^B
ǮB
��B
��BBbB#�B49BA�BZBq�Bw�Bz�B~�B�PB�oB�bB��B�9B�LB�jBĜBĜBĜB��B��B��B�5B�B�yB��BB��B�fB�B��Bq�BE�B  B
�B
�qB
�oB
�DB
�DB
�FB
ƨB
B
�XB
��B
��B
��B
��B
��B
��B
}�B
[#B
N�B
#�B	�TB	�jB	��B	�uB	� B	q�B	ffB	ZB	F�B	/B	)�B	!�B	�B	�B	�B	�B	�B	�B	�B	oB	{B	uB	uB	{B	hB	VB	JB	JB	\B	\B	VB	{B	�B	 �B	�B	�B	�B	�B	DB�B��B�B�B�B	F�B	cTB	y�B	}�B	}�B	�oB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�uB	�bB	�VB	�=B	�+B	�B	�B	�B	}�B	r�B	cTB	Q�B	:^B	+B	%�B	�B	�B	�B	#�B	H�B	R�B	H�B	>wB	9XB	33B	/B	/B	+B	-B	<jB	G�B	L�B	XB	aHB	]/B	VB	YB	XB	O�B	O�B	A�B	7LB	<jB	@�B	?}B	K�B	^5B	e`B	jB	u�B	�7B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�RB	�LB	�XB	B	ƨB	ǮB	ƨB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�
B	��B	��B	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	�
B	�)B	�/B	�5B	�BB	�HB	�ZB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
  B
  B
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
B
B
B
B
%B
%B
+B
+B
1B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
JB
JB
PB
JB
JB
PB
PB
PB
PB
JB
JB
JB
JB
JB
JB
JB
JB
PB
PB
PB
JB

=B
	7B
1B
	7B
	7B
	7B
1B
%B
B
B
B
B
%B
%B
B
B
B
+B
	7B
DB
DB
JB
JB
JB
PB
PB
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
{B
{B
{B
{B
{B
{B
{B
�B
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
�B
�B
�B
�B
�B
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
!�B
"�B
$�B
/B
7LB
;dB
B�B
H�B
N�B
R�B
VB
ZB
_;B
cTB
gmB
l�B
p�B
s�B
w�B
{�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	hFB	iKB	hHB	hDB	hGB	g>B	gCB	g@B	f;B	gAB	g?B	g?B	g?B	f5B	g@B	gBB	g@B	g@B	g>B	g@B	g>B	g@B	g>B	g>B	g=B	g@B	g@B	g@B	g>B	g@B	gAB	gAB	gAB	gAB	g=B	e3B	e1B	�!B	�8B	��B
&�B
X�B
�:B
�1B
ǀB
ʎB
��B�B2B#�B4
BAYBY�BqzBw�Bz�B~�B�B�>B�1B��B�
B�B�6B�lB�kB�kBʍBϮBϯB�B�RB�GB��B �B��B�4B��B�rBqwBEnB
��B
��B
�9B
�8B
�B
�B
�B
�tB
�WB
�"B
�KB
��B
�[B
�cB
�xB
��B
}�B
Z�B
N�B
#�B	�B	�1B	��B	�7B	�B	qkB	f'B	Y�B	FjB	.�B	)�B	!�B	rB	hB	`B	ZB	UB	PB	IB	0B	=B	5B	6B	9B	)B	B	B	B	B	B	B	:B	RB	 �B	zB	zB	vB	rB	 B�tB��B�IB�fB�sB	FeB	cB	y�B	}�B	}�B	�,B	�UB	�cB	�hB	�zB	��B	��B	��B	��B	��B	��B	�TB	�3B	�B	�B	��B	��B	��B	��B	��B	}�B	rkB	cB	Q�B	:B	*�B	%�B	zB	RB	:B	#�B	HpB	R�B	HnB	>/B	9B	2�B	.�B	.�B	*�B	,�B	<#B	GfB	L�B	W�B	a B	\�B	U�B	X�B	W�B	O�B	O�B	AAB	7B	<#B	@<B	?4B	KB	]�B	eB	j5B	uyB	��B	�7B	�jB	�8B	�=B	�EB	�KB	�}B	��B	��B	�
B	�B	�B	�GB	�aB	�iB	�`B	�lB	�kB	�kB	�zB	͌B	МB	ҫB	ԷB	ԵB	չB	ռB	սB	��B	��B	��B	��B	��B	��B	��B	ӮB	͊B	�qB	�kB	�nB	�~B	̂B	͌B	ОB	ӯB	��B	��B	��B	��B	��B	��B	�B	�2B	�=B	�AB	�BB	�EB	�DB	�;B	�8B	�6B	�7B	�<B	�7B	�4B	�BB	�HB	�TB	�VB	�VB	�UB	�_B	�gB	�gB	�iB	�hB	�iB	�oB	�mB	�nB	�kB	�fB	�ZB	�eB	��B	�yB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
B
�B
�B
B
B
B
B
 B
�B
B
B
�B
�B
�B
B
B
B
B
�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B

�B
�B
 B
 B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
$B
1B
+B
.B
/B
0B
.B
/B
3B
AB
DB
AB
?B
DB
?B
?B
GB
HB
KB
IB
OB
NB
TB
TB
SB
UB
SB
\B
YB
_B
bB
gB
iB
lB
qB
qB
uB
!}B
"�B
$�B
.�B
6�B
;B
BCB
HgB
N�B
R�B
U�B
Y�B
^�B
c	B
g!B
l@B
pYB
siB
w�B
{�B
}�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.58 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008052019040510080520190405100805  AO  ARCAADJP                                                                    20170719000047    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170719000047  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170719000047  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100805  IP                  G�O�G�O�G�O�                