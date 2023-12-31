CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:54Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125954  20190405100759  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @����Y21   @���y\�2@/�?|�h�d ě��T1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy� D�  D�<�D���D�� D� D�C3D�� D�ٚD�fD�@ D�l�D��3D�3D�C3D�y�D��3D�	�D�<�D�l�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@ҏ\A	G�A)G�AIG�AiG�A���A���A���A���Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2Q�B:Q�BBQ�BJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�{CF�{CH�{CJ�{CL�{CN�{CP�{CR�{CT�{CV�{CX�{CZ�{C\�{C^�{C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�W
C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD�DD��DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn+�Dn��Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�Dt��Dy�D��D�O\D��)D��D�"�D�U�D���D��)D��D�R�D�\D���D��D�U�Dڌ)D���D�)D�O\D�\D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aʰ!Aʴ9AʶFAʶFAʶFAʴ9Aʴ9Aʴ9AʸRAʺ^Aʴ9Aʩ�Aʣ�AʓuA�v�A�p�A�dZA�M�A�K�A�K�A�I�A�G�A�E�A�C�A�A�A�;dA�$�A��A��A��A�1A��
Aɴ9AɑhA�|�A�t�A�l�A�ZA�O�A�XA�bNA���A�I�Aˉ7A�  A���A�dZA�VAʰ!A�XAɶFA�z�A��yAȶFAȲ-A��A�XA�z�Aȉ7A���Aţ�AľwA�A�-A���A���A��A�G�A��HA��A�|�A�l�A���A��hA�C�A��A�?}A��PA��A��\A�"�A��A��9A�G�A�7LA��RA�S�A�;dA��-A���A�A�/A��A��!A�ĜA�A�A��A��\A��hA��mA��\A�t�A�?}A�  A��A|�/AyAw|�Au/Ar��Aq�-AoG�Am�AlffAj��Ag7LAd1'Aa�
A[��AY��AU�mASAPv�AN$�AM�AL�AK�;AIXAG��AF~�AEAD�jAB��AA/A?\)A<�HA;�7A9�TA9��A8VA6�A6��A4�A2VA2�/A3��A3oA2z�A0��A0��A/��A/�A/;dA-�hA,�9A,^5A+"�A*��A*��A)��A'��A'+A%��A#��A#oA"��A"�A (�An�A
=A�;A9XAz�AM�A|�A�AA��A��A�mAdZA��A�A��A�\A�AffAM�A  Al�A�/Ar�A��AVA-A`BA	��A�\A�jA��A9XA�7A��A��A�A9XA�mA|�A��A9XA�mA?}A M�@��m@���A �!A �R@���@�(�@�
=@��#@���@�ƨ@�dZ@�ff@�7L@��9@�j@�bN@�9X@��
@�+@��@�V@��T@�O�@�7L@��@��
@�F@��@�^5@��#@�P@�v�@�5?@�x�@��@�V@��`@�@�x�@�J@�@�o@�J@�p�@�%@�j@�bN@�z�@��@�@�9@�9@�Z@�l�@��@�~�@�J@�x�@��@㝲@�"�@��@�V@⟾@�n�@�5?@�+@�ȴ@���@⟾@�v�@�\@�9@ޏ\@�J@ݙ�@��/@�A�@ۅ@ڟ�@�M�@ڗ�@ڸR@���@�ȴ@�=q@ؼj@�Q�@�  @�ƨ@�l�@և+@�V@�33@��y@���@�S�@ӥ�@�K�@�-@�p�@�V@Ь@�  @�dZ@�l�@�l�@�t�@�\)@�K�@�"�@�~�@͡�@͉7@�X@��/@̋D@�bN@���@�~�@��@��@�/@Ȭ@�1'@ǝ�@�33@���@�=q@ŉ7@���@�r�@�9X@� �@��
@��y@�^5@��#@��-@�p�@�?}@�%@���@�bN@�  @�K�@��H@���@���@�v�@�V@�=q@��@�p�@���@���@�\)@��R@�x�@�/@��`@��u@�  @���@�C�@���@�v�@��@�J@�{@���@�X@�9X@��@���@��@�+@�ȴ@���@�^5@�J@���@��-@��@�O�@���@�I�@���@�33@�"�@��@���@��!@�$�@�`B@��@�%@��@���@���@���@��9@�9X@�ƨ@���@�C�@��H@�@��@���@���@�1'@��@��m@���@�l�@�+@�ȴ@�~�@�-@��T@���@�@��^@���@�?}@�z�@� �@��
@���@���@�n�@���@��@��-@��@�?}@��9@�Q�@��@��P@�dZ@�"�@�@���@�5?@���@���@��7@�`B@���@��9@�1@�l�@�+@���@���@��\@��+@�n�@�~�@�~�@�ff@�@��T@���@�&�@���@��j@���@��@�bN@�bN@�Q�@�A�@��@��@� �@��@��P@��y@{�F@qx�@i�7@^��@W��@O
=@G��@=/@4�D@.��@)X@$��@l�@�@V@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aʰ!Aʴ9AʶFAʶFAʶFAʴ9Aʴ9Aʴ9AʸRAʺ^Aʴ9Aʩ�Aʣ�AʓuA�v�A�p�A�dZA�M�A�K�A�K�A�I�A�G�A�E�A�C�A�A�A�;dA�$�A��A��A��A�1A��
Aɴ9AɑhA�|�A�t�A�l�A�ZA�O�A�XA�bNA���A�I�Aˉ7A�  A���A�dZA�VAʰ!A�XAɶFA�z�A��yAȶFAȲ-A��A�XA�z�Aȉ7A���Aţ�AľwA�A�-A���A���A��A�G�A��HA��A�|�A�l�A���A��hA�C�A��A�?}A��PA��A��\A�"�A��A��9A�G�A�7LA��RA�S�A�;dA��-A���A�A�/A��A��!A�ĜA�A�A��A��\A��hA��mA��\A�t�A�?}A�  A��A|�/AyAw|�Au/Ar��Aq�-AoG�Am�AlffAj��Ag7LAd1'Aa�
A[��AY��AU�mASAPv�AN$�AM�AL�AK�;AIXAG��AF~�AEAD�jAB��AA/A?\)A<�HA;�7A9�TA9��A8VA6�A6��A4�A2VA2�/A3��A3oA2z�A0��A0��A/��A/�A/;dA-�hA,�9A,^5A+"�A*��A*��A)��A'��A'+A%��A#��A#oA"��A"�A (�An�A
=A�;A9XAz�AM�A|�A�AA��A��A�mAdZA��A�A��A�\A�AffAM�A  Al�A�/Ar�A��AVA-A`BA	��A�\A�jA��A9XA�7A��A��A�A9XA�mA|�A��A9XA�mA?}A M�@��m@���A �!A �R@���@�(�@�
=@��#@���@�ƨ@�dZ@�ff@�7L@��9@�j@�bN@�9X@��
@�+@��@�V@��T@�O�@�7L@��@��
@�F@��@�^5@��#@�P@�v�@�5?@�x�@��@�V@��`@�@�x�@�J@�@�o@�J@�p�@�%@�j@�bN@�z�@��@�@�9@�9@�Z@�l�@��@�~�@�J@�x�@��@㝲@�"�@��@�V@⟾@�n�@�5?@�+@�ȴ@���@⟾@�v�@�\@�9@ޏ\@�J@ݙ�@��/@�A�@ۅ@ڟ�@�M�@ڗ�@ڸR@���@�ȴ@�=q@ؼj@�Q�@�  @�ƨ@�l�@և+@�V@�33@��y@���@�S�@ӥ�@�K�@�-@�p�@�V@Ь@�  @�dZ@�l�@�l�@�t�@�\)@�K�@�"�@�~�@͡�@͉7@�X@��/@̋D@�bN@���@�~�@��@��@�/@Ȭ@�1'@ǝ�@�33@���@�=q@ŉ7@���@�r�@�9X@� �@��
@��y@�^5@��#@��-@�p�@�?}@�%@���@�bN@�  @�K�@��H@���@���@�v�@�V@�=q@��@�p�@���@���@�\)@��R@�x�@�/@��`@��u@�  @���@�C�@���@�v�@��@�J@�{@���@�X@�9X@��@���@��@�+@�ȴ@���@�^5@�J@���@��-@��@�O�@���@�I�@���@�33@�"�@��@���@��!@�$�@�`B@��@�%@��@���@���@���@��9@�9X@�ƨ@���@�C�@��H@�@��@���@���@�1'@��@��m@���@�l�@�+@�ȴ@�~�@�-@��T@���@�@��^@���@�?}@�z�@� �@��
@���@���@�n�@���@��@��-@��@�?}@��9@�Q�@��@��P@�dZ@�"�@�@���@�5?@���@���@��7@�`B@���@��9@�1@�l�@�+@���@���@��\@��+@�n�@�~�@�~�@�ff@�@��T@���@�&�@���@��j@���@��@�bN@�bN@�Q�@�A�G�O�@��@� �@��@��P@��y@{�F@qx�@i�7@^��@W��@O
=@G��@=/@4�D@.��@)X@$��@l�@�@V@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B�B�B�B��B�B�B�B�!B�3B�9B�?B�LB�RB�XB�XB�dB�qBŢBɺB��B��B��B�B�5B�sB��B	JB	�B	'�B	'�B	&�B	%�B	&�B	7LB	XB	��B
e`B
�JB
�}B
�B
�;B
�;B
�HB
�HB
�)B
�B
�NB
�B
��BVB<jBt�B�hB�LBȴB��B��B�qB��B�B��B�B33B1'B;dBC�BP�BN�BJ�BH�BD�B9XB0!B!�B�B
=B�)BĜB�-B��B|�B_;B(�B�BDB
�B
�)B
B
��B
z�B
e`B
I�B
�B	��B

=B
+B
�B
�B
JB	�B	�B	ɺB	�qB	�-B	��B	��B	�{B	�DB	� B	hsB	P�B	>wB	oB	B�B�/B�B�mB�mB�B�B�B�B�B�B�B�sB�HB�
B��B��B��B�fB�B�B	B	\B	%B	�B	D�B	N�B	bNB	k�B	gmB	ffB	r�B	y�B	�oB	��B	�B	��B	�B	�'B	�9B	�?B	�'B	��B	��B	��B	��B	��B	�B	u�B	�oB	�uB	�B	u�B	k�B	ffB	q�B	x�B	t�B	p�B	k�B	jB	r�B	v�B	v�B	v�B	v�B	v�B	y�B	z�B	z�B	~�B	{�B	v�B	r�B	q�B	m�B	aHB	\)B	bNB	cTB	_;B	\)B	YB	W
B	T�B	Q�B	P�B	N�B	L�B	L�B	VB	W
B	S�B	S�B	W
B	aHB	gmB	cTB	^5B	[#B	]/B	_;B	^5B	]/B	\)B	^5B	aHB	cTB	dZB	e`B	gmB	l�B	n�B	o�B	p�B	r�B	s�B	v�B	v�B	~�B	�B	~�B	~�B	z�B	{�B	|�B	~�B	�B	�B	�%B	�DB	��B	��B	�FB	�?B	�'B	�B	�B	�B	�B	�'B	�-B	�9B	�?B	�XB	�qB	�wB	�wB	�wB	�dB	�XB	�LB	�9B	�3B	�-B	�-B	�XB	�qB	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ɺB	��B	��B	��B	��B	�/B	�5B	�;B	�;B	�;B	�HB	�BB	�;B	�;B	�5B	�5B	�B	�
B	�
B	�B	�5B	�TB	�TB	�HB	�;B	�5B	�5B	�5B	�NB	�NB	�NB	�TB	�TB	�NB	�NB	�ZB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�sB	�sB	�sB	�mB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
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
%B
%B
%B
%B
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
	7B
	7B
	7B
	7B

=B

=B

=B

=B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
JB
PB
PB
PB
JB
PB
PB
PB
VB
VB
\B
\B
bB
bB
hB
bB
bB
bB
bB
bB
hB
oB
oB
oB
hB
hB
hB
hB
oB
uB
oB
oB
uB
uB
{B
{B
�B
�B
�B
 �B
+B
2-B
7LB
;dB
?}B
F�B
L�B
Q�B
W
B
[#B
_;B
dZB
gmB
k�B
o�B
s�B
w�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�*B�)B�7B�CB�sBɋBΫB��B��B��B�	B�HB��B	B	�B	'�B	'�B	&�B	%�B	&�B	7B	W�B	̜B
e3B
�B
�QB
��B
�
B
�B
�B
�B
��B
��B
�!B
�B
��B'B<:Bt�B�8B�BȃB��B͡B�DB˖BPB��B�B3B0�B;2BCdBP�BN�BJ�BHBDiB9"B/�B!�BfB
B��B�fB��B�cB|�B_B(�B^BB
�uB
��B
�XB
��B
z�B
e(B
I�B
gB	��B

B
�B
eB
cB
B	�nB	��B	ɁB	�5B	��B	��B	�pB	�?B	�B	�B	h9B	P�B	>8B	3B	 �B�@B��B��B�-B�-B�GB�uB�uB�kB�_B�RB�EB�2B�B��BѫBΙBԼB�%B�=B�cB	�B	B	�B	|B	DZB	N�B	b
B	kAB	g+B	f&B	rmB	y�B	�,B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	�|B	�`B	�WB	�HB	��B	u�B	�-B	�2B	��B	uB	kAB	f!B	qeB	x�B	txB	p_B	k?B	j;B	rjB	v�B	v�B	v�B	v�B	v�B	y�B	z�B	z�B	~�B	{�B	v�B	rjB	qeB	mJB	aB	[�B	bB	cB	^�B	[�B	X�B	V�B	T�B	Q�B	P�B	N�B	L�B	L�B	U�B	V�B	S�B	S�B	V�B	aB	g&B	cB	]�B	Z�B	\�B	^�B	]�B	\�B	[�B	]�B	`�B	cB	dB	eB	g'B	lDB	nNB	oUB	p\B	rhB	smB	v�B	v�B	~�B	��B	~�B	~�B	z�B	{�B	|�B	~�B	��B	��B	��B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�.B	�/B	�1B	�B	�B	�B	��B	��B	��B	��B	�B	�'B	�9B	�SB	�jB	̃B	ϖB	ѡB	ҫB	НB	�xB	�oB	�sB	�rB	�B	̃B	͋B	ҨB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�$B	�#B	�#B	�%B	�#B	�)B	�/B	�*B	�(B	�,B	�#B	�/B	�6B	�5B	�-B	�BB	�GB	�AB	�BB	�?B	�<B	�FB	�MB	�TB	�UB	�ZB	�[B	�ZB	�YB	�XB	�YB	�^B	�`B	�aB	�_B	�aB	�`B	�^B	�`B	�eB	�lB	�nB	�lB	�fB	�`B	�fB	�iB	�sB	�rB	�xB	��B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
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
�B
�B
�B
�B
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
�B
�B
�B
�B
	�B
	�B
	�B
	�B
�B
�B
	�B
	�B
	�B
	�B
	�B

�B

�B
B
B
B
B
 B
B
B
B
	B
B
B
B
B
B
B
B
B
B
B
B
B
&B
&B
$B
B
B
B
B
#B
*B
'B
$B
)B
)B
1B
0G�O�B
BB
gB
 {B
*�B
1�B
6�B
;B
?2B
F^B
L�B
Q�B
V�B
Z�B
^�B
dB
g"B
k8B
oSB
slB
w�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.58 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007592019040510075920190405100759  AO  ARCAADJP                                                                    20181121125954    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125954  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125954  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100759  IP                  G�O�G�O�G�O�                