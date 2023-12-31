CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-01-03T06:39:27Z creation;2021-01-03T06:39:29Z conversion to V3.1;2023-06-29T05:47:34Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        t  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ix   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  MX   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  t    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �T   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �p   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210103063927  20230705041504  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0675_286                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�S��N�1   @�S�y\� @681&�x��b�O�M1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DJ��DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dk��Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ DԼ�D���D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�fD�#311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @^�R@��\@ҏ\A�A)G�AIG�AiG�A���A���A���A���Aģ�Aԣ�A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2Q�B:Q�BBQ�BJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�{CF�{CH�{CJ�{CL�{CN�{CP�{CR�{CT�{CV�{CX�{CZ�{C\�{C^�{C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�{Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D��D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD��DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK�DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX�DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk��Dl�Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�Du%Du�Dv%Dv�Dw%Dw�Dx%Dx�Dy%Dy�Dz%Dz�D{%D{�D|%D|�D}%D}�D~%D~�D%D�D��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D�D�ҏD��D�R�DÒ�D�ҏD��D�R�DĒ�D�ҏD��D�R�DŒ�D�ҏD��D�R�Dƒ�D�ҏD��D�R�Dǒ�D�ҏD��D�R�DȒ�D�ҏD��D�R�Dɒ�D�ҏD��D�R�Dʒ�D�ҏD��D�R�D˒�D�ҏD��D�R�D̒�D�ҏD��D�R�D͒�D�ҏD��D�R�DΒ�D�ҏD��D�R�Dϒ�D�ҏD��D�R�DВ�D�ҏD��D�R�Dђ�D�ҏD��D�R�DҒ�D�ҏD��D�R�DӒ�D�ҏD��D�R�DԒ�D��\D�\D�R�DՒ�D�ҏD��D�R�D֒�D�ҏD��D�R�Dג�D�ҏD��D�R�Dؒ�D�ҏD��D�R�Dْ�D�ҏD��D�R�Dڒ�D�ҏD��D�R�Dے�D�ҏD��D�R�Dܒ�D�ҏD��D�R�Dݒ�D�ҏD��D�R�Dޒ�D�ҏD��D�R�Dߒ�D�ҏD��D�R�D���D�ҏD��D�R�DᒏD�ҏD��D�R�D⒏D�ҏD��D�R�D㒏D�ҏD��D�R�D䒏D�ҏD��D�R�D咏D�ҏD��D�R�D撏D�ҏD��D�R�D璏D�ҏD��D�R�D蒏D�ҏD��D�R�D钏D�ҏD��D�R�D꒏D�ҏD��D�R�D뒏D�ҏD��D�R�D쒏D�ҏD��D�R�D풏D�ҏD��D�R�DD�ҏD��D�R�DD�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D�D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�R�D���D�ҏD��D�5�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�l�A�l�A�p�A�l�A�S�A�G�A�A�A�7LA�/A� �A��A�{A�VA�1A�  A��A��`A��;A��;A��yA��A���A���A��A��A�A���A�A�VA�{A��A��AüjAã�AöFA���A�  A�oA�1'A�  AöFAüjA�jA�ffA���A�|�A�l�A�p�A�9XA�K�A�`BA�`BA�G�A��A��A�M�A���A�ffA�1'A��+A���A�r�A��A���A�v�A�A�A�A�S�A��yA�ĜA��A��A��#A��A�ĜA��;A�%A��A�t�A���A�v�A�;dA���A�-A��yA�%A��A�-A�K�A�n�A��RA�E�A�|�A�ȴA���A�v�A���A�p�A�|�A�E�A� �A�JA~��A{�^Ayx�Ax�/Aw��Au�^Aq��An�Aln�AkC�Aj�\AiO�AgVAe�#Adn�AaK�A_�A^A\��AZ�uAXM�AUl�AS\)AQdZAPjANffALI�AJJAHr�AGoAE�PADA�ABI�AAhsA?%A=`BA<~�A<bA;/A9�hA8 �A7t�A6bNA5�A4Q�A2n�A1��A01A/XA.A,�A+�wA*5?A(�yA'�A&��A%+A#�7A"v�A {AȴA��A/A�TA33AA�/AE�A`BA��A��A��A(�A�mAAffA�
A��A(�A�FAA$�A�A
=A��A
�A	��A�A��A �Ax�A��A$�A��AI�A�;A%A $�@��F@���@��-@��^@�bN@��P@���@��u@�@�+@�Ĝ@�o@��T@�G�@���@�F@�v�@�7@�w@�33@�
=@�M�@��@�x�@�X@�j@���@�E�@���@�p�@�bN@�@އ+@�-@ݲ-@��`@�^5@ش9@�ƨ@�;d@�ȴ@�=q@�X@�b@���@�^5@�/@��/@��@ͩ�@��@�z�@� �@�S�@�V@��@�C�@�v�@�X@�j@î@�|�@���@��^@�7L@��/@�9X@��w@�\)@���@���@�p�@��@���@� �@���@�C�@���@��`@���@�33@�;d@��+@�@�&�@�r�@���@�o@�=q@�p�@���@��u@�(�@��
@�K�@���@��@���@��h@��7@�`B@��@�I�@��
@�"�@��R@�-@���@��@�Z@�\)@���@��@�?}@�z�@��
@��P@�+@�
=@���@�n�@�J@�p�@��@��@�b@���@���@�S�@��@���@�M�@��T@�p�@��@�Ĝ@��9@��@���@��u@�Z@�  @��
@��@�K�@��@���@��!@���@���@���@��\@��+@�V@���@�@���@�p�@�X@�&�@��@�r�@�I�@�(�@�ƨ@�|�@�l�@�S�@�;d@�o@��H@���@�^5@���@��T@��#@�`B@���@��@�9X@�b@��w@�dZ@���@�v�@�-@�{@�@��T@��7@�hs@���@���@�I�@�  @��w@�t�@�o@���@�{@��@�@���@��h@��7@��@�`B@�?}@�&�@��`@��9@��@���@�9X@�  @��@��F@�C�@���@���@�ff@�V@�$�@���@��T@��#@���@��7@�G�@��/@��@�z�@�j@�I�@�9X@��@���@��F@���@�dZ@�K�@�"�@�@���@��H@��R@���@��+@�~�@�v�@�n�@�ff@�M�@�5?@�@���@���@���@�x�@�O�@�G�@��@���@��/@��9@��D@�1'@��m@�ƨ@��w@��w@��@���@��P@��P@��P@��@�dZ@��@��@���@���@��!@���@��\@�v�@�^5@�^5@�V@�E�@�$�@�J@��@���@��@��j@��u@�Q�@�1'@� �@�1@l�@~�R@~v�@~@}�@}@}�h@}�@}/@|j@{�m@{�@{dZ@z�@z�H@z��@y�#@yx�@x��@x �@w��@w��@v��@vV@v@u@u�h@u`B@t��@t�@tI�@s�F@sS�@s33@r�@q�@qx�@q%@p��@p1'@o�w@ol�@o;d@o�@nȴ@nv�@nV@n5?@n@m��@m�h@m�@m�@m�@mp�@m`B@m?}@l��@lj@l1@kƨ@k��@k��@k"�@j��@j=q@jM�@i�#@i7L@h�9@h �@g�w@gl�@g;d@f��@f�R@fv�@fE�@f5?@f{@e@e�h@e�@e`B@e?}@e?}@e/@d�/@dz�@d1@c��@c��@cƨ@c"�@b�!@b�\@a��@a�^@`��@`�@`bN@`b@_�P@^�@^��@^ff@^5?@^@]�T@]�h@]O�@]V@\��@\Z@\�@[��@[��@[��@[ƨ@[C�@Z��@Z��@Z~�@Z-@Y��@Y��@Y�7@Y�7@Y�7@YX@Y�@XĜ@Xr�@X �@W�@W��@W|�@W�@V�+@VE�@V$�@V$�@U@Up�@U/@T��@T�@T(�@Sƨ@S��@St�@R~�@R-@Q��@Q��@Q��@Q�7@Qhs@P�9@O�@Ol�@O+@N��@N�@N��@Nv�@Nff@NE�@M�@M�h@M�@L�/@Lz�@L9X@L1@K��@K�
@K�F@K�F@Kt�@KC�@K"�@J�H@J�!@Jn�@J-@JJ@I��@I�#@I�^@I��@IX@H��@H�u@HQ�@H1'@H �@H  @G��@G|�@G;d@G�@F�y@FV@E��@D��@D�@D�D@DZ@D(�@C��@C�F@Ct�@CC�@B��@B^5@B-@A��@Ax�@@��@@�@@r�@@bN@@b@?|�@?+@>�y@>��@>v�@>ff@>$�@>{@>{@=�@=@=`B@=?}@<�@<�@<Z@;��@;�@;33@:�!@:-@:�@:J@9�7@8Ĝ@8bN@8Q�@81'@8 �@8b@8  @8  @7��@6�y@6��@6V@6{@5?}@4��@4�j@4��@41@3dZ@3S�@3S�@3C�@2�@2M�@1��@1�^@1��@1��@1�7@1hs@1�@0�9@0A�@/��@/�P@/
=@.E�@.@-�@-�T@-�h@-�@,�j@,�@,��@,9X@+�F@+�@+�@+�@+dZ@+C�@+"�@*�H@*^5@*J@)�^@)��@)G�@)%@)%@(�`@(Ĝ@(��@(��@(��@(�9@(�@(Q�@(1'@'�;@'��@'|�@'l�@'K�@&ȴ@&��@&$�@%��@%�h@%�@%p�@%`B@%O�@%/@%V@$�D@$9X@$�@#�m@#ƨ@#��@#t�@#S�@#S�@#C�@#"�@"�H@"�!@"n�@"�@!��@!�^@!�7@!�7@!X@!7L@ ��@ �9@ �@�@�;@�;@�;@�;@�;@�P@\)@;d@+@�y@��@V@5?@@�T@@��@O�@/@�j@�D@j@(�@1@�m@�
@ƨ@��@C�@�!@��@�\@�\@n�@��@&�@�@��@Ĝ@�u@�@Q�@1'@ �@b@b@�@�;@��@�@��@|�@�y@��@v�@E�@$�@{@�@@�-@��@p�@?}@��@��@z�@j@I�@9X@1@ƨ@��@S�@C�@33@"�@�@��@n�@�@��@�7@X@%@��@�`@�9@A�@b@�;@�w@�P@l�@K�@�@�y@ȴ@��@�+@V@{@��@�@?}@/@�@�@��@j@Z@�@��@C�@"�@o@
�@
��@
�\@
M�@
-@
�@
J@
J@	�#@	�7@	x�@	7L@	&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�l�A�l�A�p�A�l�A�S�A�G�A�A�A�7LA�/A� �A��A�{A�VA�1A�  A��A��`A��;A��;A��yA��A���A���A��A��A�A���A�A�VA�{A��A��AüjAã�AöFA���A�  A�oA�1'A�  AöFAüjA�jA�ffA���A�|�A�l�A�p�A�9XA�K�A�`BA�`BA�G�A��A��A�M�A���A�ffA�1'A��+A���A�r�A��A���A�v�A�A�A�A�S�A��yA�ĜA��A��A��#A��A�ĜA��;A�%A��A�t�A���A�v�A�;dA���A�-A��yA�%A��A�-A�K�A�n�A��RA�E�A�|�A�ȴA���A�v�A���A�p�A�|�A�E�A� �A�JA~��A{�^Ayx�Ax�/Aw��Au�^Aq��An�Aln�AkC�Aj�\AiO�AgVAe�#Adn�AaK�A_�A^A\��AZ�uAXM�AUl�AS\)AQdZAPjANffALI�AJJAHr�AGoAE�PADA�ABI�AAhsA?%A=`BA<~�A<bA;/A9�hA8 �A7t�A6bNA5�A4Q�A2n�A1��A01A/XA.A,�A+�wA*5?A(�yA'�A&��A%+A#�7A"v�A {AȴA��A/A�TA33AA�/AE�A`BA��A��A��A(�A�mAAffA�
A��A(�A�FAA$�A�A
=A��A
�A	��A�A��A �Ax�A��A$�A��AI�A�;A%A $�@��F@���@��-@��^@�bN@��P@���@��u@�@�+@�Ĝ@�o@��T@�G�@���@�F@�v�@�7@�w@�33@�
=@�M�@��@�x�@�X@�j@���@�E�@���@�p�@�bN@�@އ+@�-@ݲ-@��`@�^5@ش9@�ƨ@�;d@�ȴ@�=q@�X@�b@���@�^5@�/@��/@��@ͩ�@��@�z�@� �@�S�@�V@��@�C�@�v�@�X@�j@î@�|�@���@��^@�7L@��/@�9X@��w@�\)@���@���@�p�@��@���@� �@���@�C�@���@��`@���@�33@�;d@��+@�@�&�@�r�@���@�o@�=q@�p�@���@��u@�(�@��
@�K�@���@��@���@��h@��7@�`B@��@�I�@��
@�"�@��R@�-@���@��@�Z@�\)@���@��@�?}@�z�@��
@��P@�+@�
=@���@�n�@�J@�p�@��@��@�b@���@���@�S�@��@���@�M�@��T@�p�@��@�Ĝ@��9@��@���@��u@�Z@�  @��
@��@�K�@��@���@��!@���@���@���@��\@��+@�V@���@�@���@�p�@�X@�&�@��@�r�@�I�@�(�@�ƨ@�|�@�l�@�S�@�;d@�o@��H@���@�^5@���@��T@��#@�`B@���@��@�9X@�b@��w@�dZ@���@�v�@�-@�{@�@��T@��7@�hs@���@���@�I�@�  @��w@�t�@�o@���@�{@��@�@���@��h@��7@��@�`B@�?}@�&�@��`@��9@��@���@�9X@�  @��@��F@�C�@���@���@�ff@�V@�$�@���@��T@��#@���@��7@�G�@��/@��@�z�@�j@�I�@�9X@��@���@��F@���@�dZ@�K�@�"�@�@���@��H@��R@���@��+@�~�@�v�@�n�@�ff@�M�@�5?@�@���@���@���@�x�@�O�@�G�@��@���@��/@��9@��D@�1'@��m@�ƨ@��w@��w@��@���@��P@��P@��P@��@�dZ@��@��@���@���@��!@���@��\@�v�@�^5@�^5@�V@�E�@�$�@�J@��@���@��@��j@��u@�Q�@�1'@� �@�1@l�@~�R@~v�@~@}�@}@}�h@}�@}/@|j@{�m@{�@{dZ@z�@z�H@z��@y�#@yx�@x��@x �@w��@w��@v��@vV@v@u@u�h@u`B@t��@t�@tI�@s�F@sS�@s33@r�@q�@qx�@q%@p��@p1'@o�w@ol�@o;d@o�@nȴ@nv�@nV@n5?@n@m��@m�h@m�@m�@m�@mp�@m`B@m?}@l��@lj@l1@kƨ@k��@k��@k"�@j��@j=q@jM�@i�#@i7L@h�9@h �@g�w@gl�@g;d@f��@f�R@fv�@fE�@f5?@f{@e@e�h@e�@e`B@e?}@e?}@e/@d�/@dz�@d1@c��@c��@cƨ@c"�@b�!@b�\@a��@a�^@`��@`�@`bN@`b@_�P@^�@^��@^ff@^5?@^@]�T@]�h@]O�@]V@\��@\Z@\�@[��@[��@[��@[ƨ@[C�@Z��@Z��@Z~�@Z-@Y��@Y��@Y�7@Y�7@Y�7@YX@Y�@XĜ@Xr�@X �@W�@W��@W|�@W�@V�+@VE�@V$�@V$�@U@Up�@U/@T��@T�@T(�@Sƨ@S��@St�@R~�@R-@Q��@Q��@Q��@Q�7@Qhs@P�9@O�@Ol�@O+@N��@N�@N��@Nv�@Nff@NE�@M�@M�h@M�@L�/@Lz�@L9X@L1@K��@K�
@K�F@K�F@Kt�@KC�@K"�@J�H@J�!@Jn�@J-@JJ@I��@I�#@I�^@I��@IX@H��@H�u@HQ�@H1'@H �@H  @G��@G|�@G;d@G�@F�y@FV@E��@D��@D�@D�D@DZ@D(�@C��@C�F@Ct�@CC�@B��@B^5@B-@A��@Ax�@@��@@�@@r�@@bN@@b@?|�@?+@>�y@>��@>v�@>ff@>$�@>{@>{@=�@=@=`B@=?}@<�@<�@<Z@;��@;�@;33@:�!@:-@:�@:J@9�7@8Ĝ@8bN@8Q�@81'@8 �@8b@8  @8  @7��@6�y@6��@6V@6{@5?}@4��@4�j@4��@41@3dZ@3S�@3S�@3C�@2�@2M�@1��@1�^@1��@1��@1�7@1hs@1�@0�9@0A�@/��@/�P@/
=@.E�@.@-�@-�T@-�h@-�@,�j@,�@,��@,9X@+�F@+�@+�@+�@+dZ@+C�@+"�@*�H@*^5@*J@)�^@)��@)G�@)%@)%@(�`@(Ĝ@(��@(��@(��@(�9@(�@(Q�@(1'@'�;@'��@'|�@'l�@'K�@&ȴ@&��@&$�@%��@%�h@%�@%p�@%`B@%O�@%/@%V@$�D@$9X@$�@#�m@#ƨ@#��@#t�@#S�@#S�@#C�@#"�@"�H@"�!@"n�@"�@!��@!�^@!�7@!�7@!X@!7L@ ��@ �9@ �@�@�;@�;@�;@�;@�;@�P@\)@;d@+@�y@��@V@5?@@�T@@��@O�@/@�j@�D@j@(�@1@�m@�
@ƨ@��@C�@�!@��@�\@�\@n�@��@&�@�@��@Ĝ@�u@�@Q�@1'@ �@b@b@�@�;@��@�@��@|�@�y@��@v�@E�@$�@{@�@@�-@��@p�@?}@��@��@z�@j@I�@9X@1@ƨ@��@S�@C�@33@"�@�@��@n�@�@��@�7@X@%@��@�`@�9@A�@b@�;@�w@�P@l�@K�@�@�y@ȴ@��@�+@V@{@��@�@?}@/@�@�@��@j@Z@�@��@C�@"�@o@
�@
��@
�\@
M�@
-@
�@
J@
J@	�#@	�7@	x�@	7L@	&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�)B�;B�fB�B��BBB	7BVB�B�B�B(�B&�B(�B7LBE�BW
B\)BcTB`BB\)Bu�Bx�B�+B�uB{�Bl�Bm�B]/BW
B\)B`BBhsBcTBcTBbNB`BBffBe`BaHB_;B]/BW
BQ�BC�B?}B9XB0!B�BDBB�B�`B�BƨB�RB�B��B�Bw�BjBT�BL�BD�B@�B49B)�B�B1B
��B
�B
�yB
�/B
��B
��B
��B
��B
��B
�=B
u�B
bNB
N�B
9XB
�B
JB
%B	��B	�B	��B	�qB	�B	��B	��B	��B	�1B	|�B	s�B	`BB	R�B	I�B	C�B	49B	&�B	�B		7B��B��B�B�BB��B��BĜB�jB�FB�B��B��B��B�uB�{B�hB�\B�=B�1B�B�B� Bz�Bw�Bu�Br�Bo�Bm�BiyBe`BaHB]/B[#BXBR�BP�BO�BL�BK�BI�BJ�BM�BO�BN�BL�BM�BJ�BI�BH�BF�BE�BE�BC�BB�BA�B@�B?}B@�B?}B?}B>wBA�BB�B?}B;dB7LB33B2-B2-B1'B/B/B.B/B,B,B+B+B,B)�B+B.B0!B1'B33B5?B:^B=qB<jB;dB=qB>wB@�BF�BG�BG�BH�BH�BK�BM�BO�BR�BR�BR�BT�BVBW
BVBVBVBVBW
BW
BVBVBVBVBW
BYB]/B\)B^5B^5BdZBdZBdZBffBk�Bk�Bm�Bp�Bt�Bu�Bw�Bx�By�Bz�B|�B� B�B�B�B�B�%B�7B�VB�\B�bB�hB�oB�uB�uB�{B��B��B��B��B��B��B��B��B�B�!B�3B�LB�dB�qB�}B��BĜBŢBƨBȴBȴBɺBɺB��B��B��B�
B�B�#B�BB�TB�`B�mB�sB�B�B��B��B��B	B	B	B	B	B	1B	JB	PB	oB	�B	�B	�B	"�B	%�B	(�B	,B	.B	1'B	2-B	2-B	2-B	33B	49B	6FB	<jB	>wB	@�B	E�B	J�B	L�B	N�B	O�B	P�B	Q�B	R�B	R�B	T�B	ZB	\)B	^5B	aHB	bNB	cTB	hsB	jB	jB	k�B	n�B	p�B	r�B	s�B	t�B	t�B	u�B	v�B	{�B	|�B	~�B	� B	�B	�+B	�=B	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�FB	�LB	�RB	�XB	�^B	�^B	�^B	�dB	�jB	�qB	�}B	��B	��B	��B	ÖB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B
	7B
DB

=B
DB
JB
JB
PB
PB
VB
VB
VB
\B
\B
\B
\B
bB
bB
hB
hB
oB
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
{B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
,B
,B
-B
-B
.B
-B
-B
-B
-B
.B
.B
/B
1'B
1'B
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
49B
49B
49B
49B
5?B
6FB
6FB
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
9XB
9XB
9XB
9XB
9XB
:^B
:^B
9XB
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
>wB
>wB
>wB
>wB
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
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
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
H�B
I�B
I�B
J�B
J�B
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
M�B
M�B
M�B
M�B
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
S�B
T�B
T�B
T�B
T�B
VB
VB
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
XB
XB
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
\)B
\)B
\)B
\)B
\)B
]/B
]/B
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
_;B
_;B
_;B
_;B
`BB
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
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
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
hsB
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
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
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
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BΥB͟B͹B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�2B�;B��B�B�B	B"B?ByB�B(�B&�B(�B6�BEBV�B[�Bc�B`�B\xBvByrB�RB��B}BsBs�BabB[	B_VBe�Bj�Be`Bd�Bc�Bc�Bj0Bf�Bb�B`\B_�B\xBT{BD�BA;B<�B4�B5B�BB��B�B�qBȚB��B�B��B�+Bz^Bl�BVSBM�BE�BBAB5�B,�B�B	�B
�]B
��B
�6B
�B
�NB
ªB
�QB
�:B
��B
��B
x�B
e,B
R�B
<jB
 �B
jB
�B	��B	��B	��B	� B	�UB	��B	�\B	��B	��B	B	v�B	b�B	TaB	K^B	FtB	7B	*B	�B	DB��B�$B��B�B��B�jB�YB�B�lB��B�yB�hB��B�,B��B�@B��B�)B�lB�?B��B�B|ByXBv�Bt9Bp�BoBk6BgBb�B^�B\�BY�BT�BS[BQ4BM�BL�BKBK�BOBBQBO�BM�BN�BK�BJ�BIlBGBF�BF?BDMBC{BBuBAB@iBAoB@4B@4B?�BB�BC�B@�B<�B9>B4B2�B33B2|B/�B/�B/ B/�B,qB,�B+�B-B,�B*B+�B.�B0�B1�B4B6B:�B=�B<�B<B>B>�BAUBF�BG�BG�BH�BH�BK�BNVBP�BSBSBS&BU�BV�BW$BVBVmBV�BWYBW�BWYBV9BVBVSBV�BW�BY�B]dB\�B^�B_!Bd�Bd�Bd�Bf�BlBl�BnBp�Bu%Bv`Bx8By	By�B{0B}qB�B�'B�aB�3B�9B�tB��B�VB�\B�}B��B��B��B��B�MB�B��B��B�B�B�:B�8B�eB�IB�oB��B��B�JB��B�}B��B��BżBƎBȀBȀBɺB��B�B��B�:B�$B�EB�WB�vB�B��B�B��B��B�B��B��B�B	B	B	B	9B	SB	1B	~B	�B	TB	mB	�B	�B	"�B	%�B	)B	,"B	.B	0�B	1�B	1�B	2B	2�B	4B	6`B	<PB	>]B	@�B	E�B	J�B	L�B	N�B	O�B	P�B	Q�B	R�B	R�B	T�B	ZB	\B	^B	aB	b4B	cnB	hXB	jeB	jeB	k�B	n�B	p�B	r|B	s�B	t�B	t�B	u�B	v�B	{�B	|�B	~�B	�B	�'B	�B	�#B	�<B	�bB	�oB	�{B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�AB	�B	�B	�B	�>B	�*B	�*B	�*B	�0B	�6B	�VB	�cB	�iB	�OB	��B	�{B	�mB	ňB	ƨB	ɠB	ˬB	͹B	ϫB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	��B	��B	�B	��B	�B	�B	�B	�B	�!B	�B	�B	�B	�B	�B	�B	�4B	� B	� B	� B	�:B	� B	� B	� B	�@B	�FB	�,B	�2B	�2B	�2B	�8B	�RB	�>B	�XB	�XB	�yB	�B	�kB	�WB	�CB	�CB	�]B	�]B	�CB	�CB	�]B	�]B	�wB	�B	�B	�oB	�UB	�oB	�oB	�oB	�oB	�vB	�[B	�B	�vB	�vB	�|B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B
 B
 �B
B
B
�B
�B
B
B
�B
�B
�B
�B
B
B
B
B
	B
	B
	7B
B

#B
)B
0B
0B
6B
6B
"B
"B
"B
(B
(B
(B
(B
HB
.B
B
B
 B
:B
:B
:B
@B
[B
@B
@B
@B
@B
[B
{B
gB
SB
yB
B
�B
�B
qB
qB
qB
qB
qB
�B
xB
xB
xB
xB
xB
xB
xB
xB
dB
~B
~B
�B
�B
dB
dB
xB
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
#�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
*�B
+�B
+�B
,�B
,�B
-�B
,�B
-B
,�B
,�B
-�B
-�B
/B
0�B
0�B
1B
1'B
1B
1�B
1�B
2B
2-B
3B
2�B
2�B
4B
4B
4B
49B
5?B
6+B
6B
72B
72B
7B
72B
7B
7B
7B
8RB
88B
8B
9>B
9$B
9$B
9$B
9$B
:*B
:B
9$B
:DB
:DB
:*B
:DB
;0B
;0B
;0B
<6B
<6B
<6B
<6B
<6B
<PB
=<B
=<B
=VB
=<B
=<B
=<B
>]B
>BB
>]B
>BB
>]B
?cB
?}B
@iB
@OB
@OB
@OB
@OB
@OB
AUB
AoB
A�B
A�B
B[B
BuB
BuB
BuB
CaB
CaB
CaB
C{B
D�B
DgB
DgB
EmB
E�B
E�B
EmB
ESB
ESB
EmB
FtB
FtB
FtB
FtB
FtB
GzB
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
J�B
J�B
JrB
JrB
JrB
J�B
J�B
J�B
K�B
K�B
K�B
K�B
M�B
M�B
M�B
M�B
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
S�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
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
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
ZB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
]B
\�B
\�B
^B
]�B
^B
^B
^B
^B
^B
_!B
^�B
^�B
^�B
^�B
^�B
_B
`B
`B
`B
`B
`'B
`B
a-B
aB
aB
aB
a-B
aB
aB
a-B
bB
bB
bB
c B
c:B
cB
c:B
c B
c:B
cTB
dB
dB
dB
d&B
dZB
e`B
e,B
e,B
e,B
f2B
f2B
fLB
fLB
fB
f2B
fB
f2B
fB
fB
fLB
f2B
f2B
fLB
g8B
g8B
g8B
g8B
gB
g8B
g8B
h$B
hXB
h>B
h>B
h>B
h>B
iDB
i*B
iDB
iDB
iDB
jKB
jeB
jeB
j0B
j0B
jeB
jKB
jKB
kkB
kQB
kQB
lqB
lWB
lWB
l=B
lWB
lWB
lqB
m]B
m]B
m]B
m]B
ncB
n}B
ncB
ncB
ncB
ncB
o�B
oiB
oiB
oiB
o�B
poB
poB
poB
poB
poB
p�B
p�B
poB
q�B
qvB
raB
raB
r|B
r|B
r|B
r|B
r|B
raB
raB
raB
r|B
s�B
s�B
s�B
shB
tn11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.58(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202101080035462021010800354620210108003546202306231726552023062317265520230623172655202101090031432021010900314320210109003143  JA  ARFMdecpA19c                                                                20210103153916  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210103063927  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210103063928  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210103063928  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210103063928  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210103063928  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210103063928  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210103063928  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210103063929  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210103063929                      G�O�G�O�G�O�                JA  ARUP                                                                        20210103065157                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20210103153648  CV  JULD            G�O�G�O�Fʞ                JM  ARCAJMQC2.0                                                                 20210107153546  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210107153546  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2019V1                                                       20210108153143  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082655  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041504                      G�O�G�O�G�O�                