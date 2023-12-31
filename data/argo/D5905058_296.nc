CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-02-15T18:40:21Z creation;2021-02-15T18:40:23Z conversion to V3.1;2023-06-29T05:47:14Z update;     
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
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ά   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210215184021  20230705041505  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              (A   JA  I2_0675_296                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�^�1�b 1   @�^����@6�$�/�b����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D<��D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DL��DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��3D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@��A��A'\)AH��Ah��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB
=qB=qB=qB"=qB)�
B2=qB:=qBB=qBJ=qBR=qBZ=qBb=qBj=qBr=qBz=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �\C�\C�\C�\C�\C
�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C �\C"�\C$�\C&�\C(�\C*�\C,�\C.�\C0�\C2�\C4�\C6�\C8�\C:�\C<�\C>�\C@�\CB�\CD�\CF�\CH�\CJ�\CL�\CN�\CP�\CR�\CT�\CV�\CX�\CZ�\C\�\C^�\C`�\Cb�\Cd�\Cf�\Ch�\Cj�\Cl�\Cn�\Cp�\Cr�\Ct�\Cv�\Cx�\Cz�\C|�\C~�\C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�D #�D ��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D	#�D	��D
#�D
��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D #�D ��D!#�D!�qD"#�D"��D##�D#��D$#�D$�=D%#�D%��D&#�D&��D'#�D'��D(#�D(��D)#�D)��D*#�D*��D+#�D+��D,#�D,��D-#�D-��D.#�D.��D/#�D/��D0#�D0��D1#�D1��D2#�D2��D3#�D3��D4#�D4��D5#�D5��D6#�D6��D7#�D7��D8#�D8��D9#�D9��D:#�D:��D;#�D;��D<#�D<��D=qD=�qD>#�D>��D?#�D?��D@#�D@��DA#�DA��DB#�DB��DC#�DC��DD#�DD��DE#�DE��DF#�DF��DG#�DG��DH#�DH��DI#�DI��DJ#�DJ��DK#�DK��DL*=DL��DMqDM��DN#�DN��DO#�DO��DP#�DP��DQ#�DQ��DR#�DR��DS#�DS��DT#�DT��DU#�DU��DV#�DV��DW#�DW��DX#�DX��DY#�DY��DZ#�DZ��D[#�D[��D\#�D\��D]#�D]��D^#�D^��D_#�D_��D`#�D`��Da#�Da��Db#�Db��Dc#�Dc��Dd#�Dd��De#�De��Df#�Df��Dg#�Dg��Dh#�Dh��Di#�Di��Dj#�Dj��Dk#�Dk��Dl#�Dl��Dm#�Dm��Dn#�Dn��Do#�Do��Dp#�Dp��Dq#�Dq��Dr#�Dr��Ds#�Ds��Dt#�Dt��Du#�Du��Dv#�Dv��Dw#�Dw��Dx#�Dx��Dy#�Dy��Dz#�Dz��D{#�D{��D|#�D|��D}#�D}��D~#�D~��D#�D��D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D�D���D��D�Q�DÑ�D���D��D�Q�Dđ�D���D��D�Q�Dő�D���D��D�Q�DƑ�D���D��D�Q�DǑ�D���D��D�Q�Dȑ�D���D��D�Q�Dɑ�D���D��D�Q�Dʑ�D���D��D�Q�Dˑ�D���D��D�Q�D̑�D���D��D�Q�D͑�D���D��D�Q�DΑ�D���D��D�Q�Dϑ�D���D��D�Q�DБ�D���D��D�Q�Dё�D���D��D�Q�Dґ�D���D��D�Q�Dӑ�D���D��D�Q�Dԑ�D���D��D�Q�DՑ�D���D��D�Q�D֑�D���D��D�Q�Dב�D���D��D�Q�Dؑ�D���D��D�Q�Dّ�D���D��D�Q�Dڑ�D���D��D�Q�Dۑ�D���D��D�Q�Dܑ�D���D��D�Q�Dݑ�D���D��D�Q�Dޑ�D��D��D�Q�Dߑ�D���D��D�Q�D���D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A��/A��/A��/A��/A��/A��
A��#A��
A��/A��A��#A��/A��/A��HA��#A��A���A��
A���A���A��jA���A��uA��\A��PA��DA��A�p�A�hsA�hsA�ffA�hsA�hsA�jA�jA�jA�l�A�l�A�l�A�n�A�n�A�n�A�p�A�r�A�r�A�t�A�t�A�t�A�ffA�7LA��A��hA�Q�A��`A�~�A�ĜA���A�$�A�VA���A�1A�VA��^A�r�A�$�A���A�%A�M�A���A�S�A��/A��\A���A�~�A�A�n�A��A�XA��#A��A� �A���A�JA��A�ƨA�E�A��-A�A��/A���A�7LA�bA��A��A���A��9A�jA���A�JA��\A��+A���A�~�A�33A���A�S�A�O�A�+A�9XA��9A�|�A��A�VA���A�(�AhsAA~��A~�A{��Az$�Axv�At(�AqC�Ap�yAp�RAohsAm�7Al�Al  AjȴAg�mAf��AdI�AbE�A`5?A_O�A_%A^��A]AZ��AW�AT��ASx�AOS�AMVAJ  AH9XAF��AC�A@jA=�PA<  A;��A;oA8(�A7�A61'A5
=A2��A1t�A/33A.bA-��A,=qA+VA(��A'C�A'A&�9A&jA&VA&-A%��A$ĜA#VA!�A!K�A {AVA~�A�wA�A�AdZAM�AffAt�A�DA�;Ax�A��A�AE�A�
A7LAM�AI�A��A
�jA
A	�TA	XA�#A��A�
A�RA�7A"�A�RAI�A��Ap�A ��@���@�v�@�O�@�I�@��P@���@��;@��\@�S�@�v�@�$�@��@���@��
@��@�  @�;d@���@�Q�@�@旍@�Z@�$�@�z�@ߥ�@�ȴ@�J@܃@�I�@�l�@�G�@׍P@�$�@�z�@��m@ӶF@�p�@�1@�|�@�;d@��@Ώ\@��T@̴9@ʇ+@ɉ7@�/@��@ȋD@�+@Ɨ�@���@�hs@�Q�@�Q�@Õ�@���@�ff@�@�X@��`@��@��F@�l�@�~�@��@�%@���@�  @�
=@��#@�j@���@�S�@�C�@�
=@�$�@�X@��@�V@��/@��@�r�@�(�@��@���@�{@�hs@�&�@�bN@��P@��+@�M�@�@�7L@�Q�@�K�@�C�@�33@�ȴ@�@�hs@��@��9@�b@���@���@�=q@��@���@�7L@��/@��@���@�C�@��R@�E�@��h@�7L@��@��@�&�@�7L@���@���@�r�@�bN@��@��F@�dZ@�;d@��@��\@�E�@�J@��7@��@�z�@�j@�Z@�I�@�A�@�9X@�1'@� �@��m@��@�S�@�"�@��\@�$�@��@�J@��@��@�`B@�O�@�%@��@�A�@�1'@�1'@��@��m@�t�@�\)@�;d@�@���@��+@�V@�5?@�@���@��^@���@�hs@�G�@��@���@���@��@��/@���@��@�A�@� �@�(�@�1@��@��w@���@�K�@��@��y@�ȴ@��R@���@�n�@�V@��@���@��^@��-@���@�p�@�hs@�X@�O�@�/@��/@��@�j@�I�@�9X@�  @��;@���@�l�@�dZ@�S�@�o@���@���@�^5@��@��@�hs@��/@��9@��D@�z�@�j@�bN@�Q�@�I�@�(�@��@�b@��@�ƨ@��w@�t�@�;d@�
=@��@�~�@�V@�@�?}@���@��/@���@�r�@�bN@�I�@�A�@��@��w@��@�C�@�+@�o@�o@���@���@��+@�=q@�$�@���@��@�?}@�%@���@��@��@�Z@�  @���@�;d@���@��+@�ff@�V@�E�@�=q@�-@���@�7L@��@���@��@��j@�r�@�I�@�b@+@~��@~�+@~E�@}`B@}?}@|��@|��@|Z@|�@|1@{�
@{�F@{S�@{"�@z��@z=q@y�7@x��@x��@xA�@xb@w��@w�@w;d@w�@v��@v�R@v��@vv�@vE�@v{@u�-@u`B@u/@t�D@s��@s33@so@r�H@q�@qX@q7L@p��@pbN@p �@o�@o;d@n�+@nE�@n$�@n{@m�-@mp�@l��@l�@l�@k��@kS�@k@j��@jn�@i��@i�^@ix�@hĜ@hbN@h �@hb@g�;@gK�@f��@fV@e�@eO�@eV@d�j@d��@dz�@dZ@dI�@d�@c��@c�F@c�F@c��@c��@c��@c�@co@bn�@b�@a��@a%@`�9@`�u@`A�@`1'@`  @_�@_K�@_+@_+@_+@_+@_
=@^�y@^v�@^{@]�@]�h@]�@\�/@\�D@[��@[�F@[��@[S�@[@Z�\@ZM�@Z-@Y��@YG�@Y�@X��@X�u@Xr�@XbN@Xb@W��@WK�@W�@V��@Vȴ@Vff@V$�@V$�@V$�@V{@V{@V{@U�@T��@T9X@T(�@S�
@S��@R�@RM�@R�@QX@P��@P��@Pb@O+@N{@M��@M`B@M?}@L��@L�j@L�@L�D@LZ@L�@K�
@Kt�@K"�@K@J�@J��@J^5@J-@I�^@I%@H��@Hr�@H1'@Hb@G�@G\)@F�y@F�+@Fv�@FV@F$�@F@E@E/@D��@DZ@D1@C��@Cƨ@C��@Ct�@CC�@C@B�\@B~�@B^5@B=q@A�@A�7@@�9@@r�@@�@@�@@r�@@bN@@A�@@A�@@1'@@b@?�@?��@?��@?\)@?�@>�@>ff@=�T@=@=�h@=p�@=O�@<�@<�j@<Z@<9X@<�@;��@;S�@;o@:��@:�@9�@9�#@9�^@9��@9hs@9&�@8��@8�@8�@8bN@8A�@7�w@7K�@6��@6ȴ@6��@6��@6ff@6$�@5��@5�@5O�@5?}@5�@4��@4�@4��@4I�@3��@3ƨ@3t�@3"�@2�H@2M�@1��@1x�@1&�@0Ĝ@0bN@0 �@/�@/��@/��@/|�@/\)@/K�@/
=@.�R@.��@.�+@.V@-�T@-@-@-�-@-/@-V@,�/@,�@,��@,j@,Z@,(�@,1@+ƨ@+ƨ@+��@+t�@+C�@*�@*��@*��@*�!@*^5@)��@)��@)X@)�@(��@(�`@(Ĝ@(Q�@( �@'��@'��@'l�@'K�@'�@&ȴ@&�R@&V@&$�@%�T@%��@%?}@$��@$�@$j@$Z@$I�@#ƨ@#ƨ@#�F@#33@"�H@"��@"n�@"�@!�^@!x�@!�@ �`@ Ĝ@ �u@ Q�@   @�@��@|�@l�@�@��@��@ȴ@�+@E�@�T@�-@p�@?}@?}@�@��@�j@��@1@ƨ@ƨ@��@t�@C�@o@��@�!@�\@n�@-@��@�#@��@X@��@��@�u@�@A�@ �@b@�;@K�@�@�@�R@��@ff@{@@@�h@/@�/@�D@Z@(�@ƨ@��@t�@S�@"�@@��@^5@-@�@��@�^@7L@��@Ĝ@�9@�u@bN@1'@  @�@K�@�@�@
=@��@ȴ@�+@E�@�@�h@O�@?}@�@��@��@�@�D@I�@1@�F@�F@��@t�@C�@o@
�@
�\@
�\@
n�@
^5@
M�@
=q@
-@	��@	�@	��@	��@	�7@	X@	X@	G�@	7L@	�@	%@��@��@bN@Q�@A�@1'@ �@b@�@�@�P@K�@�@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A��/A��/A��/A��/A��/A��
A��#A��
A��/A��A��#A��/A��/A��HA��#A��A���A��
A���A���A��jA���A��uA��\A��PA��DA��A�p�A�hsA�hsA�ffA�hsA�hsA�jA�jA�jA�l�A�l�A�l�A�n�A�n�A�n�A�p�A�r�A�r�A�t�A�t�A�t�A�ffA�7LA��A��hA�Q�A��`A�~�A�ĜA���A�$�A�VA���A�1A�VA��^A�r�A�$�A���A�%A�M�A���A�S�A��/A��\A���A�~�A�A�n�A��A�XA��#A��A� �A���A�JA��A�ƨA�E�A��-A�A��/A���A�7LA�bA��A��A���A��9A�jA���A�JA��\A��+A���A�~�A�33A���A�S�A�O�A�+A�9XA��9A�|�A��A�VA���A�(�AhsAA~��A~�A{��Az$�Axv�At(�AqC�Ap�yAp�RAohsAm�7Al�Al  AjȴAg�mAf��AdI�AbE�A`5?A_O�A_%A^��A]AZ��AW�AT��ASx�AOS�AMVAJ  AH9XAF��AC�A@jA=�PA<  A;��A;oA8(�A7�A61'A5
=A2��A1t�A/33A.bA-��A,=qA+VA(��A'C�A'A&�9A&jA&VA&-A%��A$ĜA#VA!�A!K�A {AVA~�A�wA�A�AdZAM�AffAt�A�DA�;Ax�A��A�AE�A�
A7LAM�AI�A��A
�jA
A	�TA	XA�#A��A�
A�RA�7A"�A�RAI�A��Ap�A ��@���@�v�@�O�@�I�@��P@���@��;@��\@�S�@�v�@�$�@��@���@��
@��@�  @�;d@���@�Q�@�@旍@�Z@�$�@�z�@ߥ�@�ȴ@�J@܃@�I�@�l�@�G�@׍P@�$�@�z�@��m@ӶF@�p�@�1@�|�@�;d@��@Ώ\@��T@̴9@ʇ+@ɉ7@�/@��@ȋD@�+@Ɨ�@���@�hs@�Q�@�Q�@Õ�@���@�ff@�@�X@��`@��@��F@�l�@�~�@��@�%@���@�  @�
=@��#@�j@���@�S�@�C�@�
=@�$�@�X@��@�V@��/@��@�r�@�(�@��@���@�{@�hs@�&�@�bN@��P@��+@�M�@�@�7L@�Q�@�K�@�C�@�33@�ȴ@�@�hs@��@��9@�b@���@���@�=q@��@���@�7L@��/@��@���@�C�@��R@�E�@��h@�7L@��@��@�&�@�7L@���@���@�r�@�bN@��@��F@�dZ@�;d@��@��\@�E�@�J@��7@��@�z�@�j@�Z@�I�@�A�@�9X@�1'@� �@��m@��@�S�@�"�@��\@�$�@��@�J@��@��@�`B@�O�@�%@��@�A�@�1'@�1'@��@��m@�t�@�\)@�;d@�@���@��+@�V@�5?@�@���@��^@���@�hs@�G�@��@���@���@��@��/@���@��@�A�@� �@�(�@�1@��@��w@���@�K�@��@��y@�ȴ@��R@���@�n�@�V@��@���@��^@��-@���@�p�@�hs@�X@�O�@�/@��/@��@�j@�I�@�9X@�  @��;@���@�l�@�dZ@�S�@�o@���@���@�^5@��@��@�hs@��/@��9@��D@�z�@�j@�bN@�Q�@�I�@�(�@��@�b@��@�ƨ@��w@�t�@�;d@�
=@��@�~�@�V@�@�?}@���@��/@���@�r�@�bN@�I�@�A�@��@��w@��@�C�@�+@�o@�o@���@���@��+@�=q@�$�@���@��@�?}@�%@���@��@��@�Z@�  @���@�;d@���@��+@�ff@�V@�E�@�=q@�-@���@�7L@��@���@��@��j@�r�@�I�@�b@+@~��@~�+@~E�@}`B@}?}@|��@|��@|Z@|�@|1@{�
@{�F@{S�@{"�@z��@z=q@y�7@x��@x��@xA�@xb@w��@w�@w;d@w�@v��@v�R@v��@vv�@vE�@v{@u�-@u`B@u/@t�D@s��@s33@so@r�H@q�@qX@q7L@p��@pbN@p �@o�@o;d@n�+@nE�@n$�@n{@m�-@mp�@l��@l�@l�@k��@kS�@k@j��@jn�@i��@i�^@ix�@hĜ@hbN@h �@hb@g�;@gK�@f��@fV@e�@eO�@eV@d�j@d��@dz�@dZ@dI�@d�@c��@c�F@c�F@c��@c��@c��@c�@co@bn�@b�@a��@a%@`�9@`�u@`A�@`1'@`  @_�@_K�@_+@_+@_+@_+@_
=@^�y@^v�@^{@]�@]�h@]�@\�/@\�D@[��@[�F@[��@[S�@[@Z�\@ZM�@Z-@Y��@YG�@Y�@X��@X�u@Xr�@XbN@Xb@W��@WK�@W�@V��@Vȴ@Vff@V$�@V$�@V$�@V{@V{@V{@U�@T��@T9X@T(�@S�
@S��@R�@RM�@R�@QX@P��@P��@Pb@O+@N{@M��@M`B@M?}@L��@L�j@L�@L�D@LZ@L�@K�
@Kt�@K"�@K@J�@J��@J^5@J-@I�^@I%@H��@Hr�@H1'@Hb@G�@G\)@F�y@F�+@Fv�@FV@F$�@F@E@E/@D��@DZ@D1@C��@Cƨ@C��@Ct�@CC�@C@B�\@B~�@B^5@B=q@A�@A�7@@�9@@r�@@�@@�@@r�@@bN@@A�@@A�@@1'@@b@?�@?��@?��@?\)@?�@>�@>ff@=�T@=@=�h@=p�@=O�@<�@<�j@<Z@<9X@<�@;��@;S�@;o@:��@:�@9�@9�#@9�^@9��@9hs@9&�@8��@8�@8�@8bN@8A�@7�w@7K�@6��@6ȴ@6��@6��@6ff@6$�@5��@5�@5O�@5?}@5�@4��@4�@4��@4I�@3��@3ƨ@3t�@3"�@2�H@2M�@1��@1x�@1&�@0Ĝ@0bN@0 �@/�@/��@/��@/|�@/\)@/K�@/
=@.�R@.��@.�+@.V@-�T@-@-@-�-@-/@-V@,�/@,�@,��@,j@,Z@,(�@,1@+ƨ@+ƨ@+��@+t�@+C�@*�@*��@*��@*�!@*^5@)��@)��@)X@)�@(��@(�`@(Ĝ@(Q�@( �@'��@'��@'l�@'K�@'�@&ȴ@&�R@&V@&$�@%�T@%��@%?}@$��@$�@$j@$Z@$I�@#ƨ@#ƨ@#�F@#33@"�H@"��@"n�@"�@!�^@!x�@!�@ �`@ Ĝ@ �u@ Q�@   @�@��@|�@l�@�@��@��@ȴ@�+@E�@�T@�-@p�@?}@?}@�@��@�j@��@1@ƨ@ƨ@��@t�@C�@o@��@�!@�\@n�@-@��@�#@��@X@��@��@�u@�@A�@ �@b@�;@K�@�@�@�R@��@ff@{@@@�h@/@�/@�D@Z@(�@ƨ@��@t�@S�@"�@@��@^5@-@�@��@�^@7L@��@Ĝ@�9@�u@bN@1'@  @�@K�@�@�@
=@��@ȴ@�+@E�@�@�h@O�@?}@�@��@��@�@�D@I�@1@�F@�F@��@t�@C�@o@
�@
�\@
�\@
n�@
^5@
M�@
=q@
-@	��@	�@	��@	��@	�7@	X@	X@	G�@	7L@	�@	%@��@��@bN@Q�@A�@1'@ �@b@�@�@�P@K�@�@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�'B�^B��B�B�BB�BB�#B�)B�B��BBB+B
=BVB�B"�B$�B&�B+B/B1'B0!B1'B5?B6FB49B49B5?B0!B)�B%�B'�B+B(�B$�B�B�B1B��B��B��B�B�5B�5B�/B�/B�#B�B��B��B�}B�RB��B�PBr�BM�B-B�B
��B
�mB
��B
�}B
��B
�+B
t�B
]/B
E�B
=qB
:^B
6FB
33B
$�B
�B
	7B	��B	�B	�B	��B	��B	B	�XB	�3B	�B	��B	�oB	�B	x�B	k�B	dZB	aHB	_;B	\)B	F�B	49B	$�B	�B	DB��B�B�`B�5B��B�}B�?B�B�B��B��B��B�uB�PB�=B�B}�By�Bw�Bt�Bo�Bm�BhsBgmBgmBiyBiyBiyBhsBffBaHBaHB_;B`BB^5B\)B\)B[#B[#BXBW
BT�BR�BP�BM�BK�BI�BF�BC�BB�B@�B>wB=qB;dB:^B8RB7LB7LB6FB49B2-B2-B1'B0!B0!B/B0!B2-B6FB:^B<jB;dB<jB<jB9XB5?B7LB8RB7LB8RB9XB9XB9XB;dB>wBF�BB�BC�BE�BC�BA�B<jB9XB9XB:^B=qB?}BJ�BT�BT�BO�BL�BM�BO�BW
BW
BR�BR�BR�BS�BT�BW
BYB\)B[#B\)B\)B_;BhsBn�Bo�Bn�Bl�Bl�Bn�Bp�Bp�Bq�Bs�Bt�By�Bz�B{�B~�B�B�%B�1B�VB�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�9B�9B�FB�FB�^BÖBŢBǮB��B��B��B��B��B�B�#B�5B�HB�NB�TB�ZB�mB�B�B�B��B��B��B	B	B	B	1B	JB	bB	uB	�B	�B	�B	"�B	%�B	&�B	)�B	-B	.B	0!B	49B	7LB	=qB	=qB	?}B	@�B	A�B	A�B	B�B	B�B	E�B	G�B	J�B	L�B	Q�B	T�B	T�B	T�B	VB	ZB	[#B	_;B	aHB	e`B	gmB	hsB	jB	k�B	l�B	m�B	m�B	o�B	q�B	t�B	v�B	w�B	y�B	|�B	~�B	� B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�7B	�=B	�PB	�PB	�PB	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�3B	�?B	�?B	�?B	�LB	�XB	�^B	�jB	�qB	�wB	��B	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�`B	�`B	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
	7B
	7B
	7B
	7B
DB
DB
DB
JB
PB
JB
PB
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
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
 �B
 �B
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
%�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
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
1'B
0!B
.B
/B
/B
/B
0!B
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
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
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
;dB
;dB
;dB
;dB
;dB
<jB
<jB
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
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
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
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
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
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
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
W
B
XB
XB
XB
XB
XB
YB
YB
YB
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
[#B
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
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
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
jB
jB
jB
jB
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
o�B
o�B
p�B
p�B
p�B
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
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�"B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�AB��B�dBچB��B�B�jB�BB�[B��B�B%B�B0B�B�B#nB%�B(>B,WB0;B1�B1�B4B88B7LB6FB7�B9�B1[B+B'�B)�B+�B*�B'8BIB+B	lB�.B��B��B�BބB�5B�B�IB�qBںB�B͟B�B�B��B��Bw�BQhB0�B �B
��B
�B
��B
ðB
�!B
�rB
xRB
_�B
F�B
=�B
:�B
7fB
5tB
'B
B
jB	�8B	��B	�mB	ԕB	͟B	ÖB	�DB	�B	��B	��B	�B	�YB	z�B	lWB	d�B	a�B	a-B	_�B	JrB	6�B	'mB	B	"B	 OB��B�B�hB�B�uB��B��B�=B��B��B�1B�2B��B��B�YB.Bz�ByrBv`Br-Bn�Bh�Bg�Bg�BiyBi�Bj0Bi�Bh>Bb�BbB`�BaHB_B]/B]�B]/B\�BY�BX�BVBS�BQ�BNpBL�BK^BGzBDBC{BA�B@iB>(B<PB:�B8�B8RB8�B7�B5ZB3�B3hB1�B0�B0�B/iB0�B3B7B;B="B;�B=B=�B:^B6`B8�B8�B7�B8�B9�B:B:^B<�B?.BG�BB�BC�BF�BD�BB�B=VB9�B9�B:�B>B?�BK^BV9BVBP�BM�BNBP.BX+BW�BS&BSBS&BTFBUgBW�BZB\�B[=B\)B\�B_�Bh�Bn�Bo�Bn�Bl�Bl�Bo Bp�Bp�Bq�Bs�Bu%By�Bz�B|PBcB�AB�?B��B��B�B� B��B��B��B��B��B��B��B�xB��B��B��B��B�B�@B�B�B�B�kB�}B��B�TB�nB�zB��B��B�aBňB��B�)B��B��B�B�MBٚB�#B�5B�HB�NB�B�B�B��B�B��B��B�	B��B	�B	�B	�B	�B	JB	HB	[B	�B	�B	�B	"�B	%�B	&�B	)�B	-B	./B	0;B	4nB	7fB	=<B	=VB	?HB	@iB	AUB	AUB	B[B	BuB	E�B	G�B	J�B	L�B	Q�B	T�B	T�B	T�B	VB	Y�B	[	B	_!B	aHB	e`B	g8B	h>B	jKB	k�B	l�B	m]B	mwB	o�B	q�B	t�B	v�B	w�B	y�B	|�B	~�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�=B	�B	�B	�6B	�(B	�bB	�hB	�uB	��B	�eB	�qB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	��B	��B	��B	��B	��B	�B	�B	��B	�%B	�B	�%B	�2B	�$B	�DB	�jB	�VB	��B	��B	āB	�mB	�tB	ǔB	ǔB	�zB	�zB	ȀB	ȚB	ɆB	ɠB	ʌB	ʦB	ˬB	̳B	͹B	οB	��B	��B	��B	�FB	�B	��B	��B	�B	��B	��B	��B	�B	�/B	�B	�!B	�'B	�'B	�B	�B	�-B	�NB	�:B	�,B	�FB	�`B	�RB	�XB	�_B	�KB	�eB	�B	�B	�B	�B	�B	�B	�oB	�B	�vB	�vB	�vB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
B
+B
1B
	B
	B
	7B
	RB
DB
)B
B
0B
B
0B
jB
<B
(B
(B
(B
BB
.B
HB
NB
hB
TB
:B
[B
@B
[B
aB
aB
MB
gB
SB
SB
YB
YB
sB
�B
yB
�B
B
kB
kB
�B
kB
kB
kB
�B
qB
qB
WB
qB
WB
�B
qB
�B
�B
�B
�B
�B
~B
�B
�B
�B
�B
 �B
 �B
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
%�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-)B
/ B
.�B
.�B
/ B
/B
0B
0B
0;B
1B
0�B
1'B
1'B
0!B
-�B
.�B
/ B
/ B
/�B
/�B
/�B
0B
0�B
0�B
1'B
1�B
1�B
2B
1�B
3B
2�B
3B
3B
4B
4B
4B
4B
4B
49B
5%B
5B
5B
5%B
6+B
6+B
6B
6FB
6+B
72B
7B
7B
8B
8B
8B
8B
8B
9>B
9>B
9$B
9$B
9XB
:DB
:^B
;JB
;B
;B
;B
;B
<B
<6B
<6B
<6B
<6B
<6B
<6B
<6B
=<B
=VB
=VB
=qB
>BB
>BB
>BB
>]B
>BB
>BB
?HB
?HB
?HB
?}B
@OB
@OB
@�B
AoB
AUB
BAB
B[B
BuB
B[B
B[B
CaB
D�B
DMB
DgB
DgB
D�B
E�B
EmB
FtB
FtB
FtB
FtB
FtB
F�B
GzB
GzB
G_B
GzB
GzB
HfB
GzB
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
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
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
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
T�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
W
B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[	B
Z�B
[	B
Z�B
[	B
\B
[�B
[�B
[�B
[�B
\�B
\�B
]B
\�B
]B
\�B
]B
^B
]�B
^B
^B
^B
^B
^B
_B
_!B
_B
_B
_B
`'B
`B
`B
`'B
a-B
aB
a-B
aB
bB
bB
b4B
b4B
bB
c B
c B
c B
c:B
c B
d@B
dB
d&B
d&B
d&B
eFB
e,B
e,B
f2B
f2B
fLB
fLB
f2B
f2B
f2B
g8B
g8B
g8B
gRB
g8B
hsB
h>B
i*B
i*B
i_B
iDB
iDB
iDB
iDB
jKB
jeB
jKB
jKB
jKB
jeB
kkB
kQB
kQB
lWB
m]B
mCB
m]B
m]B
ncB
mwB
m]B
m]B
m]B
n}B
nIB
oiB
oiB
o�B
p�B
p�B
oiB
oOB
pUB
pUB
pUB
qvB
qvB
qvB
q[B
q�B
qvB
r|B
r�B
r|B
r|B
raB
r|B
raB
r|B
s�B
s�B
t�B
tnB
tnB
t�B
tnB
t�B
t�B
t�B
u�B
u�B
utB
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.56(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202103160938522021031609385220210316093852202306231727442023062317274420230623172744202103180027042021031800270420210318002704  JA  ARFMdecpA19c                                                                20210216034019  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210215184021  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210215184022  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210215184022  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210215184023  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210215184023  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210215184023  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210215184023  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210215184023  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210215184023                      G�O�G�O�G�O�                JA  ARUP                                                                        20210215185306                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20210216153334  CV  JULD_LOCATION   G�O�G�O�F��&                JM  ARGQJMQC2.0                                                                 20210216153334  CV  LATITUDE        G�O�G�O�A���                JM  ARGQJMQC2.0                                                                 20210216153334  CV  LONGITUDE       G�O�G�O���o                JM  ARCAJMQC2.0                                                                 20210316003852  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210316003852  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2019V1                                                       20210317152704  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082744  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041505                      G�O�G�O�G�O�                